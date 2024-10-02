{-# LANGUAGE GADTs #-}


module Main where


import Prelude hiding            ( lookup )

import System.Environment        ( getArgs )
import System.Directory          ( doesFileExist, listDirectory ) 

import Text.Megaparsec.Error     ( errorBundlePretty )

import Control.Monad.State
   ( StateT, MonadState(get), gets, modify, evalStateT )
import Control.Monad.Reader      ( ReaderT(runReaderT), asks )
import Control.Monad.Identity ( Identity(runIdentity) )

import qualified Data.Map as M

import AST
import Parser                    ( runParse )

import qualified VM.AST as V 
import VM.VM ( vmToAsm ) 

import ASM.Assembler ( checkEval )


type Filename  = String
type Classname = String
type VarCounts = (Int,Int,Int,Int)
type Ctx       = M.Map String Var


-- State:
-------------------------------------------------------------------------------
type CompUnit = ReaderT (Filename,Classname) (StateT (Ctx,VarCounts,V.Instruction,Int) Identity)


-- Read filename and classname:
askF,askC :: CompUnit String
askF = asks fst
askC = asks snd

-- Insert and lookup to variable context:
insert :: String -> Var -> CompUnit ()
insert s v = modify (\(ctx,i,ins,n) -> (M.insert s v ctx,i,ins,n))

lookup :: String -> CompUnit (Maybe Var)
lookup s = gets (M.lookup s . (\(ctx,_,_,_) -> ctx))

getCtx :: CompUnit Ctx
getCtx = gets (\(ctx,_,_,_) -> ctx)

-- Lookup and increment variable counts:
getF, getS, getA, getV :: CompUnit Int
getF = gets (\(_,(f,_,_,_),_,_) -> f)
getS = gets (\(_,(_,s,_,_),_,_) -> s)
getA = gets (\(_,(_,_,a,_),_,_) -> a)
getV = gets (\(_,(_,_,_,v),_,_) -> v)

incF, incS, incA, incV :: CompUnit ()
incF = modify (\(ctx,(f,s,a,v),ins,n) -> (ctx,(f+1,s,a,v),ins,n))
incS = modify (\(ctx,(f,s,a,v),ins,n) -> (ctx,(f,s+1,a,v),ins,n))
incA = modify (\(ctx,(f,s,a,v),ins,n) -> (ctx,(f,s,a+1,v),ins,n))
incV = modify (\(ctx,(f,s,a,v),ins,n) -> (ctx,(f,s,a,v+1),ins,n))

-- Instruction stack:
push :: V.Command -> CompUnit ()
push c = modify (\(ctx,i,ins,n) -> (ctx,i,c:ins,n))

vmStack :: CompUnit V.Instruction
vmStack = gets (\(_,_,ins,_) -> ins)

-- Lookup and increment label count:
getL :: CompUnit Int
getL = gets (\(_,_,_,n) -> n)

incL :: CompUnit ()
incL = modify (\(ctx,i,ins,n) -> (ctx,i,ins,n+1))


-- Compilation:
-------------------------------------------------------------------------------
compile :: String -> [Class] -> V.Instruction
compile s = (:) (V.Func (V.Call "Sys.init" 0)) . reverse . concatMap (compileClass s)

compileClass :: String -> Class -> V.Instruction
compileClass file (Class name subDecls) = compUnit file name subDecls


runCompUnit :: Filename -> Classname -> CompUnit t -> t
runCompUnit f c t = runIdentity (evalStateT (runReaderT t (f,c)) (M.empty,(0,0,0,0),[],0))

compUnit :: Filename -> Classname -> ClassDecl -> V.Instruction
compUnit f c = runCompUnit f c . compileJack

compileJack :: ClassDecl -> CompUnit V.Instruction
compileJack cd = compileClassDecl cd *> vmStack

-- Class-level declarations:
compileClassDecl :: ClassDecl -> CompUnit ()
compileClassDecl (ClassDecl vs sr) = mapM_ compileKindDecl vs *> compileSubroutines sr


-- Compiling class-level variables: 
compileKindDecl :: VarDecl -> CompUnit ()
compileKindDecl (Static t v)   = mapM_ (insertVar KStatic t) v
compileKindDecl (Field t v)    = mapM_ (insertVar KField t) v

insertVar :: Kind -> Type -> String -> CompUnit ()
insertVar KVar t v      = getV >>= \n -> incV *> insert v (Var t KVar n)
insertVar KArg t v      = getA >>= \n -> incA *> insert v (Var t KArg n)
insertVar KField t v    = getF >>= \n -> incF *> insert v (Var t KField n)
insertVar KStatic t v   = getS >>= \n -> incS *> insert v (Var t KStatic n)


-- Compiling subroutines:
compileSubroutines :: [Subroutine] -> CompUnit () 
compileSubroutines = mapM_ (\sd -> get >>= \(ctx,(f,s,_,_),_,_) 
   -> compileSubroutine sd *> modify (\(_,_,ins,n) -> (ctx,(f,s,0,0),ins,n)))

compileSubroutine :: Subroutine -> CompUnit ()
compileSubroutine (Method t s mp (SBody v stmts))        = do
   cName <- askC
   insertVar KArg (TClass cName) "this"
   compileArgs mp 
   compileHead (Just t) v s
   compileBody v stmts

compileSubroutine (Constructor _ s mp (SBody v stmts))   = do
   compileArgs mp 
   compileHead Nothing v s 
   i <- getF 
   mapM_ push [V.Push V.Constant i,V.Func (V.Call "Memory.alloc" 1),V.Pop V.Pointer 0]
   compileBody v stmts 

compileSubroutine (Function _ s mp (SBody v stmts))      = 
   compileArgs mp 
   *> compileHead Nothing v s
   *> compileBody v stmts

compileArgs :: Maybe ParameterList -> CompUnit ()
compileArgs mp = case mp of 
   Just (ParameterList args)  -> mapM_ (\(ParameterType t' s') -> insertVar KArg t' s') args
   Nothing                    -> return ()

compileHead :: Maybe Type -> [VarDec] -> String -> CompUnit ()
compileHead (Just _) v s = do 
   let i = foldr (\(VarDec _ vs) x -> x + length vs) 0 v
   fileName <- askF
   push $ V.Func (V.Declare (fileName ++ "." ++ s) i)
   mapM_ push [V.Push V.Argument 0, V.Pop V.Pointer 0]
compileHead _ v s = do 
   let i = foldr (\(VarDec _ vs) x -> x + length vs) 0 v
   fileName <- askF
   push $ V.Func (V.Declare (fileName ++ "." ++ s) i)


compileBody :: [VarDec] -> [Stmt] -> CompUnit ()
compileBody v stmts = do 
   mapM_ (\(VarDec t vars) -> mapM_ (insertVar KVar t) vars) v
   mapM_ compileStmt stmts


-- Compiling statements:
compileStmt :: Stmt -> CompUnit ()
compileStmt (Do s)               = compileSubCall s *> push (V.Pop V.Temp 0)

compileStmt (Let s Nothing e)    = do
   mVar <- lookup s
   case mVar of
      Nothing           -> return ()
      Just (Var _ k i)  -> compileExpr e *> push (V.Pop (kindToSeg k) i)

compileStmt (Let s (Just e1) e2) = do
   mVar <- lookup s
   case mVar of
      Nothing           -> return ()
      Just (Var _ k i)  -> do
         compileExpr e1
         push (V.Push (kindToSeg k) i)
         compileOp Add
         compileExpr e2
         mapM_ push [V.Pop V.Temp 0,V.Pop V.Pointer 1,V.Push V.Temp 0,V.Pop V.That 0]

compileStmt (Return me)          = case me of
   Nothing -> push (V.Push V.Constant 0) *> push (V.Func V.Return)
   Just e  -> compileExpr e *> push (V.Func V.Return)

compileStmt (If e s Nothing)     = do
   compileExpr e
   l <- getL
   incL
   push (V.Branch (V.IfGoto ("IF_TRUE" ++ show l)))
   push (V.Branch (V.Goto ("IF_FALSE" ++ show l)))
   push (V.Branch (V.VMLabel ("IF_TRUE" ++ show l)))
   mapM_ compileStmt s
   push (V.Branch (V.VMLabel ("IF_FALSE" ++ show l)))

compileStmt (If e s1 (Just s2))  = do
   compileExpr e
   l <- getL
   incL
   push (V.Branch (V.IfGoto ("IF_TRUE" ++ show l)))
   push (V.Branch (V.Goto ("IF_FALSE" ++ show l)))
   push (V.Branch (V.VMLabel ("IF_TRUE" ++ show l)))
   mapM_ compileStmt s1
   push (V.Branch (V.Goto ("IF_END" ++ show l)))
   push (V.Branch (V.VMLabel ("IF_FALSE" ++ show l)))
   mapM_ compileStmt s2
   push (V.Branch (V.VMLabel ("IF_END" ++ show l)))

compileStmt (While e s)          = do
   l <- getL
   incL
   push (V.Branch (V.VMLabel ("WHILE_EXP" ++ show l)))
   compileExpr e
   compileUn Not
   push (V.Branch (V.IfGoto ("WHILE_END" ++ show l)))
   mapM_ compileStmt s
   push (V.Branch (V.Goto ("WHILE_EXP" ++ show l)))
   push (V.Branch (V.VMLabel ("WHILE_END" ++ show l)))


-- Compiling expressions and terms:
compileExpr :: Expr -> CompUnit ()
compileExpr (Term t)       = compileTerm t
compileExpr (Bin op e1 e2) = compileExpr e1 *> compileExpr e2 *> compileOp op
compileExpr (Un op e)      = compileExpr e *> compileUn op

compileOp :: Op -> CompUnit ()
compileOp Add  = push (V.ACL (V.Arith V.Add))
compileOp Sub  = push (V.ACL (V.Arith V.Sub))
compileOp Eq   = push (V.ACL (V.VMComp V.Eq))
compileOp Lt   = push (V.ACL (V.VMComp V.Lt))
compileOp Gt   = push (V.ACL (V.VMComp V.Gt))
compileOp Or   = push (V.ACL (V.Logic V.Or))
compileOp And  = push (V.ACL (V.Logic V.And))
compileOp Mul  = push (V.Func (V.Call "Math.multiply" 2))
compileOp Div  = push (V.Func (V.Call "Math.divide" 2))

compileUn :: Un -> CompUnit ()
compileUn Neg = push (V.ACL (V.Arith V.Neg))
compileUn Not = push (V.ACL (V.Logic V.Not))


compileTerm :: Term -> CompUnit ()
compileTerm (Literal l)       = compileLiteral l
compileTerm (TVar v Nothing)  = do
   ctx <- getCtx
   v' <- lookup v
   case v' of
      Nothing              -> error $ show ctx
      Just (Var _ k i)     -> push (V.Push (kindToSeg k) i)

compileTerm (TVar v (Just e)) = do
   v' <- lookup v
   case v' of
      Nothing           -> return ()
      Just (Var _ k i)  -> do
         compileExpr e
         push (V.Push (kindToSeg k) i)
         compileOp Add
         mapM_ push [V.Pop V.Pointer 1,V.Push V.That 0]

compileTerm (Expr e)          = compileExpr e
compileTerm (TermSubCall sc)  = compileSubCall sc

kindToSeg :: Kind -> V.Segment
kindToSeg KVar    = V.Local
kindToSeg KArg    = V.Argument
kindToSeg KField  = V.This
kindToSeg KStatic = V.Static

compileLiteral :: Literal -> CompUnit ()
compileLiteral (Int i)        = push (V.Push V.Constant i)
compileLiteral (String s)     = do
   push (V.Push V.Constant (length s))
   push (V.Func (V.Call "String.new" 1))
   mapM_ (mapM push . (\c -> [V.Push V.Constant (fromEnum c), V.Func (V.Call "String.appendChar" 2)])) s
compileLiteral (Bool True)    = push (V.Push V.Constant 0) *> compileUn Not
compileLiteral (Bool False)   = push (V.Push V.Constant 0)
compileLiteral Null           = push (V.Push V.Constant 0)
compileLiteral This           = push (V.Push V.Pointer 0)

compileSubCall :: SubCall -> CompUnit ()
compileSubCall (SubCall Nothing s e)   = do
   cName <- askC
   push (V.Push V.Pointer 0)
   mapM_ compileExpr e 
   push (V.Func (V.Call (cName ++ "." ++ s) 1))
compileSubCall (SubCall (Just x) s e)  = do
   mv <- lookup x
   case mv of
      Just (Var (TClass c) KVar n)  -> do
         push (V.Push V.Local n)
         mapM_ compileExpr e
         push (V.Func (V.Call (c ++ "." ++ s) (length e + 1)))
      Just (Var (TClass c) _ n)     -> do
         push (V.Push V.This n)
         mapM_ compileExpr e
         push (V.Func (V.Call (c ++ "." ++ s) (length e + 1)))
      _                             -> do
         mapM_ compileExpr e
         push (V.Func (V.Call (x ++ "." ++ s) (length e)))


-- Main:
-------------------------------------------------------------------------------
main :: IO ()
main = do
   (arg:_)  <- getArgs
   inpBool <- doesFileExist arg 
   if inpBool 
   then do 
      text <- readFile arg
      case runParse text of
         Left e   -> error $ errorBundlePretty e
         Right cu ->
            let 
               path = after '.' (reverse arg)
               name = removeFilepath path ""
               asm = checkEval $ vmToAsm (compile name cu) name
            in writeFile (reverse path ++ ".hack") (unlines asm)
   else do 
      dir <- listDirectory arg
      let dir' = filter (\x -> take 5 (reverse x) == "kcaj.") dir
      cu <- parseDir dir' arg
      let 
         asm = checkEval $ vmToAsm (compile "Main" cu) "Main"
         in writeFile (arg ++ "Main.hack") (unlines asm)

parseDir :: [String] -> String -> IO [Class] 
parseDir [] _           = return [] 
parseDir (f:rest) root  = do 
   f' <- readFile (root ++ f) 
   case runParse f' of 
      Left err -> error $ errorBundlePretty err 
      Right cu -> (++) cu <$> parseDir rest root

before :: Char -> String -> String 
before _ []    = []
before c (h:t) 
   | c == h    = [] 
   | otherwise = h : before c t

after :: Char -> String -> String
after _ []       = []
after s (x:name)
   | s == x    = name
   | otherwise = after s name

removeFilepath :: String -> String -> String
removeFilepath []       name = name
removeFilepath ('/':_)  name = name
removeFilepath ('\\':_) name = name
removeFilepath (x:rest) name = removeFilepath rest (x : name)

