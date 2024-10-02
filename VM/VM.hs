{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTs #-}


module VM.VM ( vmToAsm ) where


import VM.AST
import ASM.AST
import Control.Monad.State 
import Control.Monad.Identity 


-- Compiling virtual machine to assembly:
-------------------------------------------------------------------------------
type VM = StateT ([Assembly],String,String,Int) Identity

runVM :: VM t -> String -> t
runVM p file = runIdentity $ evalStateT p ([],"",file,0)

pushAsm :: Assembly -> VM ()
pushAsm a = modify (\(asm,f,file,i) -> (a:asm,f,file,i))

asmStack :: VM [Assembly]
asmStack = gets (\(asm,_,_,_) -> asm)

getFunc :: VM String
getFunc = gets (\(_,f,_,_) -> f)

putFunc :: String -> VM ()
putFunc f = modify (\(asm,_,file,i) -> (asm,f,file,i))

getFile :: VM String 
getFile = gets (\(_,_,file,_) -> file)

inc :: VM ()
inc = modify (\(asm,f,file,i) -> (asm,f,file,i+1))

count :: VM Int
count = gets (\(_,_,_,i) -> i)

vmToAsm :: Instruction -> String -> [Assembly]
vmToAsm = runVM . compileVM


compileVM :: Instruction -> VM [Assembly]
compileVM ins = do 
   mapM_ compileCommand ins 
   asm <- asmStack
   file <- getFile 
   -- This wasn't a very clean way to add this stuff in, but it's functional
   return $ [AInstr (Left 256),CInstr (CDest DestD CompA),AInstr (Right "SP"),CInstr (CDest DestM CompD)] 
      ++ reverse ([CInstr (CJump CompZero JMP), AInstr (Right (file ++ ".LOOP")),Label (file ++ ".LOOP")] ++ asm)

compileCommand :: Command -> VM ()
compileCommand (ACL c)       = compileACL c
compileCommand (Branch b)    = compileBranch b
compileCommand (Func f)      = compileFunc f
compileCommand (Push seg i)  = compilePushSeg seg i False *> pushPattern
compileCommand (Pop seg i)   =
   case seg of
      Pointer -> compilePopSeg seg i
      Temp    -> compilePopSeg seg i
      _       -> compilePopSeg seg i *> popPattern


compilePushSeg :: Segment -> Int -> Bool -> VM ()
compilePushSeg Constant i _ = mapM_ pushAsm 
   [  AInstr (Left i)
   ,  CInstr (CDest DestD CompA)
   ]
compilePushSeg Pointer i _
   | i == 0 = mapM_ pushAsm [AInstr (Right "THIS"),CInstr (CDest DestD CompM)]
   | i == 1 = mapM_ pushAsm [AInstr (Right "THAT"),CInstr (CDest DestD CompM)]
compilePushSeg Temp i _     = mapM_ pushAsm [AInstr (Left (i+16)),CInstr (CDest DestD CompM)]
compilePushSeg Static i _   = do 
   file <- getFile 
   mapM_ pushAsm 
      [  AInstr (Right (file ++ "." ++ show i))
      ,  CInstr (CDest DestD CompM) 
      ]
compilePushSeg seg i b
   | b         = mapM_ pushAsm [AInstr (Right (showSeg seg)),CInstr (CDest DestD CompM)]
   | otherwise = mapM_ pushAsm 
      [  AInstr (Right (showSeg seg))
      ,  CInstr (CDest DestD CompM)
      ,  AInstr (Left i)
      ,  CInstr (CDest DestA CompDPA)
      ]

pushPattern :: VM ()
pushPattern = mapM_ pushAsm 
   [ AInstr (Right "SP")
   , CInstr (CDest DestA CompM)
   , CInstr (CDest DestM CompD)
   , AInstr (Right "SP")
   , CInstr (CDest DestM CompMPOne)
   ]


compilePopSeg :: Segment -> Int -> VM ()
compilePopSeg Constant _ = error "Constant is not a valid segment for pop commands."
compilePopSeg Pointer i
   | i == 0 = mapM_ pushAsm 
      [  AInstr (Right "SP")
      ,  CInstr (CDest DestAM CompMMOne)
      ,  CInstr (CDest DestD CompM)
      ,  AInstr (Left 3)
      ,  CInstr (CDest DestM CompD)
      ]
   | i == 1 = mapM_ pushAsm 
      [  AInstr (Right "SP")
      ,  CInstr (CDest DestAM CompMMOne)
      ,  CInstr (CDest DestD CompM)
      ,  AInstr (Left 4)
      ,  CInstr (CDest DestM CompD)
      ]
compilePopSeg Temp i     = mapM_ pushAsm 
   [  AInstr (Right "SP")
   ,  CInstr (CDest DestAM CompMMOne)
   ,  CInstr (CDest DestD CompM)
   ,  AInstr (Left (i+5))
   ,  CInstr (CDest DestM CompD)
   ]
compilePopSeg Static i   = do 
   file <- getFile 
   mapM_ pushAsm [AInstr (Right (file ++ "." ++ show i)),CInstr (CDest DestD CompA)]
compilePopSeg seg i      = mapM_ pushAsm 
   [  AInstr (Right (showSeg seg))
   ,  CInstr (CDest DestD CompM)
   ,  AInstr (Left i)
   ,  CInstr (CDest DestD CompDPA)
   ]

popPattern :: VM ()
popPattern = mapM_ pushAsm 
   [  AInstr (Right "R15")
   ,  CInstr (CDest DestM CompD)
   ,  AInstr (Right "SP")
   ,  CInstr (CDest DestAM CompMMOne)
   ,  CInstr (CDest DestD CompM)
   ,  AInstr (Right "R15")
   ,  CInstr (CDest DestA CompM)
   ,  CInstr (CDest DestM CompD)
   ]

showSeg :: Segment -> String
showSeg Local    = "LCL"
showSeg Argument = "ARG"
showSeg This     = "THIS"
showSeg That     = "THAT"
showSeg q        = error $ show q ++ " is a special segment that requires special showing."


-- ACL commands:
compileACL :: ACL -> VM ()
compileACL (Arith Neg) = compileUn *> compileArith Neg
compileACL (Arith a)   = compileBin *> compileArith a
compileACL (VMComp c)  = compileBin *> compileComp c
compileACL (Logic Not) = compileUn *> compileLogic Not
compileACL (Logic l)   = compileBin *> compileLogic l

compileArith :: Arith -> VM ()
compileArith Add = pushAsm $ CInstr (CDest DestM CompDPM) -- need to allow for MPD
compileArith Sub = pushAsm $ CInstr (CDest DestM CompMMD)
compileArith Neg = pushAsm $ CInstr (CDest DestM CompMM)

compileComp :: VMComp  -> VM ()
compileComp comp = do
   i <- count
   let i' = i+1
   inc
   mapM_ pushAsm 
      [  CInstr (CDest DestD CompMMD)
      ,  AInstr (Right (showComp comp ++ show i))
      ,  CInstr (CJump CompD (showJump comp))
      ,  AInstr (Right "SP")
      ,  CInstr (CDest DestA CompMMOne)
      ,  CInstr (CDest DestM CompMOne)
      ,  AInstr (Right ("END" ++ show i'))
      ,  CInstr (CJump CompZero JMP)
      ,  Label (showComp comp ++ show i)
      ,  AInstr (Right "SP")
      ,  CInstr (CDest DestA CompMMOne)
      ,  CInstr (CDest DestM CompZero)
      ,  Label ("END" ++ show i')
      ]

showComp :: VMComp -> String
showComp Eq = "EQ"
showComp Lt = "LT"
showComp Gt = "GT"

showJump :: VMComp -> Jump
showJump Eq = JNE
showJump Lt = JGE
showJump Gt = JLE

compileLogic :: Logic -> VM ()
compileLogic And = pushAsm (CInstr (CDest DestM CompDNM)) -- need to allow for M&D
compileLogic Or  = pushAsm (CInstr (CDest DestM CompDOM)) -- need to allow for MOD
compileLogic Not = pushAsm (CInstr (CDest DestM CompNM))

compileBin :: VM ()
compileBin = mapM_ pushAsm 
   [  AInstr (Right "SP")
   ,  CInstr (CDest DestAM CompMMOne)
   ,  CInstr (CDest DestD CompM)
   ,  CInstr (CDest DestA CompAMOne)
   ]

compileUn :: VM () 
compileUn = mapM_ pushAsm [AInstr (Right "SP"),CInstr (CDest DestA CompMMOne)]


-- Branches:
compileBranch :: Branch -> VM ()
compileBranch b = do
   func <- getFunc
   mapM_ pushAsm $ compileBranch' func b

compileBranch' :: String -> Branch -> [Assembly]
compileBranch' f (VMLabel l) = [Label (f ++ "$" ++ l)]
compileBranch' f (Goto l)    = [AInstr (Right (f ++ "$" ++ l)),CInstr (CJump CompZero JMP)]
compileBranch' f (IfGoto l)  = 
   [  AInstr (Right "SP")
   ,  CInstr (CDest DestAM CompMMOne)
   ,  CInstr (CDest DestD CompM)
   ,  CInstr (CDest DestA CompAMOne)
   ,  AInstr (Right (f ++ "$" ++ l))
   ,  CInstr (CJump CompD JNE)
   ]


-- Functions:
compileFunc :: Func -> VM ()
compileFunc (Declare f n)  = do
   putFunc f
   mapM_ pushAsm [Label f,AInstr (Left 0),CInstr (CDest DestD CompA)]
   compileLocal n
   where
      compileLocal :: Int -> VM ()
      compileLocal 0 = return ()
      compileLocal i = pushPattern *> compileLocal (i-1)

compileFunc (Call f n)     = do
   i <- count
   inc
   func  <- getFunc
   mapM_ pushAsm [AInstr (Right (func ++ "$ret." ++ show i)),CInstr (CDest DestD CompA)]
   pushPattern 
   mapM_ (\s -> compilePushSeg s 0 True *> pushPattern) [Local,Argument,This,That]
   mapM_ pushAsm 
      [  AInstr (Left (n+5))
      ,  CInstr (CDest DestD CompA)
      ,  AInstr (Right "SP")
      ,  CInstr (CDest DestD CompMMD)
      ,  AInstr (Right "ARG")
      ,  CInstr (CDest DestM CompD)
      ,  AInstr (Right "SP")
      ,  CInstr (CDest DestD CompM)
      ,  AInstr (Right "LCL")
      ,  CInstr (CDest DestM CompD)
      ,  AInstr (Right f)
      ,  CInstr (CJump CompZero JMP)
      ,  Label (func ++ "$ret." ++ show i)
      ]


compileFunc Return         = do 
   mapM_ pushAsm 
      [  AInstr (Right "LCL")
      ,  CInstr (CDest DestD CompM)
      ,  AInstr (Right "R13")
      ,  CInstr (CDest DestM CompD)
      ,  AInstr (Left 5)
      ,  CInstr (CDest DestA CompDMA)
      ,  CInstr (CDest DestD CompM)
      ,  AInstr (Right "R14")
      ,  CInstr (CDest DestM CompD)
      ]
   compileCommand (Pop Argument 0)
   mapM_ pushAsm 
      [  AInstr (Right "ARG")
      ,  CInstr (CDest DestD CompM)
      ,  AInstr (Right "SP")
      ,  CInstr (CDest DestM CompDPOne)
      ]
   mapM_ compileSegReset [That,This,Argument,Local]
   mapM_ pushAsm 
      [  AInstr (Right "R14")
      ,  CInstr (CDest DestA CompM)
      ,  CInstr (CJump CompZero JMP)
      ]

compileSegReset :: Segment -> VM ()
compileSegReset s = mapM_ pushAsm 
   [  AInstr (Right "R13")
   ,  CInstr (CDest DestD CompMMOne)
   ,  CInstr (CDest DestAM CompD)
   ,  CInstr (CDest DestD CompM)
   ,  AInstr (Right (showSeg s))
   ,  CInstr (CDest DestM CompD)
   ]

