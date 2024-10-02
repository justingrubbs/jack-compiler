{-# LANGUAGE GADTs #-}


module AST where 


-- Data types:
-------------------------------------------------------------------------------
-- Program structure: 
data Class = Class String ClassDecl deriving Show

data ClassDecl where
   ClassDecl :: [VarDecl] -> [Subroutine] -> ClassDecl
   deriving Show

data VarDecl where
   Static :: Type -> [String] -> VarDecl
   Field  :: Type -> [String] -> VarDecl
   deriving Show

data Type = TInt | TChar | TBool | TVoid | TClass String deriving Show

data Kind = KField | KStatic | KArg | KVar deriving Show 


data Subroutine where 
   Constructor :: Type -> String -> (Maybe ParameterList) -> SBody -> Subroutine
   Function    :: Type -> String -> (Maybe ParameterList) -> SBody -> Subroutine
   Method      :: Type -> String -> (Maybe ParameterList) -> SBody -> Subroutine
   deriving Show

newtype ParameterList = ParameterList [ParameterType] deriving Show

data ParameterType = ParameterType Type String deriving Show

data SBody = SBody [VarDec] [Stmt] deriving Show

data VarDec = VarDec Type [String] deriving Show

data Var = Var Type Kind Int deriving Show


-- Statements:
data Stmt where
   Let    :: String -> Maybe Expr -> Expr -> Stmt
   If     :: Expr -> [Stmt] -> Maybe [Stmt] -> Stmt
   While  :: Expr -> [Stmt] -> Stmt
   Do     :: SubCall -> Stmt
   Return :: Maybe Expr -> Stmt
   deriving Show


-- Expressions:
data Expr where
   Term  :: Term -> Expr
   Bin   :: Op -> Expr -> Expr -> Expr
   Un    :: Un -> Expr -> Expr
   deriving Show

data Term where
   Literal     :: Literal -> Term
   TVar        :: String -> Maybe Expr -> Term
   Expr        :: Expr -> Term
   TermSubCall :: SubCall -> Term
   deriving Show

data Literal where
   Int      :: Int -> Literal
   String   :: String -> Literal
   Bool     :: Bool -> Literal
   Null     :: Literal
   This     :: Literal
   deriving Show

data Op = Add | Sub | Mul | Div | And | Or | Lt | Gt | Eq deriving Show
data Un = Neg | Not deriving Show

data SubCall = SubCall (Maybe String) String [Expr] deriving Show
