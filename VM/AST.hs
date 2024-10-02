{-# LANGUAGE GADTs #-}


module VM.AST where 


type Instruction = [Command]

data Command where
   Push     :: Segment -> Int -> Command
   Pop      :: Segment -> Int -> Command
   ACL      :: ACL -> Command
   Branch   :: Branch -> Command
   Func     :: Func -> Command
   deriving Show

data Segment = Argument | Local | Static | Constant
   | This | That | Pointer | Temp deriving Show

data ACL where
   Arith    :: Arith -> ACL
   VMComp   :: VMComp -> ACL
   Logic    :: Logic -> ACL
   deriving Show

data Arith = Add | Sub | Neg deriving Show

data VMComp = Eq | Gt | Lt deriving Show

data Logic = And | Or | Not deriving Show

data Branch where
   VMLabel  :: String -> Branch
   Goto     :: String -> Branch
   IfGoto   :: String -> Branch
   deriving Show

data Func where
   Declare :: String -> Int -> Func
   Call    :: String -> Int -> Func
   Return  :: Func
   deriving Show

