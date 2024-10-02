{-# LANGUAGE GADTs #-}


module ASM.AST where 


type Var = String

data Assembly where
   AInstr :: Either Int Var -> Assembly
   CInstr :: CInstruction -> Assembly
   Label  :: Var -> Assembly
   deriving Show

data CInstruction where
   CComp :: Comp -> CInstruction
   CDest :: Dest -> Comp -> CInstruction
   CJump :: Comp -> Jump -> CInstruction
   CAll  :: Dest -> Comp -> Jump -> CInstruction
   deriving Show

data Comp =
   -- a == 0
   CompZero | CompOne | CompMOne | CompD | CompA | CompND
   | CompNA | CompMD | CompMA | CompDPOne | CompAPOne | CompDMOne
   | CompAMOne | CompDPA | CompDMA | CompAMD | CompDNA | CompDOA
   -- a == 1
   | CompM | CompNM | CompMM | CompMPOne | CompMMOne
   | CompDPM | CompDMM | CompMMD | CompDNM | CompDOM deriving Show

data Dest = DestM | DestD | DestDM | DestA
   | DestAM | DestAD | DestADM deriving Show

data Jump = JGT | JEQ | JGE | JLT | JNE | JLE | JMP deriving Show

