{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}


module ASM.Assembler where


import ASM.AST
import qualified Data.Map as M
import Control.Monad.State 
   ( gets
   , modify
   , evalStateT
   , StateT 
   )
import Control.Monad.Identity ( Identity(runIdentity) )



-- Interpretting labels:
-------------------------------------------------------------------------------
type Ctx = StateT (M.Map Var Int, Int) Identity

getC :: Ctx (M.Map Var Int)
getC = gets fst

getI :: Ctx Int
getI = gets snd

putC :: Var -> Int -> Ctx ()
putC var i = modify (\(ctx,n) -> (M.insert var i ctx, n))

inc :: Ctx ()
inc = modify (\(ctx,i) -> (ctx,i+1))

runCtx :: Ctx t -> t
runCtx p = runIdentity $ evalStateT p (M.empty,16)

labels :: Int -> [Assembly] -> Ctx [Assembly]
labels _ []               = return []
labels i (Label l : rest) = do
   putC l (i+1)
   labels i rest
labels i (x : rest)       = do
   rest' <- labels (i+1) rest
   return (x : rest')


-- Evaluation:
-------------------------------------------------------------------------------
checkEval :: [Assembly] -> [String]
checkEval = runCtx . evalAsm

evalAsm :: [Assembly] -> Ctx [String]
evalAsm a = do
   a' <- labels (-1) a
   mapM evalAsm' a'

evalAsm' :: Assembly -> Ctx String
evalAsm' (AInstr (Left n))
   | n < 16384 = return $ '0' : (giveZeroes . reverse . binDigits $ n)
   | otherwise = return $ giveZeroes . reverse . binDigits $ n
evalAsm' (AInstr (Right v)) = do
   v' <- evalA v
   evalAsm' (AInstr (Left v'))
evalAsm' (CInstr c)         = return $ "111" ++ evalC c
evalAsm' (Label _)          = return []

evalA :: Var -> Ctx Int
evalA "SCREEN" = return 16384
evalA "KBD"    = return 24576
evalA "SP"     = return 0
evalA "LCL"    = return 1
evalA "ARG"    = return 2
evalA "THIS"   = return 3
evalA "THAT"   = return 4
evalA "R0"     = return 0
evalA "R1"     = return 1
evalA "R2"     = return 2
evalA "R3"     = return 3
evalA "R4"     = return 4
evalA "R5"     = return 5
evalA "R6"     = return 6
evalA "R7"     = return 7
evalA "R8"     = return 8
evalA "R9"     = return 9
evalA "R10"    = return 10
evalA "R11"    = return 11
evalA "R12"    = return 12
evalA "R13"    = return 13
evalA "R14"    = return 14
evalA "R15"    = return 15
evalA var      = do
   ctx <- getC
   case M.lookup var ctx of
      Just x  -> return x
      Nothing -> do 
         i <- getI 
         putC var i 
         inc
         return i

evalC :: CInstruction -> String
evalC (CAll d c j) = evalComp c ++ evalDest d ++ evalJump j
evalC (CJump c j)  = evalComp c ++ "000" ++ evalJump j
evalC (CDest d c)  = evalComp c ++ evalDest d ++ "000"
evalC (CComp c)    = evalComp c ++ "000000"

evalComp :: Comp -> String
evalComp c = case c of
   CompZero  -> "0101010"
   CompOne   -> "0111111"
   CompMOne  -> "0111010"
   CompD     -> "0001100"
   CompA     -> "0110000"
   CompND    -> "0001101"
   CompNA    -> "0110001"
   CompMD    -> "0001111"
   CompMA    -> "0110011"
   CompDPOne -> "0011111"
   CompAPOne -> "0110111"
   CompDMOne -> "0001110"
   CompAMOne -> "0110010"
   CompDPA   -> "0000010"
   CompDMA   -> "0010011"
   CompAMD   -> "0000111"
   CompDNA   -> "0000000"
   CompDOA   -> "0010101"
   CompM     -> "1110000"
   CompNM    -> "1110001"
   CompMM    -> "1110011"
   CompMPOne -> "1110111"
   CompMMOne -> "1110010"
   CompDPM   -> "1000010"
   CompDMM   -> "1010011"
   CompMMD   -> "1000111"
   CompDNM   -> "1000000"
   CompDOM   -> "1010101"

evalDest :: Dest -> String
evalDest d = case d of
   DestADM -> "111"
   DestM   -> "001"
   DestD   -> "010"
   DestDM  -> "011"
   DestA   -> "100"
   DestAM  -> "101"
   DestAD  -> "110"

evalJump :: Jump -> String
evalJump j = case j of
   JGT -> "001"
   JEQ -> "010"
   JGE -> "011"
   JLT -> "100"
   JNE -> "101"
   JLE -> "110"
   JMP -> "111"

-- To convert decimal A-instructions:
   -- https://www.tutorialspoint.com/haskell-program-to-convert-decimal-to-binary#:~:text=In%20Haskell%2C%20a%20decimal%20number,2%20and%20taking%20the%20remainder.
binDigits :: Int -> String
binDigits 0 = "0"
binDigits n =
   let (q, r) = n `divMod` 2
   in show r ++ binDigits q

giveZeroes :: String -> String
giveZeroes str
   | length str < 15 = giveZeroes ('0' : str)
   | otherwise       = str

