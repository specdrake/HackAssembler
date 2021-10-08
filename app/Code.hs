module Code where

import Parser as P

codeGenLine :: ALine -> String
codeGenLine (LCommand comm) = codeGenComm comm
codeGenLine _ = ""

codeGenComm :: Command -> String
codeGenComm (AComm ainstr) = codeGenA ainstr 
codeGenComm (CComm cinstr) = codeGenC cinstr

codeGenA :: AInstr -> String
codeGenA aInstr = '0' : padZeroes 15 (decToBin aInstr)

-- converts a decimal string to binary
decToBin :: String -> String
decToBin dec = 
    let
        dec_int = read dec :: Integer 
        bin = toBin dec_int "" 
        toBin n str = 
            if n <= 0 
            then str 
            else toBin (n `div` 2) $ (
                if n `mod` 2 == 1 
                then '1'    
                else '0') : str
    in 
        bin

-- pad zeroes to make the result of a certain length
padZeroes :: Int -> String -> String
padZeroes n str = 
    if length str < n
    then replicate (n - length str) '0' ++ str
    else str
    

codeGenC :: CInstr -> String
codeGenC (OnlyComp comp) = "111" ++ codeGenComp comp ++ codeGenDest DNull ++ codeGenJump JNull
codeGenC (CompJump comp jump) = "111" ++ codeGenComp comp ++ codeGenDest DNull ++ codeGenJump jump
codeGenC (DestComp dest comp) = "111" ++ codeGenComp comp ++ codeGenDest dest ++ codeGenJump JNull
codeGenC (Full dest comp jump) = "111" ++ codeGenComp comp ++ codeGenDest dest ++ codeGenJump jump

codeGenDest :: Dest -> String
codeGenDest DNull = "000"
codeGenDest M = "001"
codeGenDest D = "010"
codeGenDest MD = "011"
codeGenDest A = "100"
codeGenDest AM = "101"
codeGenDest AD = "110"
codeGenDest AMD = "111"

codeGenJump :: Jump -> String
codeGenJump JNull = "000"
codeGenJump JGT = "001"
codeGenJump JEQ = "010"
codeGenJump JGE = "011"
codeGenJump JLT = "100"
codeGenJump JNE = "101"
codeGenJump JLE = "110"
codeGenJump JMP = "111"

codeGenComp :: Comp -> String
codeGenComp Zero = "0101010"
codeGenComp One = "0111111"
codeGenComp MinusOne = "0111010"
codeGenComp CompD = "0001100"
codeGenComp CompA = "0110000"
codeGenComp CompM = "1110000"
codeGenComp NotD = "0001101"
codeGenComp NotA = "0110001"
codeGenComp NotM = "1110001"
codeGenComp MinusD = "0001111"
codeGenComp MinusA = "0110011"
codeGenComp MinusM = "1110011"
codeGenComp DPlusOne = "0011111"
codeGenComp APlusOne = "0110111"
codeGenComp MPlusOne = "1110111"
codeGenComp DMinusOne = "0001110"
codeGenComp AMinusOne = "0110010"
codeGenComp MMinusOne = "1110010"
codeGenComp DPlusA = "0000010"
codeGenComp DPlusM = "1000010"
codeGenComp DMinusA = "0010011"
codeGenComp DMinusM = "1010011"
codeGenComp AMinusD = "0000111"
codeGenComp MMinusD = "1000111"
codeGenComp DAndA = "0000000"
codeGenComp DAndM = "1000000"
codeGenComp DOrA = "0010101"
codeGenComp DOrM = "1010101"
