module SymbolTable where

import qualified Data.Map as M
import Data.Char (isUpper)

data Symbol = Predef | Label | Variable deriving Show

isPredef :: String -> Bool
isPredef "" = False
isPredef str = 
    let
        st = initST
    in 
        case M.lookup str st of
            Just _ -> True
            _ -> False

isLabel :: String -> Bool
isLabel "" = False
isLabel str = (not (isPredef str)) && (isUpper (head str)) 

isVar :: String -> Bool
isVar "" = False
isVar str = not $ (isPredef str) || (isLabel str)


initST :: M.Map String Integer 
initST = 
    let 
        predefList = [("SP", 0), ("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4)
                    ,("R0", 0)
                    ,("R1", 1)
                    ,("R2", 2)
                    ,("R3", 3)
                    ,("R4", 4)
                    ,("R5", 5)
                    ,("R6", 6)
                    ,("R7", 7)
                    ,("R8", 8)
                    ,("R9", 9)
                    ,("R10", 10)
                    ,("R11", 11)
                    ,("R12", 12)
                    ,("R13", 13)
                    ,("R14", 14)
                    ,("R15", 15)
                    ,("SCREEN", 16384), ("KBD", 24576)
                    ]
    in
        M.fromList predefList


