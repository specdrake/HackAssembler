{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (openFile, IOMode(..), hGetContents)
import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import qualified Data.Text as T
import Data.Text (strip) 
import Parser as P

-- initializes the I/O files and drives the process
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    -- check correct command line usage
    when (length args /= 1) $ do
        die $ "Usage: " ++ progName ++ "name.asm"

    -- mapM_ putStrLn args

    inpFile <- openFile ("testfiles/" ++ head args) ReadMode
    contents <- hGetContents inpFile
    -- let output = foldl (\out instr -> (parseInstr instr) : out) [] (lines contents) 

    let output = foldl (\out instr -> out ++ [(P.runParseLine instr)] ) [] (lines contents) 
    mapM_ (\y -> 
        case y of
            Right x -> putStrLn (show x) 
            _ -> die $ "Error: parseError") output
    -- inp <- getLine
    -- let res = if head inp == '@' then aInstrToBin (tail inp) else inp
    -- putStrLn res

parseInstr :: String -> String
parseInstr "" = ""
parseInstr _instr = 
    let 
        instr = T.unpack $ T.strip (T.pack _instr)
    in
        if head instr == '@' 
        then aInstrToBin (tail instr)
        else ""

aInstrToBin :: String -> String
aInstrToBin aInstr = '0' : padZeroes 16 (decToBin aInstr)

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
    

