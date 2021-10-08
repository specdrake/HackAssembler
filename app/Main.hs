{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (openFile, IOMode(..), hGetContents)
import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import qualified Data.Text as T
import Data.Text (strip) 
import Parser as P ( runParseLine )
import Code as C

-- initializes the I/O files and drives the process
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    -- check correct command line usage
    when (length args /= 1) $ do
        die $ "Usage: " ++ progName ++ " <filepath>"

    -- mapM_ putStrLn args
    let inpFilePath = head args
    inpFile <- openFile inpFilePath ReadMode
    contents <- hGetContents inpFile
    -- let output = foldl (\out instr -> (parseInstr instr) : out) [] (lines contents) 

    let output = foldl (\out instr -> out ++ [P.runParseLine instr] ) [] (lines contents) 
    let outFilePath = if length inpFilePath > 4 && (drop (length inpFilePath - 4)) inpFilePath == ".asm"
                      then (take (length inpFilePath - 4)) inpFilePath ++ ".out"
                      else inpFilePath ++ ".out"
    writeFile outFilePath ""
    mapM_ (\y -> 
        case y of
            (Right line) -> let
                                val = C.codeGenLine line  
                            in
                                if val == "" 
                                then return ()
                                else appendFile outFilePath (val++"\n")
            _ -> die $ "Error: parseError") output

    putStrLn $ "Written to: " ++ outFilePath
    -- inp <- getLine
    -- let res = if head inp == '@' then aInstrToBin (tail inp) else inp
    -- putStrLn res

-- parseInstr :: String -> String
-- parseInstr "" = ""
-- parseInstr _instr = 
--     let 
--         instr = T.unpack $ T.strip (T.pack _instr)
--     in
--         if head instr == '@' 
--         then aInstrToBin (tail instr)
--         else ""

