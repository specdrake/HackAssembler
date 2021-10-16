{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (openFile, IOMode(..), hGetContents)
import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import qualified Data.Text as T
import Data.Text (strip) 
import Parser as P 
import Code as C
import SymbolTable as S
import qualified Data.Map as M
import Debug.Trace (trace)
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
    contents <- lines <$> hGetContents inpFile
    -- let output = foldl (\out instr -> (parseInstr instr) : out) [] (lines contents) 

    full contents
    die "done with full"

    let output = foldl (\out instr -> out ++ [P.runParseLine instr] ) [] contents 
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

parseSymbol :: String -> Maybe String
parseSymbol "" = Nothing
parseSymbol _instr = 
    let 
        instr = T.unpack $ T.strip (T.pack _instr)
    in
        if head instr == '@' 
        then Just $ tail instr
        else Nothing


full :: [String] -> IO ()
full input = do
    let 
        inST = S.initST

        genTuple t@(out,st,m) line = 
            case val of
                Nothing -> t
                Just v -> if isVar v 
                          then (out, M.insertWith (flip const) v m st, m + 1)
                          else t
            where val = parseSymbol line

        (trimmed, finST, fm) = foldl (\t@(out,st, m) line -> case P.runParseLine line of
                                        Right P.Blank -> t
                                        Right (P.LComment _) -> t
                                        Right (P.LCommand _) -> t
                                        Left _ -> genTuple t line
                                            ) ([], inST, 16) input
                    
    mapM_ (print) (M.toList finST)
    print fm
