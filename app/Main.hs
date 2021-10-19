{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
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
import Text.ParserCombinators.Parsec as TP
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

    final <- full contents

    -- let output = foldl (\out instr -> out ++ [P.runParseLine instr] ) [] contents 
    let outFilePath = if length inpFilePath > 4 && (drop (length inpFilePath - 4)) inpFilePath == ".asm"
                      then (take (length inpFilePath - 4)) inpFilePath ++ ".out"
                      else inpFilePath ++ ".out"
    writeFile outFilePath ""
    -- mapM_ (\y -> appendFile outFilePath (y ++ "\n")) final
    mapM_ (\y -> 
            let 
                val = C.codeGenLine y
            in
                if val == "" 
                then return ()
                else appendFile outFilePath (val++"\n")
            ) final

    putStrLn $ "Written to: " ++ outFilePath
    -- inp <- getLine
    -- let res = if head inp == '@' then aInstrToBin (tail inp) else inp
    -- putStrLn res

parseSymbol1 :: String -> Maybe String
parseSymbol1 "" = Nothing
parseSymbol1 _instr = 
    let 
        instr = T.unpack $ T.strip (T.pack _instr)
    in
        if head instr == '@' 
        then Just $ tail instr
        else Nothing

numberFunc:: [Either TP.ParseError ALine] -> ((Integer, M.Map String Integer), [(Integer, ALine)])
numberFunc input = foldl (\((n, s), b) a -> 
                                    case a of
                                        Left _ -> ((n, s), b)
                                        Right Blank -> ((n,s), b)
                                        Right (LComment _) -> ((n,s), b)
                                        Right (x@(LSymbol (P.Label str))) -> ((n,(M.insertWith (flip const) (str) n s)), b)
                                        Right x -> ((n+1,s), b ++ [(n, x)])
                                    ) ((0,S.initST),[]) input



-- numberFunc :: [Either TP.ParseError ALine] -> ((Integer, M.Map String Integer), [(Integer, ALine)])
-- numberFunc inputInit = 
--     let
--         input = numberFuncInit inputInit
--         st = snd (fst input)
--         (n, output) = foldl (\(num, b) (n, a) -> 
--                         case a of
--                             LSymbol (P.Variable x) ->
--                                 case M.lookup x st of 
--                                     Nothing -> (num + 1, b ++ [(num, a)])
--                                     Just _ -> (num, b)
--                             _ -> (num + 1, b ++ [(num, a)])
--                                     ) (0,[]) (snd input)
--     in
--         ((n,st), output)

full :: [String] -> IO [ALine]
full input = do
    let 
        -- ansI = numberFuncInit . map P.runParseLine $ input
        ans = numberFunc . map P.runParseLine $ input

        inST = S.initST

        genTuple t@(out,st,m) line = 
                                        case M.lookup line st of
                                            Nothing -> (out ++ [P.LCommand (P.AComm (show m))], M.insertWith (flip const) line m st, m+1)
                                            Just val -> (out ++ [P.LCommand (P.AComm (show val))], st, m)


        (final, finST, fm) = foldl (\t@(out,st, m) line -> case snd line of
                                        P.Blank -> t
                                        (P.LComment _) -> t
                                        l@(P.LCommand _) -> (out ++ [l], st, m)
                                        (P.LSymbol (P.Variable line)) -> genTuple t line
                                        (P.LSymbol (P.Label line)) -> (out, st, m)
                                            ) ([], (snd (fst ans)), 16) (snd ans)
                    
    -- mapM_ print(snd ansI)
    -- print "-----------------"
    mapM_ print (snd ans)
    print (M.toList (snd . fst $ ans))
    mapM_ print final
    -- print (fromMaybe 16 (M.lookup ((\(((_, LSymbol (P.Variable x))):_) -> x) (snd ans)) (snd .fst $ ans)))
    return final
