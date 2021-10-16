module Parser where
import Data.Char 
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import qualified Data.Text as T
import Data.Text (strip) 
import Text.ParserCombinators.Parsec as P
import Text.Parsec


type AInstr = String

data CInstr= OnlyComp Comp | CompJump Comp Jump | DestComp Dest Comp | Full Dest Comp Jump
    deriving (Show)

data Command = AComm AInstr | CComm CInstr  deriving (Show)

data ALine = LCommand Command | LComment Comment | Blank deriving (Show)

data Comment = Comment String deriving (Show)

data Dest = DNull | M | D | MD | A | AM | AD | AMD deriving (Show)
data Comp = Zero | One | MinusOne | CompD | CompA | CompM | NotD | NotA | NotM | MinusD 
            | MinusA | MinusM | DPlusOne | APlusOne | MPlusOne | DMinusOne | AMinusOne | MMinusOne 
            | DPlusA | DPlusM | DMinusA | DMinusM | AMinusD | MMinusD | DAndA | DAndM | DOrA | DOrM 
            deriving (Show)
data Jump = JNull | JGT | JEQ | JGE | JLT | JNE | JLE | JMP deriving (Show)


-- commandType :: String -> Command
-- commandType str = 
--     let 
--         instr = T.unpack $ T.strip (T.pack str)
--     in
--         if head instr == '@' 
--         then A_Command
--         else C_Command

whitespace :: Parser ()
whitespace = void $ P.many $ oneOf " \n\t"

parseDest :: ParsecT String u Identity Dest
parseDest = do
        P.try $ string "MD" *> pure MD
    P.<|> string "M" *> pure M
    P.<|> string "D" *> pure D
    P.<|> (P.try $ string "AMD" *> pure AMD)
    P.<|> (P.try $ string "AM" *> pure AM)
    P.<|> (P.try $ string "AD" *> pure AD)
    P.<|> string "A" *> pure A

parseComp :: ParsecT String u Identity Comp
parseComp = do
        string "0" *> pure Zero
    P.<|> string "1" *> pure One
    P.<|> (P.try $ string "-1" *> pure MinusOne)
    P.<|> (P.try $ string "!D" *> pure NotD)
    P.<|> (P.try $ string "!A" *> pure NotA)
    P.<|> string "!M" *> pure NotM
    P.<|> (P.try $ string "-D" *> pure MinusD)
    P.<|> (P.try $ string "-A" *> pure MinusA)
    P.<|> string "-M" *> pure MinusM
    P.<|> (P.try $ string "D+1" *> pure DPlusOne)
    P.<|> (P.try $ string "A+1" *> pure APlusOne)
    P.<|> (P.try $ string "M+1" *> pure MPlusOne)
    P.<|> (P.try $ string "D-1" *> pure DMinusOne)
    P.<|> (P.try $ string "A-1" *> pure AMinusOne)
    P.<|> (P.try $ string "M-1" *> pure MMinusOne)
    P.<|> (P.try $ string "D+A" *> pure DPlusA)
    P.<|> (P.try $ string "D+M" *> pure DPlusM)
    P.<|> (P.try $ string "D-A" *> pure DMinusA)
    P.<|> (P.try $ string "D-M" *> pure DMinusM)
    P.<|> (P.try $ string "A-D" *> pure AMinusD)
    P.<|> (P.try $ string "M-D" *> pure MMinusD)
    P.<|> (P.try $ string "D&A" *> pure DAndA)
    P.<|> (P.try $ string "D&M" *> pure DAndM)
    P.<|> (P.try $ string "D|A" *> pure DOrA)
    P.<|> (P.try $ string "D|M" *> pure DOrM)
    P.<|> (P.try $ string "D" *> pure CompD)
    P.<|> (P.try $ string "A" *> pure CompA)
    P.<|> (P.try $ string "M" *> pure CompM)

parseJump :: ParsecT String u Identity Jump
parseJump = do
        P.try $ string "JGT" *> pure JGT
    P.<|> (P.try $ string "JEQ" *> pure JEQ)
    P.<|> (P.try $ string "JGE" *> pure JGE)
    P.<|> (P.try $ string "JLT" *> pure JLT)
    P.<|> (P.try $ string "JNE" *> pure JNE)
    P.<|> (P.try $ string "JLE" *> pure JLE)
    P.<|> (P.try $ string "JMP" *> pure JMP)

parseDigs :: P.Parser String
parseDigs = do
    value <- some digit
    return value

parseOnlyComp :: P.Parser CInstr
parseOnlyComp = do
    whitespace
    value <- parseComp
    return $ OnlyComp value

parseCompJump :: P.Parser CInstr
parseCompJump = do
    whitespace
    comp <- parseComp
    whitespace
    _ <- P.char ';'
    whitespace
    jump <- parseJump
    return $ CompJump comp jump

parseDestComp :: P.Parser CInstr
parseDestComp = do
    whitespace
    dest <- parseDest
    whitespace
    _ <- char '='
    whitespace
    comp <- parseComp
    return $ DestComp dest comp
    
parseFull :: P.Parser CInstr
parseFull = do
    whitespace
    dest <- parseDest
    whitespace
    _ <- P.char '='
    (CompJump comp jump) <- parseCompJump
    return $ Full dest comp jump

parseCInstr :: P.Parser CInstr
parseCInstr = do
    P.try parseFull
    P.<|> P.try parseDestComp
    P.<|> P.try parseCompJump
    P.<|> P.try parseOnlyComp

parseComment :: P.Parser Comment
parseComment = do
    whitespace
    _ <- char '/'
    _ <- char '/'
    comm <- P.many anyChar
    return $ Comment comm

parseAInstr :: Parser AInstr
parseAInstr = do
    whitespace
    _ <- char '@';
    value <- some digit
    return value

parseComm :: P.Parser Command
parseComm = do
    P.try $ do
        cinstr <- parseCInstr
        return $ CComm cinstr
    P.<|> do 
        ainstr <- parseAInstr 
        return $ AComm ainstr
    
eol :: Parser String
eol = P.try (string "\n\r")
  P.<|> P.try (string "\r\n")
  P.<|> string "\n"
  P.<|> string "\r"
  P.<?> "end of line"

parseBlank :: Parser ALine 
parseBlank = do
    whitespace
    P.optional eol
    eof
    return Blank

parseLine :: P.Parser ALine
parseLine = do
    P.try $ do
        comment <- parseComment
        return $ LComment comment
    P.<|> ( P.try $ do
        comm <- parseComm
        return $ LCommand comm)
    P.<|> (P.try $ do
        parseBlank)

runParseLine :: String -> Either ParseError ALine
runParseLine line = do
    P.runParser parseLine () "" line



