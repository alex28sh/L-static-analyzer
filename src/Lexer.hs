module Lexer where

import           Control.Monad              (void)
import           Data                       (Parser, reserved)
import           Data.Char                  (isDigit, isLetter)
import           Text.Megaparsec            (MonadParsec (try), between, many,
                                             satisfy, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             lowerChar, spaceChar, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf                (printf)


sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

roundBr, angleBr, boxBr, curvyBr :: Parser a -> Parser a
roundBr = between (symbol "(") (symbol ")")
angleBr = between (symbol "<") (symbol ">")
boxBr   = between (symbol "[") (symbol "]")
curvyBr = between (symbol "{") (symbol "}")

notReserved :: MonadFail m => [String] -> String -> m String
notReserved reserved x | x `elem` reserved = fail $ printf "%s is reserved" (show x)
notReserved reserved x = return x

lIdentifier :: Parser String
lIdentifier =
    (lexeme . try) (p >>= notReserved reserved)
  where
    p = (:) <$> firstChar <*> many nonFirstChar
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

comma :: Parser String
comma = symbol ","

digit :: Parser Char
digit = (lexeme . try) (satisfy isDigit)
