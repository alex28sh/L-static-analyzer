module Intermediate.Parser.PrgParser where

import           Data.List                       (partition)
import           Intermediate.Data               (Parser)
import           Intermediate.Parser.Lexer
import           Intermediate.Parser.StmtsParser
import           Intermediate.Parser.ExprParser (parseType)
import           Intermediate.Syntax
import           Text.Megaparsec                 (eof, many, sepBy)

defParser :: Parser Definition
defParser = symbol "def" *> (Definition <$> lIdentifier <*> parseFunArgs <*> parseBlock)
    where
        parseFunArgs = roundBr (parseFunArg `sepBy` comma)
          where
            parseFunArg = do
                ty <- parseType
                name <- lIdentifier
                return (ty, name)

defsParser :: Parser Definitions
defsParser = many defParser

prgParser :: Parser Program
prgParser = do
    defs <- defsParser
    case partition (\(Definition name _ _) -> name == "main") defs of
        ([], _)                        -> fail "No main function found"
        ([Definition _ _ stmts], defs) -> return $ Program defs stmts
        (_, _)                         -> fail "Multiple main functions found"

completePrgParser :: Parser Program
completePrgParser = prgParser <* eof
