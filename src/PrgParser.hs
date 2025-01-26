module PrgParser where

import           Data            (Parser)
import           Data.List       (partition)
import           Lexer
import           StmtsParser
import           StmtsParser     (parseBlock)
import           Syntax
import           Syntax          (Definitions)
import           Text.Megaparsec (MonadParsec (try), choice, eof, many,
                                  notFollowedBy, sepBy, sepBy1, some, (<?>),
                                  (<|>))

defParser :: Parser Definition
defParser = symbol "def" *> (Definition <$> lIdentifier <*> parseFunArgs <*> parseBlock)
    where
        parseFunArgs = roundBr (lIdentifier `sepBy` comma)

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
