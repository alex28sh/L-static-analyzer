module PrgParser where 

import Syntax 
import Lexer
import StmtsParser
import Syntax (Definitions)
import Data.List (partition)
import Data (Parser)
import           Text.Megaparsec                (MonadParsec (try), choice, eof,
                                                 many, sepBy, sepBy1, some,
                                                 (<?>), (<|>), notFollowedBy)
import StmtsParser (parseBlock)

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
        ([], _) -> fail "No main function found"
        ([Definition _ _ stmts], defs) -> return $ Program defs stmts
        (_, _) -> fail "Multiple main functions found"

completePrgParser :: Parser Program
completePrgParser = prgParser <* eof