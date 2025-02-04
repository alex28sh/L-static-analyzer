{-# LANGUAGE FlexibleInstances #-}
module Intermediate.Parser.StmtsParser where

import           Data.Functor                   (($>))
import           Intermediate.Data              (Parser)

import           Intermediate.Parser.ExprParser (parseExpr, parseFunCall)
import           Intermediate.Parser.Lexer      (curvyBr, lIdentifier, sc,
                                                 symbol)
import           Intermediate.Syntax
import           Text.Megaparsec                (MonadParsec (try), eof, many,
                                                 (<|>))



completeStmts :: Parser Statement
completeStmts = stmtsParser <* eof


stmtParserColon :: Parser Statement
stmtParserColon =
        (try parseReturn <|> try parseFunCallStmt <|> try parseAssignment
    <|> try parseWrite <|> try parseRead <|> try parseSkip <|> try parseVarDecl) <* symbol ";"
    where
        parseReturn =
            symbol "return" *> (ReturnStmt <$> parseExpr)
        parseFunCallStmt = do
            FunCall name args <- parseFunCall
            return $ FunCallStmt name args
        parseAssignment =
            Assignment <$> lIdentifier <*> (symbol ":=" *> parseExpr)
        parseWrite =
            symbol "write" *> (Write <$> parseExpr)
        parseRead =
            symbol "read" *> (Read <$> lIdentifier)
        parseVarDecl =
            symbol "var" *> (VarDecl <$> lIdentifier)
        parseSkip =
            symbol "skip" $> Skip

stmtParser :: Parser Statement
stmtParser =
        sc *> (try stmtParserColon <|> try parseWhile <|> try parseIf)
    where
        parseWhile =
            symbol "while" *>
                (While <$> parseExpr <*> parseBlock)
        parseIf =
            symbol "if" *>
                (If <$> parseExpr <*>
                    parseBlock <*>
                (symbol "else" *>
                    parseBlock))

stmtsParser :: Parser Statement
stmtsParser = do
    (x:lst) <- many stmtParser
    return $ foldl SeqStmt x lst


parseBlock :: Parser Statement
parseBlock = curvyBr stmtsParser
