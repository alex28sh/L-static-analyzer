{-# LANGUAGE FlexibleInstances #-}
module StmtsParser where

import           Control.Monad.Combinators.Expr
import           Data                           (Parser)
import           Data.Functor                   (($>))
import           Data.Map                       hiding (map)
import           Debug.Trace                    (trace)
import           Lexer                          (comma, curvyBr, digit,
                                                 lIdentifier, lexeme, roundBr,
                                                 sc, symbol)
import           Text.Megaparsec                (MonadParsec (try), choice, eof,
                                                 many, notFollowedBy, sepBy,
                                                 sepBy1, some, (<?>), (<|>))

import           ExprParser                     (parseExpr, parseExprInBr,
                                                 parseFunCall)
import           Syntax


completeStmts :: Parser Statements
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

stmtsParser :: Parser Statements
stmtsParser = many stmtParser

parseBlock :: Parser Statements
parseBlock = curvyBr stmtsParser
