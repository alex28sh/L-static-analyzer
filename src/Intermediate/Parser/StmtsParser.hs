{-# LANGUAGE FlexibleInstances #-}
module Intermediate.Parser.StmtsParser where

import           Data.Functor                   (($>))
import           Intermediate.Data              (Parser)

import           Intermediate.Parser.ExprParser (parseExpr, parseFunCall, parseType)
import           Intermediate.Parser.Lexer      (curvyBr, lIdentifier, sc,
                                                 symbol, boxBr)
import           Intermediate.Syntax
import           Text.Megaparsec                (MonadParsec (try), eof, many,
                                                 lookAhead, (<|>))



completeStmts :: Parser Statement
completeStmts = stmtsParser <* eof


stmtParserColon :: Parser Statement
stmtParserColon =
        (try parseReturn <|> try parseFunCallStmt <|> try parseAssignment
    <|> try parseWrite <|> try parseRead <|> try parseSkip <|> parseVarDecl) <* symbol ";"
    where
        parseReturn =
            symbol "return" *> (ReturnStmt <$> parseExpr)
        parseFunCallStmt = do
            FunCall name args <- parseFunCall
            return $ FunCallStmt name args
        parseAssignment = do
            lhs <- parseAccess
            symbol ":="
            rhs <- parseExpr
            return $ Assignment lhs rhs
        parseWrite =
            symbol "write" *> (Write <$> parseExpr)
        parseRead =
            symbol "read" *> (Read <$> parseAccess)
        parseVarDecl =
            VarDecl <$> parseType <*> lIdentifier
        parseSkip =
            symbol "skip" $> Skip

parseAccess :: Parser Access
parseAccess = do
    root <- Variable <$> lIdentifier
    buildAccess root
  where
    buildAccess acc = do
      hasIdx <- (True <$ lookAhead (symbol "[")) <|> pure False
      if hasIdx
        then do
          idx <- boxBr parseExpr
          buildAccess (ArrayIdx acc idx)
        else return acc

stmtParser :: Parser Statement
stmtParser =
        sc *> (try stmtParserColon <|> try parseWhile <|> parseIf)
    where
        parseWhile =
            symbol "while" *>
                (While <$> parseExpr <*> parseBlock)
        parseIf =
            symbol "if" *> (If <$> parseExpr <*> parseBlock <*> optionalElse)
          where
            optionalElse = (symbol "else" *> parseBlock) <|> pure Skip

stmtsParser :: Parser Statement
stmtsParser = do
    (x:lst) <- many stmtParser
    return $ foldl SeqStmt x lst


parseBlock :: Parser Statement
parseBlock = curvyBr stmtsParser
