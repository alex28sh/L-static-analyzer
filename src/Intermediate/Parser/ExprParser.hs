{-# LANGUAGE FlexibleInstances #-}
module Intermediate.Parser.ExprParser where

import           Control.Monad.Combinators.Expr
import           Intermediate.Data              (Parser)
import           Intermediate.Parser.Lexer      (comma, digit, lIdentifier,
                                                 roundBr, boxBr, sc, symbol)
import           Intermediate.Syntax
import           Text.Megaparsec                (MonadParsec (try), eof,
                                                 notFollowedBy, sepBy, some,
                                                 many, (<|>))
import           Data.Functor                   (($>))


completeExpr :: Parser Expression
completeExpr = parseExpr <* eof

parseExpr :: Parser Expression
parseExpr =
    makeExprParser parseSimpleExpr operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
    [
     [
        binary "/" $ BinExpr Div,
        binary "*" $ BinExpr Mul,
        binary "%" $ BinExpr Mod
     ],
     [
        binary "-" $ BinExpr Sub,
        binary "+" $ BinExpr Add
     ],
     [
        binary ">=" $ BinExpr Ge,
        binary "<=" $ BinExpr Le,
        binary ">" $ BinExpr Gt,
        binary "<" $ BinExpr Lt
     ],
     [
        binary "==" $ BinExpr Eq,
        binary "!=" $ BinExpr Neq
     ],
     [
        binary "&&" $ BinExpr And
     ],
     [
        binary "||" $ BinExpr Or
     ]
    ]
    where
        binary name f = InfixL (f <$ symbol name)

parseExprInBr :: Parser Expression
parseExprInBr = sc *> roundBr parseExpr

parseConst :: Parser Expression
parseConst =
    sc *> try (Const . read <$> some digit)

-- Parse an Access (variable or nested array indexing)
parseAccess :: Parser Access
parseAccess = do
    sc
    root <- Variable <$> lIdentifier
    buildAccess root
  where
    buildAccess acc = (do
        idx <- boxBr parseExpr
        buildAccess (ArrayIdx acc idx)) <|> return acc

-- Wrap access into an Expression
parseAccessExpr :: Parser Expression
parseAccessExpr = Access <$> parseAccess

parseNewArr :: Parser Expression
parseNewArr = do
    sc *> symbol "new"
    ty <- parseType
    size <- boxBr parseExpr
    return $ NewArr ty size

-- Parse simple types like int, int*, int** etc.
parseType :: Parser Type
parseType = do
    base <- symbol "int" $> TInt
    stars <- many (symbol "*")
    return $ foldl (const . TPtr) base stars

parseFunCall :: Parser Expression
parseFunCall =
    FunCall <$>
    lIdentifier <*>
    roundBr parseArguments

parseArguments :: Parser [Expression]
parseArguments =
    sc *> sepBy parseExpr comma

parseSimpleExpr :: Parser Expression
parseSimpleExpr =
    try parseExprInBr <|> try parseNewArr <|> try parseFunCall <|> try parseAccessExpr <|> parseConst
