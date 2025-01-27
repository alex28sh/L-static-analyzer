{-# LANGUAGE FlexibleInstances #-}
module Parser.ExprParser where

import           Control.Monad.Combinators.Expr
import           Data                           (Parser)
import           Parser.Lexer                   (comma, digit, lIdentifier,
                                                 roundBr, sc, symbol)
import           Syntax
import           Text.Megaparsec                (MonadParsec (try), eof,
                                                 notFollowedBy, sepBy, some,
                                                 (<|>))


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
        binary ">" $ BinExpr Gt,
        binary "<" $ BinExpr Lt,
        binary ">=" $ BinExpr Ge,
        binary "<=" $ BinExpr Le
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

parseVar :: Parser Expression
parseVar =
    sc *> try (Variable <$> lIdentifier) <* notFollowedBy (symbol "(")

parseArguments :: Parser [Expression]
parseArguments =
    sc *> sepBy parseExpr comma

parseFunCall :: Parser Expression
parseFunCall =
    FunCall <$>
    lIdentifier <*>
    roundBr parseArguments

parseSimpleExpr :: Parser Expression
parseSimpleExpr =
    try parseExprInBr <|> try parseVar <|> try parseConst <|> try parseFunCall
