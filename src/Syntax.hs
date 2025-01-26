module Syntax where

import           Data.Map

data BinOp =
    Sub |
    Add |
    Div |
    Mul |
    Mod |
    Eq  |
    Neq |
    Gt  |
    Ge  |
    Lt  |
    Le  |
    And |
    Or
    deriving (Eq, Ord)

binOpShowMap :: Map BinOp String
binOpShowMap = fromList [(Sub, "-"),
                         (Add, "+"),
                         (Div, "/"),
                         (Mul, "*"),
                         (Mod, "%"),
                         (Eq, "=="),
                         (Neq, "!="),
                         (Gt, ">"),
                         (Ge, ">="),
                         (Lt, "<"),
                         (Le, "<="),
                         (And, "&&"),
                         (Or, "||")]


instance Show BinOp where
    show binOp = binOpShowMap ! binOp

data UnOp =
    SubUn
    deriving (Eq, Ord)

unOpShowMap :: Map UnOp String
unOpShowMap = fromList [(SubUn, "-")]

instance Show UnOp where
    show unOp = unOpShowMap ! unOp

data Expression =
    BinExpr BinOp Expression Expression |
    UnExpr UnOp Expression |
    FunCall String [Expression] |
    Variable String |
    Const Int
    deriving (Eq, Show)

data Statement =
    ReturnStmt Expression |
    FunCallStmt String [Expression] |
    Assignment String Expression |
    Write Expression |
    Read String |
    While Expression Statements |
    If Expression Statements Statements |
    VarDecl String |
    Skip
    deriving (Eq, Show)

type Statements = [Statement]
type Args = [String]

data Definition =
    Definition String Args Statements
    deriving (Eq, Show)

type Definitions = [Definition]

data Program = Program Definitions Statements
    deriving (Eq, Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
