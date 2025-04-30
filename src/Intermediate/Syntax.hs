{-# LANGUAGE GADTs              #-}
{-# LANGUAGE Rank2Types         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}

module Intermediate.Syntax where

import GHC.Generics
import           Data.Map
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)

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
    deriving (Eq, Ord, Generic)

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
    deriving (Eq, Ord, Generic)

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
    deriving (Eq, Show, Generic)

data Statement =
    ReturnStmt Expression |
    FunCallStmt String [Expression] |
    Assignment String Expression |
    Write Expression |
    Read String |
    While Expression Statement |
    If Expression Statement Statement |
    VarDecl String |
    SeqStmt Statement Statement |
    Skip
    deriving (Eq, Show, Generic)

type Args = [String]

data Definition =
    Definition String Args Statement
    deriving (Eq, Show, Generic)

type Definitions = [Definition]

data Program = Program Definitions Statement
    deriving (Eq, Show, Generic)

instance ToJSON UnOp where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON BinOp where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Expression where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Statement where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Definition where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Program where
    toEncoding = genericToEncoding defaultOptions