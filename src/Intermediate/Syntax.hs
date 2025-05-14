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

data Type = TInt | TPtr Type
    deriving (Eq, Ord, Generic)

instance Show Type where
    show TInt       = "int"
    show (TPtr t)   = show t ++ "*"

data Expression =
    BinExpr BinOp Expression Expression |
    UnExpr UnOp Expression |
    FunCall String [Expression] |
    Access Access | 
    Const Int |
    NewArr Type Expression 
    deriving (Eq, Show, Generic)

data Access = 
    ArrayIdx Access Expression |
    Variable String
    deriving (Eq, Show, Generic)

data Statement =
    ReturnStmt Expression |
    FunCallStmt String [Expression] |
    Assignment Access Expression |
    Write Expression |
    Read Access |
    While Expression Statement |
    If Expression Statement Statement |
    VarDecl Type String |
    SeqStmt Statement Statement |
    Skip
    deriving (Eq, Show, Generic)

type Arg = (Type, String)
type Args = [Arg]

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

instance ToJSON Type where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Access where
    toEncoding = genericToEncoding defaultOptions
