{-# LANGUAGE GADTs              #-}
{-# LANGUAGE Rank2Types         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Typed.Syntax where

import           Intermediate.Syntax (Args, Expression, Access)

data Statement a where
    ReturnStmt :: Expression -> Statement Int
    FunCallStmt :: String -> [Expression] -> Statement ()
    Assignment :: Access -> Expression -> Statement ()
    Write :: Expression -> Statement ()
    Read :: Access -> Statement ()
    While :: Expression -> Statement () -> Statement ()
    If :: Expression -> Statement a -> Statement a -> Statement a
    VarDecl :: String -> Statement ()
    Skip :: Statement ()
    Seq :: Statement () -> Statement a -> Statement a
    SeqInt :: Statement Int -> Statement a -> Statement Int

data Definition where
    Definition :: String -> Args -> AnyStmt -> Definition

data AnyStmt where
  IntStmt :: Statement Int -> AnyStmt
  UnitStmt :: Statement () -> AnyStmt

type Definitions = [Definition]

data Program = Program Definitions (Statement Int)
