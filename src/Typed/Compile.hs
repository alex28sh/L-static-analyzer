{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Typed.Compile where

import           Control.Monad.State        (MonadState (get),
                                             MonadTrans (lift), StateT, modify)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Intermediate.Syntax        as I
import           Typed.Syntax

data TCompileError = TypeError
  deriving (Eq, Show)

unpackPack :: (forall a. Statement a -> Statement a) -> AnyStmt -> AnyStmt
unpackPack f (IntStmt e)  = IntStmt $ f e
unpackPack f (UnitStmt e) = UnitStmt $ f e

compile :: (Monad m) => I.Statement -> ExceptT TCompileError m AnyStmt
compile (I.ReturnStmt exp) = return $ IntStmt $ ReturnStmt exp
compile I.Skip = return $ UnitStmt Skip
compile (I.Write e) = return $ UnitStmt $ Write e
compile (I.Read v) = return $ UnitStmt $ Read v
compile (I.VarDecl x) = return $ UnitStmt $ VarDecl x
compile (I.SeqStmt s1 s2) = do
    r1 <- compile s1
    r2 <- compile s2
    case (r1, r2) of
        (IntStmt r1, IntStmt r2)   -> return $ IntStmt $ SeqInt r1 r2
        (IntStmt r1, UnitStmt r2)  -> return $ IntStmt $ SeqInt r1 r2
        (UnitStmt r1, IntStmt r2)  -> return $ IntStmt $ Seq r1 r2
        (UnitStmt r1, UnitStmt r2) -> return $ UnitStmt $ Seq r1 r2
compile (I.If e s1 s2) = do
    r1 <- compile s1
    r2 <- compile s2
    case (r1, r2) of
        (IntStmt r1, IntStmt r2)   -> return $ IntStmt $ If e r1 r2
        (UnitStmt r1, UnitStmt r2) -> return $ UnitStmt $ If e r1 r2
        _                          -> throwE TypeError
compile (I.While e s) = do
    r <- compile s
    case r of
        UnitStmt r -> return $ UnitStmt $ While e r
        _          -> throwE TypeError
compile (I.Assignment x e) = return $ UnitStmt $ Assignment x e
compile (I.FunCallStmt name es) = return $ UnitStmt $ FunCallStmt name es

compilePrg :: (Monad m) => I.Program -> ExceptT TCompileError m Program
compilePrg (I.Program defs stmt) = do
    defs <- mapM mapDef defs
    stmt <- compile stmt
    case stmt of
        IntStmt stmt -> return $ Program defs stmt
        _            -> throwE TypeError
    where
        mapDef :: (Monad m) => I.Definition -> ExceptT TCompileError m Definition
        mapDef (I.Definition s args stmt) = compile stmt >>= \stmt -> return $ Definition s args stmt
