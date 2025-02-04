
module Typed.Eval where

import           Control.Monad                 (void, when)
import           Control.Monad.State           (MonadState (get),
                                                MonadTrans (lift), StateT,
                                                evalStateT, modify)
import           Control.Monad.Trans.Except    (ExceptT, runExceptT, throwE)
import qualified Data.Map.Strict               as M
import           Intermediate.Interpreter.Eval (mapBinOp, mapUnOp)
import           Intermediate.Syntax           (BinOp (..), Expression (..),
                                                UnOp (..))
import           Text.Read                     (readMaybe)
import           Typed.PostProcessing          (performChecks)
import           Typed.Syntax

data Error = ParsingErr String | DivByZero deriving (Show)

type VarMap = M.Map String Int
type DefMap = M.Map String Definition
type EvalM = StateT (VarMap, DefMap) (ExceptT Error IO)

updateVar :: String -> Int -> EvalM ()
updateVar x v = modify $ \(mapVar, mapDef) -> (M.insert x v mapVar, mapDef)

addVar :: String -> EvalM ()
addVar x = modify $ \(mapVar, mapDef) -> (M.insert x 0 mapVar, mapDef)

evalExpr :: Expression -> EvalM Int
evalExpr (Variable s) = do
  (varMap, _) <- get
  return $ varMap M.! s
evalExpr (Const x) = return x
evalExpr (BinExpr op e1 e2) = do
  x <- evalExpr e1
  y <- evalExpr e2
  when (y == 0 && op == Div) $ lift (throwE DivByZero)
  return $ (mapBinOp M.! op) x y
evalExpr (UnExpr op e) = evalExpr e >>= \x -> return $ (mapUnOp M.! op) x
evalExpr (FunCall name args) = callFun name args

readSafe :: (Read a, MonadTrans t, Monad m, Monad (t (ExceptT Error m))) => String -> t (ExceptT Error m) a
readSafe str =
  case readMaybe str of
    Just n  -> return n
    Nothing -> lift $ throwE $ ParsingErr str

callFun :: String -> [Expression] -> EvalM Int
callFun name args = do
    (_, mapDef) <- get
    let (Definition _ argsNames (IntStmt stmts)) = mapDef M.! name
    argsVals <- mapM evalExpr args
    let argsMap = M.fromList $ zip argsNames argsVals
    lift $ evalStateT (evalStmt stmts) (argsMap, mapDef)

callFunVoid :: String -> [Expression] -> EvalM ()
callFunVoid name args = do
    (_, mapDef) <- get
    let (Definition _ argsNames stmts) = mapDef M.! name
    argsVals <- mapM evalExpr args
    let argsMap = M.fromList $ zip argsNames argsVals
    case stmts of
      (IntStmt stmts) -> void $ lift $ evalStateT (evalStmt stmts) (argsMap, mapDef)
      (UnitStmt stmts) -> lift $ evalStateT (evalStmt stmts) (argsMap, mapDef)

evalStmt :: Statement a -> EvalM a
evalStmt (SeqInt s _) = evalStmt s
evalStmt (Seq s1 s2) = evalStmt s1 >> evalStmt s2
evalStmt (ReturnStmt e) = evalExpr e
evalStmt (VarDecl x) = addVar x
evalStmt Skip = return ()
evalStmt (Assignment x e) = evalExpr e >>= updateVar x
evalStmt (If e s1 s2) = evalExpr e >>= \x -> evalStmt (if x /= 0 then s1 else s2)
evalStmt (While e s) = evalExpr e >>= \x -> when (x /= 0) (evalStmt (Seq s $ While e s))
evalStmt (Read x) = (lift $ lift getLine) >>= readSafe >>= updateVar x
evalStmt (Write e) = evalExpr e >>= \v -> lift $ lift $ print v
evalStmt (FunCallStmt name args) = callFunVoid name args


evalPrg :: Program -> IO ()
evalPrg p@(Program defs stmts) = do
    let mapDef = M.fromList $ map (\d@(Definition name _ _) -> (name, d)) defs
    reschecks <- runExceptT (performChecks p)
    case reschecks of
      Left err -> error $ show err
      Right () -> print "checked successfully"
    res <- runExceptT $ evalStateT (evalStmt stmts) (M.empty, mapDef)
    case res of
      Left err -> print err
      Right _  -> return ()
