module Interpreter.Eval where

import           Control.Monad              (when)
import           Control.Monad.State        (MonadState (get),
                                             MonadTrans (lift), StateT,
                                             evalStateT, modify)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Bits                  ((.&.), (.|.))
import           Data.Composition           ((.:))
import qualified Data.Map.Strict            as M
import           Syntax
import           Text.Read                  (readMaybe)

type VarMap = M.Map String Int
type VarScope = [String]
type DefMap = M.Map String Definition

data Error = ParsingErr String | NoReturn String | DivByZero deriving (Show)

type EvalM = StateT (VarMap, VarScope, DefMap) (ExceptT Error IO)

updateVar :: String -> Int -> EvalM ()
updateVar x v = modify $ \(mapVar, scope, mapDef) -> (M.insert x v mapVar, scope, mapDef)

addVar :: String -> EvalM ()
addVar x = modify $ \(mapVar, scope, mapDef) -> (M.insert x 0 mapVar, x:scope, mapDef)

mapBinOp :: M.Map BinOp (Int -> Int -> Int)
mapBinOp = M.fromList [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div),
                    (Mod, mod), (Eq, convert (==)), (Neq, convert (/=)), (Gt, convert (>)), (Ge, convert (>=)),
                    (Lt, convert (<)), (Le, convert (<=)), (And, ( .&. )), (Or, ( .|. ))]
    where
        convert :: (Int -> Int -> Bool) -> Int -> Int -> Int
        convert f = fromEnum .: f

mapUnOp :: M.Map UnOp (Int -> Int)
mapUnOp = M.fromList [(SubUn, negate)]

evalExpr :: Expression -> EvalM Int
evalExpr (Variable x) = do
  (mapVar, _, _) <- get
  return $ mapVar M.! x
evalExpr (Const x) = return x
evalExpr (BinExpr op l r) = do
  x <- evalExpr l
  y <- evalExpr r
  when (y == 0 && op == Div) $ lift (throwE DivByZero)
  return $ (mapBinOp M.! op) x y
evalExpr (UnExpr op e) = evalExpr e >>= \x -> return $ (mapUnOp M.! op) x
evalExpr (FunCall name args) = callFun name args

callFun :: String -> [Expression] -> EvalM Int
callFun name args = do
    (_, _, mapDef) <- get
    let (Definition _ argsNames stmts) = mapDef M.! name
    argsVals <- mapM evalExpr args
    let argsMap = M.fromList $ zip argsNames argsVals
    res <- lift $ evalStateT (evalStmts stmts) (argsMap, argsNames, mapDef)
    case res of
        Just x -> return x
        Nothing -> lift $ throwE $ NoReturn (name ++ show stmts ++ show argsVals)

readSafe :: (Read a, MonadTrans t, Monad m, Monad (t (ExceptT Error m))) => String -> t (ExceptT Error m) a
readSafe str =
  case readMaybe str of
    Just n  -> return n
    Nothing -> lift $ throwE $ ParsingErr str

evalStmts :: Statements -> EvalM (Maybe Int)
evalStmts stmts =
    case stmts of
        [] -> return Nothing
        (s:ss) -> evalStmt s >>=
            \v -> case v of
                Just x  -> return v
                Nothing -> evalStmts ss

evalStmt :: Statement -> EvalM (Maybe Int)
evalStmt (ReturnStmt e) = Just <$> evalExpr e
evalStmt (FunCallStmt name args) = callFun name args >> return Nothing
evalStmt (VarDecl x) = addVar x >> return Nothing
evalStmt (Assignment x e) = (evalExpr e >>= updateVar x) >> return Nothing
evalStmt (If e s1 s2) = evalExpr e >>= \x -> if x /= 0 then evalStmts s1 else evalStmts s2
evalStmt (While e s) = do
    x <- evalExpr e
    if x /= 0 then
        evalStmts (s ++ [While e s])
    else
        return Nothing
evalStmt (Read x) = (lift $ lift getLine) >>= readSafe >>= updateVar x >> return Nothing
evalStmt (Write e) = (evalExpr e >>= \v -> lift $ lift $ print v) >> return Nothing
evalStmt Skip = return Nothing


evalPrg :: Program -> IO ()
evalPrg (Program defs stmts) = do
    let mapDef = M.fromList $ map (\d@(Definition name _ _) -> (name, d)) defs
    res <- runExceptT $ evalStateT (evalStmts stmts) (M.empty, [], mapDef)
    case res of
        Left err -> print err
        Right _  -> return ()
