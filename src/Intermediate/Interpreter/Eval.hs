module Intermediate.Interpreter.Eval where

import           Control.Monad              (when)
import           Control.Monad.State        (MonadState (get),
                                             MonadTrans (lift), StateT,
                                             evalStateT, runStateT, modify)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Bits                  ((.&.), (.|.))
import           Data.Composition           ((.:))
import qualified Data.Map.Strict            as M
import           Intermediate.Syntax
import           Text.Read                  (readMaybe)

type Addr = Int
type Heap = M.Map Addr (M.Map Int Value)

data Value = VInt Int | VPtr Addr
    deriving (Show)

type VarMap = M.Map String Value
type VarScope = [String]
type DefMap = M.Map String Definition

type EvalState = (VarMap, VarScope, DefMap, Heap, Addr)  -- last Addr is next free

data Error = ParsingErr String | NoReturn String | DivByZero deriving (Show)

type EvalM = StateT EvalState (ExceptT Error IO)

updateVar :: String -> Value -> EvalM ()
updateVar x v = modify $ \(mapVar, scope, mapDef, heap, nextA) -> (M.insert x v mapVar, scope, mapDef, heap, nextA)

updateVarInt :: String -> Int -> EvalM ()
updateVarInt x v = updateVar x (VInt v)

addVar :: String -> EvalM ()
addVar x = modify $ \(mapVar, scope, mapDef, heap, nextA) -> (M.insert x (VInt 0) mapVar, x:scope, mapDef, heap, nextA)

mapBinOp :: M.Map BinOp (Int -> Int -> Int)
mapBinOp = M.fromList [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div),
                    (Mod, mod), (Eq, convert (==)), (Neq, convert (/=)), (Gt, convert (>)), (Ge, convert (>=)),
                    (Lt, convert (<)), (Le, convert (<=)), (And, ( .&. )), (Or, ( .|. ))]
    where
        convert :: (Int -> Int -> Bool) -> Int -> Int -> Int
        convert f = fromEnum .: f

mapUnOp :: M.Map UnOp (Int -> Int)
mapUnOp = M.fromList [(SubUn, negate)]

evalAccess :: Access -> EvalM Value
evalAccess (Variable x) = do
  (mapVar, _, _, _, _) <- get
  return $ mapVar M.! x
evalAccess (ArrayIdx arrE idxE) = do
  idx <- evalExpr idxE
  arrVal <- evalAccess arrE
  case arrVal of
    VPtr addr -> do
      (_,_,_,heap,_) <- get
      let arrMap = M.findWithDefault M.empty addr heap
      return $ M.findWithDefault (VInt 0) idx arrMap
    _ -> error "Indexing non-array"

evalExpr :: Expression -> EvalM Int
evalExpr (Access acc) = do 
  v <- evalAccess acc
  case v of
    VInt n -> return n
    VPtr _ -> error "Expected an array index"
evalExpr (Const x) = return x
evalExpr (BinExpr op l r) = do
  x <- evalExpr l
  y <- evalExpr r
  when (y == 0 && op == Div) $ lift (throwE DivByZero)
  return $ (mapBinOp M.! op) x y
evalExpr (UnExpr op e) = evalExpr e >>= \x -> return $ (mapUnOp M.! op) x
evalExpr (FunCall name args) = callFun name args
evalExpr (NewArr _ sizeExpr) = evalExpr sizeExpr >> return 0 -- new returns dummy int for now


callFun :: String -> [Expression] -> EvalM Int
callFun name args = do
    (vm, sc, mapDef, heap, nextA) <- get
    let (Definition _ argsPairs stmts) = mapDef M.! name
    let argsNames = map snd argsPairs
    argsVals <- mapM evalValue args
    let argsMap = M.fromList $ zip argsNames argsVals

    -- Run function body and capture resulting heap / addr
    let initialState = (argsMap, argsNames, mapDef, heap, nextA)
    let runM = runStateT (evalStmt stmts) initialState
    (res, (_, _, _, heap', nextA')) <- lift runM

    -- Propagate heap changes back to caller
    modify $ \(v,s,d,_,_) -> (v,s,d,heap',nextA')

    case res of
      Just x  -> return x
      Nothing -> lift $ throwE $ NoReturn (name ++ show stmts ++ show argsVals)

readSafe :: (Read a, MonadTrans t, Monad m, Monad (t (ExceptT Error m))) => String -> t (ExceptT Error m) a
readSafe str =
  case readMaybe str of
    Just n  -> return n
    Nothing -> lift $ throwE $ ParsingErr str

evalStmt :: Statement -> EvalM (Maybe Int)
evalStmt (SeqStmt s ss) = evalStmt s >>=
    \v -> case v of
        Just x  -> return v
        Nothing -> evalStmt ss
evalStmt (ReturnStmt e) = Just <$> evalExpr e
evalStmt (FunCallStmt name args) = callFun name args >> return Nothing
evalStmt (VarDecl _ x) = addVar x >> return Nothing
evalStmt (Assignment lhs rhs) = do
  -- Determine value to store
  val <- case rhs of
           NewArr _ szE -> do
             sz <- evalExpr szE
             addr <- allocArray sz
             return (VPtr addr)
           _ -> VInt <$> evalExpr rhs

  case lhs of
    Variable name -> updateVar name val >> return Nothing
    _ -> do
      (rootVar, indices) <- collectIndices lhs
      (vm, sc, md, heap, nextA) <- get
      rootPtrAddr <- case M.lookup rootVar vm of
                        Just (VPtr a) -> return a
                        _ -> lift $ throwE $ ParsingErr ("Variable " ++ rootVar ++ " is not an array pointer")
      -- write recursively
      let heap' = writeArray rootPtrAddr indices val heap
      modify $ \(v,s,d,_,n) -> (v,s,d,heap',n)
      return Nothing

  where
    collectIndices :: Access -> EvalM (String, [Int])
    collectIndices (Variable x) = return (x, [])
    collectIndices (ArrayIdx acc idxE) = do
      (root, rest) <- collectIndices acc
      idxVal <- evalExpr idxE
      return (root, rest ++ [idxVal])

    -- writeArray addr [i1,i2,..] val heap
    writeArray :: Addr -> [Int] -> Value -> Heap -> Heap
    writeArray addr [i] v h =
      let arrMap = M.findWithDefault M.empty addr h
          newArr = M.insert i v arrMap
      in M.insert addr newArr h
    writeArray addr (i:is) v h =
      let arrMap = M.findWithDefault M.empty addr h
          ptrVal = M.findWithDefault (VPtr (-1)) i arrMap
          nextAddr = case ptrVal of
                       VPtr a | a >= 0 -> a
                       _ -> error "Indexing non-array in writeArray"
          h' = writeArray nextAddr is v h
          arrMap' = M.insert i (VPtr nextAddr) (M.findWithDefault M.empty addr h')
      in M.insert addr arrMap' h'
    writeArray _ [] _ h = h

evalStmt (If e s1 s2) = evalExpr e >>= \x -> evalStmt (if x /= 0 then s1 else s2)
evalStmt (While e s) = do
    x <- evalExpr e
    if x /= 0 then evalStmt (SeqStmt s $ While e s) else return Nothing
evalStmt (Read (Variable x)) = 
    (lift $ lift $ putStrLn ("Enter value for variable " ++ x ++ ":")) >>
    (lift $ lift getLine) >>= readSafe >>= \n -> updateVar x (VInt n) >> return Nothing
evalStmt (Read _) = (lift $ throwE $ ParsingErr "Can only read into simple variable") >> return Nothing
evalStmt (Write e) = evalExpr e >>= \v -> lift (lift $ print v) >> return Nothing
evalStmt Skip = return Nothing

evalPrg :: Program -> IO ()
evalPrg (Program defs stmts) = do
    let mapDef = M.fromList $ map (\d@(Definition name _ _) -> (name, d)) defs
    res <- runExceptT $ evalStateT (evalStmt stmts) (M.empty, [], mapDef, M.empty, 0)
    case res of
        Left err -> print err
        Right _  -> return ()

-- allocate array in heap, return address
allocArray :: Int -> EvalM Addr
allocArray sz = do
  modify $ \(vm, sc, md, heap, nextA) -> (vm, sc, md, M.insert nextA (M.fromList [(i, VInt 0) | i <- [0..sz-1]]) heap, nextA+1)
  (_,_,_,_,addr) <- get
  return (addr-1)

evalValue :: Expression -> EvalM Value
evalValue (Access acc) = evalAccess acc
evalValue e = VInt <$> evalExpr e
