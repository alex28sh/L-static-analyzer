{-# LANGUAGE Rank2Types #-}
module Typed.PostProcessing where


import           Control.Monad              (when)
import           Control.Monad.State        (MonadState (get),
                                             MonadTrans (lift),
                                             StateT (runStateT), evalStateT,
                                             modify, put)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.List
import           Intermediate.Syntax        (Expression (..))
import           Typed.Syntax


data TPostError = TypeFunError | NonExistingVar String | NonExistingFun String
  deriving (Eq, Show)

checkFunCallsExpr :: (Monad m) => Definitions -> Expression -> ExceptT TPostError m ()
checkFunCallsExpr defs (BinExpr _ l r) = checkFunCallsExpr defs l >> checkFunCallsExpr defs r
checkFunCallsExpr defs (UnExpr _ l) = checkFunCallsExpr defs l
checkFunCallsExpr defs (FunCall s es) = mapM_ (checkFunCallsExpr defs) es >> do
    let f = find (\(Definition name _ _ ) -> name == s) defs
    case f of
        Just (Definition _ _ (IntStmt _)) -> return ()
        Just _                            -> throwE TypeFunError
        Nothing                           -> throwE $ NonExistingFun s
checkFunCallsExpr _ _  = return ()


checkFunCallsStmt :: (Monad m) => Definitions -> Statement a -> ExceptT TPostError m ()
checkFunCallsStmt defs (Seq q1 q2) = checkFunCallsStmt defs q1 >> checkFunCallsStmt defs q2
checkFunCallsStmt defs (SeqInt q1 q2) = checkFunCallsStmt defs q1 >> checkFunCallsStmt defs q2
checkFunCallsStmt defs (Assignment _ e) = checkFunCallsExpr defs e
checkFunCallsStmt defs (While e s) = checkFunCallsExpr defs e >> checkFunCallsStmt defs s
checkFunCallsStmt defs (If e s1 s2) = checkFunCallsExpr defs e >> checkFunCallsStmt defs s1 >> checkFunCallsStmt defs s2
checkFunCallsStmt defs (Write e) = checkFunCallsExpr defs e
checkFunCallsStmt _ _ = return ()


type Env = [String]

checkVarsExpr :: (Monad m) => Expression -> StateT Env (ExceptT TPostError m) ()
checkVarsExpr (BinExpr _ l r) = checkVarsExpr l >> checkVarsExpr r
checkVarsExpr (UnExpr _ l) = checkVarsExpr l
checkVarsExpr (FunCall _ es) = mapM_ checkVarsExpr es
checkVarsExpr (Variable x) = get >>= \env -> when (x `notElem` env) (lift $ throwE $ NonExistingVar x)
checkVarsExpr _ = return ()

checkVarsStmt :: (Monad m) => Statement a -> StateT Env (ExceptT TPostError m) ()
checkVarsStmt (Seq q1 q2) = checkVarsStmt q1 >> checkVarsStmt q2
checkVarsStmt (SeqInt q1 q2) = checkVarsStmt q1 >> checkVarsStmt q2
checkVarsStmt (Assignment x e) = get >>= \env -> when (x `notElem` env) (lift $ throwE $ NonExistingVar x) >> checkVarsExpr e
checkVarsStmt (While e s) = checkVarsExpr e >> checkVarsStmt s
checkVarsStmt (If e s1 s2) = checkVarsExpr e >> get >>= \st -> put st >> checkVarsStmt s1 >> put st >> checkVarsStmt s2
checkVarsStmt (Write e) = checkVarsExpr e
checkVarsStmt (VarDecl x) = modify (x:)
checkVarsStmt _ = return ()

wrapper :: (forall a. Statement a -> f) -> AnyStmt -> f
wrapper f (IntStmt s)  = f s
wrapper f (UnitStmt s) = f s

performChecks :: (Monad m) => Program -> ExceptT TPostError m ()
performChecks (Program defs stmt) =
    (pure (map (\(Definition _ _ st) -> st) defs ++ [IntStmt stmt]) >>= \stmts ->
    mapM_ (wrapper (checkFunCallsStmt defs)) stmts) >>
    evalStateT (checkVarsStmt stmt) [] >>
    mapM_ (\(Definition _ args st) -> wrapper (\st -> evalStateT (checkVarsStmt st) args) st) defs
