module Evaluator
  ( EvalError(..)
  , Value(..)
  , evalProgram
  ) where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Env = Env (Map Name Value)
  deriving (Eq, Show)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Name [Name] [Stmt] Env
  | VUnit
  deriving (Eq, Show)

data EvalError
  = UnknownVar Name
  | TypeMismatch String
  | InvalidCall Name
  | InvalidCondition Value
  | GenericError String
  deriving (Eq, Show)

defaultEnv :: Env
defaultEnv = Env Map.empty

lookupEnv :: Env -> Name -> Maybe Value
lookupEnv (Env env) name = Map.lookup name env

insertEnv :: Env -> Name -> Value -> Env
insertEnv (Env env) k v = Env (Map.insert k v env)

evalProgram :: Program -> Either EvalError Value
evalProgram (Program stmts) = evalStmts defaultEnv stmts

evalStmts :: Env -> [Stmt] -> Either EvalError Value
evalStmts env [] = Right VUnit
evalStmts env (stmt:rest) = do
  result <- evalStmt env stmt
  case result of
    VUnit -> evalStmts env rest
    value -> Right value

evalStmt :: Env -> Stmt -> Either EvalError Value
evalStmt env (BindStmt name expr) = do
  value <- evalExpr env expr
  let env' = insertEnv env name value
  pure VUnit

evalStmt env (TypeStmt name typ) = pure VUnit
evalStmt env (FuncStmt funcDef) = do
  let closure = VClosure (funcName funcDef) (map fst (funcParams funcDef)) (funcBody funcDef) env
  let env' = insertEnv env (funcName funcDef) closure
  pure VUnit

evalStmt env (ReturnStmt expr) = evalExpr env expr
evalStmt env (IfStmt cond thenBranch elseBranch) = do
  value <- evalExpr env cond
  case value of
    VBool True -> evalStmts env thenBranch
    VBool False -> evalStmts env elseBranch
    other -> Left (InvalidCondition other)

evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (Block stmts) = evalStmts env stmts

evalExpr :: Env -> Expr -> Either EvalError Value
evalExpr _ (IntLit n) = Right (VInt n)
evalExpr _ (BoolLit b) = Right (VBool b)
evalExpr env (Var name) =
  case lookupEnv env name of
    Just value -> Right value
    Nothing -> Left (UnknownVar name)

evalExpr env (UnaryOp Not e) = do
  val <- evalExpr env e
  case val of
    VBool b -> Right (VBool (not b))
    _ -> Left (TypeMismatch "Not expects Bool")

evalExpr env (BinaryOp Add left right) = evalArithmetic (+) env left right
evalExpr env (BinaryOp Sub left right) = evalArithmetic (-) env left right
evalExpr env (BinaryOp Mul left right) = evalArithmetic (*) env left right
evalExpr env (BinaryOp Div left right) = evalArithmetic div env left right
evalExpr env (BinaryOp Eq left right) = do
  lv <- evalExpr env left
  rv <- evalExpr env right
  pure (VBool (lv == rv))

evalExpr env (BinaryOp Lt left right) = do
  lv <- evalExpr env left
  rv <- evalExpr env right
  case (lv, rv) of
    (VInt a, VInt b) -> Right (VBool (a < b))
    _ -> Left (TypeMismatch "Lt expects Int")

evalExpr env (BinaryOp Gt left right) = do
  lv <- evalExpr env left
  rv <- evalExpr env right
  case (lv, rv) of
    (VInt a, VInt b) -> Right (VBool (a > b))
    _ -> Left (TypeMismatch "Gt expects Int")

evalExpr env (BinaryOp And left right) = do
  lv <- evalExpr env left
  rv <- evalExpr env right
  case (lv, rv) of
    (VBool a, VBool b) -> Right (VBool (a && b))
    _ -> Left (TypeMismatch "And expects Bool")

evalExpr env (BinaryOp Or left right) = do
  lv <- evalExpr env left
  rv <- evalExpr env right
  case (lv, rv) of
    (VBool a, VBool b) -> Right (VBool (a || b))
    _ -> Left (TypeMismatch "Or expects Bool")

evalExpr env (Call name args) = do
  closure <- lookupValue env name
  case closure of
    VClosure funcName params body fnEnv -> do
      values <- mapM (evalExpr env) args
      let extended = foldl (\acc (param, value) -> insertEnv acc param value) fnEnv (zip params values)
      let callEnv = insertEnv extended funcName closure
      evalStmts callEnv body
    _ -> Left (InvalidCall name)

evalExpr env (IfExpr cond tBranch fBranch) = do
  cv <- evalExpr env cond
  case cv of
    VBool True -> evalExpr env tBranch
    VBool False -> evalExpr env fBranch
    other -> Left (InvalidCondition other)

evalArithmetic :: (Integer -> Integer -> Integer) -> Env -> Expr -> Expr -> Either EvalError Value
evalArithmetic op env left right = do
  lv <- evalExpr env left
  rv <- evalExpr env right
  case (lv, rv) of
    (VInt a, VInt b) -> Right (VInt (op a b))
    _ -> Left (TypeMismatch "Arithmetic expects Int")

lookupValue :: Env -> Name -> Either EvalError Value
lookupValue env name =
  case lookupEnv env name of
    Just value -> Right value
    Nothing -> Left (UnknownVar name)
