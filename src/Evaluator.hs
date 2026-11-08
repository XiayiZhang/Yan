module Evaluator
  ( EvalError(..)
  , Value(..)
  , evalProgram
  ) where

import AST
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

newtype Env = Env (Map Name Value)
  deriving (Eq, Show)

data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VArray [Value]
  | VClosure Name [Name] [Stmt] Env
  | VUnit
  deriving (Eq, Show)

data EvalError
  = UnknownVar Name
  | TypeMismatch String
  | InvalidCall Name
  | InvalidCondition Value
  | InvalidArrayIndex Value
  | NotArray Name
  | IndexOutOfBounds Integer Int
  | ArityMismatch Name Int Int
  | GenericError String
  deriving (Eq, Show)

defaultEnv :: Env
defaultEnv = Env Map.empty

lookupEnv :: Env -> Name -> Maybe Value
lookupEnv (Env env) name = Map.lookup name env

insertEnv :: Env -> Name -> Value -> Env
insertEnv (Env env) name value = Env (Map.insert name value env)

evalProgram :: Program -> IO (Either EvalError Value)
evalProgram (Program statements) = do
  result <- evalStmts defaultEnv statements
  pure $ fmap (\(value, _, _) -> value) result

evalStmts :: Env -> [Stmt] -> IO (Either EvalError (Value, Env, Bool))
evalStmts env [] = pure (Right (VUnit, env, False))
evalStmts env (statement : rest) = do
  result <- evalStmt env statement
  case result of
    Left err -> pure (Left err)
    Right (value, nextEnv, returned)
      | returned -> pure (Right (value, nextEnv, True))
      | otherwise -> do
          remaining <- evalStmts nextEnv rest
          pure $ case remaining of
            Left err -> Left err
            Right (finalValue, finalEnv, didReturn) ->
              Right (if finalValue == VUnit then value else finalValue, finalEnv, didReturn)

evalStmt :: Env -> Stmt -> IO (Either EvalError (Value, Env, Bool))
evalStmt env (BindStmt name expr) = do
  result <- evalExpr env expr
  pure $ fmap (\value -> (VUnit, insertEnv env name value, False)) result
evalStmt env (TypeStmt _ _) = pure (Right (VUnit, env, False))
evalStmt env (FuncStmt function) =
  let name = funcName function
      closure = VClosure name (map fst (funcParams function)) (funcBody function) recursiveEnv
      recursiveEnv = insertEnv env name closure
   in pure (Right (VUnit, recursiveEnv, False))
evalStmt env (ReturnStmt expr) = do
  result <- evalExpr env expr
  pure $ fmap (\value -> (value, env, True)) result
evalStmt env (PrintStmt expr) = do
  result <- evalExpr env expr
  case result of
    Left err -> pure (Left err)
    Right value -> do
      putStrLn (renderValue value)
      pure (Right (VUnit, env, False))
evalStmt env (SetArrayStmt name indexExpr valueExpr) = do
  indexResult <- evalExpr env indexExpr
  valueResult <- evalExpr env valueExpr
  pure $ do
    indexValue <- indexResult
    index <- expectIndex indexValue
    value <- valueResult
    currentValue <- maybe (Left (UnknownVar name)) Right (lookupEnv env name)
    elements <- case currentValue of
      VArray values -> Right values
      _ -> Left (NotArray name)
    if index < 0 || index >= toInteger (length elements)
      then Left (IndexOutOfBounds index (length elements))
      else Right ()
    let position = fromInteger index
        updated =
          VArray
            (take position elements ++ [value] ++ drop (position + 1) elements)
    pure (VUnit, insertEnv env name updated, False)
evalStmt env (IfStmt condition thenBranch elseBranch) = do
  conditionResult <- evalExpr env condition
  case conditionResult of
    Left err -> pure (Left err)
    Right (VBool True) -> evalStmts env thenBranch
    Right (VBool False) -> evalStmts env elseBranch
    Right other -> pure (Left (InvalidCondition other))
evalStmt env (ExprStmt expr) = do
  result <- evalExpr env expr
  pure $ fmap (\value -> (value, env, False)) result
evalStmt env (Block statements) = evalStmts env statements

evalExpr :: Env -> Expr -> IO (Either EvalError Value)
evalExpr _ (IntLit value) = pure (Right (VInt value))
evalExpr _ (BoolLit value) = pure (Right (VBool value))
evalExpr _ (StringLit value) = pure (Right (VString (Text.unpack value)))
evalExpr env (ArrayLit elements) = do
  results <- mapM (evalExpr env) elements
  pure (VArray <$> sequence results)
evalExpr env (ArrayGet arrayExpr indexExpr) = do
  arrayResult <- evalExpr env arrayExpr
  indexResult <- evalExpr env indexExpr
  pure $ do
    arrayValue <- arrayResult
    indexValue <- indexResult
    index <- expectIndex indexValue
    elements <- case arrayValue of
      VArray values -> Right values
      other -> Left (TypeMismatch ("取 expects an array, got " ++ renderValue other))
    if index < 0 || index >= toInteger (length elements)
      then Left (IndexOutOfBounds index (length elements))
      else Right ()
    pure (elements !! fromInteger index)
evalExpr env (Var name) =
  pure $ case lookupEnv env name of
    Just value -> Right value
    Nothing -> Left (UnknownVar name)
evalExpr env (UnaryOp Not expr) = do
  result <- evalExpr env expr
  pure $ do
    value <- result
    case value of
      VBool boolean -> Right (VBool (not boolean))
      _ -> Left (TypeMismatch "Not expects Bool")
evalExpr env (BinaryOp operator left right) = do
  leftResult <- evalExpr env left
  rightResult <- evalExpr env right
  pure $ do
    leftValue <- leftResult
    rightValue <- rightResult
    evalBinary operator leftValue rightValue
evalExpr env (Call name args) = do
  case lookupEnv env name of
    Nothing -> pure (Left (UnknownVar name))
    Just (VClosure functionName params body closureEnv)
      | length params /= length args ->
          pure (Left (ArityMismatch name (length params) (length args)))
      | otherwise -> do
          argumentResults <- mapM (evalExpr env) args
          case sequence argumentResults of
            Left err -> pure (Left err)
            Right values -> do
              let callEnv =
                    foldl
                      (\current (param, value) -> insertEnv current param value)
                      closureEnv
                      (zip params values)
              result <- evalStmts (insertEnv callEnv functionName (VClosure functionName params body closureEnv)) body
              pure $ case result of
                Left err -> Left err
                Right (value, _, _) -> Right value
    Just _ -> pure (Left (InvalidCall name))
evalExpr env (IfExpr condition trueExpr falseExpr) = do
  conditionResult <- evalExpr env condition
  case conditionResult of
    Left err -> pure (Left err)
    Right (VBool True) -> evalExpr env trueExpr
    Right (VBool False) -> evalExpr env falseExpr
    Right other -> pure (Left (InvalidCondition other))

evalBinary :: BinOp -> Value -> Value -> Either EvalError Value
evalBinary Add (VInt left) (VInt right) = Right (VInt (left + right))
evalBinary Sub (VInt left) (VInt right) = Right (VInt (left - right))
evalBinary Mul (VInt left) (VInt right) = Right (VInt (left * right))
evalBinary Div (VInt _) (VInt 0) = Left (GenericError "division by zero")
evalBinary Div (VInt left) (VInt right) = Right (VInt (left `div` right))
evalBinary Eq left right = Right (VBool (left == right))
evalBinary Neq left right = Right (VBool (left /= right))
evalBinary Lt (VInt left) (VInt right) = Right (VBool (left < right))
evalBinary Gt (VInt left) (VInt right) = Right (VBool (left > right))
evalBinary And (VBool left) (VBool right) = Right (VBool (left && right))
evalBinary Or (VBool left) (VBool right) = Right (VBool (left || right))
evalBinary operator left right =
  Left (TypeMismatch ("Invalid operands for " ++ show operator ++ ": " ++ renderValue left ++ ", " ++ renderValue right))

expectIndex :: Value -> Either EvalError Integer
expectIndex (VInt index) = Right index
expectIndex other = Left (InvalidArrayIndex other)

renderValue :: Value -> String
renderValue (VInt value) = show value
renderValue (VBool True) = "真"
renderValue (VBool False) = "假"
renderValue (VString value) = value
renderValue (VArray values) = "列 " ++ intercalate " " (map renderValue values)
renderValue (VClosure name _ _ _) = "<函数 " ++ show name ++ ">"
renderValue VUnit = "无"
