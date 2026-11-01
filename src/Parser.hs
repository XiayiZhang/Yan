module Parser
  ( parseProgram
  ) where

import AST
import Lexer (Line(..), lexProgram)
import Data.Text (Text, pack)
import Data.List (break, isPrefixOf, findIndex)

parseProgram :: String -> Either String Program
parseProgram input =
  case parseBlock 0 (lexProgram input) of
    Left err -> Left err
    Right (_, stmts) -> Right (Program stmts)

parseBlock :: Int -> [Line] -> Either String ([Line], [Stmt])
parseBlock _ [] = Right ([], [])
parseBlock indent lines =
  case span ((== indent) . fst . unLine) lines of
    (same, rest) ->
      if null same
        then if any ((< indent) . fst . unLine) lines
               then Right (rest, [])
               else Right ([], [])
        else do
          stmts <- mapM (parseStmt . snd . unLine) same
          Right (rest, stmts)

parseStmt :: String -> Either String Stmt
parseStmt line
  | "令 " `isPrefixOf` line = parseBindingOrFunc line
  | "若 " `isPrefixOf` line = parseIfStmt line
  | "归 " `isPrefixOf` line =
      let exprText = unwords (drop 1 (words line))
       in ReturnStmt <$> parseExprWords exprText
  | otherwise =
      let exprText = unwords (words line)
       in ExprStmt <$> parseExprWords exprText

parseBindingOrFunc :: String -> Either String Stmt
parseBindingOrFunc line =
  let ws = words line
      rest = drop 1 ws
   in if "之法" `elem` rest
        then parseFunctionDef line
        else do
          let lhsIdx = findIndex (== "者") rest
              rhsIdx = findIndex (== "也") rest
          case (lhsIdx, rhsIdx) of
            (Just i, Just j) | j > i ->
              let lhs = take i rest
                  rhs = take (j - i - 1) (drop (i + 1) rest)
                  lhsText = unwords lhs
                  rhsText = unwords rhs
               in if null rhsText
                    then Left "缺少定义体"
                    else if any (`elem` rhs) ["整", "数", "布尔", "真", "假"]
                           then Right (TypeStmt (pack lhsText) (parseTypeName rhsText))
                           else do
                                  expr <- parseExprWords rhsText
                                  Right (BindStmt (pack lhsText) expr)
            _ -> Left "绑定语法错误"

parseFunctionDef :: String -> Either String Stmt
parseFunctionDef line = do
  let ws = words line
      (beforeMethod, afterMethod) = splitByKeyword "之法" ws
      name = pack (unwords beforeMethod)
      afterSansPrefix = drop 1 afterMethod
      (paramsPart, retPart) = splitByKeyword "受" afterSansPrefix
      (_, returnPart) = splitByKeyword "归" retPart
      params = parseParamList paramsPart
      retType = parseTypeName (unwords returnPart)
  pure (FuncStmt (FuncDef name params retType []))

parseIfStmt :: String -> Either String Stmt
parseIfStmt line = do
  let rest = unwords (drop 1 (words line))
  cond <- parseExprWords rest
  pure (IfStmt cond [] [])

splitByKeyword :: String -> [String] -> ([String], [String])
splitByKeyword kw lst =
  case break (== kw) lst of
    (left, []) -> (left, [])
    (left, _ : right) -> (left, right)

parseParamList :: [String] -> [(Name, TypeName)]
parseParamList [] = []
parseParamList xs =
  let (names, types) = unzip (map (\s -> (pack s, parseTypeName s)) xs)
  in zip names types

parseTypeName :: String -> TypeName
parseTypeName s
  | s == "整" || s == "数" = TypeInt
  | s == "布尔" = TypeBool
  | s == "真" || s == "假" = TypeBool
  | otherwise = TypeAlias (pack s)

parseExprWords :: String -> Either String Expr
parseExprWords input =
  let ws = words input
   in parseExprWordsList ws

parseExprWordsList :: [String] -> Either String Expr
parseExprWordsList [] = Left "空表达式"
parseExprWordsList ["真"] = Right (BoolLit True)
parseExprWordsList ["假"] = Right (BoolLit False)
parseExprWordsList ["零"] = Right (IntLit 0)
parseExprWordsList ["一"] = Right (IntLit 1)
parseExprWordsList ["二"] = Right (IntLit 2)
parseExprWordsList [x] = case reads x :: [(Integer, String)] of
  [(n, "")] -> Right (IntLit n)
  _ -> Right (Var (pack x))
parseExprWordsList (x:op:y:rest)
  | op == "加" = BinaryOp Add <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "减" = BinaryOp Sub <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "乘" = BinaryOp Mul <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "除" = BinaryOp Div <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "等" = BinaryOp Eq <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "大于" = BinaryOp Gt <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "小于" = BinaryOp Lt <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "且" = BinaryOp And <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | op == "或" = BinaryOp Or <$> pure (parseVarOrLit x) <*> pure (parseVarOrLit y)
  | otherwise = Left ("无法解析操作符: " ++ op)
parseExprWordsList (x:xs) =
  case parseExprWordsList [x] of
    Right e -> Right e
    Left err -> Left err

parseVarOrLit :: String -> Expr
parseVarOrLit s =
  case reads s :: [(Integer, String)] of
    [(n, "")] -> IntLit n
    _ -> case s of
      "真" -> BoolLit True
      "假" -> BoolLit False
      _ -> Var (pack s)
