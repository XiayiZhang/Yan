module Parser
  ( parseProgram
  ) where

import AST
import Lexer (Line(..), lexProgram)
import Data.Text (pack)
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
  case words line of
    "令" : declaration -> do
      (nameWords, afterMethod) <- splitRequired "之法" declaration
      if null nameWords
        then Left "函数定义缺少函数名"
        else case afterMethod of
          "受" : parameterAndReturn -> do
            (parameterWords, afterReturn) <- splitRequired "归" parameterAndReturn
            (returnTypeWords, trailing) <- splitRequired "者" afterReturn
            if null returnTypeWords
              then Left "函数定义缺少返回类型"
              else if not (null trailing)
                then Left "函数定义结束符“者”后存在多余内容"
                else do
                  params <- parseParamList parameterWords
                  let returnType = parseTypeName (unwords returnTypeWords)
                  pure (FuncStmt (FuncDef (pack (unwords nameWords)) params returnType []))
          _ -> Left "函数定义缺少“受”"
    _ -> Left "函数定义必须以“令”开头"

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

splitRequired :: String -> [String] -> Either String ([String], [String])
splitRequired keyword tokens =
  case break (== keyword) tokens of
    (_, []) -> Left ("函数定义缺少“" ++ keyword ++ "”")
    (before, _ : after) -> Right (before, after)

parseParamList :: [String] -> Either String [(Name, TypeName)]
parseParamList = traverse parseParam
  where
    parseParam token
      | token `elem` ["令", "之法", "受", "归", "者", "也"] =
          Left ("函数参数中出现保留字“" ++ token ++ "”")
      | otherwise = Right (pack token, parseTypeName token)

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
parseExprWordsList [x] =
  case parseInteger x of
    Just n -> Right (IntLit n)
    Nothing -> Right (Var (pack x))
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
  case s of
    "真" -> BoolLit True
    "假" -> BoolLit False
    _ -> case parseInteger s of
      Just n -> IntLit n
      Nothing -> Var (pack s)

parseInteger :: String -> Maybe Integer
parseInteger input =
  case reads input :: [(Integer, String)] of
    [(n, "")] -> Just n
    _ -> parseChineseNumber input

parseChineseNumber :: String -> Maybe Integer
parseChineseNumber input
  | null input = Nothing
  | otherwise = parseSections input Nothing 0
  where
    parseSections [] _ total = Just total
    parseSections remaining previousUnit total =
      let (section, rest) = break isLargeUnit remaining
       in case rest of
            [] -> do
              sectionValue <- parseSmallNumber section
              pure (total + sectionValue)
            unitChar : afterUnit -> do
              unit <- largeUnitValue unitChar
              if maybe False (unit >=) previousUnit
                then Nothing
                else do
                  sectionValue <- parseSmallNumber section
                  if sectionValue == 0
                    then Nothing
                    else parseSections afterUnit (Just unit) (total + sectionValue * unit)

isLargeUnit :: Char -> Bool
isLargeUnit c = c == '万' || c == '亿'

largeUnitValue :: Char -> Maybe Integer
largeUnitValue '万' = Just 10000
largeUnitValue '亿' = Just 100000000
largeUnitValue _ = Nothing

parseSmallNumber :: String -> Maybe Integer
parseSmallNumber [] = Nothing
parseSmallNumber digits
  | all isChineseDigit digits =
      readDigits <$> traverse chineseDigitValue digits
  | otherwise = do
      (total, pending, _) <- foldl parseSmallChar (Just (0, Nothing, 10000)) digits
      pure (total + maybe 0 id pending)
  where
    readDigits = foldl (\value digit -> value * 10 + toInteger digit) 0

    parseSmallChar Nothing _ = Nothing
    parseSmallChar (Just (total, pending, previousUnit)) c
      | isChineseDigit c = do
          digit <- chineseDigitValue c
          case pending of
            Just _ -> Nothing
            Nothing
              | digit == 0 -> Just (total, Nothing, previousUnit)
              | otherwise -> Just (total, Just (toInteger digit), previousUnit)
      | otherwise = do
          unit <- smallUnitValue c
          if unit >= previousUnit
            then Nothing
            else do
              coefficient <- case pending of
                Just value -> Just value
                Nothing
                  | unit == 10 && total == 0 -> Just 1
                  | otherwise -> Nothing
              Just (total + coefficient * unit, Nothing, unit)

isChineseDigit :: Char -> Bool
isChineseDigit c = c `elem` "零〇一二两三四五六七八九"

chineseDigitValue :: Char -> Maybe Int
chineseDigitValue c =
  lookup c
    [ ('零', 0), ('〇', 0), ('一', 1), ('二', 2), ('两', 2)
    , ('三', 3), ('四', 4), ('五', 5), ('六', 6)
    , ('七', 7), ('八', 8), ('九', 9)
    ]

smallUnitValue :: Char -> Maybe Integer
smallUnitValue '十' = Just 10
smallUnitValue '百' = Just 100
smallUnitValue '千' = Just 1000
smallUnitValue _ = Nothing
