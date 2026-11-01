module Main where

import AST (Program)
import System.Environment (getArgs)
import Parser (parseProgram)
import Evaluator (evalProgram)

main :: IO ()
main = do
  args <- getArgs
  let source = unlines
        [ "令 整 者 数 也"
        , "令 布尔 者 真 假 也"
        , "令 求阶乘 之法 受 数 归 数 者"
        , "    若 数 等 于 零"
        , "        归 一"
        , "    若 非"
        , "        归 数 乘 求阶乘 数 减 一"
        ]
  case args of
    [] -> runProgram False source
    ["--debug-ast"] -> runProgram True source
    _ -> putStrLn "Usage: Yan [--debug-ast]"

runProgram :: Bool -> String -> IO ()
runProgram debugAst source =
  case parseProgram source of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right program -> do
      if debugAst
        then printAst program
        else pure ()
      case evalProgram program of
        Left err -> print err
        Right value -> print value

printAst :: Program -> IO ()
printAst program = do
  putStrLn "AST:"
  print program
