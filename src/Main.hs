module Main where

import AST (Program)
import Control.Exception (evaluate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, ioeGetErrorString)
import Data.List (isSuffixOf)
import Parser (parseProgram)
import Evaluator (evalProgram)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr "Usage: Yan [--debug-ast] <file.yn>"
      exitFailure
    Right (debugAst, path)
      | not (".yn" `isSuffixOf` path) -> do
          hPutStrLn stderr "Input file must have a .yn extension."
          exitFailure
      | otherwise -> do
          source <- catchIOError (readSource path) $ \err -> do
            hPutStrLn stderr ("Could not read " ++ path ++ ": " ++ ioeGetErrorString err)
            exitFailure
          runProgram debugAst source

parseArgs :: [String] -> Either String (Bool, FilePath)
parseArgs [path] = Right (False, path)
parseArgs ["--debug-ast", path] = Right (True, path)
parseArgs [path, "--debug-ast"] = Right (True, path)
parseArgs _ = Left "Expected one .yn source file and an optional --debug-ast flag."

readSource :: FilePath -> IO String
readSource path = do
  source <- readFile path
  _ <- evaluate (length source)
  pure source

runProgram :: Bool -> String -> IO ()
runProgram debugAst source =
  case parseProgram source of
    Left err -> do
      hPutStrLn stderr $ "Parse error: " ++ err
      exitFailure
    Right program -> do
      if debugAst
        then printAst program
        else pure ()
      result <- evalProgram program
      case result of
        Left err -> do
          print err
          exitFailure
        Right value -> print value

printAst :: Program -> IO ()
printAst program = do
  putStrLn "AST:"
  print program
