module Lexer
  ( Line(..)
  , lexProgram
  ) where

import Data.Char (isSpace)

newtype Line = Line { unLine :: (Int, String) }
  deriving (Eq, Show)

lexProgram :: String -> [Line]
lexProgram = map toLine . filter (not . blank) . lines
  where
    blank = all isSpace
    toLine s = Line (countIndent s, trimLeft s)

countIndent :: String -> Int
countIndent = length . takeWhile (== ' ')

trimLeft :: String -> String
trimLeft = dropWhile isSpace
