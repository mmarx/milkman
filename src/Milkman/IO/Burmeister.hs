module Milkman.IO.Burmeister (parseBurmeister)
       where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text (Text)

import Milkman.Context

parseBurmeister :: Monad m => Parser (m Context)
parseBurmeister = do
  (no, na) <- parseHeader
  objs <- count no takeLine
  atts <- count na takeLine
  cs <- count no $ parseRow na
  return $! mkContext objs atts cs

parseHeader :: Parser (Int, Int)
parseHeader = do
  _ <- char 'B'
  endOfLine
  endOfLine
  no <- decimal
  endOfLine
  na <- decimal
  endOfLine
  endOfLine
  return (no, na)

parseRow :: Int -> Parser [Bool]
parseRow na = do
  cs <- count na parseCross
  endOfLine
  return cs

parseCross :: Parser Bool
parseCross = do
  c <- anyChar
  case c of
    'X' -> return True
    '.' -> return False
    _ -> fail "not a cross."

takeLine :: Parser Text
takeLine = do
  l <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return $! l
