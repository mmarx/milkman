{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Milkman.IO.Burmeister ( parseBurmeister
                             , showBurmeister
                             )
       where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text ( Text
                 , pack
                 )
import qualified Data.Text as T

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

showBurmeister :: Context -> Text
showBurmeister c = T.unlines $ [ "B"
                               , ""
                               , pack $ show no
                               , pack $ show na
                               , ""
                               ] ++ os ++ as ++ is
  where (gs, os) = unzip $ objects c
        (ms, as) = unzip $ attributes c
        (no, na) = (length gs, length ms)
        i = go [] $ incidence c
        is = map (T.concat . map showCross) i
        go :: [[Bool]] -> [Bool] -> [[Bool]]
        go rs [] = rs
        go !rs !rest = go (rs ++ [Prelude.take na rest]) $ drop na rest
        showCross :: Bool -> Text
        showCross True = "X"
        showCross False = "."
