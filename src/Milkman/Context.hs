{-# LANGUAGE TypeOperators #-}

module Milkman.Context ( Context
                       , mkContext
                       ) where

import Control.Monad (when)
import Data.List (nub)
import Data.Text (Text)
import Data.Array.Repa ( U
                       , Array
                       , fromListUnboxed
                       )
import Data.Array.Repa.Index ( Z (Z)
                             , (:.) ((:.))
                             , DIM2
                             , DIM2
                             )
import Data.Map.Strict ( Map
                       , fromList
                       )

type Names = Map Int Text
type Incidence = Array U DIM2 Bool
data Context = Context Names Names Incidence
             deriving (Show)

mkContext :: Monad m => [Text] -> [Text] -> [[Bool]] -> m Context
mkContext objs atts rows = do
  let nr = length rows
      nc = nub $ map length rows
  when (no /= nr) $ fail "Number of objects does not match number of rows."
  when (length nc /= 1) $ fail "Non-uniform number of columns."
  when (any (na/=) nc) $ fail "Number of attributes does not match number of columns."
  return $! Context g m i
    where no = length objs
          na = length atts
          g = fromList $ zip [0..] objs
          m = fromList $ zip [0..] atts
          i = fromListUnboxed (Z :. no :. na :: DIM2) $ concat rows
