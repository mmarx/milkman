module Milkman.Context.Context
       where

import Data.Array.Repa ( Array
                       , U
                       )
import Data.Array.Repa.Index (DIM2)
import Data.Map.Strict (Map)
import Data.Text (Text)

type Names = Map Int Text
type Incidence = Array U DIM2 Bool
data Context = Context Names Names Incidence
             deriving (Show)

newtype Object = Object { unObject :: Int }
               deriving (Read, Show, Eq, Ord)

newtype Attribute = Attribute { unAttribute :: Int }
                  deriving (Read, Show, Eq, Ord)
