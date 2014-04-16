{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Milkman.Context.Context
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

The internals of the Context type
-}

module Milkman.Context.Context ( Attribute ( Attribute
                                           , unAttribute
                                           )
                               , Context (Context)
                               , Incidence
                               , Names
                               , Object ( Object
                                        , unObject
                                        )
                               , incident
                               ) where

import Data.Array.Repa ( Array
                       , U
                       )
import qualified Data.Array.Repa as R
import Data.Array.Repa.Index ( Z (Z)
                             , (:.) ((:.))
                             , DIM2
                             )
import Data.Map.Strict (Map)
import Data.Text (Text)

-- |Map from Object/Attribute indices to names
type Names a = Map a Text

-- |Incidence relation
type Incidence = Array U DIM2 Bool

-- |Formal context
data Context = Context (Names Object) (Names Attribute) Incidence
             deriving ( Eq
                      , Show
                      )

-- |Object of a formal context
newtype Object = Object { unObject :: Int }
               deriving ( Enum
                        , Eq
                        , Num
                        , Ord
                        , Read
                        , Show
                        )

-- |Attribute of a formal context
newtype Attribute = Attribute { unAttribute :: Int }
                  deriving ( Enum
                           , Eq
                           , Num
                           , Ord
                           , Read
                           , Show
                           )

-- |Check if a given object and a given attribute are contained in a
-- given incidence relation
incident :: Incidence -> Object -> Attribute -> Bool
incident i (Object o) (Attribute a) = i R.! (Z :. o :. a :: DIM2)
