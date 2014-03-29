{- |
Module      :  Milkman.Factors
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

Computation of the boolean conceptual factorization of formal contexts
-}

module Milkman.Factors (factorContexts) where

import Data.Map.Strict ( elems
                       , keys
                       )
import Data.Text (pack)

import Milkman.Context ( Concept
                       , mkContext
                       )
import Milkman.Context.Context (Context (Context))

-- |Given a context and a conceptual cover, compute the two boolean
-- factor contexts
factorContexts :: Monad m
               => Context              -- ^ formal context
               -> [Concept]            -- ^ conceptual cover
               -> m (Context, Context) -- ^ factor contexts
factorContexts (Context g m _) cs = do
  let fs = [ pack $ "f_" ++ show j
           | j <- [1..(length cs)]
           ]
  gf <- mkContext (elems g) fs [ [ o `elem` e
                                | (e, _) <- cs
                                ]
                              | o <- keys g
                              ]
  fm <- mkContext fs (elems m) [ [ a `elem` i
                                | a <- keys m
                                ]
                              | (_, i) <- cs
                              ]
  return (gf, fm)
