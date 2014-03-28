module Milkman.Factors (factorContexts)
       where

import Control.Applicative ((<$>))

import Data.Map.Strict ( elems
                       , keys
                       )
import Data.Text (pack)

import Milkman.Context ( Concept
                       , Context (..)
                       , mkContext
                       , unAttribute
                       , unObject
                       )

factorContexts :: Monad m => Context -> [Concept] -> m (Context, Context)
factorContexts (Context g m _) cs = do
  let fs = [ pack $ "f_" ++ show j
           | j <- [1..(length cs)]
           ]
  gf <- mkContext (elems g) fs [ [ o `elem` (unObject <$> e)
                                | (e, _) <- cs
                                ]
                              | o <- keys g
                              ]
  fm <- mkContext fs (elems m) [ [ a `elem` (unAttribute <$> i)
                                | a <- keys m
                                ]
                              | (_, i) <- cs
                              ]
  return (gf, fm)
