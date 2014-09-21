{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Milkman.CoversTests (testCovers) where

import Test.Tasty ( TestTree
                  , localOption
                  )
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Monadic

import Milkman.Context
import Milkman.Covers
import Milkman.Factors
import Milkman.Test.Context ()

testCovers :: TestTree
testCovers = localOption (QC.QuickCheckMaxSize 35) $ $(testGroupGenerator)

factorizationActuallyFactors cxt cover = do
  (gf, fm) <- factorContexts cxt cover
  pc <- productContext gf fm
  assert $ cxt == pc

prop_ConceptualFactorizationFactors = monadicIO $ do
  cxt <- pick QC.arbitrary
  pre . not . null . crosses $ cxt     -- don't try to factor an empty context
  pre $ (length $ concepts cxt) <= 23 -- stick to small contexts
  let cs = conceptualCovers cxt
  mapM_ (factorizationActuallyFactors cxt) cs


prop_PreconceptualFactorizationFactors = monadicIO $ do
  cxt <- pick QC.arbitrary
  pre . not . null . crosses $ cxt
  pre $ (length $ concepts cxt) <= 23
  let (mas, mos) = unzip [ minimalCovers cover
                         | cover <- conceptualCovers cxt
                         ]
  mapM_ (factorizationActuallyFactors cxt) $ concat $ mos ++ mas
