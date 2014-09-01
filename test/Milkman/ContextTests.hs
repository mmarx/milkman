{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Milkman.ContextTests (testContext) where

import Test.Tasty (TestTree)
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty.HUnit
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck (testProperty)

import Milkman.Context
import Milkman.Test.Context ()

testContext :: TestTree
testContext = $(testGroupGenerator)

case_mkContextNoObjectsFails = mkContext [] ["Foo"] [[True]]
                               @?= Nothing

case_mkContextNoAttributesFails = mkContext ["Foo"] [] [[True]]
                                  @?= Nothing

case_mkContextObjectRowsFails = mkContext ["Foo", "Bar"] ["Quux"] []
                                      @?= Nothing

case_mkContextNonUniformColsFails = mkContext ["Foo", "Bar"]
                                    ["Quux"]
                                    [ []
                                    , [True]
                                    ]
                                    @?= Nothing

case_mkContextAttributeColsFails = mkContext ["Foo", "Bar"]
                                   ["Quux", "FooBar"]
                                   [ [True]
                                   , [False]
                                   ]
                                   @?= Nothing

prop_complementNonIdentic = \c -> c /= complement c
prop_complementInvolution = \c -> (c==) . complement . complement $ c
prop_nonEmptyObjects = \c -> not . null . objects $ c
prop_nonEmptyAttributes = \c -> not . null . attributes $ c
prop_tightCrossesAreCrosses = \c ->
  let i = incidence c
      i' = incidence $ tightCrosses c
  in and $ zipWith impl i' i
  where impl False _ = True
        impl True True = True
        impl True False = False
