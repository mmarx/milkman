module Main (main)
       where

import Test.Tasty ( TestTree
                  , defaultMainWithIngredients
                  , testGroup
                  )
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners ( consoleTestReporter
                          , listingTests
                          )
import Test.Tasty.Runners.AntXML (antXMLRunner)

import Milkman.ContextTests (testContext)
import Milkman.CoversTests (testCovers)

main :: IO ()
main = defaultMainWithIngredients [ listingTests
                                  , rerunningTests [ consoleTestReporter ]
                                  , antXMLRunner
                                  ] tests

tests :: TestTree
tests = testGroup "milkman" [ testGroup "Context" [testContext]
                            , testGroup "Covers" [testCovers]
                            ]
