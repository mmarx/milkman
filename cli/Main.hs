module Main (main)
       where

import Prelude hiding (writeFile)

import Data.String (fromString)
import Data.Text.IO (writeFile)
import System.Environment (getArgs)
import System.FilePath ( (<.>)
                       , dropExtension
                       )

import Milkman.IO ( parseFile
                  , showBurmeister
                  )
import Milkman.Context ( Concept
                       , Context
                       , concepts
                       , showIncidence
                       , tightCrosses
                       )
import Milkman.Covers (conceptualCovers)
import Milkman.Factors (factorContexts)

writeFactors :: String -> Context -> (Int, [Concept]) -> IO ()
writeFactors base cxt (idx, cover) = do
  let base' = concat [ dropExtension base
                     , "-factorization-"
                     , show idx
                     ]
  (gf, fm) <- factorContexts cxt cover

  putStrLn $ showIncidence gf
  putStrLn ""

  putStrLn $ showIncidence fm
  putStrLn ""

  writeFile (base' ++ "-objects" <.> "cxt") $ showBurmeister gf
  writeFile (base' ++ "-attributes" <.> "cxt") $ showBurmeister fm

factor :: String -> IO ()
factor s = do
  eec <- parseFile $ fromString s
  case eec of
    Left err -> print $ "failed reading context: " ++ err
    Right cxt -> do
      put cxt

      cs <- conceptualCovers cxt

      print . length $ concepts cxt
      print . length $ cs
      putStrLn ""

      mapM_ (writeFactors s cxt) $ zip [1..] cs

      tis <- tightCrosses cxt
      put tis
  where put c = putStrLn (showIncidence c) >> putStrLn ""


main :: IO ()
main = getArgs >>= mapM_ factor
