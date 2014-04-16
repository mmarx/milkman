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
import Milkman.Covers ( conceptualCovers
                      , minimalCovers
                      )
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

  let (mo, ma) = minimalCovers cover
  print . length $ mo
  putStrLn ""
  mapM_ (\s -> print s >> putStrLn "") mo

  print . length $ ma
  putStrLn ""
  mapM_ (\s -> print s >> putStrLn "") ma

factor :: String -> IO ()
factor s = do
  eec <- parseFile $ fromString s
  case eec of
    Left err -> print $ "failed reading context: " ++ err
    Right cxt -> do
      put cxt

      let cs = conceptualCovers cxt

      print . length $ concepts cxt
      print . length $ cs
      putStrLn ""

      mapM_ (writeFactors s cxt) $ zip [1..] (take 1 cs)

      let tis = tightCrosses cxt
      put tis
  where put c = putStrLn (showIncidence c) >> putStrLn ""


main :: IO ()
main = getArgs >>= mapM_ factor
