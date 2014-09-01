module Main (main)
       where

import Prelude hiding (writeFile)

import Control.Monad (when)
import Data.String (fromString)
import Data.Text.IO (writeFile)
import System.FilePath ( (<.>)
                       , (</>)
                       , takeBaseName
                       )

import Options.Applicative

import Milkman.IO ( parseFile
                  , showBurmeister
                  )
import Milkman.Context ( Concept
                       , Context
                       , concepts
                       , showIncidence
                       )
import Milkman.Covers ( conceptualCovers
                      , minimalCovers
                      )
import Milkman.Factors (factorContexts)

data Options = Options { outputPrefix :: String
                       , verbose :: Bool
                       , preconceptual :: Bool
                       , inputFiles :: [String]
                       }

put :: Context -> IO ()
put c = putStrLn (showIncidence c) >> putStrLn ""

writeFactors :: Options -> String -> Context -> (Int, [Concept]) -> IO ()
writeFactors opts input cxt (idx, cover) = do
  let out n = concat [ outputPrefix opts </> takeBaseName input
                     , "-factorization-"
                     , show idx
                     , "-"
                     , n
                     ] <.> "cxt"
  (gf, fm) <- factorContexts cxt cover

  when (verbose opts) $ do
    putStrLn $ "Factorization " <> (show idx) <> ":"
    put gf
    put fm

  putStrLn $ "Writing `" <> (out "objects") <> "'."
  writeFile (out "objects") $ showBurmeister gf
  putStrLn $ "Writing `" <> (out "attributes") <> "'."
  writeFile (out "attributes") $ showBurmeister fm

  when (preconceptual opts) $ do
    let (mo, ma) = minimalCovers cover
    print . length $ mo
    putStrLn ""
    mapM_ (\s -> print s >> putStrLn "") mo

    print . length $ ma
    putStrLn ""
    mapM_ (\s -> print s >> putStrLn "") ma

factor :: Options -> IO ()
factor opts = do
  when (verbose opts) $ do
    putStrLn $ "Outputting to " <> outputPrefix opts
    putStrLn ("Computing "
              <> (if preconceptual opts then "pre" else "")
              <> "conceptual covers.")
  mapM_ (factor' opts) $ inputFiles opts

factor' :: Options -> String -> IO ()
factor' opts s = do
  putStrLn $ "Processing `" <> s <> "'."
  eec <- parseFile $ fromString s
  case eec of
    Left err -> print $ "failed reading context: " ++ err -- FIXME STDERR
    Right cxt -> do
      when (verbose opts) $ put cxt

      let cs = conceptualCovers cxt

      putStrLn ("Context has "
                <> (show . length . concepts $ cxt)
                <> " concepts.")
      putStrLn ("Context has "
                <> (show . length $ cs)
                <> " minimal conceptual covers.")
      putStrLn ""

      mapM_ (writeFactors opts s cxt) $ zip [1..] cs

options :: Parser Options
options = Options
          <$> strOption (long "output-prefix"
                         <> short 'o'
                         <> metavar "PREFIX"
                         <> help "Prefix for the output files")
          <*> switch (long "verbose"
                      <> short 'v'
                      <> help "Whether to additional information")
          <*> switch (long "minimal-covers"
                      <> help ("Whether to to generate minimal "
                      ++ "pre-conceptual covers instead of conceptual covers"))
          <*> many (argument str $ metavar "CONTEXTS...")


main :: IO ()
main = execParser opts >>= factor
  where opts = info (helper <*> options)
               (fullDesc
                <> progDesc ("Compute (pre-)conceptual covers of "
                             ++ "boolean contexts")
                <> header "milkman - boolean context covering")
