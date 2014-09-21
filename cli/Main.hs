module Main (main)
       where

import Prelude hiding (writeFile)

import Control.Monad (when)
import Data.List (intercalate)
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
                       , productContext
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

-- |Pretty-print a context
put :: Context -> IO ()
put c = putStrLn (showIncidence c) >> putStrLn ""

-- |Write factorization to disk
writeFactors :: Options          -- ^ CLI options
             -> String           -- ^ input basename
             -> Context          -- ^ factored context
             -> (Int, [Concept]) -- ^ index, conceptual cover
             -> IO ()
writeFactors opts input cxt cs@(idx, cover) = do
  let write = writeFactors' opts cxt input
  write "" cs

  when (preconceptual opts) $ do
   let (mo, ma) = minimalCovers cover
       write' = write . ((show idx <> "-")++)

   putStrLn ("Factorization has "
             <> (show . length $ mo)
             <> " minimal object covers.")
   putStrLn ""
   mapM_ (write' "minimal-objects") $ zip [1..] mo

   putStrLn ("Factorization has "
             <> (show . length $ ma)
             <> " minimal attribute covers.")
   putStrLn ""
   mapM_ (write' "minimal-attributes") $ zip [1..] ma

-- |Actually write the factorization to disk
writeFactors' :: Options         -- ^ CLI options
              -> Context         -- ^ context
              -> String          -- ^ input basename
              -> String          -- ^ extra annotation
              -> (Int, [Concept]) -- ^ index, cover
              -> IO ()
writeFactors' opts cxt input extra (idx, cover) = do
  let out n = intercalate "-" (filter (/="")
              [ outputPrefix opts </> takeBaseName input
              , "factorization"
              , extra
              , show idx
              , n
              ]) <.> "cxt"
  (gf, fm) <- factorContexts cxt cover
  pc <- productContext gf fm
  when (pc /= cxt) $ do
    putStrLn "Original:"
    put cxt
    putStrLn "Product:"
    put pc
    fail $ "Product does not equal original context."

  when (verbose opts) $ do
    putStrLn $ "Factorization " <> show idx <> ":"
    put gf
    put fm

  putStrLn $ "Writing `" <> out "objects" <> "'."
  writeFile (out "objects") $ showBurmeister gf
  putStrLn $ "Writing `" <> out "attributes" <> "'."
  writeFile (out "attributes") $ showBurmeister fm

-- |Factor all contexts given as CLI arguments
factor :: Options -> IO ()
factor opts = do
  when (verbose opts) $ do
    putStrLn $ "Outputting to " <> outputPrefix opts
    putStrLn ("Computing "
              <> (if preconceptual opts then "pre" else "")
              <> "conceptual covers.")
  mapM_ (factor' opts) $ inputFiles opts

-- |Factor a context given as a CLI argument
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

-- |Parser for CLI options
options :: Parser Options
options = Options
          <$> strOption (long "output-prefix"
                         <> short 'o'
                         <> metavar "PREFIX"
                         <> help "Prefix for the output files")
          <*> switch (long "verbose"
                      <> short 'v'
                      <> help "Whether to additional information")
          <*> switch (long "pre-conceptual"
                      <> help ("Whether to to generate minimal "
                      ++ "pre-conceptual covers instead of conceptual covers"))
          <*> many (argument str $ metavar "CONTEXTS...")


-- |Entry point
main :: IO ()
main = execParser opts >>= factor
  where opts = info (helper <*> options)
               (fullDesc
                <> progDesc ("Compute (pre-)conceptual covers of "
                             ++ "boolean contexts")
                <> header "milkman - boolean context covering")
