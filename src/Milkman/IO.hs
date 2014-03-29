{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Milkman.IO
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

I/O for formal contexts
-}

module Milkman.IO ( parseFile
                  , showBurmeister
                  )
       where

import Prelude hiding (FilePath)

import Control.Exception ( try
                         , SomeException
                         )
import Data.Attoparsec.Text (Parser)
import Data.Conduit ( ($$)
                    , (=$)
                    , runResourceT
                   )
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Filesystem (sourceFile)
import Data.Conduit.Text ( decode
                         , utf8
                         )
import Data.Monoid ((<>))
import qualified Text.XML as X
import Filesystem.Path.CurrentOS (FilePath)

import Milkman.Context
import Milkman.IO.Burmeister ( parseBurmeister
                             , showBurmeister
                             )
import Milkman.IO.Conexp (parseConexp)

-- |Parser for context formats
parseContext :: Monad m => Parser (m Context)
parseContext = parseBurmeister

-- |Try to parse a file as a formal context, returning either an error
-- message, or the parsed context
parseFile :: FilePath -> IO (Either String Context)
parseFile file = do
  result <- try $ runResourceT $ sourceFile file
            $$ decode utf8
            =$ sinkParser parseContext
  case result of
    (Left e) -> do               --  failed to parse as something else, try XML
      result' <- try $ X.readFile X.def file
      case result' of
        (Left e') -> return . Left $ showE e <> "\n" <> showE e'
        (Right doc) -> return $ parseConexp doc
    (Right Nothing) -> return . Left $ "Could not parse Context."
    (Right (Just c)) -> return . Right $ c
  where showE :: SomeException -> String
        showE = show
