{-# LANGUAGE OverloadedStrings #-}
module Milkman.IO.Conexp (parseConexp)
       where

import Data.Text (Text)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

import Milkman.Context

getAttribute :: Cursor -> [(Int, Text)]
getAttribute a = zip is ns
  where is = map (read . T.unpack) $ a $| attribute "Identifier"
        ns = a $// content

getObject :: Cursor -> [(Text, [Int])]
getObject o = [ (T.concat ns, map (read . T.unpack . T.concat) is) ]
  where ns = o $/ element "Name"
               &/ content
        is = o $/ element "Intent"
               &/ element "HasAttribute"
               &| attribute "AttributeIdentifier"

toContext :: Monad m => [(Int, Text)] -> [(Text, [Int])] -> m Context
toContext atrs objs = mkContext g m i
  where g = map fst objs
        m = map snd atrs
        i = [ [ a `elem` as | (a, _) <- atrs ]
            | (_, as) <- objs
            ]

parseConexp :: Monad m => Document -> m Context
parseConexp doc = let root = documentRoot doc
                      cursor = fromDocument doc
                  in
  if elementName root /= "ConceptualSystem"
  then fail "Not a conexp document"
  else let ca = cursor $// element "Attributes" &/ element "Attribute"
           co = cursor $// element "Objects" &/ element "Object"
           as = concatMap getAttribute ca
           os = concatMap getObject co
       in toContext as os
