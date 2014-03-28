{-# LANGUAGE BangPatterns #-}

module Milkman.Covers ( bruteForceConceptualCovers
                      , conceptCrosses
                      , conceptualCovers
                      , coversCrosses
                      )
       where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List ( (\\)
                 , nub
                 , sort
                 , sortBy
                 , subsequences
                 )
import Data.Map.Strict ( (!)
                       , Map
                       , fromListWith
                       , keys
                       , toList
                       )
import Data.Set ( Set
                , fromList
                , isSubsetOf
                )

import Milkman.Context ( Concept
                       , Context (..)
                       , Cross
                       , Object (..)
                       , Attribute (..)
                       , concepts
                       , incident
                       , objectAttributeConcepts
                       , tightCrosses
                       )

bruteForceConceptualCovers :: Context -> [[Concept]]
bruteForceConceptualCovers c@(Context g m i) = go pcs []
  where pcs = filter admissible . subsequences . concepts $ c
        oacs = fromList $ objectAttributeConcepts c
        admissible = (oacs `isSubsetOf`) . fromList
        crosses = [ (o, a)
                  | o <- Object <$> keys g
                  , a <- Attribute <$> keys m
                  , incident i o a
                  ]
        allCovered pc = all (covered pc) crosses
        covered pc (o, a) = any (\(ext, int) -> o `elem` ext && a `elem` int)
                                pc
        go (pc:ps) !cs
          | allCovered pc = go ps (pc:cs)
          | otherwise = go ps cs
        go [] cs = cs

conceptCrosses :: Concept -> Set Cross
conceptCrosses (ext, int) = fromList [ (o, a)
                                     | o <- ext
                                     , a <- int
                                     ]

coversCross :: Cross -> Concept -> Bool
coversCross (o, a) (ext, int) = o `elem` ext && a `elem` int

coversCrosses :: [Concept] -> [Cross] -> Bool
coversCrosses cs crs = all (\cr -> any (coversCross cr) cs) crs

conceptualCovers :: Monad m => Context -> m [[Concept]]
conceptualCovers ctx = do
  let cs = concepts ctx
      oacs = objectAttributeConcepts ctx
      avail = cs \\ oacs
  Context gt mt it <- tightCrosses ctx
  let tights = [ (o, a)
               | o <- Object <$> keys gt
               , a <- Attribute <$> keys mt
               , incident it o a
               ]
      oaTight = nub [ (o, a)
                    | (ext, int) <- oacs
                    , o <- ext
                    , a <- int
                    , incident it o a
                    ]
      remTight = tights \\ oaTight
      tightConcepts = fromListWith (++) [ (t, filter (coversCross t) avail)
                                        | t <- remTight
                                        ]
      tcs = map fst $ sortBy (compare `on` (length . snd))
                    $ toList tightConcepts
      ccs = allCovers tightConcepts oacs tcs tights
      dim = minimum $ length <$> ccs
      mcs = filter ((dim==) . length) ccs
  return $! nub $ sort <$> mcs

data State = State { candidates :: Map Cross [Concept]
                   , preCover :: ![Concept]
                   , remCrosses :: ![Cross]
                   , allCrosses :: ![Cross]
                   , sizeBound :: !Int
                   }

initState :: Map Cross [Concept] -> [Concept] -> [Cross] -> [Cross] -> State
initState cs pc rc ac = State { candidates = cs
                              , preCover = pc
                              , remCrosses = rc
                              , allCrosses = ac
                              , sizeBound = length ac
                              }

updateState :: (Cross, Concept) -> State -> State
updateState (_, co) s = s' { sizeBound = bound }
  where preCover' = co : preCover s
        s' = s { preCover = preCover'
               , remCrosses = filter uncovered $ remCrosses s
               }
        bound = if isCover s'
                  then length $ preCover s'
                  else sizeBound s
        uncovered c = not $ any (coversCross c) $ preCover'

isCover :: State -> Bool
isCover s = null (remCrosses s) || coversCrosses (preCover s) (allCrosses s)

step :: State -> [State]
step s = if isCover s || length (preCover s) > sizeBound s
           then []
           else let cr = head $ remCrosses s
                in map (flip updateState s) $ [ (cr, co)
                                              | co <- (candidates s) ! cr
                                              ]

search :: State -> [[Concept]]
search s = if null $ remCrosses s
             then [preCover s]
             else step s >>= search

allCovers :: Map Cross [Concept] -> [Concept] -> [Cross] -> [Cross] -> [[Concept]]
allCovers cs pc rc = search . initState cs pc rc
