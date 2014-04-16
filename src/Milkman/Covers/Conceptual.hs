{-# LANGUAGE BangPatterns #-}

{- |
Module      :  Milkman.Covers.Conceptual
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

Computation of (minimal) conceptual covers of formal contexts
-}

module Milkman.Covers.Conceptual ( bruteForceConceptualCovers
                                 , conceptCrosses
                                 , conceptualCovers
                                 , coversCrosses
                                 ) where

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
                       , Cross
                       , concepts
                       , objectAttributeConcepts
                       , tightCrosses
                       )
import Milkman.Context.Context ( Context (Context)
                               , incident
                               )

-- |Compute all conceptual covers, using a brute-force approach.
-- This might take a long time if the given context has lots of concepts.
bruteForceConceptualCovers :: Context -> [[Concept]]
bruteForceConceptualCovers c@(Context g m i) = go pcs []
  where pcs = filter admissible . subsequences . concepts $ c
        oacs = fromList $ objectAttributeConcepts c
        admissible = (oacs `isSubsetOf`) . fromList
        crosses = [ (o, a)
                  | o <- keys g
                  , a <- keys m
                  , incident i o a
                  ]
        allCovered pc = all (covered pc) crosses
        covered pc (o, a) = any (\(ext, int) -> o `elem` ext && a `elem` int)
                                pc
        go (pc:ps) !cs
          | allCovered pc = go ps (pc:cs)
          | otherwise = go ps cs
        go [] cs = cs


-- |Find all crosses covered by a given concept.
conceptCrosses :: Concept -> Set Cross
conceptCrosses (ext, int) = fromList [ (o, a)
                                     | o <- ext
                                     , a <- int
                                     ]

-- |Check whether a given cross is covered by a given concept.
coversCross :: Cross -> Concept -> Bool
coversCross (o, a) (ext, int) = o `elem` ext && a `elem` int

-- |Check whether a given set of concepts covers a given set of crosses.
coversCrosses :: [Concept] -> [Cross] -> Bool
coversCrosses cs = all (\cr -> any (coversCross cr) cs)

-- |Find all minimal conceptual covers.
-- This algorithm exploits the facts that the concepts that are both
-- object concepts and attribute concepts are contained in every
-- cover, and that any cover only needs to cover the tight crosses,
-- since any non-tight cross will be covered automatically. It also
-- tries to prune potential covers that get too large from the search
-- tree.
conceptualCovers :: Context -> [[Concept]]
conceptualCovers ctx = let cs = concepts ctx
                           oacs = objectAttributeConcepts ctx
                           avail = cs \\ oacs
                           Context gt mt it = tightCrosses ctx
                           tights = [ (o, a)
                                    | o <- keys gt
                                    , a <- keys mt
                                    , incident it o a
                                    ]
                           oaTight = nub [ (o, a)
                                         | (ext, int) <- oacs
                                         , o <- ext
                                         , a <- int
                                         , incident it o a
                                         ]
                           remTight = tights \\ oaTight
                           tightConcepts = fromListWith (++)
                                           [ (t, filter (coversCross t) avail)
                                           | t <- remTight
                                           ]
                           tcs = map fst
                                 $ sortBy (compare `on` (length . snd))
                                 $ toList tightConcepts
                           ccs = allCovers tightConcepts oacs tcs tights
                           dim = minimum $ length <$> ccs
                           mcs = filter ((dim==) . length) ccs
                       in nub $ sort <$> mcs

-- |Search state for the minimal conceptual cover search
data State = State { candidates :: Map Cross [Concept] -- ^ map from crosses to concepts covering them
                   , preCover :: ![Concept]            -- ^ partial cover found thus far
                   , remCrosses :: ![Cross]            -- ^ crosses that remain to be covered
                   , allCrosses :: ![Cross]            -- ^ all of the crosses that should be covered
                   , sizeBound :: !Int                 -- ^ size of the smallest cover seen so far
                   }

-- |Initialize the search state
initState :: Map Cross [Concept] -- ^ concepts that cover a given cross
          -> [Concept]           -- ^ initial partial cover
          -> [Cross]             -- ^ remaining crosses to cover
          -> [Cross]             -- ^ complete list of crosses to cover
          -> State
initState cs pc rc ac = State { candidates = cs
                              , preCover = pc
                              , remCrosses = rc
                              , allCrosses = ac
                              , sizeBound = length ac
                              }

-- |Add a new concept to the partial cover
updateState :: (Cross, Concept) -> State -> State
updateState (_, co) s = s' { sizeBound = bound }
  where preCover' = co : preCover s
        s' = s { preCover = preCover'
               , remCrosses = filter uncovered $ remCrosses s
               }
        bound = if isCover s'
                  then length $ preCover s'
                  else sizeBound s
        uncovered c = not $ any (coversCross c) preCover'

-- |Check whether the partial cover is already a cover for all the crosses
isCover :: State -> Bool
isCover s = null (remCrosses s) || coversCrosses (preCover s) (allCrosses s)

-- |Try to cover the next uncovered cross
step :: State -> [State]
step s = if isCover s || length (preCover s) > sizeBound s
           then []
           else let cr = head $ remCrosses s
                in map (`updateState` s) [ (cr, co)
                                         | co <- candidates s ! cr
                                         ]

-- |Find all remaining covers, starting from a given state
search :: State -> [[Concept]]
search s = if null $ remCrosses s
             then [preCover s]
             else step s >>= search

-- |Find all eligible covers
allCovers :: Map Cross [Concept] -> [Concept] -> [Cross] -> [Cross] -> [[Concept]]
allCovers cs pc rc = search . initState cs pc rc
