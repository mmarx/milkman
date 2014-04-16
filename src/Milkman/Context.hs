{-# LANGUAGE BangPatterns, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, TypeFamilies, TypeOperators #-}

{- |
Module      :  Milkman.Context
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

Basic algorithms and utilities for formal contexts
-}

module Milkman.Context ( Concept
                       , Context ()
                       , Cross
                       , attributes
                       , augment
                       , clarify
                       , complement
                       , concepts
                       , extent
                       , incidence
                       , intent
                       , mkContext
                       , objectAttributeConcepts
                       , objectConcept
                       , objects
                       , reduce
                       , showAugmented
                       , showIncidence
                       , tightCrosses
                       ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.List ( (\\)
                 , intersect
                 , nub
                 , transpose
                 )
import Data.List.Split (chunksOf)
import Data.Text ( Text
                 , intercalate
                 )
import Data.Array.Repa ( Array
                       , computeUnboxedS
                       , fromListUnboxed
                       , toList
                       , U
                       )
import qualified Data.Array.Repa as R
import Data.Array.Repa.Index ( Z (Z)
                             , (:.) ((:.))
                             , DIM2
                             )
import Data.Array.Repa.Shape (listOfShape)
import Data.Map.Strict ( (!)
                       , assocs
                       , fromList
                       , keys
                       )
import qualified Data.Set as S
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Unboxed.Base ()
import Data.Vector.Unboxed.Deriving (derivingUnbox)

import Milkman.Context.Context ( Attribute ( Attribute
                                           , unAttribute
                                           )
                               , Context (Context)
                               , Object ( Object
                                        , unObject
                                        )
                               , Names
                               , incident
                               )

-- |Construct a formal context, performing sanity checks
mkContext :: Monad m
          => [Text]              -- ^ object names
          -> [Text]              -- ^ attribute names
          -> [[Bool]]            -- ^ incidence relation, in row-major order
          -> m Context
mkContext objs atts rows = do
  let nr = length rows
      nc = nub $ map length rows
  when (no == 0) $ fail "No objects."
  when (na == 0) $ fail "No attributes."
  when (no /= nr) $ fail "Number of objects does not match number of rows."
  when (length nc /= 1) $ fail "Non-uniform number of columns."
  when (any (na/=) nc) $ fail "Number of attributes does not match number of columns."
  return $! Context g m i
    where no = length objs
          na = length atts
          g = fromList $ zip [0..] objs
          m = fromList $ zip [0..] atts
          i = fromListUnboxed (Z :. no :. na :: DIM2) $ concat rows

-- |Objects of a formal context
objects :: Context -> [(Object, Text)]
objects (Context g _ _) = assocs g

-- |Attributes of a formal context
attributes :: Context -> [(Attribute, Text)]
attributes (Context _ m _) = assocs m

-- |Incidence relation of a formal context
incidence :: Context -> [Bool]
incidence (Context _ _ i) = toList i

-- |Compute the intent of an object, i.e. the set of all its
-- attributes
intent :: Context -> Object -> [Attribute]
intent c obj = intent' c [obj]

-- |Compute the intent of a set of objects, i.e. the set of all
-- attributes that belong to all the objects
intent' :: Context -> [Object] -> [Attribute]
intent' (Context _ m i) objs = [ att
                               | att <- keys m
                               , all (\obj -> incident i obj att) objs
                               ]

-- |Compute the extent of an attribute, i.e. the set of all objects
-- having it
extent :: Context -> Attribute -> [Object]
extent c att = extent' c [att]

-- |Compute the extent of a set of attributes, i.e. the set of all
-- objects that have all the attributes
extent' :: Context -> [Attribute] -> [Object]
extent' (Context g _ i) atts = [ obj
                               | obj <- keys g
                               , all (incident i obj) atts
                               ]

-- |Compute the complementary context, complementing the incidence
-- relation
complement :: Context -> Context
complement (Context g m i) = Context g m i'
  where i' = computeUnboxedS $ R.map not i

-- |Aggregate names
clarify' :: (Enum a, Ord a, Num a)
            => (Int -> a)         -- ^ object/attribute wrapper constructor
            -> Names a           -- ^ un-aggregated name map
            -> [(Int, [Int])]    -- ^ indices to be aggregated as another index
            -> (Names a, [Int])  -- ^ aggregated names and removed indices
clarify' c n is = let ub = length is - 1
                      ps = [ (i, j)
                           | (i, ii) <- is
                           , (j, ij) <- is
                           , ii == ij
                           , i <= j
                           ]
                      es = [ (i, map snd $ filter ((i==) . fst) ps)
                           | i <- [0..ub]
                           , i `notElem` [ j'
                                         | (i', j') <- ps
                                         , i' < i
                                         ]
                           ]
                      e' = [ (i, intercalate "/" [ n ! c j
                                                 | j <- js
                                                 ])
                           | (i, js) <- es
                           ]
                      rs = [0..ub] \\ map fst e'
                  in (fromList $ zip [0..] $ map snd e', rs)

-- |Clarify a given context
clarify :: Context -> Context
clarify c@(Context g m i) = do
  let no = length $ keys g
      na = length $ keys m
      (g', ro) = clarify' Object g
                          [ (j, unAttribute <$> intent c (Object j))
                          | j <- [0..(no - 1)]
                          ]
      (m', ra) = clarify' Attribute m
                          [ (j, unObject <$> extent c (Attribute j))
                          | j <- [0..(na - 1)]
                          ]
    in Context g' m'
       $! fromListUnboxed (Z :. length (keys g')
                           :. length (keys m')
                           :: DIM2)
       [ e
       | (j, e) <- zip [0..] $ toList i
       , (j `div` na) `notElem` ro
       , (j `mod` na) `notElem` ra
       ]

-- |Values for augmented contexts
data Arrows = UpArrow           -- ^ upward arrow
            | DownArrow         -- ^ downward arrow
            | UpDownArrow       -- ^ double arrow
            | Cross             -- ^ cross
            | Empty             -- ^ nothing
            deriving ( Enum
                     , Eq
                     , Ord
                     , Read
                     , Show
                     )

derivingUnbox "Arrows"
  [t|Arrows -> Int|]
  [|fromEnum|]
  [|toEnum|]

-- |Incidence relation of an augmented context
type AugmentedIncidence = Array U DIM2 Arrows
-- |Augmented context
data AugmentedContext = AugmentedContext (Names Object)
                                         (Names Attribute)
                                         AugmentedIncidence
                        deriving (Show)

-- |Check whether a given object/attribute pair is incident in the
-- upward arrow relation
isUpArrow :: Context -> Object -> Attribute -> Bool
isUpArrow c@(Context _ _ i) g m
  | incident i g m = False
  | otherwise = let [na, _] = listOfShape $ R.extent i
                in all (incident i g) [ n
                                      | n <- Attribute <$> [0..(na - 1)]
                                      , let m' = S.fromList $ extent c m
                                            n' = S.fromList $ extent c n
                                        in m' `S.isProperSubsetOf` n'
                                      ]

-- |Check whether a given object/attribute pair is incident in the
-- downward arrow relation
isDownArrow :: Context -> Object -> Attribute -> Bool
isDownArrow c@(Context _ _ i) g m
  | incident i g m = False
  | otherwise = let [_, no] = listOfShape $ R.extent i
                in all (\o -> incident i o m) [ h
                                             | h <- Object <$> [0..(no - 1)]
                                             , let g' = S.fromList $ intent c g
                                                   h' = S.fromList $ intent c h
                                               in g' `S.isProperSubsetOf` h'
                                             ]

-- |Augment a context by extending it with the arrow relations
augment :: Context -> AugmentedContext
augment c@(Context g m i) = AugmentedContext g m i'
  where no = length $ keys g
        na = length $ keys m
        sh = Z :. no :. na :: DIM2
        is = [ (Object j, Attribute k)
             | j <- [0..(no - 1)]
             , k <- [0..(na - 1)]
             ]
        i' = fromListUnboxed sh $ zipWith aug is $ toList i
        aug _ True = Cross
        aug (j, k) False
          | isUpArrow c j k && isDownArrow c j k = UpDownArrow
          | isUpArrow c j k                     = UpArrow
          | isDownArrow c j k                   = DownArrow
          | otherwise                           = Empty

-- |Diminish an augmented context, dropping the arrows
diminish :: AugmentedContext -> Context
diminish (AugmentedContext g m i) = Context g m i'
  where i' = computeUnboxedS $ R.map (==Cross) $ i

-- |Reduce (and clarify) a given formal context
reduce :: Context -> Context
reduce c = let AugmentedContext g m i = augment . clarify $ c
               [na, no] = listOfShape $ R.extent i
               rs = chunksOf no $ toList i
               ro = filt rs
               ra = filt $ transpose rs
               g' = fromList $ zip [0..] [ g ! j
                                         | j <- keys g
                                         , j `notElem` ro
                                         ]
               m' = fromList $ zip [0..] [ m ! j
                                         | j <- keys m
                                         , j `notElem` ra
                                         ]
               i' = fromListUnboxed (Z :. length (keys g')
                                     :. length (keys m')
                                     :: DIM2)
                    [ e
                    | (j, e) <- zip [0..] $ toList i
                    , (j `div` na) `notElem` (unObject <$> ro)
                    , (j `mod` na) `notElem` (unAttribute <$> ra)
                    ]
           in diminish $! AugmentedContext g' m' i'
  where filt l = map fst $ filter (notElem UpDownArrow . snd) $ zip [0..] l

-- |Cross in a formal context
type Cross = (Object, Attribute)

-- |formal concept
type Concept = ([Object], [Attribute])

-- |Compute the object concept induced by an object
objectConcept :: Context -> Object -> Concept
objectConcept c g = (g'', g')
  where g' = intent c g
        g'' = extent' c g'

-- |Compute the attribute concept induced by an attribute
attributeConcept :: Context -> Attribute -> Concept
attributeConcept c m = (m', m'')
  where m' = extent c m
        m'' = intent' c m'

-- |Compute the set of concepts that are both object concepts and
-- attribute concepts
objectAttributeConcepts :: Context -> [Concept]
objectAttributeConcepts c@(Context g m _) = ocs `intersect` acs
  where ocs = map (objectConcept c) $ keys g
        acs = map (attributeConcept c) $ keys m

-- |Compute the tight crosses (the double arrows of the complementary
-- context) of a given formal context
tightCrosses :: Context -> Context
tightCrosses c@(Context g m _) = Context g m i'
  where (AugmentedContext _ _ i) = augment . complement $ c
        i' = computeUnboxedS . R.map (==UpDownArrow) $ i

-- |Pretty-print the incidence relation of an augmented context
showAugmented :: AugmentedContext -> String
showAugmented (AugmentedContext g m i) = unlines [ concatMap (showI o)
                                                   $ unAttribute <$> keys m
                                                 | o <- unObject <$> keys g
                                                 ]
  where showI o a = case i R.! (Z :. o :. a :: DIM2) of
          UpArrow -> "↗"
          DownArrow -> "↙"
          UpDownArrow -> "⤢"
          Cross -> "✘"
          Empty -> "⋅"

-- |Pretty-print the incidence relation of a formal context
showIncidence :: Context -> String
showIncidence (Context g m i) = unlines [ concatMap (showI o) $ keys m
                                        | o <- keys g
                                        ]
  where showI o a
          | incident i o a = "✘"
          | otherwise = "⋅"


-- |Compute the concepts of a formal context
concepts :: Context -> [Concept]
concepts c@(Context g m _) = go (keys m) [keys g]
  where go (a:as) !exts = go as $ nub $ exts ++ [ let a' = extent c a
                                                  in e `intersect` a'
                                                | e <- exts
                                                ]
        go [] exts = map conceptify exts
          where conceptify ext = (ext, intent' c ext)
