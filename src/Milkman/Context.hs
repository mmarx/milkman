{-# LANGUAGE BangPatterns, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, TypeFamilies, TypeOperators #-}

module Milkman.Context ( Concept
                       , Context ()
                       , Cross
                       , attributes
                       , augment
                       , clarify
                       , complement
                       , concepts
                       , extent
                       , incident
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
import Control.Monad ( liftM
                     , when
                     )
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
                       , computeUnboxedP
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
                               , Incidence
                               , Names
                               )

mkContext :: Monad m => [Text] -> [Text] -> [[Bool]] -> m Context
mkContext objs atts rows = do
  let nr = length rows
      nc = nub $ map length rows
  when (no /= nr) $ fail "Number of objects does not match number of rows."
  when (length nc /= 1) $ fail "Non-uniform number of columns."
  when (any (na/=) nc) $ fail "Number of attributes does not match number of columns."
  return $! Context g m i
    where no = length objs
          na = length atts
          g = fromList $ zip [0..] objs
          m = fromList $ zip [0..] atts
          i = fromListUnboxed (Z :. no :. na :: DIM2) $ concat rows

objects :: Context -> [(Object, Text)]
objects (Context g _ _) = map (\(o, n) -> (Object o, n)) $ assocs g

attributes :: Context -> [(Attribute, Text)]
attributes (Context _ m _) = map (\(a, n) -> (Attribute a, n)) $ assocs m

incidence :: Context -> [Bool]
incidence (Context _ _ i) = toList i

incident :: Incidence -> Object -> Attribute -> Bool
incident i (Object o) (Attribute a) = i R.! (Z :. o :. a :: DIM2)

intent :: Context -> Object -> [Attribute]
intent c obj = intent' c [obj]

intent' :: Context -> [Object] -> [Attribute]
intent' (Context _ m i) objs = [ att
                               | att <- Attribute <$> keys m
                               , all (\obj -> incident i obj att) objs
                               ]

extent :: Context -> Attribute -> [Object]
extent c att = extent' c [att]

extent' :: Context -> [Attribute] -> [Object]
extent' (Context g _ i) atts = [ obj
                               | obj <- Object <$> keys g
                               , all (incident i obj) atts
                               ]

complement :: Monad m => Context -> m Context
complement (Context g m i) = Context g m `liftM` computeUnboxedP (R.map not i)

clarify' :: Names -> [(Int, [Int])] -> (Names, [Int])
clarify' n is = let ub = length is - 1
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
                    e' = [ (i, intercalate "/" [ n ! j
                                               | j <- js
                                               ])
                         | (i, js) <- es
                         ]
                    rs = [0..ub] \\ map fst e'
                in (fromList $ zip [0..] $ map snd e', rs)

clarify :: Monad m => Context -> m Context
clarify c@(Context g m i) = do
  let no = length $ keys g
      na = length $ keys m
      (g', ro) = clarify' g [ (j, unAttribute <$> (intent c $ Object j))
                            | j <- [0..(no - 1)]
                            ]
      (m', ra) = clarify' m [ (j, unObject <$> (extent c $ Attribute j))
                            | j <- [0..(na - 1)]
                            ]
  return $! Context g' m'
         $! fromListUnboxed (Z :. length (keys g')
                             :. length (keys m')
                             :: DIM2)
         [ e
         | (j, e) <- zip [0..] $ toList i
         , (j `div` na) `notElem` ro
         , (j `mod` na) `notElem` ra
         ]

data Arrows = UpArrow
            | DownArrow
            | UpDownArrow
            | Cross
            | Empty
            deriving (Read, Show, Eq, Ord, Enum)

derivingUnbox "Arrows"
  [t|Arrows -> Int|]
  [|fromEnum|]
  [|toEnum|]

type AugmentedIncidence = Array U DIM2 Arrows
data AugmentedContext = AugmentedContext Names Names AugmentedIncidence
                        deriving (Show)

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

diminish :: Monad m => AugmentedContext -> m Context
diminish (AugmentedContext g m j) = do
  i <- computeUnboxedP . R.map (==Cross) $ j
  return $! Context g m i

reduce :: Monad m => Context -> m Context
reduce c = do
  AugmentedContext g m i <- augment `liftM` clarify c
  let [na, no] = listOfShape $ R.extent i
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
         , (j `div` na) `notElem` ro
         , (j `mod` na) `notElem` ra
         ]
  diminish $! AugmentedContext g' m' i'
  where filt l = map fst $ filter (notElem UpDownArrow . snd) $ zip [0..] l

type Cross = (Object, Attribute)
type Concept = ([Object], [Attribute])

objectConcept :: Context -> Object -> Concept
objectConcept c g = (g'', g')
  where g' = intent c g
        g'' = extent' c g'

attributeConcept :: Context -> Attribute -> Concept
attributeConcept c m = (m', m'')
  where m' = extent c m
        m'' = intent' c m'

objectAttributeConcepts :: Context -> [Concept]
objectAttributeConcepts c@(Context g m _) = ocs `intersect` acs
  where ocs = map (objectConcept c) $ Object <$> keys g
        acs = map (attributeConcept c) $ Attribute <$> keys m

tightCrosses :: Monad m => Context -> m Context
tightCrosses c@(Context g m _) = do
  (AugmentedContext _ _ j) <- augment `liftM` complement c
  i' <- computeUnboxedP . R.map (==UpDownArrow) $ j
  return $! Context g m i'

showAugmented :: AugmentedContext -> String
showAugmented (AugmentedContext g m i) = unlines [ concatMap (showI o) $ keys m
                                                 | o <- keys g
                                                 ]
  where showI o a = case i R.! (Z :. o :. a :: DIM2) of
          UpArrow -> "↗"
          DownArrow -> "↙"
          UpDownArrow -> "⤢"
          Cross -> "✘"
          Empty -> "⋅"

showIncidence :: Context -> String
showIncidence (Context g m i) = unlines [ concatMap (showI o) $ Attribute <$> keys m
                                        | o <- Object <$> keys g
                                        ]
  where showI o a
          | incident i o a = "✘"
          | otherwise = "⋅"


concepts :: Context -> [Concept]
concepts c@(Context g m _) = go (Attribute <$> keys m) [Object <$> keys g]
  where go (a:as) !exts = go as $ nub $ exts ++ [ let a' = extent c a
                                                  in e `intersect` a'
                                                | e <- exts
                                                ]
        go [] exts = map conceptify exts
          where conceptify ext = (ext, intent' c ext)
