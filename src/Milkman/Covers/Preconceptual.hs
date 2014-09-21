{-# LANGUAGE BangPatterns, TupleSections #-}

{- |
Module      :  Milkman.Covers.Preconceptual
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

Computation of preconceptual covers of formal contexts
-}

module Milkman.Covers.Preconceptual (minimalCovers)
       where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Data.List ((\\))

import Milkman.Context (Concept)
import Milkman.Context.Context ( Attribute (..)
                               , Object (..)
                               )

-- |Find all minimal preconceptual covers
minimalCovers :: [Concept]       -- ^ conceptual cover
              -> ([[Concept]], [[Concept]])
minimalCovers cover = (moc, mac)
  where minimize = map (map (map Object *** map Attribute)) .
                   minimizeCover cover
        moc = minimize Minimize { at = \(i, j) -> let b = cover !! i
                                                 in ( unObject $ fst b !! j
                                                    , unAttribute <$> snd b
                                                    )
                                , js = subtract 1 . length . fst . (cover!!)
                                , toS = deleteRows
                                }
        mac = minimize Minimize { at = \(i, j) -> let b = cover !! i
                                                 in ( unAttribute $ snd b !! j
                                                    , unObject <$> fst b
                                                    )
                                , js = subtract 1 . length . snd . (cover!!)
                                , toS = deleteCols
                                }

-- |Search state for the minimal preconceptual cover search
data Minimize = Minimize { at :: (Int, Int) -> (Int, [Int]) -- ^ row (i, j)
                         , js :: Int -> Int                 -- ^ number of objects of a given factor
                         , toS :: [Concept] -> [(Int, Int)] -> [([Int], [Int])] -- ^ compute solution
                         }

-- |Find all the rows covering a givne cross
coveredBy :: [Concept]           -- ^ conceptual cover
          -> Minimize            -- ^ search state
          -> (Object, Attribute) -- ^ cross
          -> [(Int, Int)]
coveredBy cover mi (obj, attr) = concat [ [ (i, j)
                                          | j <- [0 .. (js mi i) - 1]
                                          , let (o, as) = at mi (i, j)
                                          , o == unObject obj
                                          , unAttribute attr `elem` as
                                          ]
                                        | i <- [0 .. is]
                                        ]
  where is = length cover - 1

-- |Check whether a given row is irredundant, i.e. covers a cross
-- |that isn't covered by any other row
irredundant :: [Concept]         -- ^ conceptual cover
            -> Minimize          -- ^ search state
            -> [(Int, Int)]      -- ^ rows already deleted
            -> (Int, Int)        -- ^ row to check
            -> Bool
irredundant cover mi ijs ij = or [ null $ cs i j \\ (ij:ijs)
                                 | let (i, js') = at mi ij
                                 , j <- js'
                                 ]
  where cs i j = coveredBy cover mi (Object i, Attribute j)

-- |Transform a conceptual cover into a preconceptual cover by removing rows
deleteRows :: [Concept]          -- ^ conceptual cover
           -> [(Int, Int)]       -- ^ deleted rows
           -> [([Int], [Int])]   -- ^ preconceptual cover
deleteRows cover ijs = go [] $ zip [0..] cover
  where go sn [] = sn
        go sn ((i, (objs, attrs)):ics) = let objs' = [ unObject o
                                                     | (j, o) <- zip [0..] objs
                                                     , (i, j) `notElem` ijs
                                                     ]
                                             pc = (objs', unAttribute <$> attrs)
                                       in go (pc:sn) ics

-- |Transform a conceptual cover into a preconceptual cover by removing columns
deleteCols :: [Concept]          -- ^ conceptual cover
            -> [(Int, Int)]      -- ^ deleted cols
            -> [([Int], [Int])]  -- ^ preconceptual cover
deleteCols cover ijs = go [] $ zip [0..] cover
  where go sn [] = sn
        go sn ((i, (objs, attrs)):ics) = let attrs' = [ unAttribute a
                                                     | (j, a) <- zip [0..] attrs
                                                     , (i, j) `notElem` ijs
                                                     ]
                                             pc = (unObject <$> objs, attrs')
                                       in go (pc:sn) ics


-- |Find all minimal preconceptual covers of a given conceptual cover
minimizeCover :: [Concept]          -- ^ conceptual cover
              -> Minimize           -- ^ search state
              -> [[([Int], [Int])]] -- ^ minimal preconceptual covers
minimizeCover cover mi = map (toS mi cover) $ go (0, 0) [] []
  where is = length cover - 1
        go :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]]
        go ij [] !sns =
          let redundant = removable ij []
              sns' = if redundant then [ij]:sns else sns
              stack' = [ij | redundant]
          in case next ij of
            Nothing -> sns'
            Just ij' -> go ij' stack' sns
        go ij stack@(top:rest) !sns =
          let redundant = removable ij stack
              stack' = if redundant then ij:stack else stack
              rest' = if redundant then ij:rest else rest
          in case next ij of
            Nothing -> let sns' = if any (`removable` stack') $
                                     concat [ [ (i, j)
                                              | j <- [0 .. js mi i]
                                              , (i, j) < ij
                                              ]
                                            | i <- [0 .. is]
                                            ]
                                 then sns
                                 else stack':sns
                      in case next top of
                         Nothing -> sns'
                         Just top' -> go top' rest' sns'
            Just ij' -> go ij' stack' sns

        next :: (Int, Int) -> Maybe (Int, Int)
        next (i, j)
          | j < js mi i = Just (i, j + 1)
          | j == js mi i && i < is = Just (i + 1, 0)
          | otherwise = Nothing

        removable :: (Int, Int) -> [(Int, Int)] -> Bool
        removable ij ijs = let gone = ij `elem` ijs
                               oa = (Object *** Attribute) ij
                               coverers = coveredBy cover mi oa
                               candidate = ij `elem` coverers
                               covered = not . null $ coverers \\ (ij:ijs)
                               redundant = not $ irredundant cover mi ijs ij
                           in not gone && covered && candidate && redundant
