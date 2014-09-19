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
import Data.Set ( fromList
                , isSubsetOf
                )

import Milkman.Context ( Concept
                       , Context
                       , crosses
                       )
import Milkman.Context.Context ( Attribute (..)
                               , Object (..)
                               )

-- |Find all minimal preconceptual covers
minimalCovers :: Context         -- ^ covered context
                -> [Concept]     -- ^ conceptual cover
                -> ([[([Object], [Attribute])]], [[([Attribute], [Object])]])
minimalCovers cxt cover = (moc, mac)
  where minimize = minimizeCover cxt cover
        moc = map (map Object *** map Attribute) <$>
              minimize Minimize { at = \(i, j) -> let b = cover !! i
                                                 in ( unObject $ fst b !! j
                                                    , unAttribute <$> snd b
                                                    )
                                , js = subtract 1 . length . fst . (cover!!)
                                }
        mac = map (map Attribute *** map Object) <$>
              minimize Minimize { at = \(i, j) -> let b = cover !! i
                                                 in ( unAttribute $ snd b !! j
                                                    , unObject <$> fst b
                                                    )
                                , js = subtract 1 . length . snd . (cover!!)
                                }

-- |Search state for the minimal preconceptual cover search
data Minimize = Minimize { at :: (Int, Int) -> (Int, [Int]) -- ^ row (i, j)
                         , js :: Int -> Int                 -- ^ number of objects of a given factor
                         }

-- |Find all minimal preconceptual covers of a given conceptual cover
minimizeCover :: Context            -- ^ covered context
              -> [Concept]          -- ^ conceptual cover
              -> Minimize           -- ^ search state
              -> [[([Int], [Int])]] -- ^ minimal preconceptual covers
minimizeCover cxt cover mi = map toSolution $ go (0, 0) [] []
  where is = length cover - 1
        toSolution :: [(Int, Int)] -> [([Int], [Int])]
        toSolution sn = [ (ai', bi')
                        | (i, (ai, bi)) <- zip [0..] cover
                        , let ai' = unObject <$> filter ((`notElem` sn) . (i,) . unObject) ai
                              bi' = unAttribute <$> bi
                        ]
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
                               shouldCover = fromList $ map (unObject *** unAttribute) $ crosses cxt
                               coverParts = concat [ [ (i, j)
                                                     | j <- [0 .. js mi i]
                                                     , (i, j) `notElem` ijs
                                                     , (i, j) /= ij
                                                     ]
                                                   | i <- [0 .. is]
                                                   ]
                               stillCovered = fromList $ concat [ [ (i, j)
                                                                  | let (i, js') = at mi ij'
                                                                  , j <- js'
                                                                  ]
                                                                | ij' <- coverParts
                                                                ]
                               covered = shouldCover `isSubsetOf` stillCovered
                           in not gone && covered
