module Milkman.Test.Context ()
       where

import Control.Applicative ((<$>))
import Data.List.Split (chunksOf)
import Data.Text ( Text
                 , pack
                 )
import Test.QuickCheck.Instances
import qualified Test.QuickCheck as QC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import Milkman.Context

instance QC.Arbitrary Context
  where arbitrary = gen

gen :: QC.Gen Context
gen = do
  no <- sizedChoose
  na <- sizedChoose
  let o = pack . show <$> [1 .. no]
      a = pack . show <$> [1 .. na]
  i <- QC.vector $ no * na :: QC.Gen [Bool]
  mkContext o a $ chunksOf no i
  where sizedChoose = QC.sized (\n -> QC.choose (1, n)) `QC.suchThat` (>0)
