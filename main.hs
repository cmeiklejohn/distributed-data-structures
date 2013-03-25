{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where
import Test.QuickCheck
import Test.QuickCheck.All

-- Quickcheck configuration
import SemiLattice
import Control.Monad
import Datatypes

deriving instance Prelude.Show SemiLattice__Coq_lbool
deriving instance Prelude.Eq SemiLattice__Coq_lbool

deriving instance Prelude.Show Datatypes.Coq_nat
deriving instance Prelude.Eq Datatypes.Coq_nat

instance Arbitrary SemiLattice__Coq_lbool where
  arbitrary = oneof [return Coq_true, return Coq_false]

instance Arbitrary Datatypes.Coq_nat where
  arbitrary = sized coq_nat

coq_nat :: Int -> Gen Coq_nat
coq_nat 0 = return O
coq_nat x = liftM S $ coq_nat $ x `div` 2

-- Verify addition commutativity
prop_intFold :: [Integer] -> Bool
prop_intFold xs = foldl (+) 0 xs == foldr (+) 0 xs

-- Binary function commutativity
binMergeCommutative f x y = f x y == f y x
prop_BoolMerge = binMergeCommutative (||)

-- Boolean lattice
prop_LBoolMerge = binMergeCommutative _SemiLattice__lbool_merge

-- Max lattice
prop_LMaxMerge = binMergeCommutative _SemiLattice__lmax_merge

-- Main lattice
prop_LMinMerge = binMergeCommutative _SemiLattice__lmin_merge

-- Test
runTests = $quickCheckAll

-- main :: IO ()
main = runTests
