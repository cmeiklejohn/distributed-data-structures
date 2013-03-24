{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where
import Test.QuickCheck
import Test.QuickCheck.All

-- Stuff for Lattice
import Lattice
import Control.Monad
import Datatypes

deriving instance Prelude.Show Lattice__Coq_lbool
deriving instance Prelude.Eq Lattice__Coq_lbool

instance Arbitrary Lattice__Coq_lbool where
  arbitrary = oneof [return Coq_true, return Coq_false]

-- Verify addition commutativity
prop_intFold :: [Integer] -> Bool
prop_intFold xs = foldl (+) 0 xs == foldr (+) 0 xs

-- Binary function commutativity
boolMergeCommutative f x y = f x y == f y x
prop_BoolMerge = boolMergeCommutative (||)

-- Boolean lattice
prop_LBoolMerge = boolMergeCommutative _Lattice__lbool_merge

-- Test
runTests = $quickCheckAll

-- main :: IO ()
main = runTests
