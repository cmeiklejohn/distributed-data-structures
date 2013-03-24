module Peano where

import qualified Prelude
import qualified Datatypes


pred :: Datatypes.Coq_nat -> Datatypes.Coq_nat
pred n =
  case n of {
   Datatypes.O -> n;
   Datatypes.S u -> u}

plus :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
plus n m =
  case n of {
   Datatypes.O -> m;
   Datatypes.S p -> Datatypes.S (plus p m)}

mult :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
mult n m =
  case n of {
   Datatypes.O -> Datatypes.O;
   Datatypes.S p -> plus m (mult p m)}

minus :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
minus n m =
  case n of {
   Datatypes.O -> n;
   Datatypes.S k ->
    case m of {
     Datatypes.O -> n;
     Datatypes.S l -> minus k l}}

max :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
max n m =
  case n of {
   Datatypes.O -> m;
   Datatypes.S n' ->
    case m of {
     Datatypes.O -> n;
     Datatypes.S m' -> Datatypes.S (max n' m')}}

min :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
min n m =
  case n of {
   Datatypes.O -> Datatypes.O;
   Datatypes.S n' ->
    case m of {
     Datatypes.O -> Datatypes.O;
     Datatypes.S m' -> Datatypes.S (min n' m')}}

nat_iter :: Datatypes.Coq_nat -> (a1 -> a1) -> a1 -> a1
nat_iter n f x =
  case n of {
   Datatypes.O -> x;
   Datatypes.S n' -> f (nat_iter n' f x)}

