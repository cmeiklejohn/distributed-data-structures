module Div2 where

import qualified Prelude
import qualified Datatypes
import qualified Peano


div2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
div2 n =
  case n of {
   Datatypes.O -> Datatypes.O;
   Datatypes.S n0 ->
    case n0 of {
     Datatypes.O -> Datatypes.O;
     Datatypes.S n' -> Datatypes.S (div2 n')}}

double :: Datatypes.Coq_nat -> Datatypes.Coq_nat
double n =
  Peano.plus n n

even_2n :: Datatypes.Coq_nat -> Datatypes.Coq_nat
even_2n n =
  div2 n

odd_S2n :: Datatypes.Coq_nat -> Datatypes.Coq_nat
odd_S2n n =
  div2 n

