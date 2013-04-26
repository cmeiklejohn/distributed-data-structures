module EqNat where

import qualified Prelude
import qualified Datatypes
import qualified Specif


eq_nat_decide :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
eq_nat_decide n =
  Datatypes.nat_rec (\m ->
    case m of {
     Datatypes.O -> Specif.Coq_left;
     Datatypes.S n0 -> Specif.Coq_right}) (\n0 iHn m ->
    case m of {
     Datatypes.O -> Specif.Coq_right;
     Datatypes.S n1 -> iHn n1}) n

beq_nat :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
beq_nat n m =
  case n of {
   Datatypes.O ->
    case m of {
     Datatypes.O -> Datatypes.Coq_true;
     Datatypes.S n0 -> Datatypes.Coq_false};
   Datatypes.S n1 ->
    case m of {
     Datatypes.O -> Datatypes.Coq_false;
     Datatypes.S m1 -> beq_nat n1 m1}}

