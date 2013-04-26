module Compare_dec where

import qualified Prelude
import qualified Datatypes
import qualified Logic
import qualified Specif


zerop :: Datatypes.Coq_nat -> Specif.Coq_sumbool
zerop n =
  case n of {
   Datatypes.O -> Specif.Coq_left;
   Datatypes.S n0 -> Specif.Coq_right}

lt_eq_lt_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumor
                Specif.Coq_sumbool
lt_eq_lt_dec n m =
  Datatypes.nat_rec (\m0 ->
    case m0 of {
     Datatypes.O -> Specif.Coq_inleft Specif.Coq_right;
     Datatypes.S m1 -> Specif.Coq_inleft Specif.Coq_left}) (\n0 iHn m0 ->
    case m0 of {
     Datatypes.O -> Specif.Coq_inright;
     Datatypes.S m1 -> iHn m1}) n m

gt_eq_gt_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumor
                Specif.Coq_sumbool
gt_eq_gt_dec n m =
  lt_eq_lt_dec n m

le_lt_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
le_lt_dec n m =
  Datatypes.nat_rec (\m0 -> Specif.Coq_left) (\n0 iHn m0 ->
    case m0 of {
     Datatypes.O -> Specif.Coq_right;
     Datatypes.S m1 ->
      Specif.sumbool_rec (\_ -> Specif.Coq_left) (\_ -> Specif.Coq_right)
        (iHn m1)}) n m

le_le_S_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
le_le_S_dec n m =
  le_lt_dec n m

le_ge_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
le_ge_dec n m =
  Specif.sumbool_rec (\_ -> Specif.Coq_left) (\_ -> Specif.Coq_right)
    (le_lt_dec n m)

le_gt_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
le_gt_dec n m =
  le_lt_dec n m

le_lt_eq_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
le_lt_eq_dec n m =
  let {s = lt_eq_lt_dec n m} in
  case s of {
   Specif.Coq_inleft s0 -> s0;
   Specif.Coq_inright -> Logic.coq_False_rec}

le_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
le_dec n m =
  le_gt_dec n m

lt_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
lt_dec n m =
  le_dec (Datatypes.S n) m

gt_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
gt_dec n m =
  lt_dec m n

ge_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
ge_dec n m =
  le_dec m n

nat_compare :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
               Datatypes.Coq_comparison
nat_compare n m =
  case n of {
   Datatypes.O ->
    case m of {
     Datatypes.O -> Datatypes.Eq;
     Datatypes.S n0 -> Datatypes.Lt};
   Datatypes.S n' ->
    case m of {
     Datatypes.O -> Datatypes.Gt;
     Datatypes.S m' -> nat_compare n' m'}}

nat_compare_alt :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_comparison
nat_compare_alt n m =
  case lt_eq_lt_dec n m of {
   Specif.Coq_inleft s ->
    case s of {
     Specif.Coq_left -> Datatypes.Lt;
     Specif.Coq_right -> Datatypes.Eq};
   Specif.Coq_inright -> Datatypes.Gt}

leb :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
leb m x =
  case m of {
   Datatypes.O -> Datatypes.Coq_true;
   Datatypes.S m' ->
    case x of {
     Datatypes.O -> Datatypes.Coq_false;
     Datatypes.S n' -> leb m' n'}}

