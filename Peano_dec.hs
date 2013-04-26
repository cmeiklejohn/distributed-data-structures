module Peano_dec where

import qualified Prelude
import qualified Datatypes
import qualified Specif


coq_O_or_S :: Datatypes.Coq_nat -> Specif.Coq_sumor Datatypes.Coq_nat
coq_O_or_S n =
  Datatypes.nat_rec Specif.Coq_inright (\n0 iHn -> Specif.Coq_inleft n0) n

eq_nat_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
eq_nat_dec n =
  Datatypes.nat_rec (\m ->
    case m of {
     Datatypes.O -> Specif.Coq_left;
     Datatypes.S m0 -> Specif.Coq_right}) (\n0 iHn m ->
    case m of {
     Datatypes.O -> Specif.Coq_right;
     Datatypes.S m0 ->
      Specif.sumbool_rec (\_ -> Specif.Coq_left) (\_ -> Specif.Coq_right)
        (iHn m0)}) n

