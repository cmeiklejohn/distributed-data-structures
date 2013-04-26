module Bool where

import qualified Prelude
import qualified Datatypes
import qualified Logic
import qualified Specif


__ :: any
__ = Prelude.error "Logical or arity value used"

bool_dec :: Datatypes.Coq_bool -> Datatypes.Coq_bool -> Specif.Coq_sumbool
bool_dec b1 b2 =
  Datatypes.bool_rec (\b0 ->
    case b0 of {
     Datatypes.Coq_true -> Specif.Coq_left;
     Datatypes.Coq_false -> Specif.Coq_right}) (\b0 ->
    case b0 of {
     Datatypes.Coq_true -> Specif.Coq_right;
     Datatypes.Coq_false -> Specif.Coq_left}) b1 b2

eqb :: Datatypes.Coq_bool -> Datatypes.Coq_bool -> Datatypes.Coq_bool
eqb b1 b2 =
  case b1 of {
   Datatypes.Coq_true -> b2;
   Datatypes.Coq_false ->
    case b2 of {
     Datatypes.Coq_true -> Datatypes.Coq_false;
     Datatypes.Coq_false -> Datatypes.Coq_true}}

ifb :: Datatypes.Coq_bool -> Datatypes.Coq_bool -> Datatypes.Coq_bool ->
       Datatypes.Coq_bool
ifb b1 b2 b3 =
  case b1 of {
   Datatypes.Coq_true -> b2;
   Datatypes.Coq_false -> b3}

orb_true_elim :: Datatypes.Coq_bool -> Datatypes.Coq_bool ->
                 Specif.Coq_sumbool
orb_true_elim b1 b2 =
  case b1 of {
   Datatypes.Coq_true -> Specif.Coq_left;
   Datatypes.Coq_false -> Specif.Coq_right}

andb_false_elim :: Datatypes.Coq_bool -> Datatypes.Coq_bool ->
                   Specif.Coq_sumbool
andb_false_elim b1 b2 =
  case b1 of {
   Datatypes.Coq_true -> Specif.Coq_right;
   Datatypes.Coq_false -> Specif.Coq_left}

data Coq_reflect =
   ReflectT
 | ReflectF

reflect_rect :: (() -> a1) -> (() -> a1) -> Datatypes.Coq_bool -> Coq_reflect
                -> a1
reflect_rect f f0 b r =
  case r of {
   ReflectT -> f __;
   ReflectF -> f0 __}

reflect_rec :: (() -> a1) -> (() -> a1) -> Datatypes.Coq_bool -> Coq_reflect
               -> a1
reflect_rec =
  reflect_rect

iff_reflect :: Datatypes.Coq_bool -> Coq_reflect
iff_reflect b =
  case b of {
   Datatypes.Coq_true -> Logic.and_rec (\_ _ -> ReflectT);
   Datatypes.Coq_false -> Logic.and_rec (\_ _ -> ReflectF)}

reflect_dec :: Datatypes.Coq_bool -> Coq_reflect -> Specif.Coq_sumbool
reflect_dec b h =
  case h of {
   ReflectT -> Specif.Coq_left;
   ReflectF -> Specif.Coq_right}

