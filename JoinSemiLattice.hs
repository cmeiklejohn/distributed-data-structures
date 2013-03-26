module JoinSemiLattice where

import qualified Prelude
import qualified Datatypes
import qualified Peano


data JoinSemiLattice__Coq_lbool =
   JoinSemiLattice__LBoolBottom
 | JoinSemiLattice__LBoolValue Datatypes.Coq_bool

_JoinSemiLattice__lbool_rect :: a1 -> (Datatypes.Coq_bool -> a1) ->
                                JoinSemiLattice__Coq_lbool -> a1
_JoinSemiLattice__lbool_rect f f0 l =
  case l of {
   JoinSemiLattice__LBoolBottom -> f;
   JoinSemiLattice__LBoolValue x -> f0 x}

_JoinSemiLattice__lbool_rec :: a1 -> (Datatypes.Coq_bool -> a1) ->
                               JoinSemiLattice__Coq_lbool -> a1
_JoinSemiLattice__lbool_rec =
  _JoinSemiLattice__lbool_rect

_JoinSemiLattice__lbool_merge :: JoinSemiLattice__Coq_lbool ->
                                 JoinSemiLattice__Coq_lbool ->
                                 JoinSemiLattice__Coq_lbool
_JoinSemiLattice__lbool_merge lb1 lb2 =
  case lb1 of {
   JoinSemiLattice__LBoolBottom -> lb2;
   JoinSemiLattice__LBoolValue b1 ->
    case lb2 of {
     JoinSemiLattice__LBoolBottom -> JoinSemiLattice__LBoolValue b1;
     JoinSemiLattice__LBoolValue b2 -> JoinSemiLattice__LBoolValue
      (Datatypes.orb b1 b2)}}

type JoinSemiLattice__Coq_lmax =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMax
  
_JoinSemiLattice__lmax_rect :: (Datatypes.Coq_nat -> a1) ->
                               JoinSemiLattice__Coq_lmax -> a1
_JoinSemiLattice__lmax_rect f l =
  f l

_JoinSemiLattice__lmax_rec :: (Datatypes.Coq_nat -> a1) ->
                              JoinSemiLattice__Coq_lmax -> a1
_JoinSemiLattice__lmax_rec =
  _JoinSemiLattice__lmax_rect

_JoinSemiLattice__lmax_merge :: JoinSemiLattice__Coq_lmax ->
                                JoinSemiLattice__Coq_lmax ->
                                JoinSemiLattice__Coq_lmax
_JoinSemiLattice__lmax_merge lm1 lm2 =
  Peano.max lm1 lm2

type JoinSemiLattice__Coq_lmin =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMin
  
_JoinSemiLattice__lmin_rect :: (Datatypes.Coq_nat -> a1) ->
                               JoinSemiLattice__Coq_lmin -> a1
_JoinSemiLattice__lmin_rect f l =
  f l

_JoinSemiLattice__lmin_rec :: (Datatypes.Coq_nat -> a1) ->
                              JoinSemiLattice__Coq_lmin -> a1
_JoinSemiLattice__lmin_rec =
  _JoinSemiLattice__lmin_rect

_JoinSemiLattice__lmin_merge :: JoinSemiLattice__Coq_lmin ->
                                JoinSemiLattice__Coq_lmin ->
                                JoinSemiLattice__Coq_lmin
_JoinSemiLattice__lmin_merge lm1 lm2 =
  Peano.min lm1 lm2

