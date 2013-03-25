module JoinSemiLattice where

import qualified Prelude
import qualified Datatypes
import qualified Peano


type JoinSemiLattice__Coq_lbool =
  Datatypes.Coq_bool
  -- singleton inductive, whose constructor was LBool
  
_JoinSemiLattice__lbool_rect :: (Datatypes.Coq_bool -> a1) ->
                                JoinSemiLattice__Coq_lbool -> a1
_JoinSemiLattice__lbool_rect f l =
  f l

_JoinSemiLattice__lbool_rec :: (Datatypes.Coq_bool -> a1) ->
                               JoinSemiLattice__Coq_lbool -> a1
_JoinSemiLattice__lbool_rec =
  _JoinSemiLattice__lbool_rect

_JoinSemiLattice__lbool_merge :: JoinSemiLattice__Coq_lbool ->
                                 JoinSemiLattice__Coq_lbool ->
                                 JoinSemiLattice__Coq_lbool
_JoinSemiLattice__lbool_merge lb1 lb2 =
  Datatypes.orb lb1 lb2

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

