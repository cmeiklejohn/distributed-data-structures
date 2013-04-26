module JoinSemiLattice where

import qualified Prelude
import qualified Datatypes
import qualified Peano


type JoinSemiLattice__Coq_lbool =
  Datatypes.Coq_bool
  -- singleton inductive, whose constructor was LBoolValue
  
_JoinSemiLattice__lbool_rect :: (Datatypes.Coq_bool -> a1) ->
                                JoinSemiLattice__Coq_lbool -> a1
_JoinSemiLattice__lbool_rect f l =
  f l

_JoinSemiLattice__lbool_rec :: (Datatypes.Coq_bool -> a1) ->
                               JoinSemiLattice__Coq_lbool -> a1
_JoinSemiLattice__lbool_rec =
  _JoinSemiLattice__lbool_rect

_JoinSemiLattice__lbool_reveal :: JoinSemiLattice__Coq_lbool ->
                                  Datatypes.Coq_bool
_JoinSemiLattice__lbool_reveal lb =
  lb

_JoinSemiLattice__lbool_merge :: JoinSemiLattice__Coq_lbool ->
                                 JoinSemiLattice__Coq_lbool ->
                                 JoinSemiLattice__Coq_lbool
_JoinSemiLattice__lbool_merge lb1 lb2 =
  Datatypes.orb lb1 lb2

type JoinSemiLattice__Coq_lmax_nat =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMaxNatValue
  
_JoinSemiLattice__lmax_nat_rect :: (Datatypes.Coq_nat -> a1) ->
                                   JoinSemiLattice__Coq_lmax_nat -> a1
_JoinSemiLattice__lmax_nat_rect f l =
  f l

_JoinSemiLattice__lmax_nat_rec :: (Datatypes.Coq_nat -> a1) ->
                                  JoinSemiLattice__Coq_lmax_nat -> a1
_JoinSemiLattice__lmax_nat_rec =
  _JoinSemiLattice__lmax_nat_rect

_JoinSemiLattice__lmax_nat_reveal :: JoinSemiLattice__Coq_lmax_nat ->
                                     Datatypes.Coq_nat
_JoinSemiLattice__lmax_nat_reveal lm =
  lm

_JoinSemiLattice__lmax_nat_merge :: JoinSemiLattice__Coq_lmax_nat ->
                                    JoinSemiLattice__Coq_lmax_nat ->
                                    JoinSemiLattice__Coq_lmax_nat
_JoinSemiLattice__lmax_nat_merge lm1 lm2 =
  Peano.max lm1 lm2

type JoinSemiLattice__Coq_lmin_nat =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMinNatValue
  
_JoinSemiLattice__lmin_nat_rect :: (Datatypes.Coq_nat -> a1) ->
                                   JoinSemiLattice__Coq_lmin_nat -> a1
_JoinSemiLattice__lmin_nat_rect f l =
  f l

_JoinSemiLattice__lmin_nat_rec :: (Datatypes.Coq_nat -> a1) ->
                                  JoinSemiLattice__Coq_lmin_nat -> a1
_JoinSemiLattice__lmin_nat_rec =
  _JoinSemiLattice__lmin_nat_rect

_JoinSemiLattice__lmin_nat_reveal :: JoinSemiLattice__Coq_lmin_nat ->
                                     Datatypes.Coq_nat
_JoinSemiLattice__lmin_nat_reveal lm =
  lm

_JoinSemiLattice__lmin_nat_merge :: JoinSemiLattice__Coq_lmin_nat ->
                                    JoinSemiLattice__Coq_lmin_nat ->
                                    JoinSemiLattice__Coq_lmin_nat
_JoinSemiLattice__lmin_nat_merge lm1 lm2 =
  Peano.min lm1 lm2

