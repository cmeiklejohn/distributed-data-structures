module SemiLattice where

import qualified Prelude
import qualified Datatypes
import qualified Peano


type SemiLattice__Coq_lbool =
  Datatypes.Coq_bool
  -- singleton inductive, whose constructor was LBool
  
_SemiLattice__lbool_rect :: (Datatypes.Coq_bool -> a1) ->
                            SemiLattice__Coq_lbool -> a1
_SemiLattice__lbool_rect f l =
  f l

_SemiLattice__lbool_rec :: (Datatypes.Coq_bool -> a1) ->
                           SemiLattice__Coq_lbool -> a1
_SemiLattice__lbool_rec =
  _SemiLattice__lbool_rect

_SemiLattice__lbool_merge :: SemiLattice__Coq_lbool -> SemiLattice__Coq_lbool
                             -> SemiLattice__Coq_lbool
_SemiLattice__lbool_merge lb1 lb2 =
  Datatypes.orb lb1 lb2

type SemiLattice__Coq_lmax =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMax
  
_SemiLattice__lmax_rect :: (Datatypes.Coq_nat -> a1) -> SemiLattice__Coq_lmax
                           -> a1
_SemiLattice__lmax_rect f l =
  f l

_SemiLattice__lmax_rec :: (Datatypes.Coq_nat -> a1) -> SemiLattice__Coq_lmax
                          -> a1
_SemiLattice__lmax_rec =
  _SemiLattice__lmax_rect

_SemiLattice__lmax_merge :: SemiLattice__Coq_lmax -> SemiLattice__Coq_lmax ->
                            SemiLattice__Coq_lmax
_SemiLattice__lmax_merge lm1 lm2 =
  Peano.max lm1 lm2

type SemiLattice__Coq_lmin =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMin
  
_SemiLattice__lmin_rect :: (Datatypes.Coq_nat -> a1) -> SemiLattice__Coq_lmin
                           -> a1
_SemiLattice__lmin_rect f l =
  f l

_SemiLattice__lmin_rec :: (Datatypes.Coq_nat -> a1) -> SemiLattice__Coq_lmin
                          -> a1
_SemiLattice__lmin_rec =
  _SemiLattice__lmin_rect

_SemiLattice__lmin_merge :: SemiLattice__Coq_lmin -> SemiLattice__Coq_lmin ->
                            SemiLattice__Coq_lmin
_SemiLattice__lmin_merge lm1 lm2 =
  Peano.min lm1 lm2

