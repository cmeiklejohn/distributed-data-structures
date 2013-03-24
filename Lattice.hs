module Lattice where

import qualified Prelude
import qualified Datatypes
import qualified Peano


type Lattice__Coq_lbool =
  Datatypes.Coq_bool
  -- singleton inductive, whose constructor was LBool
  
_Lattice__lbool_rect :: (Datatypes.Coq_bool -> a1) -> Lattice__Coq_lbool ->
                        a1
_Lattice__lbool_rect f l =
  f l

_Lattice__lbool_rec :: (Datatypes.Coq_bool -> a1) -> Lattice__Coq_lbool -> a1
_Lattice__lbool_rec =
  _Lattice__lbool_rect

_Lattice__lbool_merge :: Lattice__Coq_lbool -> Lattice__Coq_lbool ->
                         Lattice__Coq_lbool
_Lattice__lbool_merge lb1 lb2 =
  Datatypes.orb lb1 lb2

type Lattice__Coq_lmax =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMax
  
_Lattice__lmax_rect :: (Datatypes.Coq_nat -> a1) -> Lattice__Coq_lmax -> a1
_Lattice__lmax_rect f l =
  f l

_Lattice__lmax_rec :: (Datatypes.Coq_nat -> a1) -> Lattice__Coq_lmax -> a1
_Lattice__lmax_rec =
  _Lattice__lmax_rect

_Lattice__lmax_merge :: Lattice__Coq_lmax -> Lattice__Coq_lmax ->
                        Lattice__Coq_lmax
_Lattice__lmax_merge lm1 lm2 =
  Peano.max lm1 lm2

type Lattice__Coq_lmin =
  Datatypes.Coq_nat
  -- singleton inductive, whose constructor was LMin
  
_Lattice__lmin_rect :: (Datatypes.Coq_nat -> a1) -> Lattice__Coq_lmin -> a1
_Lattice__lmin_rect f l =
  f l

_Lattice__lmin_rec :: (Datatypes.Coq_nat -> a1) -> Lattice__Coq_lmin -> a1
_Lattice__lmin_rec =
  _Lattice__lmin_rect

_Lattice__lmin_merge :: Lattice__Coq_lmin -> Lattice__Coq_lmin ->
                        Lattice__Coq_lmin
_Lattice__lmin_merge lm1 lm2 =
  Peano.min lm1 lm2

