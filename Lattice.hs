module Lattice where

import qualified Prelude
import qualified Datatypes

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

