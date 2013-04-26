module BinNums where

import qualified Prelude

data Coq_positive =
   Coq_xI Coq_positive
 | Coq_xO Coq_positive
 | Coq_xH

positive_rect :: (Coq_positive -> a1 -> a1) -> (Coq_positive -> a1 -> a1) ->
                 a1 -> Coq_positive -> a1
positive_rect f f0 f1 p =
  case p of {
   Coq_xI p0 -> f p0 (positive_rect f f0 f1 p0);
   Coq_xO p0 -> f0 p0 (positive_rect f f0 f1 p0);
   Coq_xH -> f1}

positive_rec :: (Coq_positive -> a1 -> a1) -> (Coq_positive -> a1 -> a1) ->
                a1 -> Coq_positive -> a1
positive_rec =
  positive_rect

data N =
   N0
 | Npos Coq_positive

coq_N_rect :: a1 -> (Coq_positive -> a1) -> N -> a1
coq_N_rect f f0 n =
  case n of {
   N0 -> f;
   Npos x -> f0 x}

coq_N_rec :: a1 -> (Coq_positive -> a1) -> N -> a1
coq_N_rec =
  coq_N_rect

data Z =
   Z0
 | Zpos Coq_positive
 | Zneg Coq_positive

coq_Z_rect :: a1 -> (Coq_positive -> a1) -> (Coq_positive -> a1) -> Z -> a1
coq_Z_rect f f0 f1 z =
  case z of {
   Z0 -> f;
   Zpos x -> f0 x;
   Zneg x -> f1 x}

coq_Z_rec :: a1 -> (Coq_positive -> a1) -> (Coq_positive -> a1) -> Z -> a1
coq_Z_rec =
  coq_Z_rect

