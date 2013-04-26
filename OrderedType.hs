module OrderedType where

import qualified Prelude

__ :: any
__ = Prelude.error "Logical or arity value used"

data Compare x =
   LT
 | EQ
 | GT

coq_Compare_rect :: a1 -> a1 -> (() -> a2) -> (() -> a2) -> (() -> a2) ->
                    (Compare a1) -> a2
coq_Compare_rect x y f f0 f1 c =
  case c of {
   LT -> f __;
   EQ -> f0 __;
   GT -> f1 __}

coq_Compare_rec :: a1 -> a1 -> (() -> a2) -> (() -> a2) -> (() -> a2) ->
                   (Compare a1) -> a2
coq_Compare_rec x y =
  coq_Compare_rect x y

