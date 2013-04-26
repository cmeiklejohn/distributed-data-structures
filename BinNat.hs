module BinNat where

import qualified Prelude
import qualified BinNums
import qualified BinPos
import qualified Bool
import qualified Datatypes
import qualified Logic
import qualified Peano
import qualified Specif


__ :: any
__ = Prelude.error "Logical or arity value used"

type N__Coq_t = BinNums.N

_N__zero :: BinNums.N
_N__zero =
  BinNums.N0

_N__one :: BinNums.N
_N__one =
  BinNums.Npos BinNums.Coq_xH

_N__two :: BinNums.N
_N__two =
  BinNums.Npos (BinNums.Coq_xO BinNums.Coq_xH)

_N__succ_double :: BinNums.N -> BinNums.N
_N__succ_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xI p)}

_N__double :: BinNums.N -> BinNums.N
_N__double n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xO p)}

_N__succ :: BinNums.N -> BinNums.N
_N__succ n =
  case n of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__succ p)}

_N__pred :: BinNums.N -> BinNums.N
_N__pred n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinPos._Pos__pred_N p}

_N__succ_pos :: BinNums.N -> BinNums.Coq_positive
_N__succ_pos n =
  case n of {
   BinNums.N0 -> BinNums.Coq_xH;
   BinNums.Npos p -> BinPos._Pos__succ p}

_N__add :: BinNums.N -> BinNums.N -> BinNums.N
_N__add n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__add p q)}}

_N__sub :: BinNums.N -> BinNums.N -> BinNums.N
_N__sub n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos m' ->
      case BinPos._Pos__sub_mask n' m' of {
       BinPos.Pos__IsPos p -> BinNums.Npos p;
       _ -> BinNums.N0}}}

_N__mul :: BinNums.N -> BinNums.N -> BinNums.N
_N__mul n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__mul p q)}}

_N__compare :: BinNums.N -> BinNums.N -> Datatypes.Coq_comparison
_N__compare n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Datatypes.Eq;
     BinNums.Npos m' -> Datatypes.Lt};
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> Datatypes.Gt;
     BinNums.Npos m' -> BinPos._Pos__compare n' m'}}

_N__eqb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N__eqb n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos p -> Datatypes.Coq_false};
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> Datatypes.Coq_false;
     BinNums.Npos q -> BinPos._Pos__eqb p q}}

_N__leb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N__leb x y =
  case _N__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_N__ltb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N__ltb x y =
  case _N__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_N__min :: BinNums.N -> BinNums.N -> BinNums.N
_N__min n n' =
  case _N__compare n n' of {
   Datatypes.Gt -> n';
   _ -> n}

_N__max :: BinNums.N -> BinNums.N -> BinNums.N
_N__max n n' =
  case _N__compare n n' of {
   Datatypes.Gt -> n;
   _ -> n'}

_N__div2 :: BinNums.N -> BinNums.N
_N__div2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Npos p;
     BinNums.Coq_xO p -> BinNums.Npos p;
     BinNums.Coq_xH -> BinNums.N0}}

_N__even :: BinNums.N -> Datatypes.Coq_bool
_N__even n =
  case n of {
   BinNums.N0 -> Datatypes.Coq_true;
   BinNums.Npos p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_N__odd :: BinNums.N -> Datatypes.Coq_bool
_N__odd n =
  Datatypes.negb (_N__even n)

_N__pow :: BinNums.N -> BinNums.N -> BinNums.N
_N__pow n p =
  case p of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p0 ->
    case n of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__pow q p0)}}

_N__square :: BinNums.N -> BinNums.N
_N__square n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__square p)}

_N__log2 :: BinNums.N -> BinNums.N
_N__log2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Npos (BinPos._Pos__size p);
     BinNums.Coq_xO p -> BinNums.Npos (BinPos._Pos__size p);
     BinNums.Coq_xH -> BinNums.N0}}

_N__size :: BinNums.N -> BinNums.N
_N__size n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__size p)}

_N__size_nat :: BinNums.N -> Datatypes.Coq_nat
_N__size_nat n =
  case n of {
   BinNums.N0 -> Datatypes.O;
   BinNums.Npos p -> BinPos._Pos__size_nat p}

_N__pos_div_eucl :: BinNums.Coq_positive -> BinNums.N -> Datatypes.Coq_prod
                    BinNums.N BinNums.N
_N__pos_div_eucl a b =
  case a of {
   BinNums.Coq_xI a' ->
    case _N__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {r' = _N__succ_double r} in
      case _N__leb b r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (_N__succ_double q)
        (_N__sub r' b);
       Datatypes.Coq_false -> Datatypes.Coq_pair (_N__double q) r'}};
   BinNums.Coq_xO a' ->
    case _N__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {r' = _N__double r} in
      case _N__leb b r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (_N__succ_double q)
        (_N__sub r' b);
       Datatypes.Coq_false -> Datatypes.Coq_pair (_N__double q) r'}};
   BinNums.Coq_xH ->
    case b of {
     BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 (BinNums.Npos
      BinNums.Coq_xH);
     BinNums.Npos p ->
      case p of {
       BinNums.Coq_xH -> Datatypes.Coq_pair (BinNums.Npos BinNums.Coq_xH)
        BinNums.N0;
       _ -> Datatypes.Coq_pair BinNums.N0 (BinNums.Npos BinNums.Coq_xH)}}}

_N__div_eucl :: BinNums.N -> BinNums.N -> Datatypes.Coq_prod BinNums.N
                BinNums.N
_N__div_eucl a b =
  case a of {
   BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 BinNums.N0;
   BinNums.Npos na ->
    case b of {
     BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 a;
     BinNums.Npos p -> _N__pos_div_eucl na b}}

_N__div :: BinNums.N -> BinNums.N -> BinNums.N
_N__div a b =
  Datatypes.fst (_N__div_eucl a b)

_N__modulo :: BinNums.N -> BinNums.N -> BinNums.N
_N__modulo a b =
  Datatypes.snd (_N__div_eucl a b)

_N__gcd :: BinNums.N -> BinNums.N -> BinNums.N
_N__gcd a b =
  case a of {
   BinNums.N0 -> b;
   BinNums.Npos p ->
    case b of {
     BinNums.N0 -> a;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__gcd p q)}}

_N__ggcd :: BinNums.N -> BinNums.N -> Datatypes.Coq_prod BinNums.N
            (Datatypes.Coq_prod BinNums.N BinNums.N)
_N__ggcd a b =
  case a of {
   BinNums.N0 -> Datatypes.Coq_pair b (Datatypes.Coq_pair BinNums.N0
    (BinNums.Npos BinNums.Coq_xH));
   BinNums.Npos p ->
    case b of {
     BinNums.N0 -> Datatypes.Coq_pair a (Datatypes.Coq_pair (BinNums.Npos
      BinNums.Coq_xH) BinNums.N0);
     BinNums.Npos q ->
      case BinPos._Pos__ggcd p q of {
       Datatypes.Coq_pair g p0 ->
        case p0 of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Npos g)
          (Datatypes.Coq_pair (BinNums.Npos aa) (BinNums.Npos bb))}}}}

_N__sqrtrem :: BinNums.N -> Datatypes.Coq_prod BinNums.N BinNums.N
_N__sqrtrem n =
  case n of {
   BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 BinNums.N0;
   BinNums.Npos p ->
    case BinPos._Pos__sqrtrem p of {
     Datatypes.Coq_pair s m ->
      case m of {
       BinPos.Pos__IsPos r -> Datatypes.Coq_pair (BinNums.Npos s)
        (BinNums.Npos r);
       _ -> Datatypes.Coq_pair (BinNums.Npos s) BinNums.N0}}}

_N__sqrt :: BinNums.N -> BinNums.N
_N__sqrt n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__sqrt p)}

_N__lor :: BinNums.N -> BinNums.N -> BinNums.N
_N__lor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__lor p q)}}

_N__land :: BinNums.N -> BinNums.N -> BinNums.N
_N__land n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinPos._Pos__land p q}}

_N__ldiff :: BinNums.N -> BinNums.N -> BinNums.N
_N__ldiff n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__ldiff p q}}

_N__lxor :: BinNums.N -> BinNums.N -> BinNums.N
_N__lxor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__lxor p q}}

_N__shiftl_nat :: BinNums.N -> Datatypes.Coq_nat -> BinNums.N
_N__shiftl_nat a n =
  Peano.nat_iter n _N__double a

_N__shiftr_nat :: BinNums.N -> Datatypes.Coq_nat -> BinNums.N
_N__shiftr_nat a n =
  Peano.nat_iter n _N__div2 a

_N__shiftl :: BinNums.N -> BinNums.N -> BinNums.N
_N__shiftl a n =
  case a of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos a0 -> BinNums.Npos (BinPos._Pos__shiftl a0 n)}

_N__shiftr :: BinNums.N -> BinNums.N -> BinNums.N
_N__shiftr a n =
  case n of {
   BinNums.N0 -> a;
   BinNums.Npos p -> BinPos._Pos__iter p _N__div2 a}

_N__testbit_nat :: BinNums.N -> Datatypes.Coq_nat -> Datatypes.Coq_bool
_N__testbit_nat a =
  case a of {
   BinNums.N0 -> (\x -> Datatypes.Coq_false);
   BinNums.Npos p -> BinPos._Pos__testbit_nat p}

_N__testbit :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N__testbit a n =
  case a of {
   BinNums.N0 -> Datatypes.Coq_false;
   BinNums.Npos p -> BinPos._Pos__testbit p n}

_N__to_nat :: BinNums.N -> Datatypes.Coq_nat
_N__to_nat a =
  case a of {
   BinNums.N0 -> Datatypes.O;
   BinNums.Npos p -> BinPos._Pos__to_nat p}

_N__of_nat :: Datatypes.Coq_nat -> BinNums.N
_N__of_nat n =
  case n of {
   Datatypes.O -> BinNums.N0;
   Datatypes.S n' -> BinNums.Npos (BinPos._Pos__of_succ_nat n')}

_N__iter :: BinNums.N -> (a1 -> a1) -> a1 -> a1
_N__iter n f x =
  case n of {
   BinNums.N0 -> x;
   BinNums.Npos p -> BinPos._Pos__iter p f x}

_N__eq_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N__eq_dec n m =
  BinNums.coq_N_rec (\m0 ->
    case m0 of {
     BinNums.N0 -> Specif.Coq_left;
     BinNums.Npos p -> Specif.Coq_right}) (\p m0 ->
    case m0 of {
     BinNums.N0 -> Specif.Coq_right;
     BinNums.Npos p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (BinPos._Pos__eq_dec p p0)}) n m

_N__discr :: BinNums.N -> Specif.Coq_sumor BinNums.Coq_positive
_N__discr n =
  case n of {
   BinNums.N0 -> Specif.Coq_inright;
   BinNums.Npos p -> Specif.Coq_inleft p}

_N__binary_rect :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 -> a1)
                   -> BinNums.N -> a1
_N__binary_rect f0 f2 fS2 n =
  let {f2' = \p -> f2 (BinNums.Npos p)} in
  let {fS2' = \p -> fS2 (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinNums.positive_rect fS2' f2' (fS2 BinNums.N0 f0) p}

_N__binary_rec :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 -> a1) ->
                  BinNums.N -> a1
_N__binary_rec =
  _N__binary_rect

_N__peano_rect :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N__peano_rect f0 f n =
  let {f' = \p -> f (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinPos._Pos__peano_rect (f BinNums.N0 f0) f' p}

_N__peano_rec :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N__peano_rec =
  _N__peano_rect

_N__leb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N__leb_spec0 x y =
  Bool.iff_reflect (_N__leb x y)

_N__ltb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N__ltb_spec0 x y =
  Bool.iff_reflect (_N__ltb x y)

_N__recursion :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N__recursion =
  _N__peano_rect

_N__sqrt_up :: BinNums.N -> BinNums.N
_N__sqrt_up a =
  case _N__compare BinNums.N0 a of {
   Datatypes.Lt -> _N__succ (_N__sqrt (_N__pred a));
   _ -> BinNums.N0}

_N__log2_up :: BinNums.N -> BinNums.N
_N__log2_up a =
  case _N__compare (BinNums.Npos BinNums.Coq_xH) a of {
   Datatypes.Lt -> _N__succ (_N__log2 (_N__pred a));
   _ -> BinNums.N0}

_N__lcm :: BinNums.N -> BinNums.N -> BinNums.N
_N__lcm a b =
  _N__mul a (_N__div b (_N__gcd a b))

_N__eqb_spec :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N__eqb_spec x y =
  Bool.iff_reflect (_N__eqb x y)

_N__b2n :: Datatypes.Coq_bool -> BinNums.N
_N__b2n b =
  case b of {
   Datatypes.Coq_true -> BinNums.Npos BinNums.Coq_xH;
   Datatypes.Coq_false -> BinNums.N0}

_N__setbit :: BinNums.N -> BinNums.N -> BinNums.N
_N__setbit a n =
  _N__lor a (_N__shiftl (BinNums.Npos BinNums.Coq_xH) n)

_N__clearbit :: BinNums.N -> BinNums.N -> BinNums.N
_N__clearbit a n =
  _N__ldiff a (_N__shiftl (BinNums.Npos BinNums.Coq_xH) n)

_N__ones :: BinNums.N -> BinNums.N
_N__ones n =
  _N__pred (_N__shiftl (BinNums.Npos BinNums.Coq_xH) n)

_N__lnot :: BinNums.N -> BinNums.N -> BinNums.N
_N__lnot a n =
  _N__lxor a (_N__ones n)

_N__Private_Dec__max_case_strong :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                    BinNums.N -> () -> a1 -> a1) -> (() ->
                                    a1) -> (() -> a1) -> a1
_N__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (_N__max n m) __ (hl __);
   _ -> compat m (_N__max n m) __ (hr __)}

_N__Private_Dec__max_case :: BinNums.N -> BinNums.N -> (BinNums.N ->
                             BinNums.N -> () -> a1 -> a1) -> a1 -> a1 -> a1
_N__Private_Dec__max_case n m x x0 x1 =
  _N__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_N__Private_Dec__max_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N__Private_Dec__max_dec n m =
  _N__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_N__Private_Dec__min_case_strong :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                    BinNums.N -> () -> a1 -> a1) -> (() ->
                                    a1) -> (() -> a1) -> a1
_N__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (_N__min n m) __ (hr __);
   _ -> compat n (_N__min n m) __ (hl __)}

_N__Private_Dec__min_case :: BinNums.N -> BinNums.N -> (BinNums.N ->
                             BinNums.N -> () -> a1 -> a1) -> a1 -> a1 -> a1
_N__Private_Dec__min_case n m x x0 x1 =
  _N__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_N__Private_Dec__min_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N__Private_Dec__min_dec n m =
  _N__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_N__max_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() -> a1) ->
                       a1
_N__max_case_strong n m x x0 =
  _N__Private_Dec__max_case_strong n m (\x1 y _ x2 -> Logic.eq_rect __ x2 __)
    x x0

_N__max_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N__max_case n m x x0 =
  _N__max_case_strong n m (\_ -> x) (\_ -> x0)

_N__max_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N__max_dec =
  _N__Private_Dec__max_dec

_N__min_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() -> a1) ->
                       a1
_N__min_case_strong n m x x0 =
  _N__Private_Dec__min_case_strong n m (\x1 y _ x2 -> Logic.eq_rect __ x2 __)
    x x0

_N__min_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N__min_case n m x x0 =
  _N__min_case_strong n m (\_ -> x) (\_ -> x0)

_N__min_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N__min_dec =
  _N__Private_Dec__min_dec

coq_N_rec_double :: BinNums.N -> a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N
                    -> a1 -> a1) -> a1
coq_N_rec_double a f0 f2 fS2 =
  _N__binary_rec f0 f2 fS2 a

