{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module OrdersEx where

import qualified Prelude
import qualified BinInt
import qualified BinNat
import qualified BinNums
import qualified BinPos
import qualified Bool
import qualified Compare_dec
import qualified Datatypes
import qualified Div2
import qualified EqNat
import qualified Logic
import qualified NPeano
import qualified Peano
import qualified Peano_dec
import qualified Specif



unsafeCoerce :: a -> b
#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
unsafeCoerce = GHC.Base.unsafeCoerce#
#else
-- HUGS
import qualified IOExts
unsafeCoerce = IOExts.unsafeCoerce
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

_Nat_as_OT__recursion :: a1 -> (Datatypes.Coq_nat -> a1 -> a1) ->
                         Datatypes.Coq_nat -> a1
_Nat_as_OT__recursion =
  Datatypes.nat_rect

type Nat_as_OT__Coq_t = Datatypes.Coq_nat

_Nat_as_OT__eqb :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_bool
_Nat_as_OT__eqb =
  EqNat.beq_nat

_Nat_as_OT__compare :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Datatypes.Coq_comparison
_Nat_as_OT__compare =
  Compare_dec.nat_compare

_Nat_as_OT__zero :: Datatypes.Coq_nat
_Nat_as_OT__zero =
  Datatypes.O

_Nat_as_OT__one :: Datatypes.Coq_nat
_Nat_as_OT__one =
  Datatypes.S Datatypes.O

_Nat_as_OT__two :: Datatypes.Coq_nat
_Nat_as_OT__two =
  Datatypes.S (Datatypes.S Datatypes.O)

_Nat_as_OT__succ :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__succ x =
  Datatypes.S x

_Nat_as_OT__pred :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__pred =
  Peano.pred

_Nat_as_OT__add :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__add =
  Peano.plus

_Nat_as_OT__sub :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__sub =
  Peano.minus

_Nat_as_OT__mul :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__mul =
  Peano.mult

_Nat_as_OT__ltb :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_bool
_Nat_as_OT__ltb =
  NPeano.ltb

_Nat_as_OT__leb :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_bool
_Nat_as_OT__leb =
  NPeano.leb

_Nat_as_OT__min :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__min =
  Peano.min

_Nat_as_OT__max :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__max =
  Peano.max

_Nat_as_OT__eq_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Specif.Coq_sumbool
_Nat_as_OT__eq_dec =
  Peano_dec.eq_nat_dec

_Nat_as_OT__even :: Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat_as_OT__even =
  NPeano.even

_Nat_as_OT__odd :: Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat_as_OT__odd =
  NPeano.odd

_Nat_as_OT__pow :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__pow =
  NPeano.pow

_Nat_as_OT__square :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__square =
  NPeano.square

_Nat_as_OT__log2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__log2 =
  NPeano.log2

_Nat_as_OT__sqrt :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__sqrt =
  NPeano.sqrt

_Nat_as_OT__div :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__div =
  NPeano.div

_Nat_as_OT__modulo :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_OT__modulo =
  NPeano.modulo

_Nat_as_OT__gcd :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__gcd =
  NPeano.gcd

_Nat_as_OT__testbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Datatypes.Coq_bool
_Nat_as_OT__testbit =
  NPeano.testbit

_Nat_as_OT__shiftl :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_OT__shiftl =
  NPeano.shiftl

_Nat_as_OT__shiftr :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_OT__shiftr =
  NPeano.shiftr

_Nat_as_OT__lxor :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                    Datatypes.Coq_nat
_Nat_as_OT__lxor =
  NPeano.lxor

_Nat_as_OT__land :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                    Datatypes.Coq_nat
_Nat_as_OT__land =
  NPeano.land

_Nat_as_OT__lor :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__lor =
  NPeano.lor

_Nat_as_OT__ldiff :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                     Datatypes.Coq_nat
_Nat_as_OT__ldiff =
  NPeano.ldiff

_Nat_as_OT__div2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__div2 =
  Div2.div2

_Nat_as_OT__sqrt_up :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__sqrt_up a =
  case Compare_dec.nat_compare Datatypes.O a of {
   Datatypes.Lt -> Datatypes.S (NPeano.sqrt (Peano.pred a));
   _ -> Datatypes.O}

_Nat_as_OT__log2_up :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__log2_up a =
  case Compare_dec.nat_compare (Datatypes.S Datatypes.O) a of {
   Datatypes.Lt -> Datatypes.S (NPeano.log2 (Peano.pred a));
   _ -> Datatypes.O}

_Nat_as_OT__lcm :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_OT__lcm a b =
  Peano.mult a (NPeano.div b (NPeano.gcd a b))

_Nat_as_OT__eqb_spec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                        Bool.Coq_reflect
_Nat_as_OT__eqb_spec x y =
  Bool.iff_reflect (EqNat.beq_nat x y)

_Nat_as_OT__b2n :: Datatypes.Coq_bool -> Datatypes.Coq_nat
_Nat_as_OT__b2n b =
  case b of {
   Datatypes.Coq_true -> Datatypes.S Datatypes.O;
   Datatypes.Coq_false -> Datatypes.O}

_Nat_as_OT__setbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_OT__setbit a n =
  NPeano.lor a (NPeano.shiftl (Datatypes.S Datatypes.O) n)

_Nat_as_OT__clearbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                        Datatypes.Coq_nat
_Nat_as_OT__clearbit a n =
  NPeano.ldiff a (NPeano.shiftl (Datatypes.S Datatypes.O) n)

_Nat_as_OT__ones :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_OT__ones n =
  Peano.pred (NPeano.shiftl (Datatypes.S Datatypes.O) n)

_Nat_as_OT__lnot :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                    Datatypes.Coq_nat
_Nat_as_OT__lnot a n =
  NPeano.lxor a (_Nat_as_OT__ones n)

_Nat_as_OT__Private_Dec__max_case_strong :: Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat ->
                                            (Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat -> () -> a1 ->
                                            a1) -> (() -> a1) -> (() -> a1)
                                            -> a1
_Nat_as_OT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (Compare_dec.nat_compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (Peano.max n m) __ (hl __);
   _ -> compat m (Peano.max n m) __ (hr __)}

_Nat_as_OT__Private_Dec__max_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat
                                     -> (Datatypes.Coq_nat ->
                                     Datatypes.Coq_nat -> () -> a1 -> a1) ->
                                     a1 -> a1 -> a1
_Nat_as_OT__Private_Dec__max_case n m x x0 x1 =
  _Nat_as_OT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Nat_as_OT__Private_Dec__max_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                                    Specif.Coq_sumbool
_Nat_as_OT__Private_Dec__max_dec n m =
  _Nat_as_OT__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Nat_as_OT__Private_Dec__min_case_strong :: Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat ->
                                            (Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat -> () -> a1 ->
                                            a1) -> (() -> a1) -> (() -> a1)
                                            -> a1
_Nat_as_OT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (Compare_dec.nat_compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (Peano.min n m) __ (hr __);
   _ -> compat n (Peano.min n m) __ (hl __)}

_Nat_as_OT__Private_Dec__min_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat
                                     -> (Datatypes.Coq_nat ->
                                     Datatypes.Coq_nat -> () -> a1 -> a1) ->
                                     a1 -> a1 -> a1
_Nat_as_OT__Private_Dec__min_case n m x x0 x1 =
  _Nat_as_OT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Nat_as_OT__Private_Dec__min_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                                    Specif.Coq_sumbool
_Nat_as_OT__Private_Dec__min_dec n m =
  _Nat_as_OT__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Nat_as_OT__max_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> (()
                               -> a1) -> (() -> a1) -> a1
_Nat_as_OT__max_case_strong n m x x0 =
  _Nat_as_OT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Nat_as_OT__max_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> a1 -> a1 ->
                        a1
_Nat_as_OT__max_case n m x x0 =
  _Nat_as_OT__max_case_strong n m (\_ -> x) (\_ -> x0)

_Nat_as_OT__max_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Specif.Coq_sumbool
_Nat_as_OT__max_dec =
  _Nat_as_OT__Private_Dec__max_dec

_Nat_as_OT__min_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> (()
                               -> a1) -> (() -> a1) -> a1
_Nat_as_OT__min_case_strong n m x x0 =
  _Nat_as_OT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Nat_as_OT__min_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> a1 -> a1 ->
                        a1
_Nat_as_OT__min_case n m x x0 =
  _Nat_as_OT__min_case_strong n m (\_ -> x) (\_ -> x0)

_Nat_as_OT__min_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Specif.Coq_sumbool
_Nat_as_OT__min_dec =
  _Nat_as_OT__Private_Dec__min_dec

type Positive_as_OT__Coq_t = BinNums.Coq_positive

_Positive_as_OT__succ :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__succ x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xO (_Positive_as_OT__succ p);
   BinNums.Coq_xO p -> BinNums.Coq_xI p;
   BinNums.Coq_xH -> BinNums.Coq_xO BinNums.Coq_xH}

_Positive_as_OT__add :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__add x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Positive_as_OT__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xI (_Positive_as_OT__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xO (_Positive_as_OT__succ p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Positive_as_OT__add p q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Positive_as_OT__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xI p};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Positive_as_OT__succ q);
     BinNums.Coq_xO q -> BinNums.Coq_xI q;
     BinNums.Coq_xH -> BinNums.Coq_xO BinNums.Coq_xH}}

_Positive_as_OT__add_carry :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              BinNums.Coq_positive
_Positive_as_OT__add_carry x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Positive_as_OT__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Positive_as_OT__add_carry p q);
     BinNums.Coq_xH -> BinNums.Coq_xI (_Positive_as_OT__succ p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Positive_as_OT__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xI (_Positive_as_OT__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xO (_Positive_as_OT__succ p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Positive_as_OT__succ q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Positive_as_OT__succ q);
     BinNums.Coq_xH -> BinNums.Coq_xI BinNums.Coq_xH}}

_Positive_as_OT__pred_double :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__pred_double x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xI (BinNums.Coq_xO p);
   BinNums.Coq_xO p -> BinNums.Coq_xI (_Positive_as_OT__pred_double p);
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_OT__pred :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__pred x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xO p;
   BinNums.Coq_xO p -> _Positive_as_OT__pred_double p;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_OT__pred_N :: BinNums.Coq_positive -> BinNums.N
_Positive_as_OT__pred_N x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Npos (BinNums.Coq_xO p);
   BinNums.Coq_xO p -> BinNums.Npos (_Positive_as_OT__pred_double p);
   BinNums.Coq_xH -> BinNums.N0}

data Positive_as_OT__Coq_mask =
   Positive_as_OT__IsNul
 | Positive_as_OT__IsPos BinNums.Coq_positive
 | Positive_as_OT__IsNeg

_Positive_as_OT__mask_rect :: a1 -> (BinNums.Coq_positive -> a1) -> a1 ->
                              Positive_as_OT__Coq_mask -> a1
_Positive_as_OT__mask_rect f f0 f1 m =
  case m of {
   Positive_as_OT__IsNul -> f;
   Positive_as_OT__IsPos x -> f0 x;
   Positive_as_OT__IsNeg -> f1}

_Positive_as_OT__mask_rec :: a1 -> (BinNums.Coq_positive -> a1) -> a1 ->
                             Positive_as_OT__Coq_mask -> a1
_Positive_as_OT__mask_rec =
  _Positive_as_OT__mask_rect

_Positive_as_OT__succ_double_mask :: Positive_as_OT__Coq_mask ->
                                     Positive_as_OT__Coq_mask
_Positive_as_OT__succ_double_mask x =
  case x of {
   Positive_as_OT__IsNul -> Positive_as_OT__IsPos BinNums.Coq_xH;
   Positive_as_OT__IsPos p -> Positive_as_OT__IsPos (BinNums.Coq_xI p);
   Positive_as_OT__IsNeg -> Positive_as_OT__IsNeg}

_Positive_as_OT__double_mask :: Positive_as_OT__Coq_mask ->
                                Positive_as_OT__Coq_mask
_Positive_as_OT__double_mask x =
  case x of {
   Positive_as_OT__IsPos p -> Positive_as_OT__IsPos (BinNums.Coq_xO p);
   x0 -> x0}

_Positive_as_OT__double_pred_mask :: BinNums.Coq_positive ->
                                     Positive_as_OT__Coq_mask
_Positive_as_OT__double_pred_mask x =
  case x of {
   BinNums.Coq_xI p -> Positive_as_OT__IsPos (BinNums.Coq_xO (BinNums.Coq_xO
    p));
   BinNums.Coq_xO p -> Positive_as_OT__IsPos (BinNums.Coq_xO
    (_Positive_as_OT__pred_double p));
   BinNums.Coq_xH -> Positive_as_OT__IsNul}

_Positive_as_OT__pred_mask :: Positive_as_OT__Coq_mask ->
                              Positive_as_OT__Coq_mask
_Positive_as_OT__pred_mask p =
  case p of {
   Positive_as_OT__IsPos q ->
    case q of {
     BinNums.Coq_xH -> Positive_as_OT__IsNul;
     _ -> Positive_as_OT__IsPos (_Positive_as_OT__pred q)};
   _ -> Positive_as_OT__IsNeg}

_Positive_as_OT__sub_mask :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             Positive_as_OT__Coq_mask
_Positive_as_OT__sub_mask x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_OT__double_mask (_Positive_as_OT__sub_mask p q);
     BinNums.Coq_xO q ->
      _Positive_as_OT__succ_double_mask (_Positive_as_OT__sub_mask p q);
     BinNums.Coq_xH -> Positive_as_OT__IsPos (BinNums.Coq_xO p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_OT__succ_double_mask (_Positive_as_OT__sub_mask_carry p q);
     BinNums.Coq_xO q ->
      _Positive_as_OT__double_mask (_Positive_as_OT__sub_mask p q);
     BinNums.Coq_xH -> Positive_as_OT__IsPos (_Positive_as_OT__pred_double p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xH -> Positive_as_OT__IsNul;
     _ -> Positive_as_OT__IsNeg}}

_Positive_as_OT__sub_mask_carry :: BinNums.Coq_positive ->
                                   BinNums.Coq_positive ->
                                   Positive_as_OT__Coq_mask
_Positive_as_OT__sub_mask_carry x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_OT__succ_double_mask (_Positive_as_OT__sub_mask_carry p q);
     BinNums.Coq_xO q ->
      _Positive_as_OT__double_mask (_Positive_as_OT__sub_mask p q);
     BinNums.Coq_xH -> Positive_as_OT__IsPos (_Positive_as_OT__pred_double p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_OT__double_mask (_Positive_as_OT__sub_mask_carry p q);
     BinNums.Coq_xO q ->
      _Positive_as_OT__succ_double_mask (_Positive_as_OT__sub_mask_carry p q);
     BinNums.Coq_xH -> _Positive_as_OT__double_pred_mask p};
   BinNums.Coq_xH -> Positive_as_OT__IsNeg}

_Positive_as_OT__sub :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__sub x y =
  case _Positive_as_OT__sub_mask x y of {
   Positive_as_OT__IsPos z -> z;
   _ -> BinNums.Coq_xH}

_Positive_as_OT__mul :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__mul x y =
  case x of {
   BinNums.Coq_xI p ->
    _Positive_as_OT__add y (BinNums.Coq_xO (_Positive_as_OT__mul p y));
   BinNums.Coq_xO p -> BinNums.Coq_xO (_Positive_as_OT__mul p y);
   BinNums.Coq_xH -> y}

_Positive_as_OT__iter :: BinNums.Coq_positive -> (a1 -> a1) -> a1 -> a1
_Positive_as_OT__iter n f x =
  case n of {
   BinNums.Coq_xI n' ->
    f (_Positive_as_OT__iter n' f (_Positive_as_OT__iter n' f x));
   BinNums.Coq_xO n' ->
    _Positive_as_OT__iter n' f (_Positive_as_OT__iter n' f x);
   BinNums.Coq_xH -> f x}

_Positive_as_OT__pow :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__pow x y =
  _Positive_as_OT__iter y (_Positive_as_OT__mul x) BinNums.Coq_xH

_Positive_as_OT__square :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__square p =
  case p of {
   BinNums.Coq_xI p0 -> BinNums.Coq_xI (BinNums.Coq_xO
    (_Positive_as_OT__add (_Positive_as_OT__square p0) p0));
   BinNums.Coq_xO p0 -> BinNums.Coq_xO (BinNums.Coq_xO
    (_Positive_as_OT__square p0));
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_OT__div2 :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__div2 p =
  case p of {
   BinNums.Coq_xI p0 -> p0;
   BinNums.Coq_xO p0 -> p0;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_OT__div2_up :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__div2_up p =
  case p of {
   BinNums.Coq_xI p0 -> _Positive_as_OT__succ p0;
   BinNums.Coq_xO p0 -> p0;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_OT__size_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat
_Positive_as_OT__size_nat p =
  case p of {
   BinNums.Coq_xI p0 -> Datatypes.S (_Positive_as_OT__size_nat p0);
   BinNums.Coq_xO p0 -> Datatypes.S (_Positive_as_OT__size_nat p0);
   BinNums.Coq_xH -> Datatypes.S Datatypes.O}

_Positive_as_OT__size :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__size p =
  case p of {
   BinNums.Coq_xI p0 -> _Positive_as_OT__succ (_Positive_as_OT__size p0);
   BinNums.Coq_xO p0 -> _Positive_as_OT__succ (_Positive_as_OT__size p0);
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_OT__compare_cont :: BinNums.Coq_positive -> BinNums.Coq_positive
                                 -> Datatypes.Coq_comparison ->
                                 Datatypes.Coq_comparison
_Positive_as_OT__compare_cont x y r =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Positive_as_OT__compare_cont p q r;
     BinNums.Coq_xO q -> _Positive_as_OT__compare_cont p q Datatypes.Gt;
     BinNums.Coq_xH -> Datatypes.Gt};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Positive_as_OT__compare_cont p q Datatypes.Lt;
     BinNums.Coq_xO q -> _Positive_as_OT__compare_cont p q r;
     BinNums.Coq_xH -> Datatypes.Gt};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xH -> r;
     _ -> Datatypes.Lt}}

_Positive_as_OT__compare :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                            Datatypes.Coq_comparison
_Positive_as_OT__compare x y =
  _Positive_as_OT__compare_cont x y Datatypes.Eq

_Positive_as_OT__min :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__min p p' =
  case _Positive_as_OT__compare p p' of {
   Datatypes.Gt -> p';
   _ -> p}

_Positive_as_OT__max :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__max p p' =
  case _Positive_as_OT__compare p p' of {
   Datatypes.Gt -> p;
   _ -> p'}

_Positive_as_OT__eqb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        Datatypes.Coq_bool
_Positive_as_OT__eqb p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Positive_as_OT__eqb p0 q0;
     _ -> Datatypes.Coq_false};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xO q0 -> _Positive_as_OT__eqb p0 q0;
     _ -> Datatypes.Coq_false};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xH -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_Positive_as_OT__leb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        Datatypes.Coq_bool
_Positive_as_OT__leb x y =
  case _Positive_as_OT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Positive_as_OT__ltb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        Datatypes.Coq_bool
_Positive_as_OT__ltb x y =
  case _Positive_as_OT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Positive_as_OT__sqrtrem_step :: (BinNums.Coq_positive ->
                                 BinNums.Coq_positive) ->
                                 (BinNums.Coq_positive ->
                                 BinNums.Coq_positive) -> (Datatypes.Coq_prod
                                 BinNums.Coq_positive
                                 Positive_as_OT__Coq_mask) ->
                                 Datatypes.Coq_prod BinNums.Coq_positive
                                 Positive_as_OT__Coq_mask
_Positive_as_OT__sqrtrem_step f g p =
  case p of {
   Datatypes.Coq_pair s y ->
    case y of {
     Positive_as_OT__IsPos r ->
      let {s' = BinNums.Coq_xI (BinNums.Coq_xO s)} in
      let {r' = g (f r)} in
      case _Positive_as_OT__leb s' r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (BinNums.Coq_xI s)
        (_Positive_as_OT__sub_mask r' s');
       Datatypes.Coq_false -> Datatypes.Coq_pair (BinNums.Coq_xO s)
        (Positive_as_OT__IsPos r')};
     _ -> Datatypes.Coq_pair (BinNums.Coq_xO s)
      (_Positive_as_OT__sub_mask (g (f BinNums.Coq_xH)) (BinNums.Coq_xO
        (BinNums.Coq_xO BinNums.Coq_xH)))}}

_Positive_as_OT__sqrtrem :: BinNums.Coq_positive -> Datatypes.Coq_prod
                            BinNums.Coq_positive Positive_as_OT__Coq_mask
_Positive_as_OT__sqrtrem p =
  case p of {
   BinNums.Coq_xI p0 ->
    case p0 of {
     BinNums.Coq_xI p1 ->
      _Positive_as_OT__sqrtrem_step (\x -> BinNums.Coq_xI x) (\x ->
        BinNums.Coq_xI x) (_Positive_as_OT__sqrtrem p1);
     BinNums.Coq_xO p1 ->
      _Positive_as_OT__sqrtrem_step (\x -> BinNums.Coq_xO x) (\x ->
        BinNums.Coq_xI x) (_Positive_as_OT__sqrtrem p1);
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
      (Positive_as_OT__IsPos (BinNums.Coq_xO BinNums.Coq_xH))};
   BinNums.Coq_xO p0 ->
    case p0 of {
     BinNums.Coq_xI p1 ->
      _Positive_as_OT__sqrtrem_step (\x -> BinNums.Coq_xI x) (\x ->
        BinNums.Coq_xO x) (_Positive_as_OT__sqrtrem p1);
     BinNums.Coq_xO p1 ->
      _Positive_as_OT__sqrtrem_step (\x -> BinNums.Coq_xO x) (\x ->
        BinNums.Coq_xO x) (_Positive_as_OT__sqrtrem p1);
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
      (Positive_as_OT__IsPos BinNums.Coq_xH)};
   BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH Positive_as_OT__IsNul}

_Positive_as_OT__sqrt :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__sqrt p =
  Datatypes.fst (_Positive_as_OT__sqrtrem p)

_Positive_as_OT__gcdn :: Datatypes.Coq_nat -> BinNums.Coq_positive ->
                         BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_OT__gcdn n a b =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S n0 ->
    case a of {
     BinNums.Coq_xI a' ->
      case b of {
       BinNums.Coq_xI b' ->
        case _Positive_as_OT__compare a' b' of {
         Datatypes.Eq -> a;
         Datatypes.Lt ->
          _Positive_as_OT__gcdn n0 (_Positive_as_OT__sub b' a') a;
         Datatypes.Gt ->
          _Positive_as_OT__gcdn n0 (_Positive_as_OT__sub a' b') b};
       BinNums.Coq_xO b0 -> _Positive_as_OT__gcdn n0 a b0;
       BinNums.Coq_xH -> BinNums.Coq_xH};
     BinNums.Coq_xO a0 ->
      case b of {
       BinNums.Coq_xI p -> _Positive_as_OT__gcdn n0 a0 b;
       BinNums.Coq_xO b0 -> BinNums.Coq_xO (_Positive_as_OT__gcdn n0 a0 b0);
       BinNums.Coq_xH -> BinNums.Coq_xH};
     BinNums.Coq_xH -> BinNums.Coq_xH}}

_Positive_as_OT__gcd :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__gcd a b =
  _Positive_as_OT__gcdn
    (Peano.plus (_Positive_as_OT__size_nat a) (_Positive_as_OT__size_nat b))
    a b

_Positive_as_OT__ggcdn :: Datatypes.Coq_nat -> BinNums.Coq_positive ->
                          BinNums.Coq_positive -> Datatypes.Coq_prod
                          BinNums.Coq_positive
                          (Datatypes.Coq_prod BinNums.Coq_positive
                          BinNums.Coq_positive)
_Positive_as_OT__ggcdn n a b =
  case n of {
   Datatypes.O -> Datatypes.Coq_pair BinNums.Coq_xH (Datatypes.Coq_pair a b);
   Datatypes.S n0 ->
    case a of {
     BinNums.Coq_xI a' ->
      case b of {
       BinNums.Coq_xI b' ->
        case _Positive_as_OT__compare a' b' of {
         Datatypes.Eq -> Datatypes.Coq_pair a (Datatypes.Coq_pair
          BinNums.Coq_xH BinNums.Coq_xH);
         Datatypes.Lt ->
          case _Positive_as_OT__ggcdn n0 (_Positive_as_OT__sub b' a') a of {
           Datatypes.Coq_pair g p ->
            case p of {
             Datatypes.Coq_pair ba aa -> Datatypes.Coq_pair g
              (Datatypes.Coq_pair aa
              (_Positive_as_OT__add aa (BinNums.Coq_xO ba)))}};
         Datatypes.Gt ->
          case _Positive_as_OT__ggcdn n0 (_Positive_as_OT__sub a' b') b of {
           Datatypes.Coq_pair g p ->
            case p of {
             Datatypes.Coq_pair ab bb -> Datatypes.Coq_pair g
              (Datatypes.Coq_pair
              (_Positive_as_OT__add bb (BinNums.Coq_xO ab)) bb)}}};
       BinNums.Coq_xO b0 ->
        case _Positive_as_OT__ggcdn n0 a b0 of {
         Datatypes.Coq_pair g p ->
          case p of {
           Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair g
            (Datatypes.Coq_pair aa (BinNums.Coq_xO bb))}};
       BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
        (Datatypes.Coq_pair a BinNums.Coq_xH)};
     BinNums.Coq_xO a0 ->
      case b of {
       BinNums.Coq_xI p ->
        case _Positive_as_OT__ggcdn n0 a0 b of {
         Datatypes.Coq_pair g p0 ->
          case p0 of {
           Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair g
            (Datatypes.Coq_pair (BinNums.Coq_xO aa) bb)}};
       BinNums.Coq_xO b0 ->
        case _Positive_as_OT__ggcdn n0 a0 b0 of {
         Datatypes.Coq_pair g p -> Datatypes.Coq_pair (BinNums.Coq_xO g) p};
       BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
        (Datatypes.Coq_pair a BinNums.Coq_xH)};
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH (Datatypes.Coq_pair
      BinNums.Coq_xH b)}}

_Positive_as_OT__ggcd :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                         Datatypes.Coq_prod BinNums.Coq_positive
                         (Datatypes.Coq_prod BinNums.Coq_positive
                         BinNums.Coq_positive)
_Positive_as_OT__ggcd a b =
  _Positive_as_OT__ggcdn
    (Peano.plus (_Positive_as_OT__size_nat a) (_Positive_as_OT__size_nat b))
    a b

_Positive_as_OT__coq_Nsucc_double :: BinNums.N -> BinNums.N
_Positive_as_OT__coq_Nsucc_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xI p)}

_Positive_as_OT__coq_Ndouble :: BinNums.N -> BinNums.N
_Positive_as_OT__coq_Ndouble n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xO p)}

_Positive_as_OT__lor :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_OT__lor p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Coq_xI (_Positive_as_OT__lor p0 q0);
     BinNums.Coq_xO q0 -> BinNums.Coq_xI (_Positive_as_OT__lor p0 q0);
     BinNums.Coq_xH -> p};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Coq_xI (_Positive_as_OT__lor p0 q0);
     BinNums.Coq_xO q0 -> BinNums.Coq_xO (_Positive_as_OT__lor p0 q0);
     BinNums.Coq_xH -> BinNums.Coq_xI p0};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.Coq_xI q0;
     _ -> q}}

_Positive_as_OT__land :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                         BinNums.N
_Positive_as_OT__land p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_OT__coq_Nsucc_double (_Positive_as_OT__land p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__land p0 q0);
     BinNums.Coq_xH -> BinNums.Npos BinNums.Coq_xH};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__land p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__land p0 q0);
     BinNums.Coq_xH -> BinNums.N0};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.N0;
     _ -> BinNums.Npos BinNums.Coq_xH}}

_Positive_as_OT__ldiff :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                          BinNums.N
_Positive_as_OT__ldiff p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__ldiff p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_OT__coq_Nsucc_double (_Positive_as_OT__ldiff p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xO p0)};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__ldiff p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__ldiff p0 q0);
     BinNums.Coq_xH -> BinNums.Npos p};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.Npos BinNums.Coq_xH;
     _ -> BinNums.N0}}

_Positive_as_OT__lxor :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                         BinNums.N
_Positive_as_OT__lxor p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__lxor p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_OT__coq_Nsucc_double (_Positive_as_OT__lxor p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xO p0)};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_OT__coq_Nsucc_double (_Positive_as_OT__lxor p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_OT__coq_Ndouble (_Positive_as_OT__lxor p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xI p0)};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Npos (BinNums.Coq_xO q0);
     BinNums.Coq_xO q0 -> BinNums.Npos (BinNums.Coq_xI q0);
     BinNums.Coq_xH -> BinNums.N0}}

_Positive_as_OT__shiftl_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                               BinNums.Coq_positive
_Positive_as_OT__shiftl_nat p n =
  Peano.nat_iter n (\x -> BinNums.Coq_xO x) p

_Positive_as_OT__shiftr_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                               BinNums.Coq_positive
_Positive_as_OT__shiftr_nat p n =
  Peano.nat_iter n _Positive_as_OT__div2 p

_Positive_as_OT__shiftl :: BinNums.Coq_positive -> BinNums.N ->
                           BinNums.Coq_positive
_Positive_as_OT__shiftl p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Positive_as_OT__iter n0 (\x -> BinNums.Coq_xO x) p}

_Positive_as_OT__shiftr :: BinNums.Coq_positive -> BinNums.N ->
                           BinNums.Coq_positive
_Positive_as_OT__shiftr p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Positive_as_OT__iter n0 _Positive_as_OT__div2 p}

_Positive_as_OT__testbit_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                                Datatypes.Coq_bool
_Positive_as_OT__testbit_nat p n =
  case p of {
   BinNums.Coq_xI p0 ->
    case n of {
     Datatypes.O -> Datatypes.Coq_true;
     Datatypes.S n' -> _Positive_as_OT__testbit_nat p0 n'};
   BinNums.Coq_xO p0 ->
    case n of {
     Datatypes.O -> Datatypes.Coq_false;
     Datatypes.S n' -> _Positive_as_OT__testbit_nat p0 n'};
   BinNums.Coq_xH ->
    case n of {
     Datatypes.O -> Datatypes.Coq_true;
     Datatypes.S n0 -> Datatypes.Coq_false}}

_Positive_as_OT__testbit :: BinNums.Coq_positive -> BinNums.N ->
                            Datatypes.Coq_bool
_Positive_as_OT__testbit p n =
  case p of {
   BinNums.Coq_xI p0 ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos n0 ->
      _Positive_as_OT__testbit p0 (_Positive_as_OT__pred_N n0)};
   BinNums.Coq_xO p0 ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_false;
     BinNums.Npos n0 ->
      _Positive_as_OT__testbit p0 (_Positive_as_OT__pred_N n0)};
   BinNums.Coq_xH ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos p0 -> Datatypes.Coq_false}}

_Positive_as_OT__iter_op :: (a1 -> a1 -> a1) -> BinNums.Coq_positive -> a1 ->
                            a1
_Positive_as_OT__iter_op op p a =
  case p of {
   BinNums.Coq_xI p0 -> op a (_Positive_as_OT__iter_op op p0 (op a a));
   BinNums.Coq_xO p0 -> _Positive_as_OT__iter_op op p0 (op a a);
   BinNums.Coq_xH -> a}

_Positive_as_OT__to_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat
_Positive_as_OT__to_nat x =
  _Positive_as_OT__iter_op Peano.plus x (Datatypes.S Datatypes.O)

_Positive_as_OT__of_nat :: Datatypes.Coq_nat -> BinNums.Coq_positive
_Positive_as_OT__of_nat n =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S x ->
    case x of {
     Datatypes.O -> BinNums.Coq_xH;
     Datatypes.S n0 -> _Positive_as_OT__succ (_Positive_as_OT__of_nat x)}}

_Positive_as_OT__of_succ_nat :: Datatypes.Coq_nat -> BinNums.Coq_positive
_Positive_as_OT__of_succ_nat n =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S x -> _Positive_as_OT__succ (_Positive_as_OT__of_succ_nat x)}

_Positive_as_OT__eq_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                           Specif.Coq_sumbool
_Positive_as_OT__eq_dec x y =
  BinNums.positive_rec (\p h y0 ->
    case y0 of {
     BinNums.Coq_xI p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (h p0);
     _ -> Specif.Coq_right}) (\p h y0 ->
    case y0 of {
     BinNums.Coq_xO p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (h p0);
     _ -> Specif.Coq_right}) (\y0 ->
    case y0 of {
     BinNums.Coq_xH -> Specif.Coq_left;
     _ -> Specif.Coq_right}) x y

_Positive_as_OT__peano_rect :: a1 -> (BinNums.Coq_positive -> a1 -> a1) ->
                               BinNums.Coq_positive -> a1
_Positive_as_OT__peano_rect a f p =
  let {
   f2 = _Positive_as_OT__peano_rect (f BinNums.Coq_xH a) (\p0 x ->
          f (_Positive_as_OT__succ (BinNums.Coq_xO p0))
            (f (BinNums.Coq_xO p0) x))}
  in
  case p of {
   BinNums.Coq_xI q -> f (BinNums.Coq_xO q) (f2 q);
   BinNums.Coq_xO q -> f2 q;
   BinNums.Coq_xH -> a}

_Positive_as_OT__peano_rec :: a1 -> (BinNums.Coq_positive -> a1 -> a1) ->
                              BinNums.Coq_positive -> a1
_Positive_as_OT__peano_rec =
  _Positive_as_OT__peano_rect

data Positive_as_OT__PeanoView =
   Positive_as_OT__PeanoOne
 | Positive_as_OT__PeanoSucc BinNums.Coq_positive Positive_as_OT__PeanoView

_Positive_as_OT__coq_PeanoView_rect :: a1 -> (BinNums.Coq_positive ->
                                       Positive_as_OT__PeanoView -> a1 -> a1)
                                       -> BinNums.Coq_positive ->
                                       Positive_as_OT__PeanoView -> a1
_Positive_as_OT__coq_PeanoView_rect f f0 p p0 =
  case p0 of {
   Positive_as_OT__PeanoOne -> f;
   Positive_as_OT__PeanoSucc p1 p2 ->
    unsafeCoerce f0 p1 p2
      (unsafeCoerce (_Positive_as_OT__coq_PeanoView_rect f f0) p1 p2)}

_Positive_as_OT__coq_PeanoView_rec :: a1 -> (BinNums.Coq_positive ->
                                      Positive_as_OT__PeanoView -> a1 -> a1)
                                      -> BinNums.Coq_positive ->
                                      Positive_as_OT__PeanoView -> a1
_Positive_as_OT__coq_PeanoView_rec =
  _Positive_as_OT__coq_PeanoView_rect

_Positive_as_OT__peanoView_xO :: BinNums.Coq_positive ->
                                 Positive_as_OT__PeanoView ->
                                 Positive_as_OT__PeanoView
_Positive_as_OT__peanoView_xO p q =
  case q of {
   Positive_as_OT__PeanoOne -> Positive_as_OT__PeanoSucc BinNums.Coq_xH
    (unsafeCoerce Positive_as_OT__PeanoOne);
   Positive_as_OT__PeanoSucc p0 q0 -> Positive_as_OT__PeanoSucc
    (_Positive_as_OT__succ (BinNums.Coq_xO p0))
    (unsafeCoerce (Positive_as_OT__PeanoSucc (BinNums.Coq_xO p0)
      (unsafeCoerce (_Positive_as_OT__peanoView_xO p0 (unsafeCoerce q0)))))}

_Positive_as_OT__peanoView_xI :: BinNums.Coq_positive ->
                                 Positive_as_OT__PeanoView ->
                                 Positive_as_OT__PeanoView
_Positive_as_OT__peanoView_xI p q =
  case q of {
   Positive_as_OT__PeanoOne -> Positive_as_OT__PeanoSucc
    (_Positive_as_OT__succ BinNums.Coq_xH)
    (unsafeCoerce (Positive_as_OT__PeanoSucc BinNums.Coq_xH
      (unsafeCoerce Positive_as_OT__PeanoOne)));
   Positive_as_OT__PeanoSucc p0 q0 -> Positive_as_OT__PeanoSucc
    (_Positive_as_OT__succ (BinNums.Coq_xI p0))
    (unsafeCoerce (Positive_as_OT__PeanoSucc (BinNums.Coq_xI p0)
      (unsafeCoerce (_Positive_as_OT__peanoView_xI p0 (unsafeCoerce q0)))))}

_Positive_as_OT__peanoView :: BinNums.Coq_positive ->
                              Positive_as_OT__PeanoView
_Positive_as_OT__peanoView p =
  case p of {
   BinNums.Coq_xI p0 ->
    _Positive_as_OT__peanoView_xI p0 (_Positive_as_OT__peanoView p0);
   BinNums.Coq_xO p0 ->
    _Positive_as_OT__peanoView_xO p0 (_Positive_as_OT__peanoView p0);
   BinNums.Coq_xH -> Positive_as_OT__PeanoOne}

_Positive_as_OT__coq_PeanoView_iter :: a1 -> (BinNums.Coq_positive -> a1 ->
                                       a1) -> BinNums.Coq_positive ->
                                       Positive_as_OT__PeanoView -> a1
_Positive_as_OT__coq_PeanoView_iter a f p q =
  case q of {
   Positive_as_OT__PeanoOne -> a;
   Positive_as_OT__PeanoSucc p0 q0 ->
    f p0 (unsafeCoerce (_Positive_as_OT__coq_PeanoView_iter a f) p0 q0)}

_Positive_as_OT__eqb_spec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             Bool.Coq_reflect
_Positive_as_OT__eqb_spec x y =
  Bool.iff_reflect (_Positive_as_OT__eqb x y)

_Positive_as_OT__switch_Eq :: Datatypes.Coq_comparison ->
                              Datatypes.Coq_comparison ->
                              Datatypes.Coq_comparison
_Positive_as_OT__switch_Eq c c' =
  case c' of {
   Datatypes.Eq -> c;
   x -> x}

_Positive_as_OT__mask2cmp :: Positive_as_OT__Coq_mask ->
                             Datatypes.Coq_comparison
_Positive_as_OT__mask2cmp p =
  case p of {
   Positive_as_OT__IsNul -> Datatypes.Eq;
   Positive_as_OT__IsPos p0 -> Datatypes.Gt;
   Positive_as_OT__IsNeg -> Datatypes.Lt}

_Positive_as_OT__leb_spec0 :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              Bool.Coq_reflect
_Positive_as_OT__leb_spec0 x y =
  Bool.iff_reflect (_Positive_as_OT__leb x y)

_Positive_as_OT__ltb_spec0 :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              Bool.Coq_reflect
_Positive_as_OT__ltb_spec0 x y =
  Bool.iff_reflect (_Positive_as_OT__ltb x y)

_Positive_as_OT__Private_Dec__max_case_strong :: BinNums.Coq_positive ->
                                                 BinNums.Coq_positive ->
                                                 (BinNums.Coq_positive ->
                                                 BinNums.Coq_positive -> ()
                                                 -> a1 -> a1) -> (() -> a1)
                                                 -> (() -> a1) -> a1
_Positive_as_OT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinPos._Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (BinPos._Pos__max n m) __ (hl __);
   _ -> compat m (BinPos._Pos__max n m) __ (hr __)}

_Positive_as_OT__Private_Dec__max_case :: BinNums.Coq_positive ->
                                          BinNums.Coq_positive ->
                                          (BinNums.Coq_positive ->
                                          BinNums.Coq_positive -> () -> a1 ->
                                          a1) -> a1 -> a1 -> a1
_Positive_as_OT__Private_Dec__max_case n m x x0 x1 =
  _Positive_as_OT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Positive_as_OT__Private_Dec__max_dec :: BinNums.Coq_positive ->
                                         BinNums.Coq_positive ->
                                         Specif.Coq_sumbool
_Positive_as_OT__Private_Dec__max_dec n m =
  _Positive_as_OT__Private_Dec__max_case n m (\x y _ h0 -> h0)
    Specif.Coq_left Specif.Coq_right

_Positive_as_OT__Private_Dec__min_case_strong :: BinNums.Coq_positive ->
                                                 BinNums.Coq_positive ->
                                                 (BinNums.Coq_positive ->
                                                 BinNums.Coq_positive -> ()
                                                 -> a1 -> a1) -> (() -> a1)
                                                 -> (() -> a1) -> a1
_Positive_as_OT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinPos._Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (BinPos._Pos__min n m) __ (hr __);
   _ -> compat n (BinPos._Pos__min n m) __ (hl __)}

_Positive_as_OT__Private_Dec__min_case :: BinNums.Coq_positive ->
                                          BinNums.Coq_positive ->
                                          (BinNums.Coq_positive ->
                                          BinNums.Coq_positive -> () -> a1 ->
                                          a1) -> a1 -> a1 -> a1
_Positive_as_OT__Private_Dec__min_case n m x x0 x1 =
  _Positive_as_OT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Positive_as_OT__Private_Dec__min_dec :: BinNums.Coq_positive ->
                                         BinNums.Coq_positive ->
                                         Specif.Coq_sumbool
_Positive_as_OT__Private_Dec__min_dec n m =
  _Positive_as_OT__Private_Dec__min_case n m (\x y _ h0 -> h0)
    Specif.Coq_left Specif.Coq_right

_Positive_as_OT__max_case_strong :: BinNums.Coq_positive ->
                                    BinNums.Coq_positive -> (() -> a1) -> (()
                                    -> a1) -> a1
_Positive_as_OT__max_case_strong n m x x0 =
  _Positive_as_OT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Positive_as_OT__max_case :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             a1 -> a1 -> a1
_Positive_as_OT__max_case n m x x0 =
  _Positive_as_OT__max_case_strong n m (\_ -> x) (\_ -> x0)

_Positive_as_OT__max_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                            Specif.Coq_sumbool
_Positive_as_OT__max_dec =
  _Positive_as_OT__Private_Dec__max_dec

_Positive_as_OT__min_case_strong :: BinNums.Coq_positive ->
                                    BinNums.Coq_positive -> (() -> a1) -> (()
                                    -> a1) -> a1
_Positive_as_OT__min_case_strong n m x x0 =
  _Positive_as_OT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Positive_as_OT__min_case :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             a1 -> a1 -> a1
_Positive_as_OT__min_case n m x x0 =
  _Positive_as_OT__min_case_strong n m (\_ -> x) (\_ -> x0)

_Positive_as_OT__min_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                            Specif.Coq_sumbool
_Positive_as_OT__min_dec =
  _Positive_as_OT__Private_Dec__min_dec

type N_as_OT__Coq_t = BinNums.N

_N_as_OT__zero :: BinNums.N
_N_as_OT__zero =
  BinNums.N0

_N_as_OT__one :: BinNums.N
_N_as_OT__one =
  BinNums.Npos BinNums.Coq_xH

_N_as_OT__two :: BinNums.N
_N_as_OT__two =
  BinNums.Npos (BinNums.Coq_xO BinNums.Coq_xH)

_N_as_OT__succ_double :: BinNums.N -> BinNums.N
_N_as_OT__succ_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xI p)}

_N_as_OT__double :: BinNums.N -> BinNums.N
_N_as_OT__double n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xO p)}

_N_as_OT__succ :: BinNums.N -> BinNums.N
_N_as_OT__succ n =
  case n of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__succ p)}

_N_as_OT__pred :: BinNums.N -> BinNums.N
_N_as_OT__pred n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinPos._Pos__pred_N p}

_N_as_OT__succ_pos :: BinNums.N -> BinNums.Coq_positive
_N_as_OT__succ_pos n =
  case n of {
   BinNums.N0 -> BinNums.Coq_xH;
   BinNums.Npos p -> BinPos._Pos__succ p}

_N_as_OT__add :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__add n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__add p q)}}

_N_as_OT__sub :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__sub n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos m' ->
      case BinPos._Pos__sub_mask n' m' of {
       BinPos.Pos__IsPos p -> BinNums.Npos p;
       _ -> BinNums.N0}}}

_N_as_OT__mul :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__mul n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__mul p q)}}

_N_as_OT__compare :: BinNums.N -> BinNums.N -> Datatypes.Coq_comparison
_N_as_OT__compare n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Datatypes.Eq;
     BinNums.Npos m' -> Datatypes.Lt};
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> Datatypes.Gt;
     BinNums.Npos m' -> BinPos._Pos__compare n' m'}}

_N_as_OT__eqb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_OT__eqb n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos p -> Datatypes.Coq_false};
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> Datatypes.Coq_false;
     BinNums.Npos q -> BinPos._Pos__eqb p q}}

_N_as_OT__leb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_OT__leb x y =
  case _N_as_OT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_N_as_OT__ltb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_OT__ltb x y =
  case _N_as_OT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_N_as_OT__min :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__min n n' =
  case _N_as_OT__compare n n' of {
   Datatypes.Gt -> n';
   _ -> n}

_N_as_OT__max :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__max n n' =
  case _N_as_OT__compare n n' of {
   Datatypes.Gt -> n;
   _ -> n'}

_N_as_OT__div2 :: BinNums.N -> BinNums.N
_N_as_OT__div2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Npos p;
     BinNums.Coq_xO p -> BinNums.Npos p;
     BinNums.Coq_xH -> BinNums.N0}}

_N_as_OT__even :: BinNums.N -> Datatypes.Coq_bool
_N_as_OT__even n =
  case n of {
   BinNums.N0 -> Datatypes.Coq_true;
   BinNums.Npos p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_N_as_OT__odd :: BinNums.N -> Datatypes.Coq_bool
_N_as_OT__odd n =
  Datatypes.negb (_N_as_OT__even n)

_N_as_OT__pow :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__pow n p =
  case p of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p0 ->
    case n of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__pow q p0)}}

_N_as_OT__square :: BinNums.N -> BinNums.N
_N_as_OT__square n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__square p)}

_N_as_OT__log2 :: BinNums.N -> BinNums.N
_N_as_OT__log2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Npos (BinPos._Pos__size p);
     BinNums.Coq_xO p -> BinNums.Npos (BinPos._Pos__size p);
     BinNums.Coq_xH -> BinNums.N0}}

_N_as_OT__size :: BinNums.N -> BinNums.N
_N_as_OT__size n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__size p)}

_N_as_OT__size_nat :: BinNums.N -> Datatypes.Coq_nat
_N_as_OT__size_nat n =
  case n of {
   BinNums.N0 -> Datatypes.O;
   BinNums.Npos p -> BinPos._Pos__size_nat p}

_N_as_OT__pos_div_eucl :: BinNums.Coq_positive -> BinNums.N ->
                          Datatypes.Coq_prod BinNums.N BinNums.N
_N_as_OT__pos_div_eucl a b =
  case a of {
   BinNums.Coq_xI a' ->
    case _N_as_OT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {r' = _N_as_OT__succ_double r} in
      case _N_as_OT__leb b r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (_N_as_OT__succ_double q)
        (_N_as_OT__sub r' b);
       Datatypes.Coq_false -> Datatypes.Coq_pair (_N_as_OT__double q) r'}};
   BinNums.Coq_xO a' ->
    case _N_as_OT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {r' = _N_as_OT__double r} in
      case _N_as_OT__leb b r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (_N_as_OT__succ_double q)
        (_N_as_OT__sub r' b);
       Datatypes.Coq_false -> Datatypes.Coq_pair (_N_as_OT__double q) r'}};
   BinNums.Coq_xH ->
    case b of {
     BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 (BinNums.Npos
      BinNums.Coq_xH);
     BinNums.Npos p ->
      case p of {
       BinNums.Coq_xH -> Datatypes.Coq_pair (BinNums.Npos BinNums.Coq_xH)
        BinNums.N0;
       _ -> Datatypes.Coq_pair BinNums.N0 (BinNums.Npos BinNums.Coq_xH)}}}

_N_as_OT__div_eucl :: BinNums.N -> BinNums.N -> Datatypes.Coq_prod BinNums.N
                      BinNums.N
_N_as_OT__div_eucl a b =
  case a of {
   BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 BinNums.N0;
   BinNums.Npos na ->
    case b of {
     BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 a;
     BinNums.Npos p -> _N_as_OT__pos_div_eucl na b}}

_N_as_OT__div :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__div a b =
  Datatypes.fst (_N_as_OT__div_eucl a b)

_N_as_OT__modulo :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__modulo a b =
  Datatypes.snd (_N_as_OT__div_eucl a b)

_N_as_OT__gcd :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__gcd a b =
  case a of {
   BinNums.N0 -> b;
   BinNums.Npos p ->
    case b of {
     BinNums.N0 -> a;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__gcd p q)}}

_N_as_OT__ggcd :: BinNums.N -> BinNums.N -> Datatypes.Coq_prod BinNums.N
                  (Datatypes.Coq_prod BinNums.N BinNums.N)
_N_as_OT__ggcd a b =
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

_N_as_OT__sqrtrem :: BinNums.N -> Datatypes.Coq_prod BinNums.N BinNums.N
_N_as_OT__sqrtrem n =
  case n of {
   BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 BinNums.N0;
   BinNums.Npos p ->
    case BinPos._Pos__sqrtrem p of {
     Datatypes.Coq_pair s m ->
      case m of {
       BinPos.Pos__IsPos r -> Datatypes.Coq_pair (BinNums.Npos s)
        (BinNums.Npos r);
       _ -> Datatypes.Coq_pair (BinNums.Npos s) BinNums.N0}}}

_N_as_OT__sqrt :: BinNums.N -> BinNums.N
_N_as_OT__sqrt n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__sqrt p)}

_N_as_OT__lor :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__lor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__lor p q)}}

_N_as_OT__land :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__land n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinPos._Pos__land p q}}

_N_as_OT__ldiff :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__ldiff n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__ldiff p q}}

_N_as_OT__lxor :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__lxor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__lxor p q}}

_N_as_OT__shiftl_nat :: BinNums.N -> Datatypes.Coq_nat -> BinNums.N
_N_as_OT__shiftl_nat a n =
  Peano.nat_iter n _N_as_OT__double a

_N_as_OT__shiftr_nat :: BinNums.N -> Datatypes.Coq_nat -> BinNums.N
_N_as_OT__shiftr_nat a n =
  Peano.nat_iter n _N_as_OT__div2 a

_N_as_OT__shiftl :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__shiftl a n =
  case a of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos a0 -> BinNums.Npos (BinPos._Pos__shiftl a0 n)}

_N_as_OT__shiftr :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__shiftr a n =
  case n of {
   BinNums.N0 -> a;
   BinNums.Npos p -> BinPos._Pos__iter p _N_as_OT__div2 a}

_N_as_OT__testbit_nat :: BinNums.N -> Datatypes.Coq_nat -> Datatypes.Coq_bool
_N_as_OT__testbit_nat a =
  case a of {
   BinNums.N0 -> (\x -> Datatypes.Coq_false);
   BinNums.Npos p -> BinPos._Pos__testbit_nat p}

_N_as_OT__testbit :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_OT__testbit a n =
  case a of {
   BinNums.N0 -> Datatypes.Coq_false;
   BinNums.Npos p -> BinPos._Pos__testbit p n}

_N_as_OT__to_nat :: BinNums.N -> Datatypes.Coq_nat
_N_as_OT__to_nat a =
  case a of {
   BinNums.N0 -> Datatypes.O;
   BinNums.Npos p -> BinPos._Pos__to_nat p}

_N_as_OT__of_nat :: Datatypes.Coq_nat -> BinNums.N
_N_as_OT__of_nat n =
  case n of {
   Datatypes.O -> BinNums.N0;
   Datatypes.S n' -> BinNums.Npos (BinPos._Pos__of_succ_nat n')}

_N_as_OT__iter :: BinNums.N -> (a1 -> a1) -> a1 -> a1
_N_as_OT__iter n f x =
  case n of {
   BinNums.N0 -> x;
   BinNums.Npos p -> BinPos._Pos__iter p f x}

_N_as_OT__eq_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N_as_OT__eq_dec n m =
  BinNums.coq_N_rec (\m0 ->
    case m0 of {
     BinNums.N0 -> Specif.Coq_left;
     BinNums.Npos p -> Specif.Coq_right}) (\p m0 ->
    case m0 of {
     BinNums.N0 -> Specif.Coq_right;
     BinNums.Npos p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (BinPos._Pos__eq_dec p p0)}) n m

_N_as_OT__discr :: BinNums.N -> Specif.Coq_sumor BinNums.Coq_positive
_N_as_OT__discr n =
  case n of {
   BinNums.N0 -> Specif.Coq_inright;
   BinNums.Npos p -> Specif.Coq_inleft p}

_N_as_OT__binary_rect :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 ->
                         a1) -> BinNums.N -> a1
_N_as_OT__binary_rect f0 f2 fS2 n =
  let {f2' = \p -> f2 (BinNums.Npos p)} in
  let {fS2' = \p -> fS2 (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinNums.positive_rect fS2' f2' (fS2 BinNums.N0 f0) p}

_N_as_OT__binary_rec :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 ->
                        a1) -> BinNums.N -> a1
_N_as_OT__binary_rec =
  _N_as_OT__binary_rect

_N_as_OT__peano_rect :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N_as_OT__peano_rect f0 f n =
  let {f' = \p -> f (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinPos._Pos__peano_rect (f BinNums.N0 f0) f' p}

_N_as_OT__peano_rec :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N_as_OT__peano_rec =
  _N_as_OT__peano_rect

_N_as_OT__leb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N_as_OT__leb_spec0 x y =
  Bool.iff_reflect (_N_as_OT__leb x y)

_N_as_OT__ltb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N_as_OT__ltb_spec0 x y =
  Bool.iff_reflect (_N_as_OT__ltb x y)

_N_as_OT__recursion :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N_as_OT__recursion =
  _N_as_OT__peano_rect

_N_as_OT__sqrt_up :: BinNums.N -> BinNums.N
_N_as_OT__sqrt_up a =
  case _N_as_OT__compare _N_as_OT__zero a of {
   Datatypes.Lt -> _N_as_OT__succ (_N_as_OT__sqrt (_N_as_OT__pred a));
   _ -> _N_as_OT__zero}

_N_as_OT__log2_up :: BinNums.N -> BinNums.N
_N_as_OT__log2_up a =
  case _N_as_OT__compare _N_as_OT__one a of {
   Datatypes.Lt -> _N_as_OT__succ (_N_as_OT__log2 (_N_as_OT__pred a));
   _ -> _N_as_OT__zero}

_N_as_OT__lcm :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__lcm a b =
  _N_as_OT__mul a (_N_as_OT__div b (_N_as_OT__gcd a b))

_N_as_OT__eqb_spec :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N_as_OT__eqb_spec x y =
  Bool.iff_reflect (_N_as_OT__eqb x y)

_N_as_OT__b2n :: Datatypes.Coq_bool -> BinNums.N
_N_as_OT__b2n b =
  case b of {
   Datatypes.Coq_true -> _N_as_OT__one;
   Datatypes.Coq_false -> _N_as_OT__zero}

_N_as_OT__setbit :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__setbit a n =
  _N_as_OT__lor a (_N_as_OT__shiftl _N_as_OT__one n)

_N_as_OT__clearbit :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__clearbit a n =
  _N_as_OT__ldiff a (_N_as_OT__shiftl _N_as_OT__one n)

_N_as_OT__ones :: BinNums.N -> BinNums.N
_N_as_OT__ones n =
  _N_as_OT__pred (_N_as_OT__shiftl _N_as_OT__one n)

_N_as_OT__lnot :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_OT__lnot a n =
  _N_as_OT__lxor a (_N_as_OT__ones n)

_N_as_OT__Private_Dec__max_case_strong :: BinNums.N -> BinNums.N ->
                                          (BinNums.N -> BinNums.N -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_N_as_OT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinNat._N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (BinNat._N__max n m) __ (hl __);
   _ -> compat m (BinNat._N__max n m) __ (hr __)}

_N_as_OT__Private_Dec__max_case :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                   BinNums.N -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_N_as_OT__Private_Dec__max_case n m x x0 x1 =
  _N_as_OT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_N_as_OT__Private_Dec__max_dec :: BinNums.N -> BinNums.N ->
                                  Specif.Coq_sumbool
_N_as_OT__Private_Dec__max_dec n m =
  _N_as_OT__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_N_as_OT__Private_Dec__min_case_strong :: BinNums.N -> BinNums.N ->
                                          (BinNums.N -> BinNums.N -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_N_as_OT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinNat._N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (BinNat._N__min n m) __ (hr __);
   _ -> compat n (BinNat._N__min n m) __ (hl __)}

_N_as_OT__Private_Dec__min_case :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                   BinNums.N -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_N_as_OT__Private_Dec__min_case n m x x0 x1 =
  _N_as_OT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_N_as_OT__Private_Dec__min_dec :: BinNums.N -> BinNums.N ->
                                  Specif.Coq_sumbool
_N_as_OT__Private_Dec__min_dec n m =
  _N_as_OT__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_N_as_OT__max_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() ->
                             a1) -> a1
_N_as_OT__max_case_strong n m x x0 =
  _N_as_OT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_N_as_OT__max_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N_as_OT__max_case n m x x0 =
  _N_as_OT__max_case_strong n m (\_ -> x) (\_ -> x0)

_N_as_OT__max_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N_as_OT__max_dec =
  _N_as_OT__Private_Dec__max_dec

_N_as_OT__min_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() ->
                             a1) -> a1
_N_as_OT__min_case_strong n m x x0 =
  _N_as_OT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_N_as_OT__min_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N_as_OT__min_case n m x x0 =
  _N_as_OT__min_case_strong n m (\_ -> x) (\_ -> x0)

_N_as_OT__min_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N_as_OT__min_dec =
  _N_as_OT__Private_Dec__min_dec

type Z_as_OT__Coq_t = BinNums.Z

_Z_as_OT__zero :: BinNums.Z
_Z_as_OT__zero =
  BinNums.Z0

_Z_as_OT__one :: BinNums.Z
_Z_as_OT__one =
  BinNums.Zpos BinNums.Coq_xH

_Z_as_OT__two :: BinNums.Z
_Z_as_OT__two =
  BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)

_Z_as_OT__double :: BinNums.Z -> BinNums.Z
_Z_as_OT__double x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos (BinNums.Coq_xO p);
   BinNums.Zneg p -> BinNums.Zneg (BinNums.Coq_xO p)}

_Z_as_OT__succ_double :: BinNums.Z -> BinNums.Z
_Z_as_OT__succ_double x =
  case x of {
   BinNums.Z0 -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zpos p -> BinNums.Zpos (BinNums.Coq_xI p);
   BinNums.Zneg p -> BinNums.Zneg (BinPos._Pos__pred_double p)}

_Z_as_OT__pred_double :: BinNums.Z -> BinNums.Z
_Z_as_OT__pred_double x =
  case x of {
   BinNums.Z0 -> BinNums.Zneg BinNums.Coq_xH;
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__pred_double p);
   BinNums.Zneg p -> BinNums.Zneg (BinNums.Coq_xI p)}

_Z_as_OT__pos_sub :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                     BinNums.Z
_Z_as_OT__pos_sub x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Z_as_OT__double (_Z_as_OT__pos_sub p q);
     BinNums.Coq_xO q -> _Z_as_OT__succ_double (_Z_as_OT__pos_sub p q);
     BinNums.Coq_xH -> BinNums.Zpos (BinNums.Coq_xO p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Z_as_OT__pred_double (_Z_as_OT__pos_sub p q);
     BinNums.Coq_xO q -> _Z_as_OT__double (_Z_as_OT__pos_sub p q);
     BinNums.Coq_xH -> BinNums.Zpos (BinPos._Pos__pred_double p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Zneg (BinNums.Coq_xO q);
     BinNums.Coq_xO q -> BinNums.Zneg (BinPos._Pos__pred_double q);
     BinNums.Coq_xH -> BinNums.Z0}}

_Z_as_OT__add :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__add x y =
  case x of {
   BinNums.Z0 -> y;
   BinNums.Zpos x' ->
    case y of {
     BinNums.Z0 -> x;
     BinNums.Zpos y' -> BinNums.Zpos (BinPos._Pos__add x' y');
     BinNums.Zneg y' -> _Z_as_OT__pos_sub x' y'};
   BinNums.Zneg x' ->
    case y of {
     BinNums.Z0 -> x;
     BinNums.Zpos y' -> _Z_as_OT__pos_sub y' x';
     BinNums.Zneg y' -> BinNums.Zneg (BinPos._Pos__add x' y')}}

_Z_as_OT__opp :: BinNums.Z -> BinNums.Z
_Z_as_OT__opp x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos x0 -> BinNums.Zneg x0;
   BinNums.Zneg x0 -> BinNums.Zpos x0}

_Z_as_OT__succ :: BinNums.Z -> BinNums.Z
_Z_as_OT__succ x =
  _Z_as_OT__add x (BinNums.Zpos BinNums.Coq_xH)

_Z_as_OT__pred :: BinNums.Z -> BinNums.Z
_Z_as_OT__pred x =
  _Z_as_OT__add x (BinNums.Zneg BinNums.Coq_xH)

_Z_as_OT__sub :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__sub m n =
  _Z_as_OT__add m (_Z_as_OT__opp n)

_Z_as_OT__mul :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__mul x y =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos x' ->
    case y of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos y' -> BinNums.Zpos (BinPos._Pos__mul x' y');
     BinNums.Zneg y' -> BinNums.Zneg (BinPos._Pos__mul x' y')};
   BinNums.Zneg x' ->
    case y of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos y' -> BinNums.Zneg (BinPos._Pos__mul x' y');
     BinNums.Zneg y' -> BinNums.Zpos (BinPos._Pos__mul x' y')}}

_Z_as_OT__pow_pos :: BinNums.Z -> BinNums.Coq_positive -> BinNums.Z
_Z_as_OT__pow_pos z n =
  BinPos._Pos__iter n (_Z_as_OT__mul z) (BinNums.Zpos BinNums.Coq_xH)

_Z_as_OT__pow :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__pow x y =
  case y of {
   BinNums.Z0 -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zpos p -> _Z_as_OT__pow_pos x p;
   BinNums.Zneg p -> BinNums.Z0}

_Z_as_OT__square :: BinNums.Z -> BinNums.Z
_Z_as_OT__square x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__square p);
   BinNums.Zneg p -> BinNums.Zpos (BinPos._Pos__square p)}

_Z_as_OT__compare :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_comparison
_Z_as_OT__compare x y =
  case x of {
   BinNums.Z0 ->
    case y of {
     BinNums.Z0 -> Datatypes.Eq;
     BinNums.Zpos y' -> Datatypes.Lt;
     BinNums.Zneg y' -> Datatypes.Gt};
   BinNums.Zpos x' ->
    case y of {
     BinNums.Zpos y' -> BinPos._Pos__compare x' y';
     _ -> Datatypes.Gt};
   BinNums.Zneg x' ->
    case y of {
     BinNums.Zneg y' -> Datatypes.coq_CompOpp (BinPos._Pos__compare x' y');
     _ -> Datatypes.Lt}}

_Z_as_OT__sgn :: BinNums.Z -> BinNums.Z
_Z_as_OT__sgn z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zneg p -> BinNums.Zneg BinNums.Coq_xH}

_Z_as_OT__leb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__leb x y =
  case _Z_as_OT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Z_as_OT__ltb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__ltb x y =
  case _Z_as_OT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Z_as_OT__geb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__geb x y =
  case _Z_as_OT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Z_as_OT__gtb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__gtb x y =
  case _Z_as_OT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Z_as_OT__eqb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__eqb x y =
  case x of {
   BinNums.Z0 ->
    case y of {
     BinNums.Z0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false};
   BinNums.Zpos p ->
    case y of {
     BinNums.Zpos q -> BinPos._Pos__eqb p q;
     _ -> Datatypes.Coq_false};
   BinNums.Zneg p ->
    case y of {
     BinNums.Zneg q -> BinPos._Pos__eqb p q;
     _ -> Datatypes.Coq_false}}

_Z_as_OT__max :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__max n m =
  case _Z_as_OT__compare n m of {
   Datatypes.Lt -> m;
   _ -> n}

_Z_as_OT__min :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__min n m =
  case _Z_as_OT__compare n m of {
   Datatypes.Gt -> m;
   _ -> n}

_Z_as_OT__abs :: BinNums.Z -> BinNums.Z
_Z_as_OT__abs z =
  case z of {
   BinNums.Zneg p -> BinNums.Zpos p;
   x -> x}

_Z_as_OT__abs_nat :: BinNums.Z -> Datatypes.Coq_nat
_Z_as_OT__abs_nat z =
  case z of {
   BinNums.Z0 -> Datatypes.O;
   BinNums.Zpos p -> BinPos._Pos__to_nat p;
   BinNums.Zneg p -> BinPos._Pos__to_nat p}

_Z_as_OT__abs_N :: BinNums.Z -> BinNums.N
_Z_as_OT__abs_N z =
  case z of {
   BinNums.Z0 -> BinNums.N0;
   BinNums.Zpos p -> BinNums.Npos p;
   BinNums.Zneg p -> BinNums.Npos p}

_Z_as_OT__to_nat :: BinNums.Z -> Datatypes.Coq_nat
_Z_as_OT__to_nat z =
  case z of {
   BinNums.Zpos p -> BinPos._Pos__to_nat p;
   _ -> Datatypes.O}

_Z_as_OT__to_N :: BinNums.Z -> BinNums.N
_Z_as_OT__to_N z =
  case z of {
   BinNums.Zpos p -> BinNums.Npos p;
   _ -> BinNums.N0}

_Z_as_OT__of_nat :: Datatypes.Coq_nat -> BinNums.Z
_Z_as_OT__of_nat n =
  case n of {
   Datatypes.O -> BinNums.Z0;
   Datatypes.S n0 -> BinNums.Zpos (BinPos._Pos__of_succ_nat n0)}

_Z_as_OT__of_N :: BinNums.N -> BinNums.Z
_Z_as_OT__of_N n =
  case n of {
   BinNums.N0 -> BinNums.Z0;
   BinNums.Npos p -> BinNums.Zpos p}

_Z_as_OT__to_pos :: BinNums.Z -> BinNums.Coq_positive
_Z_as_OT__to_pos z =
  case z of {
   BinNums.Zpos p -> p;
   _ -> BinNums.Coq_xH}

_Z_as_OT__iter :: BinNums.Z -> (a1 -> a1) -> a1 -> a1
_Z_as_OT__iter n f x =
  case n of {
   BinNums.Zpos p -> BinPos._Pos__iter p f x;
   _ -> x}

_Z_as_OT__pos_div_eucl :: BinNums.Coq_positive -> BinNums.Z ->
                          Datatypes.Coq_prod BinNums.Z BinNums.Z
_Z_as_OT__pos_div_eucl a b =
  case a of {
   BinNums.Coq_xI a' ->
    case _Z_as_OT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {
       r' = _Z_as_OT__add
              (_Z_as_OT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH))
                r) (BinNums.Zpos BinNums.Coq_xH)}
      in
      case _Z_as_OT__ltb r' b of {
       Datatypes.Coq_true -> Datatypes.Coq_pair
        (_Z_as_OT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q) r';
       Datatypes.Coq_false -> Datatypes.Coq_pair
        (_Z_as_OT__add
          (_Z_as_OT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q)
          (BinNums.Zpos BinNums.Coq_xH)) (_Z_as_OT__sub r' b)}};
   BinNums.Coq_xO a' ->
    case _Z_as_OT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {
       r' = _Z_as_OT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) r}
      in
      case _Z_as_OT__ltb r' b of {
       Datatypes.Coq_true -> Datatypes.Coq_pair
        (_Z_as_OT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q) r';
       Datatypes.Coq_false -> Datatypes.Coq_pair
        (_Z_as_OT__add
          (_Z_as_OT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q)
          (BinNums.Zpos BinNums.Coq_xH)) (_Z_as_OT__sub r' b)}};
   BinNums.Coq_xH ->
    case _Z_as_OT__leb (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) b of {
     Datatypes.Coq_true -> Datatypes.Coq_pair BinNums.Z0 (BinNums.Zpos
      BinNums.Coq_xH);
     Datatypes.Coq_false -> Datatypes.Coq_pair (BinNums.Zpos BinNums.Coq_xH)
      BinNums.Z0}}

_Z_as_OT__div_eucl :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
                      BinNums.Z
_Z_as_OT__div_eucl a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
   BinNums.Zpos a' ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
     BinNums.Zpos p -> _Z_as_OT__pos_div_eucl a' b;
     BinNums.Zneg b' ->
      case _Z_as_OT__pos_div_eucl a' (BinNums.Zpos b') of {
       Datatypes.Coq_pair q r ->
        case r of {
         BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_OT__opp q) BinNums.Z0;
         _ -> Datatypes.Coq_pair
          (_Z_as_OT__opp (_Z_as_OT__add q (BinNums.Zpos BinNums.Coq_xH)))
          (_Z_as_OT__add b r)}}};
   BinNums.Zneg a' ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
     BinNums.Zpos p ->
      case _Z_as_OT__pos_div_eucl a' b of {
       Datatypes.Coq_pair q r ->
        case r of {
         BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_OT__opp q) BinNums.Z0;
         _ -> Datatypes.Coq_pair
          (_Z_as_OT__opp (_Z_as_OT__add q (BinNums.Zpos BinNums.Coq_xH)))
          (_Z_as_OT__sub b r)}};
     BinNums.Zneg b' ->
      case _Z_as_OT__pos_div_eucl a' (BinNums.Zpos b') of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair q (_Z_as_OT__opp r)}}}

_Z_as_OT__div :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__div a b =
  case _Z_as_OT__div_eucl a b of {
   Datatypes.Coq_pair q x -> q}

_Z_as_OT__modulo :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__modulo a b =
  case _Z_as_OT__div_eucl a b of {
   Datatypes.Coq_pair x r -> r}

_Z_as_OT__quotrem :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
                     BinNums.Z
_Z_as_OT__quotrem a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 a;
     BinNums.Zpos b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z_as_OT__of_N q)
        (_Z_as_OT__of_N r)};
     BinNums.Zneg b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair
        (_Z_as_OT__opp (_Z_as_OT__of_N q)) (_Z_as_OT__of_N r)}};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 a;
     BinNums.Zpos b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair
        (_Z_as_OT__opp (_Z_as_OT__of_N q)) (_Z_as_OT__opp (_Z_as_OT__of_N r))};
     BinNums.Zneg b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z_as_OT__of_N q)
        (_Z_as_OT__opp (_Z_as_OT__of_N r))}}}

_Z_as_OT__quot :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__quot a b =
  Datatypes.fst (_Z_as_OT__quotrem a b)

_Z_as_OT__rem :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__rem a b =
  Datatypes.snd (_Z_as_OT__quotrem a b)

_Z_as_OT__even :: BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__even z =
  case z of {
   BinNums.Z0 -> Datatypes.Coq_true;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false};
   BinNums.Zneg p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_Z_as_OT__odd :: BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__odd z =
  case z of {
   BinNums.Z0 -> Datatypes.Coq_false;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_false;
     _ -> Datatypes.Coq_true};
   BinNums.Zneg p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_false;
     _ -> Datatypes.Coq_true}}

_Z_as_OT__div2 :: BinNums.Z -> BinNums.Z
_Z_as_OT__div2 z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xH -> BinNums.Z0;
     _ -> BinNums.Zpos (BinPos._Pos__div2 p)};
   BinNums.Zneg p -> BinNums.Zneg (BinPos._Pos__div2_up p)}

_Z_as_OT__quot2 :: BinNums.Z -> BinNums.Z
_Z_as_OT__quot2 z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xH -> BinNums.Z0;
     _ -> BinNums.Zpos (BinPos._Pos__div2 p)};
   BinNums.Zneg p ->
    case p of {
     BinNums.Coq_xH -> BinNums.Z0;
     _ -> BinNums.Zneg (BinPos._Pos__div2 p)}}

_Z_as_OT__log2 :: BinNums.Z -> BinNums.Z
_Z_as_OT__log2 z =
  case z of {
   BinNums.Zpos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Zpos (BinPos._Pos__size p);
     BinNums.Coq_xO p -> BinNums.Zpos (BinPos._Pos__size p);
     BinNums.Coq_xH -> BinNums.Z0};
   _ -> BinNums.Z0}

_Z_as_OT__sqrtrem :: BinNums.Z -> Datatypes.Coq_prod BinNums.Z BinNums.Z
_Z_as_OT__sqrtrem n =
  case n of {
   BinNums.Zpos p ->
    case BinPos._Pos__sqrtrem p of {
     Datatypes.Coq_pair s m ->
      case m of {
       BinPos.Pos__IsPos r -> Datatypes.Coq_pair (BinNums.Zpos s)
        (BinNums.Zpos r);
       _ -> Datatypes.Coq_pair (BinNums.Zpos s) BinNums.Z0}};
   _ -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0}

_Z_as_OT__sqrt :: BinNums.Z -> BinNums.Z
_Z_as_OT__sqrt n =
  case n of {
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__sqrt p);
   _ -> BinNums.Z0}

_Z_as_OT__gcd :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__gcd a b =
  case a of {
   BinNums.Z0 -> _Z_as_OT__abs b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> _Z_as_OT__abs a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0);
     BinNums.Zneg b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0)};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> _Z_as_OT__abs a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0);
     BinNums.Zneg b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0)}}

_Z_as_OT__ggcd :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
                  (Datatypes.Coq_prod BinNums.Z BinNums.Z)
_Z_as_OT__ggcd a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_OT__abs b) (Datatypes.Coq_pair
    BinNums.Z0 (_Z_as_OT__sgn b));
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_OT__abs a) (Datatypes.Coq_pair
      (_Z_as_OT__sgn a) BinNums.Z0);
     BinNums.Zpos b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zpos aa) (BinNums.Zpos bb))}};
     BinNums.Zneg b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zpos aa) (BinNums.Zneg bb))}}};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_OT__abs a) (Datatypes.Coq_pair
      (_Z_as_OT__sgn a) BinNums.Z0);
     BinNums.Zpos b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zneg aa) (BinNums.Zpos bb))}};
     BinNums.Zneg b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zneg aa) (BinNums.Zneg bb))}}}}

_Z_as_OT__testbit :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_OT__testbit a n =
  case n of {
   BinNums.Z0 -> _Z_as_OT__odd a;
   BinNums.Zpos p ->
    case a of {
     BinNums.Z0 -> Datatypes.Coq_false;
     BinNums.Zpos a0 -> BinPos._Pos__testbit a0 (BinNums.Npos p);
     BinNums.Zneg a0 ->
      Datatypes.negb
        (BinNat._N__testbit (BinPos._Pos__pred_N a0) (BinNums.Npos p))};
   BinNums.Zneg p -> Datatypes.Coq_false}

_Z_as_OT__shiftl :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__shiftl a n =
  case n of {
   BinNums.Z0 -> a;
   BinNums.Zpos p ->
    BinPos._Pos__iter p
      (_Z_as_OT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH))) a;
   BinNums.Zneg p -> BinPos._Pos__iter p _Z_as_OT__div2 a}

_Z_as_OT__shiftr :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__shiftr a n =
  _Z_as_OT__shiftl a (_Z_as_OT__opp n)

_Z_as_OT__lor :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__lor a b =
  case a of {
   BinNums.Z0 -> b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__lor a0 b0);
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__ldiff (BinPos._Pos__pred_N b0) (BinNums.Npos a0)))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__ldiff (BinPos._Pos__pred_N a0) (BinNums.Npos b0)));
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__land (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0)))}}

_Z_as_OT__land :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__land a b =
  case a of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos b0 -> _Z_as_OT__of_N (BinPos._Pos__land a0 b0);
     BinNums.Zneg b0 ->
      _Z_as_OT__of_N
        (BinNat._N__ldiff (BinNums.Npos a0) (BinPos._Pos__pred_N b0))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos b0 ->
      _Z_as_OT__of_N
        (BinNat._N__ldiff (BinNums.Npos b0) (BinPos._Pos__pred_N a0));
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0)))}}

_Z_as_OT__ldiff :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__ldiff a b =
  case a of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> _Z_as_OT__of_N (BinPos._Pos__ldiff a0 b0);
     BinNums.Zneg b0 ->
      _Z_as_OT__of_N
        (BinNat._N__land (BinNums.Npos a0) (BinPos._Pos__pred_N b0))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinNums.Npos b0)));
     BinNums.Zneg b0 ->
      _Z_as_OT__of_N
        (BinNat._N__ldiff (BinPos._Pos__pred_N b0) (BinPos._Pos__pred_N a0))}}

_Z_as_OT__lxor :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__lxor a b =
  case a of {
   BinNums.Z0 -> b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> _Z_as_OT__of_N (BinPos._Pos__lxor a0 b0);
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lxor (BinNums.Npos a0) (BinPos._Pos__pred_N b0)))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lxor (BinPos._Pos__pred_N a0) (BinNums.Npos b0)));
     BinNums.Zneg b0 ->
      _Z_as_OT__of_N
        (BinNat._N__lxor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0))}}

_Z_as_OT__eq_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z_as_OT__eq_dec x y =
  BinNums.coq_Z_rec (\y0 ->
    case y0 of {
     BinNums.Z0 -> Specif.Coq_left;
     _ -> Specif.Coq_right}) (\p y0 ->
    case y0 of {
     BinNums.Zpos p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (BinPos._Pos__eq_dec p p0);
     _ -> Specif.Coq_right}) (\p y0 ->
    case y0 of {
     BinNums.Zneg p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (BinPos._Pos__eq_dec p p0);
     _ -> Specif.Coq_right}) x y

_Z_as_OT__leb_spec0 :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z_as_OT__leb_spec0 x y =
  Bool.iff_reflect (_Z_as_OT__leb x y)

_Z_as_OT__ltb_spec0 :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z_as_OT__ltb_spec0 x y =
  Bool.iff_reflect (_Z_as_OT__ltb x y)

_Z_as_OT__sqrt_up :: BinNums.Z -> BinNums.Z
_Z_as_OT__sqrt_up a =
  case _Z_as_OT__compare _Z_as_OT__zero a of {
   Datatypes.Lt -> _Z_as_OT__succ (_Z_as_OT__sqrt (_Z_as_OT__pred a));
   _ -> _Z_as_OT__zero}

_Z_as_OT__log2_up :: BinNums.Z -> BinNums.Z
_Z_as_OT__log2_up a =
  case _Z_as_OT__compare _Z_as_OT__one a of {
   Datatypes.Lt -> _Z_as_OT__succ (_Z_as_OT__log2 (_Z_as_OT__pred a));
   _ -> _Z_as_OT__zero}

_Z_as_OT__Private_Div__Quot2Div__div :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__Private_Div__Quot2Div__div =
  BinInt._Z__quot

_Z_as_OT__Private_Div__Quot2Div__modulo :: BinNums.Z -> BinNums.Z ->
                                           BinNums.Z
_Z_as_OT__Private_Div__Quot2Div__modulo =
  BinInt._Z__rem

_Z_as_OT__lcm :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__lcm a b =
  _Z_as_OT__abs (_Z_as_OT__mul a (_Z_as_OT__div b (_Z_as_OT__gcd a b)))

_Z_as_OT__eqb_spec :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z_as_OT__eqb_spec x y =
  Bool.iff_reflect (_Z_as_OT__eqb x y)

_Z_as_OT__b2z :: Datatypes.Coq_bool -> BinNums.Z
_Z_as_OT__b2z b =
  case b of {
   Datatypes.Coq_true -> _Z_as_OT__one;
   Datatypes.Coq_false -> _Z_as_OT__zero}

_Z_as_OT__setbit :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__setbit a n =
  _Z_as_OT__lor a (_Z_as_OT__shiftl _Z_as_OT__one n)

_Z_as_OT__clearbit :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_OT__clearbit a n =
  _Z_as_OT__ldiff a (_Z_as_OT__shiftl _Z_as_OT__one n)

_Z_as_OT__lnot :: BinNums.Z -> BinNums.Z
_Z_as_OT__lnot a =
  _Z_as_OT__pred (_Z_as_OT__opp a)

_Z_as_OT__ones :: BinNums.Z -> BinNums.Z
_Z_as_OT__ones n =
  _Z_as_OT__pred (_Z_as_OT__shiftl _Z_as_OT__one n)

_Z_as_OT__Private_Dec__max_case_strong :: BinNums.Z -> BinNums.Z ->
                                          (BinNums.Z -> BinNums.Z -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_Z_as_OT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinInt._Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (BinInt._Z__max n m) __ (hl __);
   _ -> compat m (BinInt._Z__max n m) __ (hr __)}

_Z_as_OT__Private_Dec__max_case :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                                   BinNums.Z -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_Z_as_OT__Private_Dec__max_case n m x x0 x1 =
  _Z_as_OT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z_as_OT__Private_Dec__max_dec :: BinNums.Z -> BinNums.Z ->
                                  Specif.Coq_sumbool
_Z_as_OT__Private_Dec__max_dec n m =
  _Z_as_OT__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Z_as_OT__Private_Dec__min_case_strong :: BinNums.Z -> BinNums.Z ->
                                          (BinNums.Z -> BinNums.Z -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_Z_as_OT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinInt._Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (BinInt._Z__min n m) __ (hr __);
   _ -> compat n (BinInt._Z__min n m) __ (hl __)}

_Z_as_OT__Private_Dec__min_case :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                                   BinNums.Z -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_Z_as_OT__Private_Dec__min_case n m x x0 x1 =
  _Z_as_OT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z_as_OT__Private_Dec__min_dec :: BinNums.Z -> BinNums.Z ->
                                  Specif.Coq_sumbool
_Z_as_OT__Private_Dec__min_dec n m =
  _Z_as_OT__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Z_as_OT__max_case_strong :: BinNums.Z -> BinNums.Z -> (() -> a1) -> (() ->
                             a1) -> a1
_Z_as_OT__max_case_strong n m x x0 =
  _Z_as_OT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Z_as_OT__max_case :: BinNums.Z -> BinNums.Z -> a1 -> a1 -> a1
_Z_as_OT__max_case n m x x0 =
  _Z_as_OT__max_case_strong n m (\_ -> x) (\_ -> x0)

_Z_as_OT__max_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z_as_OT__max_dec =
  _Z_as_OT__Private_Dec__max_dec

_Z_as_OT__min_case_strong :: BinNums.Z -> BinNums.Z -> (() -> a1) -> (() ->
                             a1) -> a1
_Z_as_OT__min_case_strong n m x x0 =
  _Z_as_OT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Z_as_OT__min_case :: BinNums.Z -> BinNums.Z -> a1 -> a1 -> a1
_Z_as_OT__min_case n m x x0 =
  _Z_as_OT__min_case_strong n m (\_ -> x) (\_ -> x0)

_Z_as_OT__min_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z_as_OT__min_dec =
  _Z_as_OT__Private_Dec__min_dec

_Nat_as_DT__recursion :: a1 -> (Datatypes.Coq_nat -> a1 -> a1) ->
                         Datatypes.Coq_nat -> a1
_Nat_as_DT__recursion =
  Datatypes.nat_rect

type Nat_as_DT__Coq_t = Datatypes.Coq_nat

_Nat_as_DT__eqb :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_bool
_Nat_as_DT__eqb =
  EqNat.beq_nat

_Nat_as_DT__compare :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Datatypes.Coq_comparison
_Nat_as_DT__compare =
  Compare_dec.nat_compare

_Nat_as_DT__zero :: Datatypes.Coq_nat
_Nat_as_DT__zero =
  Datatypes.O

_Nat_as_DT__one :: Datatypes.Coq_nat
_Nat_as_DT__one =
  Datatypes.S Datatypes.O

_Nat_as_DT__two :: Datatypes.Coq_nat
_Nat_as_DT__two =
  Datatypes.S (Datatypes.S Datatypes.O)

_Nat_as_DT__succ :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__succ x =
  Datatypes.S x

_Nat_as_DT__pred :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__pred =
  Peano.pred

_Nat_as_DT__add :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__add =
  Peano.plus

_Nat_as_DT__sub :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__sub =
  Peano.minus

_Nat_as_DT__mul :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__mul =
  Peano.mult

_Nat_as_DT__ltb :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_bool
_Nat_as_DT__ltb =
  NPeano.ltb

_Nat_as_DT__leb :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_bool
_Nat_as_DT__leb =
  NPeano.leb

_Nat_as_DT__min :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__min =
  Peano.min

_Nat_as_DT__max :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__max =
  Peano.max

_Nat_as_DT__eq_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Specif.Coq_sumbool
_Nat_as_DT__eq_dec =
  Peano_dec.eq_nat_dec

_Nat_as_DT__even :: Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat_as_DT__even =
  NPeano.even

_Nat_as_DT__odd :: Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat_as_DT__odd =
  NPeano.odd

_Nat_as_DT__pow :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__pow =
  NPeano.pow

_Nat_as_DT__square :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__square =
  NPeano.square

_Nat_as_DT__log2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__log2 =
  NPeano.log2

_Nat_as_DT__sqrt :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__sqrt =
  NPeano.sqrt

_Nat_as_DT__div :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__div =
  NPeano.div

_Nat_as_DT__modulo :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_DT__modulo =
  NPeano.modulo

_Nat_as_DT__gcd :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__gcd =
  NPeano.gcd

_Nat_as_DT__testbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Datatypes.Coq_bool
_Nat_as_DT__testbit =
  NPeano.testbit

_Nat_as_DT__shiftl :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_DT__shiftl =
  NPeano.shiftl

_Nat_as_DT__shiftr :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_DT__shiftr =
  NPeano.shiftr

_Nat_as_DT__lxor :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                    Datatypes.Coq_nat
_Nat_as_DT__lxor =
  NPeano.lxor

_Nat_as_DT__land :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                    Datatypes.Coq_nat
_Nat_as_DT__land =
  NPeano.land

_Nat_as_DT__lor :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__lor =
  NPeano.lor

_Nat_as_DT__ldiff :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                     Datatypes.Coq_nat
_Nat_as_DT__ldiff =
  NPeano.ldiff

_Nat_as_DT__div2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__div2 =
  Div2.div2

_Nat_as_DT__sqrt_up :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__sqrt_up a =
  case Compare_dec.nat_compare Datatypes.O a of {
   Datatypes.Lt -> Datatypes.S (NPeano.sqrt (Peano.pred a));
   _ -> Datatypes.O}

_Nat_as_DT__log2_up :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__log2_up a =
  case Compare_dec.nat_compare (Datatypes.S Datatypes.O) a of {
   Datatypes.Lt -> Datatypes.S (NPeano.log2 (Peano.pred a));
   _ -> Datatypes.O}

_Nat_as_DT__lcm :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                   Datatypes.Coq_nat
_Nat_as_DT__lcm a b =
  Peano.mult a (NPeano.div b (NPeano.gcd a b))

_Nat_as_DT__eqb_spec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                        Bool.Coq_reflect
_Nat_as_DT__eqb_spec x y =
  Bool.iff_reflect (EqNat.beq_nat x y)

_Nat_as_DT__b2n :: Datatypes.Coq_bool -> Datatypes.Coq_nat
_Nat_as_DT__b2n b =
  case b of {
   Datatypes.Coq_true -> Datatypes.S Datatypes.O;
   Datatypes.Coq_false -> Datatypes.O}

_Nat_as_DT__setbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                      Datatypes.Coq_nat
_Nat_as_DT__setbit a n =
  NPeano.lor a (NPeano.shiftl (Datatypes.S Datatypes.O) n)

_Nat_as_DT__clearbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                        Datatypes.Coq_nat
_Nat_as_DT__clearbit a n =
  NPeano.ldiff a (NPeano.shiftl (Datatypes.S Datatypes.O) n)

_Nat_as_DT__ones :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat_as_DT__ones n =
  Peano.pred (NPeano.shiftl (Datatypes.S Datatypes.O) n)

_Nat_as_DT__lnot :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                    Datatypes.Coq_nat
_Nat_as_DT__lnot a n =
  NPeano.lxor a (_Nat_as_DT__ones n)

_Nat_as_DT__Private_Dec__max_case_strong :: Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat ->
                                            (Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat -> () -> a1 ->
                                            a1) -> (() -> a1) -> (() -> a1)
                                            -> a1
_Nat_as_DT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (Compare_dec.nat_compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (Peano.max n m) __ (hl __);
   _ -> compat m (Peano.max n m) __ (hr __)}

_Nat_as_DT__Private_Dec__max_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat
                                     -> (Datatypes.Coq_nat ->
                                     Datatypes.Coq_nat -> () -> a1 -> a1) ->
                                     a1 -> a1 -> a1
_Nat_as_DT__Private_Dec__max_case n m x x0 x1 =
  _Nat_as_DT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Nat_as_DT__Private_Dec__max_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                                    Specif.Coq_sumbool
_Nat_as_DT__Private_Dec__max_dec n m =
  _Nat_as_DT__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Nat_as_DT__Private_Dec__min_case_strong :: Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat ->
                                            (Datatypes.Coq_nat ->
                                            Datatypes.Coq_nat -> () -> a1 ->
                                            a1) -> (() -> a1) -> (() -> a1)
                                            -> a1
_Nat_as_DT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (Compare_dec.nat_compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (Peano.min n m) __ (hr __);
   _ -> compat n (Peano.min n m) __ (hl __)}

_Nat_as_DT__Private_Dec__min_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat
                                     -> (Datatypes.Coq_nat ->
                                     Datatypes.Coq_nat -> () -> a1 -> a1) ->
                                     a1 -> a1 -> a1
_Nat_as_DT__Private_Dec__min_case n m x x0 x1 =
  _Nat_as_DT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Nat_as_DT__Private_Dec__min_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                                    Specif.Coq_sumbool
_Nat_as_DT__Private_Dec__min_dec n m =
  _Nat_as_DT__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Nat_as_DT__max_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> (()
                               -> a1) -> (() -> a1) -> a1
_Nat_as_DT__max_case_strong n m x x0 =
  _Nat_as_DT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Nat_as_DT__max_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> a1 -> a1 ->
                        a1
_Nat_as_DT__max_case n m x x0 =
  _Nat_as_DT__max_case_strong n m (\_ -> x) (\_ -> x0)

_Nat_as_DT__max_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Specif.Coq_sumbool
_Nat_as_DT__max_dec =
  _Nat_as_DT__Private_Dec__max_dec

_Nat_as_DT__min_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> (()
                               -> a1) -> (() -> a1) -> a1
_Nat_as_DT__min_case_strong n m x x0 =
  _Nat_as_DT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Nat_as_DT__min_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> a1 -> a1 ->
                        a1
_Nat_as_DT__min_case n m x x0 =
  _Nat_as_DT__min_case_strong n m (\_ -> x) (\_ -> x0)

_Nat_as_DT__min_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Specif.Coq_sumbool
_Nat_as_DT__min_dec =
  _Nat_as_DT__Private_Dec__min_dec

type Positive_as_DT__Coq_t = BinNums.Coq_positive

_Positive_as_DT__succ :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__succ x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xO (_Positive_as_DT__succ p);
   BinNums.Coq_xO p -> BinNums.Coq_xI p;
   BinNums.Coq_xH -> BinNums.Coq_xO BinNums.Coq_xH}

_Positive_as_DT__add :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__add x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Positive_as_DT__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xI (_Positive_as_DT__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xO (_Positive_as_DT__succ p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Positive_as_DT__add p q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Positive_as_DT__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xI p};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Positive_as_DT__succ q);
     BinNums.Coq_xO q -> BinNums.Coq_xI q;
     BinNums.Coq_xH -> BinNums.Coq_xO BinNums.Coq_xH}}

_Positive_as_DT__add_carry :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              BinNums.Coq_positive
_Positive_as_DT__add_carry x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Positive_as_DT__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Positive_as_DT__add_carry p q);
     BinNums.Coq_xH -> BinNums.Coq_xI (_Positive_as_DT__succ p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Positive_as_DT__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xI (_Positive_as_DT__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xO (_Positive_as_DT__succ p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Positive_as_DT__succ q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Positive_as_DT__succ q);
     BinNums.Coq_xH -> BinNums.Coq_xI BinNums.Coq_xH}}

_Positive_as_DT__pred_double :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__pred_double x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xI (BinNums.Coq_xO p);
   BinNums.Coq_xO p -> BinNums.Coq_xI (_Positive_as_DT__pred_double p);
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_DT__pred :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__pred x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xO p;
   BinNums.Coq_xO p -> _Positive_as_DT__pred_double p;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_DT__pred_N :: BinNums.Coq_positive -> BinNums.N
_Positive_as_DT__pred_N x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Npos (BinNums.Coq_xO p);
   BinNums.Coq_xO p -> BinNums.Npos (_Positive_as_DT__pred_double p);
   BinNums.Coq_xH -> BinNums.N0}

data Positive_as_DT__Coq_mask =
   Positive_as_DT__IsNul
 | Positive_as_DT__IsPos BinNums.Coq_positive
 | Positive_as_DT__IsNeg

_Positive_as_DT__mask_rect :: a1 -> (BinNums.Coq_positive -> a1) -> a1 ->
                              Positive_as_DT__Coq_mask -> a1
_Positive_as_DT__mask_rect f f0 f1 m =
  case m of {
   Positive_as_DT__IsNul -> f;
   Positive_as_DT__IsPos x -> f0 x;
   Positive_as_DT__IsNeg -> f1}

_Positive_as_DT__mask_rec :: a1 -> (BinNums.Coq_positive -> a1) -> a1 ->
                             Positive_as_DT__Coq_mask -> a1
_Positive_as_DT__mask_rec =
  _Positive_as_DT__mask_rect

_Positive_as_DT__succ_double_mask :: Positive_as_DT__Coq_mask ->
                                     Positive_as_DT__Coq_mask
_Positive_as_DT__succ_double_mask x =
  case x of {
   Positive_as_DT__IsNul -> Positive_as_DT__IsPos BinNums.Coq_xH;
   Positive_as_DT__IsPos p -> Positive_as_DT__IsPos (BinNums.Coq_xI p);
   Positive_as_DT__IsNeg -> Positive_as_DT__IsNeg}

_Positive_as_DT__double_mask :: Positive_as_DT__Coq_mask ->
                                Positive_as_DT__Coq_mask
_Positive_as_DT__double_mask x =
  case x of {
   Positive_as_DT__IsPos p -> Positive_as_DT__IsPos (BinNums.Coq_xO p);
   x0 -> x0}

_Positive_as_DT__double_pred_mask :: BinNums.Coq_positive ->
                                     Positive_as_DT__Coq_mask
_Positive_as_DT__double_pred_mask x =
  case x of {
   BinNums.Coq_xI p -> Positive_as_DT__IsPos (BinNums.Coq_xO (BinNums.Coq_xO
    p));
   BinNums.Coq_xO p -> Positive_as_DT__IsPos (BinNums.Coq_xO
    (_Positive_as_DT__pred_double p));
   BinNums.Coq_xH -> Positive_as_DT__IsNul}

_Positive_as_DT__pred_mask :: Positive_as_DT__Coq_mask ->
                              Positive_as_DT__Coq_mask
_Positive_as_DT__pred_mask p =
  case p of {
   Positive_as_DT__IsPos q ->
    case q of {
     BinNums.Coq_xH -> Positive_as_DT__IsNul;
     _ -> Positive_as_DT__IsPos (_Positive_as_DT__pred q)};
   _ -> Positive_as_DT__IsNeg}

_Positive_as_DT__sub_mask :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             Positive_as_DT__Coq_mask
_Positive_as_DT__sub_mask x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_DT__double_mask (_Positive_as_DT__sub_mask p q);
     BinNums.Coq_xO q ->
      _Positive_as_DT__succ_double_mask (_Positive_as_DT__sub_mask p q);
     BinNums.Coq_xH -> Positive_as_DT__IsPos (BinNums.Coq_xO p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_DT__succ_double_mask (_Positive_as_DT__sub_mask_carry p q);
     BinNums.Coq_xO q ->
      _Positive_as_DT__double_mask (_Positive_as_DT__sub_mask p q);
     BinNums.Coq_xH -> Positive_as_DT__IsPos (_Positive_as_DT__pred_double p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xH -> Positive_as_DT__IsNul;
     _ -> Positive_as_DT__IsNeg}}

_Positive_as_DT__sub_mask_carry :: BinNums.Coq_positive ->
                                   BinNums.Coq_positive ->
                                   Positive_as_DT__Coq_mask
_Positive_as_DT__sub_mask_carry x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_DT__succ_double_mask (_Positive_as_DT__sub_mask_carry p q);
     BinNums.Coq_xO q ->
      _Positive_as_DT__double_mask (_Positive_as_DT__sub_mask p q);
     BinNums.Coq_xH -> Positive_as_DT__IsPos (_Positive_as_DT__pred_double p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q ->
      _Positive_as_DT__double_mask (_Positive_as_DT__sub_mask_carry p q);
     BinNums.Coq_xO q ->
      _Positive_as_DT__succ_double_mask (_Positive_as_DT__sub_mask_carry p q);
     BinNums.Coq_xH -> _Positive_as_DT__double_pred_mask p};
   BinNums.Coq_xH -> Positive_as_DT__IsNeg}

_Positive_as_DT__sub :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__sub x y =
  case _Positive_as_DT__sub_mask x y of {
   Positive_as_DT__IsPos z -> z;
   _ -> BinNums.Coq_xH}

_Positive_as_DT__mul :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__mul x y =
  case x of {
   BinNums.Coq_xI p ->
    _Positive_as_DT__add y (BinNums.Coq_xO (_Positive_as_DT__mul p y));
   BinNums.Coq_xO p -> BinNums.Coq_xO (_Positive_as_DT__mul p y);
   BinNums.Coq_xH -> y}

_Positive_as_DT__iter :: BinNums.Coq_positive -> (a1 -> a1) -> a1 -> a1
_Positive_as_DT__iter n f x =
  case n of {
   BinNums.Coq_xI n' ->
    f (_Positive_as_DT__iter n' f (_Positive_as_DT__iter n' f x));
   BinNums.Coq_xO n' ->
    _Positive_as_DT__iter n' f (_Positive_as_DT__iter n' f x);
   BinNums.Coq_xH -> f x}

_Positive_as_DT__pow :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__pow x y =
  _Positive_as_DT__iter y (_Positive_as_DT__mul x) BinNums.Coq_xH

_Positive_as_DT__square :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__square p =
  case p of {
   BinNums.Coq_xI p0 -> BinNums.Coq_xI (BinNums.Coq_xO
    (_Positive_as_DT__add (_Positive_as_DT__square p0) p0));
   BinNums.Coq_xO p0 -> BinNums.Coq_xO (BinNums.Coq_xO
    (_Positive_as_DT__square p0));
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_DT__div2 :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__div2 p =
  case p of {
   BinNums.Coq_xI p0 -> p0;
   BinNums.Coq_xO p0 -> p0;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_DT__div2_up :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__div2_up p =
  case p of {
   BinNums.Coq_xI p0 -> _Positive_as_DT__succ p0;
   BinNums.Coq_xO p0 -> p0;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_DT__size_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat
_Positive_as_DT__size_nat p =
  case p of {
   BinNums.Coq_xI p0 -> Datatypes.S (_Positive_as_DT__size_nat p0);
   BinNums.Coq_xO p0 -> Datatypes.S (_Positive_as_DT__size_nat p0);
   BinNums.Coq_xH -> Datatypes.S Datatypes.O}

_Positive_as_DT__size :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__size p =
  case p of {
   BinNums.Coq_xI p0 -> _Positive_as_DT__succ (_Positive_as_DT__size p0);
   BinNums.Coq_xO p0 -> _Positive_as_DT__succ (_Positive_as_DT__size p0);
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Positive_as_DT__compare_cont :: BinNums.Coq_positive -> BinNums.Coq_positive
                                 -> Datatypes.Coq_comparison ->
                                 Datatypes.Coq_comparison
_Positive_as_DT__compare_cont x y r =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Positive_as_DT__compare_cont p q r;
     BinNums.Coq_xO q -> _Positive_as_DT__compare_cont p q Datatypes.Gt;
     BinNums.Coq_xH -> Datatypes.Gt};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Positive_as_DT__compare_cont p q Datatypes.Lt;
     BinNums.Coq_xO q -> _Positive_as_DT__compare_cont p q r;
     BinNums.Coq_xH -> Datatypes.Gt};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xH -> r;
     _ -> Datatypes.Lt}}

_Positive_as_DT__compare :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                            Datatypes.Coq_comparison
_Positive_as_DT__compare x y =
  _Positive_as_DT__compare_cont x y Datatypes.Eq

_Positive_as_DT__min :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__min p p' =
  case _Positive_as_DT__compare p p' of {
   Datatypes.Gt -> p';
   _ -> p}

_Positive_as_DT__max :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__max p p' =
  case _Positive_as_DT__compare p p' of {
   Datatypes.Gt -> p;
   _ -> p'}

_Positive_as_DT__eqb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        Datatypes.Coq_bool
_Positive_as_DT__eqb p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Positive_as_DT__eqb p0 q0;
     _ -> Datatypes.Coq_false};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xO q0 -> _Positive_as_DT__eqb p0 q0;
     _ -> Datatypes.Coq_false};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xH -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_Positive_as_DT__leb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        Datatypes.Coq_bool
_Positive_as_DT__leb x y =
  case _Positive_as_DT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Positive_as_DT__ltb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        Datatypes.Coq_bool
_Positive_as_DT__ltb x y =
  case _Positive_as_DT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Positive_as_DT__sqrtrem_step :: (BinNums.Coq_positive ->
                                 BinNums.Coq_positive) ->
                                 (BinNums.Coq_positive ->
                                 BinNums.Coq_positive) -> (Datatypes.Coq_prod
                                 BinNums.Coq_positive
                                 Positive_as_DT__Coq_mask) ->
                                 Datatypes.Coq_prod BinNums.Coq_positive
                                 Positive_as_DT__Coq_mask
_Positive_as_DT__sqrtrem_step f g p =
  case p of {
   Datatypes.Coq_pair s y ->
    case y of {
     Positive_as_DT__IsPos r ->
      let {s' = BinNums.Coq_xI (BinNums.Coq_xO s)} in
      let {r' = g (f r)} in
      case _Positive_as_DT__leb s' r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (BinNums.Coq_xI s)
        (_Positive_as_DT__sub_mask r' s');
       Datatypes.Coq_false -> Datatypes.Coq_pair (BinNums.Coq_xO s)
        (Positive_as_DT__IsPos r')};
     _ -> Datatypes.Coq_pair (BinNums.Coq_xO s)
      (_Positive_as_DT__sub_mask (g (f BinNums.Coq_xH)) (BinNums.Coq_xO
        (BinNums.Coq_xO BinNums.Coq_xH)))}}

_Positive_as_DT__sqrtrem :: BinNums.Coq_positive -> Datatypes.Coq_prod
                            BinNums.Coq_positive Positive_as_DT__Coq_mask
_Positive_as_DT__sqrtrem p =
  case p of {
   BinNums.Coq_xI p0 ->
    case p0 of {
     BinNums.Coq_xI p1 ->
      _Positive_as_DT__sqrtrem_step (\x -> BinNums.Coq_xI x) (\x ->
        BinNums.Coq_xI x) (_Positive_as_DT__sqrtrem p1);
     BinNums.Coq_xO p1 ->
      _Positive_as_DT__sqrtrem_step (\x -> BinNums.Coq_xO x) (\x ->
        BinNums.Coq_xI x) (_Positive_as_DT__sqrtrem p1);
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
      (Positive_as_DT__IsPos (BinNums.Coq_xO BinNums.Coq_xH))};
   BinNums.Coq_xO p0 ->
    case p0 of {
     BinNums.Coq_xI p1 ->
      _Positive_as_DT__sqrtrem_step (\x -> BinNums.Coq_xI x) (\x ->
        BinNums.Coq_xO x) (_Positive_as_DT__sqrtrem p1);
     BinNums.Coq_xO p1 ->
      _Positive_as_DT__sqrtrem_step (\x -> BinNums.Coq_xO x) (\x ->
        BinNums.Coq_xO x) (_Positive_as_DT__sqrtrem p1);
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
      (Positive_as_DT__IsPos BinNums.Coq_xH)};
   BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH Positive_as_DT__IsNul}

_Positive_as_DT__sqrt :: BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__sqrt p =
  Datatypes.fst (_Positive_as_DT__sqrtrem p)

_Positive_as_DT__gcdn :: Datatypes.Coq_nat -> BinNums.Coq_positive ->
                         BinNums.Coq_positive -> BinNums.Coq_positive
_Positive_as_DT__gcdn n a b =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S n0 ->
    case a of {
     BinNums.Coq_xI a' ->
      case b of {
       BinNums.Coq_xI b' ->
        case _Positive_as_DT__compare a' b' of {
         Datatypes.Eq -> a;
         Datatypes.Lt ->
          _Positive_as_DT__gcdn n0 (_Positive_as_DT__sub b' a') a;
         Datatypes.Gt ->
          _Positive_as_DT__gcdn n0 (_Positive_as_DT__sub a' b') b};
       BinNums.Coq_xO b0 -> _Positive_as_DT__gcdn n0 a b0;
       BinNums.Coq_xH -> BinNums.Coq_xH};
     BinNums.Coq_xO a0 ->
      case b of {
       BinNums.Coq_xI p -> _Positive_as_DT__gcdn n0 a0 b;
       BinNums.Coq_xO b0 -> BinNums.Coq_xO (_Positive_as_DT__gcdn n0 a0 b0);
       BinNums.Coq_xH -> BinNums.Coq_xH};
     BinNums.Coq_xH -> BinNums.Coq_xH}}

_Positive_as_DT__gcd :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__gcd a b =
  _Positive_as_DT__gcdn
    (Peano.plus (_Positive_as_DT__size_nat a) (_Positive_as_DT__size_nat b))
    a b

_Positive_as_DT__ggcdn :: Datatypes.Coq_nat -> BinNums.Coq_positive ->
                          BinNums.Coq_positive -> Datatypes.Coq_prod
                          BinNums.Coq_positive
                          (Datatypes.Coq_prod BinNums.Coq_positive
                          BinNums.Coq_positive)
_Positive_as_DT__ggcdn n a b =
  case n of {
   Datatypes.O -> Datatypes.Coq_pair BinNums.Coq_xH (Datatypes.Coq_pair a b);
   Datatypes.S n0 ->
    case a of {
     BinNums.Coq_xI a' ->
      case b of {
       BinNums.Coq_xI b' ->
        case _Positive_as_DT__compare a' b' of {
         Datatypes.Eq -> Datatypes.Coq_pair a (Datatypes.Coq_pair
          BinNums.Coq_xH BinNums.Coq_xH);
         Datatypes.Lt ->
          case _Positive_as_DT__ggcdn n0 (_Positive_as_DT__sub b' a') a of {
           Datatypes.Coq_pair g p ->
            case p of {
             Datatypes.Coq_pair ba aa -> Datatypes.Coq_pair g
              (Datatypes.Coq_pair aa
              (_Positive_as_DT__add aa (BinNums.Coq_xO ba)))}};
         Datatypes.Gt ->
          case _Positive_as_DT__ggcdn n0 (_Positive_as_DT__sub a' b') b of {
           Datatypes.Coq_pair g p ->
            case p of {
             Datatypes.Coq_pair ab bb -> Datatypes.Coq_pair g
              (Datatypes.Coq_pair
              (_Positive_as_DT__add bb (BinNums.Coq_xO ab)) bb)}}};
       BinNums.Coq_xO b0 ->
        case _Positive_as_DT__ggcdn n0 a b0 of {
         Datatypes.Coq_pair g p ->
          case p of {
           Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair g
            (Datatypes.Coq_pair aa (BinNums.Coq_xO bb))}};
       BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
        (Datatypes.Coq_pair a BinNums.Coq_xH)};
     BinNums.Coq_xO a0 ->
      case b of {
       BinNums.Coq_xI p ->
        case _Positive_as_DT__ggcdn n0 a0 b of {
         Datatypes.Coq_pair g p0 ->
          case p0 of {
           Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair g
            (Datatypes.Coq_pair (BinNums.Coq_xO aa) bb)}};
       BinNums.Coq_xO b0 ->
        case _Positive_as_DT__ggcdn n0 a0 b0 of {
         Datatypes.Coq_pair g p -> Datatypes.Coq_pair (BinNums.Coq_xO g) p};
       BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
        (Datatypes.Coq_pair a BinNums.Coq_xH)};
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH (Datatypes.Coq_pair
      BinNums.Coq_xH b)}}

_Positive_as_DT__ggcd :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                         Datatypes.Coq_prod BinNums.Coq_positive
                         (Datatypes.Coq_prod BinNums.Coq_positive
                         BinNums.Coq_positive)
_Positive_as_DT__ggcd a b =
  _Positive_as_DT__ggcdn
    (Peano.plus (_Positive_as_DT__size_nat a) (_Positive_as_DT__size_nat b))
    a b

_Positive_as_DT__coq_Nsucc_double :: BinNums.N -> BinNums.N
_Positive_as_DT__coq_Nsucc_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xI p)}

_Positive_as_DT__coq_Ndouble :: BinNums.N -> BinNums.N
_Positive_as_DT__coq_Ndouble n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xO p)}

_Positive_as_DT__lor :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        BinNums.Coq_positive
_Positive_as_DT__lor p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Coq_xI (_Positive_as_DT__lor p0 q0);
     BinNums.Coq_xO q0 -> BinNums.Coq_xI (_Positive_as_DT__lor p0 q0);
     BinNums.Coq_xH -> p};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Coq_xI (_Positive_as_DT__lor p0 q0);
     BinNums.Coq_xO q0 -> BinNums.Coq_xO (_Positive_as_DT__lor p0 q0);
     BinNums.Coq_xH -> BinNums.Coq_xI p0};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.Coq_xI q0;
     _ -> q}}

_Positive_as_DT__land :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                         BinNums.N
_Positive_as_DT__land p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_DT__coq_Nsucc_double (_Positive_as_DT__land p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__land p0 q0);
     BinNums.Coq_xH -> BinNums.Npos BinNums.Coq_xH};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__land p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__land p0 q0);
     BinNums.Coq_xH -> BinNums.N0};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.N0;
     _ -> BinNums.Npos BinNums.Coq_xH}}

_Positive_as_DT__ldiff :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                          BinNums.N
_Positive_as_DT__ldiff p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__ldiff p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_DT__coq_Nsucc_double (_Positive_as_DT__ldiff p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xO p0)};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__ldiff p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__ldiff p0 q0);
     BinNums.Coq_xH -> BinNums.Npos p};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.Npos BinNums.Coq_xH;
     _ -> BinNums.N0}}

_Positive_as_DT__lxor :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                         BinNums.N
_Positive_as_DT__lxor p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__lxor p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_DT__coq_Nsucc_double (_Positive_as_DT__lxor p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xO p0)};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 ->
      _Positive_as_DT__coq_Nsucc_double (_Positive_as_DT__lxor p0 q0);
     BinNums.Coq_xO q0 ->
      _Positive_as_DT__coq_Ndouble (_Positive_as_DT__lxor p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xI p0)};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Npos (BinNums.Coq_xO q0);
     BinNums.Coq_xO q0 -> BinNums.Npos (BinNums.Coq_xI q0);
     BinNums.Coq_xH -> BinNums.N0}}

_Positive_as_DT__shiftl_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                               BinNums.Coq_positive
_Positive_as_DT__shiftl_nat p n =
  Peano.nat_iter n (\x -> BinNums.Coq_xO x) p

_Positive_as_DT__shiftr_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                               BinNums.Coq_positive
_Positive_as_DT__shiftr_nat p n =
  Peano.nat_iter n _Positive_as_DT__div2 p

_Positive_as_DT__shiftl :: BinNums.Coq_positive -> BinNums.N ->
                           BinNums.Coq_positive
_Positive_as_DT__shiftl p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Positive_as_DT__iter n0 (\x -> BinNums.Coq_xO x) p}

_Positive_as_DT__shiftr :: BinNums.Coq_positive -> BinNums.N ->
                           BinNums.Coq_positive
_Positive_as_DT__shiftr p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Positive_as_DT__iter n0 _Positive_as_DT__div2 p}

_Positive_as_DT__testbit_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                                Datatypes.Coq_bool
_Positive_as_DT__testbit_nat p n =
  case p of {
   BinNums.Coq_xI p0 ->
    case n of {
     Datatypes.O -> Datatypes.Coq_true;
     Datatypes.S n' -> _Positive_as_DT__testbit_nat p0 n'};
   BinNums.Coq_xO p0 ->
    case n of {
     Datatypes.O -> Datatypes.Coq_false;
     Datatypes.S n' -> _Positive_as_DT__testbit_nat p0 n'};
   BinNums.Coq_xH ->
    case n of {
     Datatypes.O -> Datatypes.Coq_true;
     Datatypes.S n0 -> Datatypes.Coq_false}}

_Positive_as_DT__testbit :: BinNums.Coq_positive -> BinNums.N ->
                            Datatypes.Coq_bool
_Positive_as_DT__testbit p n =
  case p of {
   BinNums.Coq_xI p0 ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos n0 ->
      _Positive_as_DT__testbit p0 (_Positive_as_DT__pred_N n0)};
   BinNums.Coq_xO p0 ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_false;
     BinNums.Npos n0 ->
      _Positive_as_DT__testbit p0 (_Positive_as_DT__pred_N n0)};
   BinNums.Coq_xH ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos p0 -> Datatypes.Coq_false}}

_Positive_as_DT__iter_op :: (a1 -> a1 -> a1) -> BinNums.Coq_positive -> a1 ->
                            a1
_Positive_as_DT__iter_op op p a =
  case p of {
   BinNums.Coq_xI p0 -> op a (_Positive_as_DT__iter_op op p0 (op a a));
   BinNums.Coq_xO p0 -> _Positive_as_DT__iter_op op p0 (op a a);
   BinNums.Coq_xH -> a}

_Positive_as_DT__to_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat
_Positive_as_DT__to_nat x =
  _Positive_as_DT__iter_op Peano.plus x (Datatypes.S Datatypes.O)

_Positive_as_DT__of_nat :: Datatypes.Coq_nat -> BinNums.Coq_positive
_Positive_as_DT__of_nat n =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S x ->
    case x of {
     Datatypes.O -> BinNums.Coq_xH;
     Datatypes.S n0 -> _Positive_as_DT__succ (_Positive_as_DT__of_nat x)}}

_Positive_as_DT__of_succ_nat :: Datatypes.Coq_nat -> BinNums.Coq_positive
_Positive_as_DT__of_succ_nat n =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S x -> _Positive_as_DT__succ (_Positive_as_DT__of_succ_nat x)}

_Positive_as_DT__eq_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                           Specif.Coq_sumbool
_Positive_as_DT__eq_dec x y =
  BinNums.positive_rec (\p h y0 ->
    case y0 of {
     BinNums.Coq_xI p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (h p0);
     _ -> Specif.Coq_right}) (\p h y0 ->
    case y0 of {
     BinNums.Coq_xO p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (h p0);
     _ -> Specif.Coq_right}) (\y0 ->
    case y0 of {
     BinNums.Coq_xH -> Specif.Coq_left;
     _ -> Specif.Coq_right}) x y

_Positive_as_DT__peano_rect :: a1 -> (BinNums.Coq_positive -> a1 -> a1) ->
                               BinNums.Coq_positive -> a1
_Positive_as_DT__peano_rect a f p =
  let {
   f2 = _Positive_as_DT__peano_rect (f BinNums.Coq_xH a) (\p0 x ->
          f (_Positive_as_DT__succ (BinNums.Coq_xO p0))
            (f (BinNums.Coq_xO p0) x))}
  in
  case p of {
   BinNums.Coq_xI q -> f (BinNums.Coq_xO q) (f2 q);
   BinNums.Coq_xO q -> f2 q;
   BinNums.Coq_xH -> a}

_Positive_as_DT__peano_rec :: a1 -> (BinNums.Coq_positive -> a1 -> a1) ->
                              BinNums.Coq_positive -> a1
_Positive_as_DT__peano_rec =
  _Positive_as_DT__peano_rect

data Positive_as_DT__PeanoView =
   Positive_as_DT__PeanoOne
 | Positive_as_DT__PeanoSucc BinNums.Coq_positive Positive_as_DT__PeanoView

_Positive_as_DT__coq_PeanoView_rect :: a1 -> (BinNums.Coq_positive ->
                                       Positive_as_DT__PeanoView -> a1 -> a1)
                                       -> BinNums.Coq_positive ->
                                       Positive_as_DT__PeanoView -> a1
_Positive_as_DT__coq_PeanoView_rect f f0 p p0 =
  case p0 of {
   Positive_as_DT__PeanoOne -> f;
   Positive_as_DT__PeanoSucc p1 p2 ->
    unsafeCoerce f0 p1 p2
      (unsafeCoerce (_Positive_as_DT__coq_PeanoView_rect f f0) p1 p2)}

_Positive_as_DT__coq_PeanoView_rec :: a1 -> (BinNums.Coq_positive ->
                                      Positive_as_DT__PeanoView -> a1 -> a1)
                                      -> BinNums.Coq_positive ->
                                      Positive_as_DT__PeanoView -> a1
_Positive_as_DT__coq_PeanoView_rec =
  _Positive_as_DT__coq_PeanoView_rect

_Positive_as_DT__peanoView_xO :: BinNums.Coq_positive ->
                                 Positive_as_DT__PeanoView ->
                                 Positive_as_DT__PeanoView
_Positive_as_DT__peanoView_xO p q =
  case q of {
   Positive_as_DT__PeanoOne -> Positive_as_DT__PeanoSucc BinNums.Coq_xH
    (unsafeCoerce Positive_as_DT__PeanoOne);
   Positive_as_DT__PeanoSucc p0 q0 -> Positive_as_DT__PeanoSucc
    (_Positive_as_DT__succ (BinNums.Coq_xO p0))
    (unsafeCoerce (Positive_as_DT__PeanoSucc (BinNums.Coq_xO p0)
      (unsafeCoerce (_Positive_as_DT__peanoView_xO p0 (unsafeCoerce q0)))))}

_Positive_as_DT__peanoView_xI :: BinNums.Coq_positive ->
                                 Positive_as_DT__PeanoView ->
                                 Positive_as_DT__PeanoView
_Positive_as_DT__peanoView_xI p q =
  case q of {
   Positive_as_DT__PeanoOne -> Positive_as_DT__PeanoSucc
    (_Positive_as_DT__succ BinNums.Coq_xH)
    (unsafeCoerce (Positive_as_DT__PeanoSucc BinNums.Coq_xH
      (unsafeCoerce Positive_as_DT__PeanoOne)));
   Positive_as_DT__PeanoSucc p0 q0 -> Positive_as_DT__PeanoSucc
    (_Positive_as_DT__succ (BinNums.Coq_xI p0))
    (unsafeCoerce (Positive_as_DT__PeanoSucc (BinNums.Coq_xI p0)
      (unsafeCoerce (_Positive_as_DT__peanoView_xI p0 (unsafeCoerce q0)))))}

_Positive_as_DT__peanoView :: BinNums.Coq_positive ->
                              Positive_as_DT__PeanoView
_Positive_as_DT__peanoView p =
  case p of {
   BinNums.Coq_xI p0 ->
    _Positive_as_DT__peanoView_xI p0 (_Positive_as_DT__peanoView p0);
   BinNums.Coq_xO p0 ->
    _Positive_as_DT__peanoView_xO p0 (_Positive_as_DT__peanoView p0);
   BinNums.Coq_xH -> Positive_as_DT__PeanoOne}

_Positive_as_DT__coq_PeanoView_iter :: a1 -> (BinNums.Coq_positive -> a1 ->
                                       a1) -> BinNums.Coq_positive ->
                                       Positive_as_DT__PeanoView -> a1
_Positive_as_DT__coq_PeanoView_iter a f p q =
  case q of {
   Positive_as_DT__PeanoOne -> a;
   Positive_as_DT__PeanoSucc p0 q0 ->
    f p0 (unsafeCoerce (_Positive_as_DT__coq_PeanoView_iter a f) p0 q0)}

_Positive_as_DT__eqb_spec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             Bool.Coq_reflect
_Positive_as_DT__eqb_spec x y =
  Bool.iff_reflect (_Positive_as_DT__eqb x y)

_Positive_as_DT__switch_Eq :: Datatypes.Coq_comparison ->
                              Datatypes.Coq_comparison ->
                              Datatypes.Coq_comparison
_Positive_as_DT__switch_Eq c c' =
  case c' of {
   Datatypes.Eq -> c;
   x -> x}

_Positive_as_DT__mask2cmp :: Positive_as_DT__Coq_mask ->
                             Datatypes.Coq_comparison
_Positive_as_DT__mask2cmp p =
  case p of {
   Positive_as_DT__IsNul -> Datatypes.Eq;
   Positive_as_DT__IsPos p0 -> Datatypes.Gt;
   Positive_as_DT__IsNeg -> Datatypes.Lt}

_Positive_as_DT__leb_spec0 :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              Bool.Coq_reflect
_Positive_as_DT__leb_spec0 x y =
  Bool.iff_reflect (_Positive_as_DT__leb x y)

_Positive_as_DT__ltb_spec0 :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              Bool.Coq_reflect
_Positive_as_DT__ltb_spec0 x y =
  Bool.iff_reflect (_Positive_as_DT__ltb x y)

_Positive_as_DT__Private_Dec__max_case_strong :: BinNums.Coq_positive ->
                                                 BinNums.Coq_positive ->
                                                 (BinNums.Coq_positive ->
                                                 BinNums.Coq_positive -> ()
                                                 -> a1 -> a1) -> (() -> a1)
                                                 -> (() -> a1) -> a1
_Positive_as_DT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinPos._Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (BinPos._Pos__max n m) __ (hl __);
   _ -> compat m (BinPos._Pos__max n m) __ (hr __)}

_Positive_as_DT__Private_Dec__max_case :: BinNums.Coq_positive ->
                                          BinNums.Coq_positive ->
                                          (BinNums.Coq_positive ->
                                          BinNums.Coq_positive -> () -> a1 ->
                                          a1) -> a1 -> a1 -> a1
_Positive_as_DT__Private_Dec__max_case n m x x0 x1 =
  _Positive_as_DT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Positive_as_DT__Private_Dec__max_dec :: BinNums.Coq_positive ->
                                         BinNums.Coq_positive ->
                                         Specif.Coq_sumbool
_Positive_as_DT__Private_Dec__max_dec n m =
  _Positive_as_DT__Private_Dec__max_case n m (\x y _ h0 -> h0)
    Specif.Coq_left Specif.Coq_right

_Positive_as_DT__Private_Dec__min_case_strong :: BinNums.Coq_positive ->
                                                 BinNums.Coq_positive ->
                                                 (BinNums.Coq_positive ->
                                                 BinNums.Coq_positive -> ()
                                                 -> a1 -> a1) -> (() -> a1)
                                                 -> (() -> a1) -> a1
_Positive_as_DT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinPos._Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (BinPos._Pos__min n m) __ (hr __);
   _ -> compat n (BinPos._Pos__min n m) __ (hl __)}

_Positive_as_DT__Private_Dec__min_case :: BinNums.Coq_positive ->
                                          BinNums.Coq_positive ->
                                          (BinNums.Coq_positive ->
                                          BinNums.Coq_positive -> () -> a1 ->
                                          a1) -> a1 -> a1 -> a1
_Positive_as_DT__Private_Dec__min_case n m x x0 x1 =
  _Positive_as_DT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Positive_as_DT__Private_Dec__min_dec :: BinNums.Coq_positive ->
                                         BinNums.Coq_positive ->
                                         Specif.Coq_sumbool
_Positive_as_DT__Private_Dec__min_dec n m =
  _Positive_as_DT__Private_Dec__min_case n m (\x y _ h0 -> h0)
    Specif.Coq_left Specif.Coq_right

_Positive_as_DT__max_case_strong :: BinNums.Coq_positive ->
                                    BinNums.Coq_positive -> (() -> a1) -> (()
                                    -> a1) -> a1
_Positive_as_DT__max_case_strong n m x x0 =
  _Positive_as_DT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Positive_as_DT__max_case :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             a1 -> a1 -> a1
_Positive_as_DT__max_case n m x x0 =
  _Positive_as_DT__max_case_strong n m (\_ -> x) (\_ -> x0)

_Positive_as_DT__max_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                            Specif.Coq_sumbool
_Positive_as_DT__max_dec =
  _Positive_as_DT__Private_Dec__max_dec

_Positive_as_DT__min_case_strong :: BinNums.Coq_positive ->
                                    BinNums.Coq_positive -> (() -> a1) -> (()
                                    -> a1) -> a1
_Positive_as_DT__min_case_strong n m x x0 =
  _Positive_as_DT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Positive_as_DT__min_case :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                             a1 -> a1 -> a1
_Positive_as_DT__min_case n m x x0 =
  _Positive_as_DT__min_case_strong n m (\_ -> x) (\_ -> x0)

_Positive_as_DT__min_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                            Specif.Coq_sumbool
_Positive_as_DT__min_dec =
  _Positive_as_DT__Private_Dec__min_dec

type N_as_DT__Coq_t = BinNums.N

_N_as_DT__zero :: BinNums.N
_N_as_DT__zero =
  BinNums.N0

_N_as_DT__one :: BinNums.N
_N_as_DT__one =
  BinNums.Npos BinNums.Coq_xH

_N_as_DT__two :: BinNums.N
_N_as_DT__two =
  BinNums.Npos (BinNums.Coq_xO BinNums.Coq_xH)

_N_as_DT__succ_double :: BinNums.N -> BinNums.N
_N_as_DT__succ_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xI p)}

_N_as_DT__double :: BinNums.N -> BinNums.N
_N_as_DT__double n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xO p)}

_N_as_DT__succ :: BinNums.N -> BinNums.N
_N_as_DT__succ n =
  case n of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__succ p)}

_N_as_DT__pred :: BinNums.N -> BinNums.N
_N_as_DT__pred n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinPos._Pos__pred_N p}

_N_as_DT__succ_pos :: BinNums.N -> BinNums.Coq_positive
_N_as_DT__succ_pos n =
  case n of {
   BinNums.N0 -> BinNums.Coq_xH;
   BinNums.Npos p -> BinPos._Pos__succ p}

_N_as_DT__add :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__add n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__add p q)}}

_N_as_DT__sub :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__sub n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos m' ->
      case BinPos._Pos__sub_mask n' m' of {
       BinPos.Pos__IsPos p -> BinNums.Npos p;
       _ -> BinNums.N0}}}

_N_as_DT__mul :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__mul n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__mul p q)}}

_N_as_DT__compare :: BinNums.N -> BinNums.N -> Datatypes.Coq_comparison
_N_as_DT__compare n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Datatypes.Eq;
     BinNums.Npos m' -> Datatypes.Lt};
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> Datatypes.Gt;
     BinNums.Npos m' -> BinPos._Pos__compare n' m'}}

_N_as_DT__eqb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_DT__eqb n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos p -> Datatypes.Coq_false};
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> Datatypes.Coq_false;
     BinNums.Npos q -> BinPos._Pos__eqb p q}}

_N_as_DT__leb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_DT__leb x y =
  case _N_as_DT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_N_as_DT__ltb :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_DT__ltb x y =
  case _N_as_DT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_N_as_DT__min :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__min n n' =
  case _N_as_DT__compare n n' of {
   Datatypes.Gt -> n';
   _ -> n}

_N_as_DT__max :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__max n n' =
  case _N_as_DT__compare n n' of {
   Datatypes.Gt -> n;
   _ -> n'}

_N_as_DT__div2 :: BinNums.N -> BinNums.N
_N_as_DT__div2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Npos p;
     BinNums.Coq_xO p -> BinNums.Npos p;
     BinNums.Coq_xH -> BinNums.N0}}

_N_as_DT__even :: BinNums.N -> Datatypes.Coq_bool
_N_as_DT__even n =
  case n of {
   BinNums.N0 -> Datatypes.Coq_true;
   BinNums.Npos p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_N_as_DT__odd :: BinNums.N -> Datatypes.Coq_bool
_N_as_DT__odd n =
  Datatypes.negb (_N_as_DT__even n)

_N_as_DT__pow :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__pow n p =
  case p of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p0 ->
    case n of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__pow q p0)}}

_N_as_DT__square :: BinNums.N -> BinNums.N
_N_as_DT__square n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__square p)}

_N_as_DT__log2 :: BinNums.N -> BinNums.N
_N_as_DT__log2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Npos (BinPos._Pos__size p);
     BinNums.Coq_xO p -> BinNums.Npos (BinPos._Pos__size p);
     BinNums.Coq_xH -> BinNums.N0}}

_N_as_DT__size :: BinNums.N -> BinNums.N
_N_as_DT__size n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__size p)}

_N_as_DT__size_nat :: BinNums.N -> Datatypes.Coq_nat
_N_as_DT__size_nat n =
  case n of {
   BinNums.N0 -> Datatypes.O;
   BinNums.Npos p -> BinPos._Pos__size_nat p}

_N_as_DT__pos_div_eucl :: BinNums.Coq_positive -> BinNums.N ->
                          Datatypes.Coq_prod BinNums.N BinNums.N
_N_as_DT__pos_div_eucl a b =
  case a of {
   BinNums.Coq_xI a' ->
    case _N_as_DT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {r' = _N_as_DT__succ_double r} in
      case _N_as_DT__leb b r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (_N_as_DT__succ_double q)
        (_N_as_DT__sub r' b);
       Datatypes.Coq_false -> Datatypes.Coq_pair (_N_as_DT__double q) r'}};
   BinNums.Coq_xO a' ->
    case _N_as_DT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {r' = _N_as_DT__double r} in
      case _N_as_DT__leb b r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (_N_as_DT__succ_double q)
        (_N_as_DT__sub r' b);
       Datatypes.Coq_false -> Datatypes.Coq_pair (_N_as_DT__double q) r'}};
   BinNums.Coq_xH ->
    case b of {
     BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 (BinNums.Npos
      BinNums.Coq_xH);
     BinNums.Npos p ->
      case p of {
       BinNums.Coq_xH -> Datatypes.Coq_pair (BinNums.Npos BinNums.Coq_xH)
        BinNums.N0;
       _ -> Datatypes.Coq_pair BinNums.N0 (BinNums.Npos BinNums.Coq_xH)}}}

_N_as_DT__div_eucl :: BinNums.N -> BinNums.N -> Datatypes.Coq_prod BinNums.N
                      BinNums.N
_N_as_DT__div_eucl a b =
  case a of {
   BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 BinNums.N0;
   BinNums.Npos na ->
    case b of {
     BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 a;
     BinNums.Npos p -> _N_as_DT__pos_div_eucl na b}}

_N_as_DT__div :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__div a b =
  Datatypes.fst (_N_as_DT__div_eucl a b)

_N_as_DT__modulo :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__modulo a b =
  Datatypes.snd (_N_as_DT__div_eucl a b)

_N_as_DT__gcd :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__gcd a b =
  case a of {
   BinNums.N0 -> b;
   BinNums.Npos p ->
    case b of {
     BinNums.N0 -> a;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__gcd p q)}}

_N_as_DT__ggcd :: BinNums.N -> BinNums.N -> Datatypes.Coq_prod BinNums.N
                  (Datatypes.Coq_prod BinNums.N BinNums.N)
_N_as_DT__ggcd a b =
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

_N_as_DT__sqrtrem :: BinNums.N -> Datatypes.Coq_prod BinNums.N BinNums.N
_N_as_DT__sqrtrem n =
  case n of {
   BinNums.N0 -> Datatypes.Coq_pair BinNums.N0 BinNums.N0;
   BinNums.Npos p ->
    case BinPos._Pos__sqrtrem p of {
     Datatypes.Coq_pair s m ->
      case m of {
       BinPos.Pos__IsPos r -> Datatypes.Coq_pair (BinNums.Npos s)
        (BinNums.Npos r);
       _ -> Datatypes.Coq_pair (BinNums.Npos s) BinNums.N0}}}

_N_as_DT__sqrt :: BinNums.N -> BinNums.N
_N_as_DT__sqrt n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__sqrt p)}

_N_as_DT__lor :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__lor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__lor p q)}}

_N_as_DT__land :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__land n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinPos._Pos__land p q}}

_N_as_DT__ldiff :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__ldiff n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__ldiff p q}}

_N_as_DT__lxor :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__lxor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__lxor p q}}

_N_as_DT__shiftl_nat :: BinNums.N -> Datatypes.Coq_nat -> BinNums.N
_N_as_DT__shiftl_nat a n =
  Peano.nat_iter n _N_as_DT__double a

_N_as_DT__shiftr_nat :: BinNums.N -> Datatypes.Coq_nat -> BinNums.N
_N_as_DT__shiftr_nat a n =
  Peano.nat_iter n _N_as_DT__div2 a

_N_as_DT__shiftl :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__shiftl a n =
  case a of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos a0 -> BinNums.Npos (BinPos._Pos__shiftl a0 n)}

_N_as_DT__shiftr :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__shiftr a n =
  case n of {
   BinNums.N0 -> a;
   BinNums.Npos p -> BinPos._Pos__iter p _N_as_DT__div2 a}

_N_as_DT__testbit_nat :: BinNums.N -> Datatypes.Coq_nat -> Datatypes.Coq_bool
_N_as_DT__testbit_nat a =
  case a of {
   BinNums.N0 -> (\x -> Datatypes.Coq_false);
   BinNums.Npos p -> BinPos._Pos__testbit_nat p}

_N_as_DT__testbit :: BinNums.N -> BinNums.N -> Datatypes.Coq_bool
_N_as_DT__testbit a n =
  case a of {
   BinNums.N0 -> Datatypes.Coq_false;
   BinNums.Npos p -> BinPos._Pos__testbit p n}

_N_as_DT__to_nat :: BinNums.N -> Datatypes.Coq_nat
_N_as_DT__to_nat a =
  case a of {
   BinNums.N0 -> Datatypes.O;
   BinNums.Npos p -> BinPos._Pos__to_nat p}

_N_as_DT__of_nat :: Datatypes.Coq_nat -> BinNums.N
_N_as_DT__of_nat n =
  case n of {
   Datatypes.O -> BinNums.N0;
   Datatypes.S n' -> BinNums.Npos (BinPos._Pos__of_succ_nat n')}

_N_as_DT__iter :: BinNums.N -> (a1 -> a1) -> a1 -> a1
_N_as_DT__iter n f x =
  case n of {
   BinNums.N0 -> x;
   BinNums.Npos p -> BinPos._Pos__iter p f x}

_N_as_DT__eq_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N_as_DT__eq_dec n m =
  BinNums.coq_N_rec (\m0 ->
    case m0 of {
     BinNums.N0 -> Specif.Coq_left;
     BinNums.Npos p -> Specif.Coq_right}) (\p m0 ->
    case m0 of {
     BinNums.N0 -> Specif.Coq_right;
     BinNums.Npos p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (BinPos._Pos__eq_dec p p0)}) n m

_N_as_DT__discr :: BinNums.N -> Specif.Coq_sumor BinNums.Coq_positive
_N_as_DT__discr n =
  case n of {
   BinNums.N0 -> Specif.Coq_inright;
   BinNums.Npos p -> Specif.Coq_inleft p}

_N_as_DT__binary_rect :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 ->
                         a1) -> BinNums.N -> a1
_N_as_DT__binary_rect f0 f2 fS2 n =
  let {f2' = \p -> f2 (BinNums.Npos p)} in
  let {fS2' = \p -> fS2 (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinNums.positive_rect fS2' f2' (fS2 BinNums.N0 f0) p}

_N_as_DT__binary_rec :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 ->
                        a1) -> BinNums.N -> a1
_N_as_DT__binary_rec =
  _N_as_DT__binary_rect

_N_as_DT__peano_rect :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N_as_DT__peano_rect f0 f n =
  let {f' = \p -> f (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinPos._Pos__peano_rect (f BinNums.N0 f0) f' p}

_N_as_DT__peano_rec :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N_as_DT__peano_rec =
  _N_as_DT__peano_rect

_N_as_DT__leb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N_as_DT__leb_spec0 x y =
  Bool.iff_reflect (_N_as_DT__leb x y)

_N_as_DT__ltb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N_as_DT__ltb_spec0 x y =
  Bool.iff_reflect (_N_as_DT__ltb x y)

_N_as_DT__recursion :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N_as_DT__recursion =
  _N_as_DT__peano_rect

_N_as_DT__sqrt_up :: BinNums.N -> BinNums.N
_N_as_DT__sqrt_up a =
  case _N_as_DT__compare _N_as_DT__zero a of {
   Datatypes.Lt -> _N_as_DT__succ (_N_as_DT__sqrt (_N_as_DT__pred a));
   _ -> _N_as_DT__zero}

_N_as_DT__log2_up :: BinNums.N -> BinNums.N
_N_as_DT__log2_up a =
  case _N_as_DT__compare _N_as_DT__one a of {
   Datatypes.Lt -> _N_as_DT__succ (_N_as_DT__log2 (_N_as_DT__pred a));
   _ -> _N_as_DT__zero}

_N_as_DT__lcm :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__lcm a b =
  _N_as_DT__mul a (_N_as_DT__div b (_N_as_DT__gcd a b))

_N_as_DT__eqb_spec :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N_as_DT__eqb_spec x y =
  Bool.iff_reflect (_N_as_DT__eqb x y)

_N_as_DT__b2n :: Datatypes.Coq_bool -> BinNums.N
_N_as_DT__b2n b =
  case b of {
   Datatypes.Coq_true -> _N_as_DT__one;
   Datatypes.Coq_false -> _N_as_DT__zero}

_N_as_DT__setbit :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__setbit a n =
  _N_as_DT__lor a (_N_as_DT__shiftl _N_as_DT__one n)

_N_as_DT__clearbit :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__clearbit a n =
  _N_as_DT__ldiff a (_N_as_DT__shiftl _N_as_DT__one n)

_N_as_DT__ones :: BinNums.N -> BinNums.N
_N_as_DT__ones n =
  _N_as_DT__pred (_N_as_DT__shiftl _N_as_DT__one n)

_N_as_DT__lnot :: BinNums.N -> BinNums.N -> BinNums.N
_N_as_DT__lnot a n =
  _N_as_DT__lxor a (_N_as_DT__ones n)

_N_as_DT__Private_Dec__max_case_strong :: BinNums.N -> BinNums.N ->
                                          (BinNums.N -> BinNums.N -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_N_as_DT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinNat._N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (BinNat._N__max n m) __ (hl __);
   _ -> compat m (BinNat._N__max n m) __ (hr __)}

_N_as_DT__Private_Dec__max_case :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                   BinNums.N -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_N_as_DT__Private_Dec__max_case n m x x0 x1 =
  _N_as_DT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_N_as_DT__Private_Dec__max_dec :: BinNums.N -> BinNums.N ->
                                  Specif.Coq_sumbool
_N_as_DT__Private_Dec__max_dec n m =
  _N_as_DT__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_N_as_DT__Private_Dec__min_case_strong :: BinNums.N -> BinNums.N ->
                                          (BinNums.N -> BinNums.N -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_N_as_DT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinNat._N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (BinNat._N__min n m) __ (hr __);
   _ -> compat n (BinNat._N__min n m) __ (hl __)}

_N_as_DT__Private_Dec__min_case :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                   BinNums.N -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_N_as_DT__Private_Dec__min_case n m x x0 x1 =
  _N_as_DT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_N_as_DT__Private_Dec__min_dec :: BinNums.N -> BinNums.N ->
                                  Specif.Coq_sumbool
_N_as_DT__Private_Dec__min_dec n m =
  _N_as_DT__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_N_as_DT__max_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() ->
                             a1) -> a1
_N_as_DT__max_case_strong n m x x0 =
  _N_as_DT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_N_as_DT__max_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N_as_DT__max_case n m x x0 =
  _N_as_DT__max_case_strong n m (\_ -> x) (\_ -> x0)

_N_as_DT__max_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N_as_DT__max_dec =
  _N_as_DT__Private_Dec__max_dec

_N_as_DT__min_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() ->
                             a1) -> a1
_N_as_DT__min_case_strong n m x x0 =
  _N_as_DT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_N_as_DT__min_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N_as_DT__min_case n m x x0 =
  _N_as_DT__min_case_strong n m (\_ -> x) (\_ -> x0)

_N_as_DT__min_dec :: BinNums.N -> BinNums.N -> Specif.Coq_sumbool
_N_as_DT__min_dec =
  _N_as_DT__Private_Dec__min_dec

type Z_as_DT__Coq_t = BinNums.Z

_Z_as_DT__zero :: BinNums.Z
_Z_as_DT__zero =
  BinNums.Z0

_Z_as_DT__one :: BinNums.Z
_Z_as_DT__one =
  BinNums.Zpos BinNums.Coq_xH

_Z_as_DT__two :: BinNums.Z
_Z_as_DT__two =
  BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)

_Z_as_DT__double :: BinNums.Z -> BinNums.Z
_Z_as_DT__double x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos (BinNums.Coq_xO p);
   BinNums.Zneg p -> BinNums.Zneg (BinNums.Coq_xO p)}

_Z_as_DT__succ_double :: BinNums.Z -> BinNums.Z
_Z_as_DT__succ_double x =
  case x of {
   BinNums.Z0 -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zpos p -> BinNums.Zpos (BinNums.Coq_xI p);
   BinNums.Zneg p -> BinNums.Zneg (BinPos._Pos__pred_double p)}

_Z_as_DT__pred_double :: BinNums.Z -> BinNums.Z
_Z_as_DT__pred_double x =
  case x of {
   BinNums.Z0 -> BinNums.Zneg BinNums.Coq_xH;
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__pred_double p);
   BinNums.Zneg p -> BinNums.Zneg (BinNums.Coq_xI p)}

_Z_as_DT__pos_sub :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                     BinNums.Z
_Z_as_DT__pos_sub x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Z_as_DT__double (_Z_as_DT__pos_sub p q);
     BinNums.Coq_xO q -> _Z_as_DT__succ_double (_Z_as_DT__pos_sub p q);
     BinNums.Coq_xH -> BinNums.Zpos (BinNums.Coq_xO p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Z_as_DT__pred_double (_Z_as_DT__pos_sub p q);
     BinNums.Coq_xO q -> _Z_as_DT__double (_Z_as_DT__pos_sub p q);
     BinNums.Coq_xH -> BinNums.Zpos (BinPos._Pos__pred_double p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Zneg (BinNums.Coq_xO q);
     BinNums.Coq_xO q -> BinNums.Zneg (BinPos._Pos__pred_double q);
     BinNums.Coq_xH -> BinNums.Z0}}

_Z_as_DT__add :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__add x y =
  case x of {
   BinNums.Z0 -> y;
   BinNums.Zpos x' ->
    case y of {
     BinNums.Z0 -> x;
     BinNums.Zpos y' -> BinNums.Zpos (BinPos._Pos__add x' y');
     BinNums.Zneg y' -> _Z_as_DT__pos_sub x' y'};
   BinNums.Zneg x' ->
    case y of {
     BinNums.Z0 -> x;
     BinNums.Zpos y' -> _Z_as_DT__pos_sub y' x';
     BinNums.Zneg y' -> BinNums.Zneg (BinPos._Pos__add x' y')}}

_Z_as_DT__opp :: BinNums.Z -> BinNums.Z
_Z_as_DT__opp x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos x0 -> BinNums.Zneg x0;
   BinNums.Zneg x0 -> BinNums.Zpos x0}

_Z_as_DT__succ :: BinNums.Z -> BinNums.Z
_Z_as_DT__succ x =
  _Z_as_DT__add x (BinNums.Zpos BinNums.Coq_xH)

_Z_as_DT__pred :: BinNums.Z -> BinNums.Z
_Z_as_DT__pred x =
  _Z_as_DT__add x (BinNums.Zneg BinNums.Coq_xH)

_Z_as_DT__sub :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__sub m n =
  _Z_as_DT__add m (_Z_as_DT__opp n)

_Z_as_DT__mul :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__mul x y =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos x' ->
    case y of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos y' -> BinNums.Zpos (BinPos._Pos__mul x' y');
     BinNums.Zneg y' -> BinNums.Zneg (BinPos._Pos__mul x' y')};
   BinNums.Zneg x' ->
    case y of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos y' -> BinNums.Zneg (BinPos._Pos__mul x' y');
     BinNums.Zneg y' -> BinNums.Zpos (BinPos._Pos__mul x' y')}}

_Z_as_DT__pow_pos :: BinNums.Z -> BinNums.Coq_positive -> BinNums.Z
_Z_as_DT__pow_pos z n =
  BinPos._Pos__iter n (_Z_as_DT__mul z) (BinNums.Zpos BinNums.Coq_xH)

_Z_as_DT__pow :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__pow x y =
  case y of {
   BinNums.Z0 -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zpos p -> _Z_as_DT__pow_pos x p;
   BinNums.Zneg p -> BinNums.Z0}

_Z_as_DT__square :: BinNums.Z -> BinNums.Z
_Z_as_DT__square x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__square p);
   BinNums.Zneg p -> BinNums.Zpos (BinPos._Pos__square p)}

_Z_as_DT__compare :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_comparison
_Z_as_DT__compare x y =
  case x of {
   BinNums.Z0 ->
    case y of {
     BinNums.Z0 -> Datatypes.Eq;
     BinNums.Zpos y' -> Datatypes.Lt;
     BinNums.Zneg y' -> Datatypes.Gt};
   BinNums.Zpos x' ->
    case y of {
     BinNums.Zpos y' -> BinPos._Pos__compare x' y';
     _ -> Datatypes.Gt};
   BinNums.Zneg x' ->
    case y of {
     BinNums.Zneg y' -> Datatypes.coq_CompOpp (BinPos._Pos__compare x' y');
     _ -> Datatypes.Lt}}

_Z_as_DT__sgn :: BinNums.Z -> BinNums.Z
_Z_as_DT__sgn z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zneg p -> BinNums.Zneg BinNums.Coq_xH}

_Z_as_DT__leb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__leb x y =
  case _Z_as_DT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Z_as_DT__ltb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__ltb x y =
  case _Z_as_DT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Z_as_DT__geb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__geb x y =
  case _Z_as_DT__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Z_as_DT__gtb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__gtb x y =
  case _Z_as_DT__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Z_as_DT__eqb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__eqb x y =
  case x of {
   BinNums.Z0 ->
    case y of {
     BinNums.Z0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false};
   BinNums.Zpos p ->
    case y of {
     BinNums.Zpos q -> BinPos._Pos__eqb p q;
     _ -> Datatypes.Coq_false};
   BinNums.Zneg p ->
    case y of {
     BinNums.Zneg q -> BinPos._Pos__eqb p q;
     _ -> Datatypes.Coq_false}}

_Z_as_DT__max :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__max n m =
  case _Z_as_DT__compare n m of {
   Datatypes.Lt -> m;
   _ -> n}

_Z_as_DT__min :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__min n m =
  case _Z_as_DT__compare n m of {
   Datatypes.Gt -> m;
   _ -> n}

_Z_as_DT__abs :: BinNums.Z -> BinNums.Z
_Z_as_DT__abs z =
  case z of {
   BinNums.Zneg p -> BinNums.Zpos p;
   x -> x}

_Z_as_DT__abs_nat :: BinNums.Z -> Datatypes.Coq_nat
_Z_as_DT__abs_nat z =
  case z of {
   BinNums.Z0 -> Datatypes.O;
   BinNums.Zpos p -> BinPos._Pos__to_nat p;
   BinNums.Zneg p -> BinPos._Pos__to_nat p}

_Z_as_DT__abs_N :: BinNums.Z -> BinNums.N
_Z_as_DT__abs_N z =
  case z of {
   BinNums.Z0 -> BinNums.N0;
   BinNums.Zpos p -> BinNums.Npos p;
   BinNums.Zneg p -> BinNums.Npos p}

_Z_as_DT__to_nat :: BinNums.Z -> Datatypes.Coq_nat
_Z_as_DT__to_nat z =
  case z of {
   BinNums.Zpos p -> BinPos._Pos__to_nat p;
   _ -> Datatypes.O}

_Z_as_DT__to_N :: BinNums.Z -> BinNums.N
_Z_as_DT__to_N z =
  case z of {
   BinNums.Zpos p -> BinNums.Npos p;
   _ -> BinNums.N0}

_Z_as_DT__of_nat :: Datatypes.Coq_nat -> BinNums.Z
_Z_as_DT__of_nat n =
  case n of {
   Datatypes.O -> BinNums.Z0;
   Datatypes.S n0 -> BinNums.Zpos (BinPos._Pos__of_succ_nat n0)}

_Z_as_DT__of_N :: BinNums.N -> BinNums.Z
_Z_as_DT__of_N n =
  case n of {
   BinNums.N0 -> BinNums.Z0;
   BinNums.Npos p -> BinNums.Zpos p}

_Z_as_DT__to_pos :: BinNums.Z -> BinNums.Coq_positive
_Z_as_DT__to_pos z =
  case z of {
   BinNums.Zpos p -> p;
   _ -> BinNums.Coq_xH}

_Z_as_DT__iter :: BinNums.Z -> (a1 -> a1) -> a1 -> a1
_Z_as_DT__iter n f x =
  case n of {
   BinNums.Zpos p -> BinPos._Pos__iter p f x;
   _ -> x}

_Z_as_DT__pos_div_eucl :: BinNums.Coq_positive -> BinNums.Z ->
                          Datatypes.Coq_prod BinNums.Z BinNums.Z
_Z_as_DT__pos_div_eucl a b =
  case a of {
   BinNums.Coq_xI a' ->
    case _Z_as_DT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {
       r' = _Z_as_DT__add
              (_Z_as_DT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH))
                r) (BinNums.Zpos BinNums.Coq_xH)}
      in
      case _Z_as_DT__ltb r' b of {
       Datatypes.Coq_true -> Datatypes.Coq_pair
        (_Z_as_DT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q) r';
       Datatypes.Coq_false -> Datatypes.Coq_pair
        (_Z_as_DT__add
          (_Z_as_DT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q)
          (BinNums.Zpos BinNums.Coq_xH)) (_Z_as_DT__sub r' b)}};
   BinNums.Coq_xO a' ->
    case _Z_as_DT__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {
       r' = _Z_as_DT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) r}
      in
      case _Z_as_DT__ltb r' b of {
       Datatypes.Coq_true -> Datatypes.Coq_pair
        (_Z_as_DT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q) r';
       Datatypes.Coq_false -> Datatypes.Coq_pair
        (_Z_as_DT__add
          (_Z_as_DT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q)
          (BinNums.Zpos BinNums.Coq_xH)) (_Z_as_DT__sub r' b)}};
   BinNums.Coq_xH ->
    case _Z_as_DT__leb (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) b of {
     Datatypes.Coq_true -> Datatypes.Coq_pair BinNums.Z0 (BinNums.Zpos
      BinNums.Coq_xH);
     Datatypes.Coq_false -> Datatypes.Coq_pair (BinNums.Zpos BinNums.Coq_xH)
      BinNums.Z0}}

_Z_as_DT__div_eucl :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
                      BinNums.Z
_Z_as_DT__div_eucl a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
   BinNums.Zpos a' ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
     BinNums.Zpos p -> _Z_as_DT__pos_div_eucl a' b;
     BinNums.Zneg b' ->
      case _Z_as_DT__pos_div_eucl a' (BinNums.Zpos b') of {
       Datatypes.Coq_pair q r ->
        case r of {
         BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_DT__opp q) BinNums.Z0;
         _ -> Datatypes.Coq_pair
          (_Z_as_DT__opp (_Z_as_DT__add q (BinNums.Zpos BinNums.Coq_xH)))
          (_Z_as_DT__add b r)}}};
   BinNums.Zneg a' ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
     BinNums.Zpos p ->
      case _Z_as_DT__pos_div_eucl a' b of {
       Datatypes.Coq_pair q r ->
        case r of {
         BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_DT__opp q) BinNums.Z0;
         _ -> Datatypes.Coq_pair
          (_Z_as_DT__opp (_Z_as_DT__add q (BinNums.Zpos BinNums.Coq_xH)))
          (_Z_as_DT__sub b r)}};
     BinNums.Zneg b' ->
      case _Z_as_DT__pos_div_eucl a' (BinNums.Zpos b') of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair q (_Z_as_DT__opp r)}}}

_Z_as_DT__div :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__div a b =
  case _Z_as_DT__div_eucl a b of {
   Datatypes.Coq_pair q x -> q}

_Z_as_DT__modulo :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__modulo a b =
  case _Z_as_DT__div_eucl a b of {
   Datatypes.Coq_pair x r -> r}

_Z_as_DT__quotrem :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
                     BinNums.Z
_Z_as_DT__quotrem a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 a;
     BinNums.Zpos b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z_as_DT__of_N q)
        (_Z_as_DT__of_N r)};
     BinNums.Zneg b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair
        (_Z_as_DT__opp (_Z_as_DT__of_N q)) (_Z_as_DT__of_N r)}};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 a;
     BinNums.Zpos b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair
        (_Z_as_DT__opp (_Z_as_DT__of_N q)) (_Z_as_DT__opp (_Z_as_DT__of_N r))};
     BinNums.Zneg b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z_as_DT__of_N q)
        (_Z_as_DT__opp (_Z_as_DT__of_N r))}}}

_Z_as_DT__quot :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__quot a b =
  Datatypes.fst (_Z_as_DT__quotrem a b)

_Z_as_DT__rem :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__rem a b =
  Datatypes.snd (_Z_as_DT__quotrem a b)

_Z_as_DT__even :: BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__even z =
  case z of {
   BinNums.Z0 -> Datatypes.Coq_true;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false};
   BinNums.Zneg p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_Z_as_DT__odd :: BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__odd z =
  case z of {
   BinNums.Z0 -> Datatypes.Coq_false;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_false;
     _ -> Datatypes.Coq_true};
   BinNums.Zneg p ->
    case p of {
     BinNums.Coq_xO p0 -> Datatypes.Coq_false;
     _ -> Datatypes.Coq_true}}

_Z_as_DT__div2 :: BinNums.Z -> BinNums.Z
_Z_as_DT__div2 z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xH -> BinNums.Z0;
     _ -> BinNums.Zpos (BinPos._Pos__div2 p)};
   BinNums.Zneg p -> BinNums.Zneg (BinPos._Pos__div2_up p)}

_Z_as_DT__quot2 :: BinNums.Z -> BinNums.Z
_Z_as_DT__quot2 z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xH -> BinNums.Z0;
     _ -> BinNums.Zpos (BinPos._Pos__div2 p)};
   BinNums.Zneg p ->
    case p of {
     BinNums.Coq_xH -> BinNums.Z0;
     _ -> BinNums.Zneg (BinPos._Pos__div2 p)}}

_Z_as_DT__log2 :: BinNums.Z -> BinNums.Z
_Z_as_DT__log2 z =
  case z of {
   BinNums.Zpos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Zpos (BinPos._Pos__size p);
     BinNums.Coq_xO p -> BinNums.Zpos (BinPos._Pos__size p);
     BinNums.Coq_xH -> BinNums.Z0};
   _ -> BinNums.Z0}

_Z_as_DT__sqrtrem :: BinNums.Z -> Datatypes.Coq_prod BinNums.Z BinNums.Z
_Z_as_DT__sqrtrem n =
  case n of {
   BinNums.Zpos p ->
    case BinPos._Pos__sqrtrem p of {
     Datatypes.Coq_pair s m ->
      case m of {
       BinPos.Pos__IsPos r -> Datatypes.Coq_pair (BinNums.Zpos s)
        (BinNums.Zpos r);
       _ -> Datatypes.Coq_pair (BinNums.Zpos s) BinNums.Z0}};
   _ -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0}

_Z_as_DT__sqrt :: BinNums.Z -> BinNums.Z
_Z_as_DT__sqrt n =
  case n of {
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__sqrt p);
   _ -> BinNums.Z0}

_Z_as_DT__gcd :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__gcd a b =
  case a of {
   BinNums.Z0 -> _Z_as_DT__abs b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> _Z_as_DT__abs a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0);
     BinNums.Zneg b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0)};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> _Z_as_DT__abs a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0);
     BinNums.Zneg b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0)}}

_Z_as_DT__ggcd :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
                  (Datatypes.Coq_prod BinNums.Z BinNums.Z)
_Z_as_DT__ggcd a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_DT__abs b) (Datatypes.Coq_pair
    BinNums.Z0 (_Z_as_DT__sgn b));
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_DT__abs a) (Datatypes.Coq_pair
      (_Z_as_DT__sgn a) BinNums.Z0);
     BinNums.Zpos b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zpos aa) (BinNums.Zpos bb))}};
     BinNums.Zneg b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zpos aa) (BinNums.Zneg bb))}}};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair (_Z_as_DT__abs a) (Datatypes.Coq_pair
      (_Z_as_DT__sgn a) BinNums.Z0);
     BinNums.Zpos b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zneg aa) (BinNums.Zpos bb))}};
     BinNums.Zneg b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       Datatypes.Coq_pair g p ->
        case p of {
         Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair (BinNums.Zpos g)
          (Datatypes.Coq_pair (BinNums.Zneg aa) (BinNums.Zneg bb))}}}}

_Z_as_DT__testbit :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z_as_DT__testbit a n =
  case n of {
   BinNums.Z0 -> _Z_as_DT__odd a;
   BinNums.Zpos p ->
    case a of {
     BinNums.Z0 -> Datatypes.Coq_false;
     BinNums.Zpos a0 -> BinPos._Pos__testbit a0 (BinNums.Npos p);
     BinNums.Zneg a0 ->
      Datatypes.negb
        (BinNat._N__testbit (BinPos._Pos__pred_N a0) (BinNums.Npos p))};
   BinNums.Zneg p -> Datatypes.Coq_false}

_Z_as_DT__shiftl :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__shiftl a n =
  case n of {
   BinNums.Z0 -> a;
   BinNums.Zpos p ->
    BinPos._Pos__iter p
      (_Z_as_DT__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH))) a;
   BinNums.Zneg p -> BinPos._Pos__iter p _Z_as_DT__div2 a}

_Z_as_DT__shiftr :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__shiftr a n =
  _Z_as_DT__shiftl a (_Z_as_DT__opp n)

_Z_as_DT__lor :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__lor a b =
  case a of {
   BinNums.Z0 -> b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__lor a0 b0);
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__ldiff (BinPos._Pos__pred_N b0) (BinNums.Npos a0)))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__ldiff (BinPos._Pos__pred_N a0) (BinNums.Npos b0)));
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__land (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0)))}}

_Z_as_DT__land :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__land a b =
  case a of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos b0 -> _Z_as_DT__of_N (BinPos._Pos__land a0 b0);
     BinNums.Zneg b0 ->
      _Z_as_DT__of_N
        (BinNat._N__ldiff (BinNums.Npos a0) (BinPos._Pos__pred_N b0))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos b0 ->
      _Z_as_DT__of_N
        (BinNat._N__ldiff (BinNums.Npos b0) (BinPos._Pos__pred_N a0));
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0)))}}

_Z_as_DT__ldiff :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__ldiff a b =
  case a of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> _Z_as_DT__of_N (BinPos._Pos__ldiff a0 b0);
     BinNums.Zneg b0 ->
      _Z_as_DT__of_N
        (BinNat._N__land (BinNums.Npos a0) (BinPos._Pos__pred_N b0))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinNums.Npos b0)));
     BinNums.Zneg b0 ->
      _Z_as_DT__of_N
        (BinNat._N__ldiff (BinPos._Pos__pred_N b0) (BinPos._Pos__pred_N a0))}}

_Z_as_DT__lxor :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__lxor a b =
  case a of {
   BinNums.Z0 -> b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> _Z_as_DT__of_N (BinPos._Pos__lxor a0 b0);
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lxor (BinNums.Npos a0) (BinPos._Pos__pred_N b0)))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lxor (BinPos._Pos__pred_N a0) (BinNums.Npos b0)));
     BinNums.Zneg b0 ->
      _Z_as_DT__of_N
        (BinNat._N__lxor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0))}}

_Z_as_DT__eq_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z_as_DT__eq_dec x y =
  BinNums.coq_Z_rec (\y0 ->
    case y0 of {
     BinNums.Z0 -> Specif.Coq_left;
     _ -> Specif.Coq_right}) (\p y0 ->
    case y0 of {
     BinNums.Zpos p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (BinPos._Pos__eq_dec p p0);
     _ -> Specif.Coq_right}) (\p y0 ->
    case y0 of {
     BinNums.Zneg p0 ->
      Specif.sumbool_rec (\_ -> Logic.eq_rec_r p0 Specif.Coq_left p) (\_ ->
        Specif.Coq_right) (BinPos._Pos__eq_dec p p0);
     _ -> Specif.Coq_right}) x y

_Z_as_DT__leb_spec0 :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z_as_DT__leb_spec0 x y =
  Bool.iff_reflect (_Z_as_DT__leb x y)

_Z_as_DT__ltb_spec0 :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z_as_DT__ltb_spec0 x y =
  Bool.iff_reflect (_Z_as_DT__ltb x y)

_Z_as_DT__sqrt_up :: BinNums.Z -> BinNums.Z
_Z_as_DT__sqrt_up a =
  case _Z_as_DT__compare _Z_as_DT__zero a of {
   Datatypes.Lt -> _Z_as_DT__succ (_Z_as_DT__sqrt (_Z_as_DT__pred a));
   _ -> _Z_as_DT__zero}

_Z_as_DT__log2_up :: BinNums.Z -> BinNums.Z
_Z_as_DT__log2_up a =
  case _Z_as_DT__compare _Z_as_DT__one a of {
   Datatypes.Lt -> _Z_as_DT__succ (_Z_as_DT__log2 (_Z_as_DT__pred a));
   _ -> _Z_as_DT__zero}

_Z_as_DT__Private_Div__Quot2Div__div :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__Private_Div__Quot2Div__div =
  BinInt._Z__quot

_Z_as_DT__Private_Div__Quot2Div__modulo :: BinNums.Z -> BinNums.Z ->
                                           BinNums.Z
_Z_as_DT__Private_Div__Quot2Div__modulo =
  BinInt._Z__rem

_Z_as_DT__lcm :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__lcm a b =
  _Z_as_DT__abs (_Z_as_DT__mul a (_Z_as_DT__div b (_Z_as_DT__gcd a b)))

_Z_as_DT__eqb_spec :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z_as_DT__eqb_spec x y =
  Bool.iff_reflect (_Z_as_DT__eqb x y)

_Z_as_DT__b2z :: Datatypes.Coq_bool -> BinNums.Z
_Z_as_DT__b2z b =
  case b of {
   Datatypes.Coq_true -> _Z_as_DT__one;
   Datatypes.Coq_false -> _Z_as_DT__zero}

_Z_as_DT__setbit :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__setbit a n =
  _Z_as_DT__lor a (_Z_as_DT__shiftl _Z_as_DT__one n)

_Z_as_DT__clearbit :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z_as_DT__clearbit a n =
  _Z_as_DT__ldiff a (_Z_as_DT__shiftl _Z_as_DT__one n)

_Z_as_DT__lnot :: BinNums.Z -> BinNums.Z
_Z_as_DT__lnot a =
  _Z_as_DT__pred (_Z_as_DT__opp a)

_Z_as_DT__ones :: BinNums.Z -> BinNums.Z
_Z_as_DT__ones n =
  _Z_as_DT__pred (_Z_as_DT__shiftl _Z_as_DT__one n)

_Z_as_DT__Private_Dec__max_case_strong :: BinNums.Z -> BinNums.Z ->
                                          (BinNums.Z -> BinNums.Z -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_Z_as_DT__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinInt._Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (BinInt._Z__max n m) __ (hl __);
   _ -> compat m (BinInt._Z__max n m) __ (hr __)}

_Z_as_DT__Private_Dec__max_case :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                                   BinNums.Z -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_Z_as_DT__Private_Dec__max_case n m x x0 x1 =
  _Z_as_DT__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z_as_DT__Private_Dec__max_dec :: BinNums.Z -> BinNums.Z ->
                                  Specif.Coq_sumbool
_Z_as_DT__Private_Dec__max_dec n m =
  _Z_as_DT__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Z_as_DT__Private_Dec__min_case_strong :: BinNums.Z -> BinNums.Z ->
                                          (BinNums.Z -> BinNums.Z -> () -> a1
                                          -> a1) -> (() -> a1) -> (() -> a1)
                                          -> a1
_Z_as_DT__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (BinInt._Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (BinInt._Z__min n m) __ (hr __);
   _ -> compat n (BinInt._Z__min n m) __ (hl __)}

_Z_as_DT__Private_Dec__min_case :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                                   BinNums.Z -> () -> a1 -> a1) -> a1 -> a1
                                   -> a1
_Z_as_DT__Private_Dec__min_case n m x x0 x1 =
  _Z_as_DT__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z_as_DT__Private_Dec__min_dec :: BinNums.Z -> BinNums.Z ->
                                  Specif.Coq_sumbool
_Z_as_DT__Private_Dec__min_dec n m =
  _Z_as_DT__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Z_as_DT__max_case_strong :: BinNums.Z -> BinNums.Z -> (() -> a1) -> (() ->
                             a1) -> a1
_Z_as_DT__max_case_strong n m x x0 =
  _Z_as_DT__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Z_as_DT__max_case :: BinNums.Z -> BinNums.Z -> a1 -> a1 -> a1
_Z_as_DT__max_case n m x x0 =
  _Z_as_DT__max_case_strong n m (\_ -> x) (\_ -> x0)

_Z_as_DT__max_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z_as_DT__max_dec =
  _Z_as_DT__Private_Dec__max_dec

_Z_as_DT__min_case_strong :: BinNums.Z -> BinNums.Z -> (() -> a1) -> (() ->
                             a1) -> a1
_Z_as_DT__min_case_strong n m x x0 =
  _Z_as_DT__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Z_as_DT__min_case :: BinNums.Z -> BinNums.Z -> a1 -> a1 -> a1
_Z_as_DT__min_case n m x x0 =
  _Z_as_DT__min_case_strong n m (\_ -> x) (\_ -> x0)

_Z_as_DT__min_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z_as_DT__min_dec =
  _Z_as_DT__Private_Dec__min_dec

