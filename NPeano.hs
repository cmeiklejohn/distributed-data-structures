module NPeano where

import qualified Prelude
import qualified Bool
import qualified Compare_dec
import qualified Datatypes
import qualified Div2
import qualified EqNat
import qualified Logic
import qualified Peano
import qualified Peano_dec
import qualified Specif


__ :: any
__ = Prelude.error "Logical or arity value used"

leb :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
leb n m =
  case n of {
   Datatypes.O -> Datatypes.Coq_true;
   Datatypes.S n' ->
    case m of {
     Datatypes.O -> Datatypes.Coq_false;
     Datatypes.S m' -> leb n' m'}}

ltb :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
ltb n m =
  leb (Datatypes.S n) m

pow :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
pow n m =
  case m of {
   Datatypes.O -> Datatypes.S Datatypes.O;
   Datatypes.S m0 -> Peano.mult n (pow n m0)}

square :: Datatypes.Coq_nat -> Datatypes.Coq_nat
square n =
  Peano.mult n n

even :: Datatypes.Coq_nat -> Datatypes.Coq_bool
even n =
  case n of {
   Datatypes.O -> Datatypes.Coq_true;
   Datatypes.S n0 ->
    case n0 of {
     Datatypes.O -> Datatypes.Coq_false;
     Datatypes.S n' -> even n'}}

odd :: Datatypes.Coq_nat -> Datatypes.Coq_bool
odd n =
  Datatypes.negb (even n)

divmod :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat ->
          Datatypes.Coq_nat -> Datatypes.Coq_prod Datatypes.Coq_nat
          Datatypes.Coq_nat
divmod x y q u =
  case x of {
   Datatypes.O -> Datatypes.Coq_pair q u;
   Datatypes.S x' ->
    case u of {
     Datatypes.O -> divmod x' y (Datatypes.S q) y;
     Datatypes.S u' -> divmod x' y q u'}}

div :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
div x y =
  case y of {
   Datatypes.O -> y;
   Datatypes.S y' -> Datatypes.fst (divmod x y' Datatypes.O y')}

modulo :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
modulo x y =
  case y of {
   Datatypes.O -> y;
   Datatypes.S y' ->
    Peano.minus y' (Datatypes.snd (divmod x y' Datatypes.O y'))}

sqrt_iter :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat ->
             Datatypes.Coq_nat -> Datatypes.Coq_nat
sqrt_iter k p q r =
  case k of {
   Datatypes.O -> p;
   Datatypes.S k' ->
    case r of {
     Datatypes.O ->
      sqrt_iter k' (Datatypes.S p) (Datatypes.S (Datatypes.S q)) (Datatypes.S
        (Datatypes.S q));
     Datatypes.S r' -> sqrt_iter k' p q r'}}

sqrt :: Datatypes.Coq_nat -> Datatypes.Coq_nat
sqrt n =
  sqrt_iter n Datatypes.O Datatypes.O Datatypes.O

log2_iter :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat ->
             Datatypes.Coq_nat -> Datatypes.Coq_nat
log2_iter k p q r =
  case k of {
   Datatypes.O -> p;
   Datatypes.S k' ->
    case r of {
     Datatypes.O -> log2_iter k' (Datatypes.S p) (Datatypes.S q) q;
     Datatypes.S r' -> log2_iter k' p (Datatypes.S q) r'}}

log2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
log2 n =
  log2_iter (Peano.pred n) Datatypes.O (Datatypes.S Datatypes.O) Datatypes.O

gcd :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
gcd a b =
  case a of {
   Datatypes.O -> b;
   Datatypes.S a' -> gcd (modulo b (Datatypes.S a')) (Datatypes.S a')}

testbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
testbit a n =
  case n of {
   Datatypes.O -> odd a;
   Datatypes.S n0 -> testbit (Div2.div2 a) n0}

shiftl :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
shiftl a n =
  Peano.nat_iter n Div2.double a

shiftr :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
shiftr a n =
  Peano.nat_iter n Div2.div2 a

bitwise :: (Datatypes.Coq_bool -> Datatypes.Coq_bool -> Datatypes.Coq_bool)
           -> Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat ->
           Datatypes.Coq_nat
bitwise op n a b =
  case n of {
   Datatypes.O -> Datatypes.O;
   Datatypes.S n' ->
    Peano.plus
      (case op (odd a) (odd b) of {
        Datatypes.Coq_true -> Datatypes.S Datatypes.O;
        Datatypes.Coq_false -> Datatypes.O})
      (Peano.mult (Datatypes.S (Datatypes.S Datatypes.O))
        (bitwise op n' (Div2.div2 a) (Div2.div2 b)))}

land :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
land a b =
  bitwise Datatypes.andb a a b

lor :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
lor a b =
  bitwise Datatypes.orb (Peano.max a b) a b

ldiff :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
ldiff a b =
  bitwise (\b0 b' -> Datatypes.andb b0 (Datatypes.negb b')) a a b

lxor :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
lxor a b =
  bitwise Datatypes.xorb (Peano.max a b) a b

_Nat__recursion :: a1 -> (Datatypes.Coq_nat -> a1 -> a1) -> Datatypes.Coq_nat
                   -> a1
_Nat__recursion =
  Datatypes.nat_rect

type Nat__Coq_t = Datatypes.Coq_nat

_Nat__eqb :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat__eqb =
  EqNat.beq_nat

_Nat__compare :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                 Datatypes.Coq_comparison
_Nat__compare =
  Compare_dec.nat_compare

_Nat__zero :: Datatypes.Coq_nat
_Nat__zero =
  Datatypes.O

_Nat__one :: Datatypes.Coq_nat
_Nat__one =
  Datatypes.S Datatypes.O

_Nat__two :: Datatypes.Coq_nat
_Nat__two =
  Datatypes.S (Datatypes.S Datatypes.O)

_Nat__succ :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__succ x =
  Datatypes.S x

_Nat__pred :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__pred =
  Peano.pred

_Nat__add :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__add =
  Peano.plus

_Nat__sub :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__sub =
  Peano.minus

_Nat__mul :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__mul =
  Peano.mult

_Nat__ltb :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat__ltb =
  ltb

_Nat__leb :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat__leb =
  leb

_Nat__min :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__min =
  Peano.min

_Nat__max :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__max =
  Peano.max

_Nat__eq_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
_Nat__eq_dec =
  Peano_dec.eq_nat_dec

_Nat__even :: Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat__even =
  even

_Nat__odd :: Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat__odd =
  odd

_Nat__pow :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__pow =
  pow

_Nat__square :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__square =
  square

_Nat__log2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__log2 =
  log2

_Nat__sqrt :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__sqrt =
  sqrt

_Nat__div :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__div =
  div

_Nat__modulo :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__modulo =
  modulo

_Nat__gcd :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__gcd =
  gcd

_Nat__testbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_bool
_Nat__testbit =
  testbit

_Nat__shiftl :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__shiftl =
  shiftl

_Nat__shiftr :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__shiftr =
  shiftr

_Nat__lxor :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__lxor =
  lxor

_Nat__land :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__land =
  land

_Nat__lor :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__lor =
  lor

_Nat__ldiff :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__ldiff =
  ldiff

_Nat__div2 :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__div2 =
  Div2.div2

_Nat__sqrt_up :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__sqrt_up a =
  case Compare_dec.nat_compare Datatypes.O a of {
   Datatypes.Lt -> Datatypes.S (sqrt (Peano.pred a));
   _ -> Datatypes.O}

_Nat__log2_up :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__log2_up a =
  case Compare_dec.nat_compare (Datatypes.S Datatypes.O) a of {
   Datatypes.Lt -> Datatypes.S (log2 (Peano.pred a));
   _ -> Datatypes.O}

_Nat__lcm :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__lcm a b =
  Peano.mult a (div b (gcd a b))

_Nat__eqb_spec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Bool.Coq_reflect
_Nat__eqb_spec x y =
  Bool.iff_reflect (EqNat.beq_nat x y)

_Nat__b2n :: Datatypes.Coq_bool -> Datatypes.Coq_nat
_Nat__b2n b =
  case b of {
   Datatypes.Coq_true -> Datatypes.S Datatypes.O;
   Datatypes.Coq_false -> Datatypes.O}

_Nat__setbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__setbit a n =
  lor a (shiftl (Datatypes.S Datatypes.O) n)

_Nat__clearbit :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__clearbit a n =
  ldiff a (shiftl (Datatypes.S Datatypes.O) n)

_Nat__ones :: Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__ones n =
  Peano.pred (shiftl (Datatypes.S Datatypes.O) n)

_Nat__lnot :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_nat
_Nat__lnot a n =
  lxor a (_Nat__ones n)

_Nat__Private_Dec__max_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat
                                      -> (Datatypes.Coq_nat ->
                                      Datatypes.Coq_nat -> () -> a1 -> a1) ->
                                      (() -> a1) -> (() -> a1) -> a1
_Nat__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (Compare_dec.nat_compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (Peano.max n m) __ (hl __);
   _ -> compat m (Peano.max n m) __ (hr __)}

_Nat__Private_Dec__max_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                               (Datatypes.Coq_nat -> Datatypes.Coq_nat -> ()
                               -> a1 -> a1) -> a1 -> a1 -> a1
_Nat__Private_Dec__max_case n m x x0 x1 =
  _Nat__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Nat__Private_Dec__max_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                              Specif.Coq_sumbool
_Nat__Private_Dec__max_dec n m =
  _Nat__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Nat__Private_Dec__min_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat
                                      -> (Datatypes.Coq_nat ->
                                      Datatypes.Coq_nat -> () -> a1 -> a1) ->
                                      (() -> a1) -> (() -> a1) -> a1
_Nat__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (Compare_dec.nat_compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (Peano.min n m) __ (hr __);
   _ -> compat n (Peano.min n m) __ (hl __)}

_Nat__Private_Dec__min_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                               (Datatypes.Coq_nat -> Datatypes.Coq_nat -> ()
                               -> a1 -> a1) -> a1 -> a1 -> a1
_Nat__Private_Dec__min_case n m x x0 x1 =
  _Nat__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Nat__Private_Dec__min_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                              Specif.Coq_sumbool
_Nat__Private_Dec__min_dec n m =
  _Nat__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Nat__max_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> (() -> a1)
                         -> (() -> a1) -> a1
_Nat__max_case_strong n m x x0 =
  _Nat__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Nat__max_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> a1 -> a1 -> a1
_Nat__max_case n m x x0 =
  _Nat__max_case_strong n m (\_ -> x) (\_ -> x0)

_Nat__max_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
_Nat__max_dec =
  _Nat__Private_Dec__max_dec

_Nat__min_case_strong :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> (() -> a1)
                         -> (() -> a1) -> a1
_Nat__min_case_strong n m x x0 =
  _Nat__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Nat__min_case :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> a1 -> a1 -> a1
_Nat__min_case n m x x0 =
  _Nat__min_case_strong n m (\_ -> x) (\_ -> x0)

_Nat__min_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Specif.Coq_sumbool
_Nat__min_dec =
  _Nat__Private_Dec__min_dec

