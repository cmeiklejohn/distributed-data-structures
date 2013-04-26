module BinInt where

import qualified Prelude
import qualified BinNat
import qualified BinNums
import qualified BinPos
import qualified Bool
import qualified Datatypes
import qualified Logic
import qualified Specif


__ :: any
__ = Prelude.error "Logical or arity value used"

type Z__Coq_t = BinNums.Z

_Z__zero :: BinNums.Z
_Z__zero =
  BinNums.Z0

_Z__one :: BinNums.Z
_Z__one =
  BinNums.Zpos BinNums.Coq_xH

_Z__two :: BinNums.Z
_Z__two =
  BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)

_Z__double :: BinNums.Z -> BinNums.Z
_Z__double x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos (BinNums.Coq_xO p);
   BinNums.Zneg p -> BinNums.Zneg (BinNums.Coq_xO p)}

_Z__succ_double :: BinNums.Z -> BinNums.Z
_Z__succ_double x =
  case x of {
   BinNums.Z0 -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zpos p -> BinNums.Zpos (BinNums.Coq_xI p);
   BinNums.Zneg p -> BinNums.Zneg (BinPos._Pos__pred_double p)}

_Z__pred_double :: BinNums.Z -> BinNums.Z
_Z__pred_double x =
  case x of {
   BinNums.Z0 -> BinNums.Zneg BinNums.Coq_xH;
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__pred_double p);
   BinNums.Zneg p -> BinNums.Zneg (BinNums.Coq_xI p)}

_Z__pos_sub :: BinNums.Coq_positive -> BinNums.Coq_positive -> BinNums.Z
_Z__pos_sub x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Z__double (_Z__pos_sub p q);
     BinNums.Coq_xO q -> _Z__succ_double (_Z__pos_sub p q);
     BinNums.Coq_xH -> BinNums.Zpos (BinNums.Coq_xO p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Z__pred_double (_Z__pos_sub p q);
     BinNums.Coq_xO q -> _Z__double (_Z__pos_sub p q);
     BinNums.Coq_xH -> BinNums.Zpos (BinPos._Pos__pred_double p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Zneg (BinNums.Coq_xO q);
     BinNums.Coq_xO q -> BinNums.Zneg (BinPos._Pos__pred_double q);
     BinNums.Coq_xH -> BinNums.Z0}}

_Z__add :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__add x y =
  case x of {
   BinNums.Z0 -> y;
   BinNums.Zpos x' ->
    case y of {
     BinNums.Z0 -> x;
     BinNums.Zpos y' -> BinNums.Zpos (BinPos._Pos__add x' y');
     BinNums.Zneg y' -> _Z__pos_sub x' y'};
   BinNums.Zneg x' ->
    case y of {
     BinNums.Z0 -> x;
     BinNums.Zpos y' -> _Z__pos_sub y' x';
     BinNums.Zneg y' -> BinNums.Zneg (BinPos._Pos__add x' y')}}

_Z__opp :: BinNums.Z -> BinNums.Z
_Z__opp x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos x0 -> BinNums.Zneg x0;
   BinNums.Zneg x0 -> BinNums.Zpos x0}

_Z__succ :: BinNums.Z -> BinNums.Z
_Z__succ x =
  _Z__add x (BinNums.Zpos BinNums.Coq_xH)

_Z__pred :: BinNums.Z -> BinNums.Z
_Z__pred x =
  _Z__add x (BinNums.Zneg BinNums.Coq_xH)

_Z__sub :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__sub m n =
  _Z__add m (_Z__opp n)

_Z__mul :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__mul x y =
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

_Z__pow_pos :: BinNums.Z -> BinNums.Coq_positive -> BinNums.Z
_Z__pow_pos z n =
  BinPos._Pos__iter n (_Z__mul z) (BinNums.Zpos BinNums.Coq_xH)

_Z__pow :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__pow x y =
  case y of {
   BinNums.Z0 -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zpos p -> _Z__pow_pos x p;
   BinNums.Zneg p -> BinNums.Z0}

_Z__square :: BinNums.Z -> BinNums.Z
_Z__square x =
  case x of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__square p);
   BinNums.Zneg p -> BinNums.Zpos (BinPos._Pos__square p)}

_Z__compare :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_comparison
_Z__compare x y =
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

_Z__sgn :: BinNums.Z -> BinNums.Z
_Z__sgn z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p -> BinNums.Zpos BinNums.Coq_xH;
   BinNums.Zneg p -> BinNums.Zneg BinNums.Coq_xH}

_Z__leb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z__leb x y =
  case _Z__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Z__ltb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z__ltb x y =
  case _Z__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Z__geb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z__geb x y =
  case _Z__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Z__gtb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z__gtb x y =
  case _Z__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Z__eqb :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z__eqb x y =
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

_Z__max :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__max n m =
  case _Z__compare n m of {
   Datatypes.Lt -> m;
   _ -> n}

_Z__min :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__min n m =
  case _Z__compare n m of {
   Datatypes.Gt -> m;
   _ -> n}

_Z__abs :: BinNums.Z -> BinNums.Z
_Z__abs z =
  case z of {
   BinNums.Zneg p -> BinNums.Zpos p;
   x -> x}

_Z__abs_nat :: BinNums.Z -> Datatypes.Coq_nat
_Z__abs_nat z =
  case z of {
   BinNums.Z0 -> Datatypes.O;
   BinNums.Zpos p -> BinPos._Pos__to_nat p;
   BinNums.Zneg p -> BinPos._Pos__to_nat p}

_Z__abs_N :: BinNums.Z -> BinNums.N
_Z__abs_N z =
  case z of {
   BinNums.Z0 -> BinNums.N0;
   BinNums.Zpos p -> BinNums.Npos p;
   BinNums.Zneg p -> BinNums.Npos p}

_Z__to_nat :: BinNums.Z -> Datatypes.Coq_nat
_Z__to_nat z =
  case z of {
   BinNums.Zpos p -> BinPos._Pos__to_nat p;
   _ -> Datatypes.O}

_Z__to_N :: BinNums.Z -> BinNums.N
_Z__to_N z =
  case z of {
   BinNums.Zpos p -> BinNums.Npos p;
   _ -> BinNums.N0}

_Z__of_nat :: Datatypes.Coq_nat -> BinNums.Z
_Z__of_nat n =
  case n of {
   Datatypes.O -> BinNums.Z0;
   Datatypes.S n0 -> BinNums.Zpos (BinPos._Pos__of_succ_nat n0)}

_Z__of_N :: BinNums.N -> BinNums.Z
_Z__of_N n =
  case n of {
   BinNums.N0 -> BinNums.Z0;
   BinNums.Npos p -> BinNums.Zpos p}

_Z__to_pos :: BinNums.Z -> BinNums.Coq_positive
_Z__to_pos z =
  case z of {
   BinNums.Zpos p -> p;
   _ -> BinNums.Coq_xH}

_Z__iter :: BinNums.Z -> (a1 -> a1) -> a1 -> a1
_Z__iter n f x =
  case n of {
   BinNums.Zpos p -> BinPos._Pos__iter p f x;
   _ -> x}

_Z__pos_div_eucl :: BinNums.Coq_positive -> BinNums.Z -> Datatypes.Coq_prod
                    BinNums.Z BinNums.Z
_Z__pos_div_eucl a b =
  case a of {
   BinNums.Coq_xI a' ->
    case _Z__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {
       r' = _Z__add
              (_Z__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) r)
              (BinNums.Zpos BinNums.Coq_xH)}
      in
      case _Z__ltb r' b of {
       Datatypes.Coq_true -> Datatypes.Coq_pair
        (_Z__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q) r';
       Datatypes.Coq_false -> Datatypes.Coq_pair
        (_Z__add (_Z__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q)
          (BinNums.Zpos BinNums.Coq_xH)) (_Z__sub r' b)}};
   BinNums.Coq_xO a' ->
    case _Z__pos_div_eucl a' b of {
     Datatypes.Coq_pair q r ->
      let {r' = _Z__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) r} in
      case _Z__ltb r' b of {
       Datatypes.Coq_true -> Datatypes.Coq_pair
        (_Z__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q) r';
       Datatypes.Coq_false -> Datatypes.Coq_pair
        (_Z__add (_Z__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) q)
          (BinNums.Zpos BinNums.Coq_xH)) (_Z__sub r' b)}};
   BinNums.Coq_xH ->
    case _Z__leb (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH)) b of {
     Datatypes.Coq_true -> Datatypes.Coq_pair BinNums.Z0 (BinNums.Zpos
      BinNums.Coq_xH);
     Datatypes.Coq_false -> Datatypes.Coq_pair (BinNums.Zpos BinNums.Coq_xH)
      BinNums.Z0}}

_Z__div_eucl :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
                BinNums.Z
_Z__div_eucl a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
   BinNums.Zpos a' ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
     BinNums.Zpos p -> _Z__pos_div_eucl a' b;
     BinNums.Zneg b' ->
      case _Z__pos_div_eucl a' (BinNums.Zpos b') of {
       Datatypes.Coq_pair q r ->
        case r of {
         BinNums.Z0 -> Datatypes.Coq_pair (_Z__opp q) BinNums.Z0;
         _ -> Datatypes.Coq_pair
          (_Z__opp (_Z__add q (BinNums.Zpos BinNums.Coq_xH))) (_Z__add b r)}}};
   BinNums.Zneg a' ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
     BinNums.Zpos p ->
      case _Z__pos_div_eucl a' b of {
       Datatypes.Coq_pair q r ->
        case r of {
         BinNums.Z0 -> Datatypes.Coq_pair (_Z__opp q) BinNums.Z0;
         _ -> Datatypes.Coq_pair
          (_Z__opp (_Z__add q (BinNums.Zpos BinNums.Coq_xH))) (_Z__sub b r)}};
     BinNums.Zneg b' ->
      case _Z__pos_div_eucl a' (BinNums.Zpos b') of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair q (_Z__opp r)}}}

_Z__div :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__div a b =
  case _Z__div_eucl a b of {
   Datatypes.Coq_pair q x -> q}

_Z__modulo :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__modulo a b =
  case _Z__div_eucl a b of {
   Datatypes.Coq_pair x r -> r}

_Z__quotrem :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
               BinNums.Z
_Z__quotrem a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 a;
     BinNums.Zpos b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z__of_N q) (_Z__of_N r)};
     BinNums.Zneg b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z__opp (_Z__of_N q))
        (_Z__of_N r)}};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair BinNums.Z0 a;
     BinNums.Zpos b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z__opp (_Z__of_N q))
        (_Z__opp (_Z__of_N r))};
     BinNums.Zneg b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       Datatypes.Coq_pair q r -> Datatypes.Coq_pair (_Z__of_N q)
        (_Z__opp (_Z__of_N r))}}}

_Z__quot :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__quot a b =
  Datatypes.fst (_Z__quotrem a b)

_Z__rem :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__rem a b =
  Datatypes.snd (_Z__quotrem a b)

_Z__even :: BinNums.Z -> Datatypes.Coq_bool
_Z__even z =
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

_Z__odd :: BinNums.Z -> Datatypes.Coq_bool
_Z__odd z =
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

_Z__div2 :: BinNums.Z -> BinNums.Z
_Z__div2 z =
  case z of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos p ->
    case p of {
     BinNums.Coq_xH -> BinNums.Z0;
     _ -> BinNums.Zpos (BinPos._Pos__div2 p)};
   BinNums.Zneg p -> BinNums.Zneg (BinPos._Pos__div2_up p)}

_Z__quot2 :: BinNums.Z -> BinNums.Z
_Z__quot2 z =
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

_Z__log2 :: BinNums.Z -> BinNums.Z
_Z__log2 z =
  case z of {
   BinNums.Zpos p0 ->
    case p0 of {
     BinNums.Coq_xI p -> BinNums.Zpos (BinPos._Pos__size p);
     BinNums.Coq_xO p -> BinNums.Zpos (BinPos._Pos__size p);
     BinNums.Coq_xH -> BinNums.Z0};
   _ -> BinNums.Z0}

_Z__sqrtrem :: BinNums.Z -> Datatypes.Coq_prod BinNums.Z BinNums.Z
_Z__sqrtrem n =
  case n of {
   BinNums.Zpos p ->
    case BinPos._Pos__sqrtrem p of {
     Datatypes.Coq_pair s m ->
      case m of {
       BinPos.Pos__IsPos r -> Datatypes.Coq_pair (BinNums.Zpos s)
        (BinNums.Zpos r);
       _ -> Datatypes.Coq_pair (BinNums.Zpos s) BinNums.Z0}};
   _ -> Datatypes.Coq_pair BinNums.Z0 BinNums.Z0}

_Z__sqrt :: BinNums.Z -> BinNums.Z
_Z__sqrt n =
  case n of {
   BinNums.Zpos p -> BinNums.Zpos (BinPos._Pos__sqrt p);
   _ -> BinNums.Z0}

_Z__gcd :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__gcd a b =
  case a of {
   BinNums.Z0 -> _Z__abs b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> _Z__abs a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0);
     BinNums.Zneg b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0)};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> _Z__abs a;
     BinNums.Zpos b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0);
     BinNums.Zneg b0 -> BinNums.Zpos (BinPos._Pos__gcd a0 b0)}}

_Z__ggcd :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_prod BinNums.Z
            (Datatypes.Coq_prod BinNums.Z BinNums.Z)
_Z__ggcd a b =
  case a of {
   BinNums.Z0 -> Datatypes.Coq_pair (_Z__abs b) (Datatypes.Coq_pair
    BinNums.Z0 (_Z__sgn b));
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> Datatypes.Coq_pair (_Z__abs a) (Datatypes.Coq_pair
      (_Z__sgn a) BinNums.Z0);
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
     BinNums.Z0 -> Datatypes.Coq_pair (_Z__abs a) (Datatypes.Coq_pair
      (_Z__sgn a) BinNums.Z0);
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

_Z__testbit :: BinNums.Z -> BinNums.Z -> Datatypes.Coq_bool
_Z__testbit a n =
  case n of {
   BinNums.Z0 -> _Z__odd a;
   BinNums.Zpos p ->
    case a of {
     BinNums.Z0 -> Datatypes.Coq_false;
     BinNums.Zpos a0 -> BinPos._Pos__testbit a0 (BinNums.Npos p);
     BinNums.Zneg a0 ->
      Datatypes.negb
        (BinNat._N__testbit (BinPos._Pos__pred_N a0) (BinNums.Npos p))};
   BinNums.Zneg p -> Datatypes.Coq_false}

_Z__shiftl :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__shiftl a n =
  case n of {
   BinNums.Z0 -> a;
   BinNums.Zpos p ->
    BinPos._Pos__iter p
      (_Z__mul (BinNums.Zpos (BinNums.Coq_xO BinNums.Coq_xH))) a;
   BinNums.Zneg p -> BinPos._Pos__iter p _Z__div2 a}

_Z__shiftr :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__shiftr a n =
  _Z__shiftl a (_Z__opp n)

_Z__lor :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__lor a b =
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

_Z__land :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__land a b =
  case a of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos b0 -> _Z__of_N (BinPos._Pos__land a0 b0);
     BinNums.Zneg b0 ->
      _Z__of_N (BinNat._N__ldiff (BinNums.Npos a0) (BinPos._Pos__pred_N b0))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> BinNums.Z0;
     BinNums.Zpos b0 ->
      _Z__of_N (BinNat._N__ldiff (BinNums.Npos b0) (BinPos._Pos__pred_N a0));
     BinNums.Zneg b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0)))}}

_Z__ldiff :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__ldiff a b =
  case a of {
   BinNums.Z0 -> BinNums.Z0;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> _Z__of_N (BinPos._Pos__ldiff a0 b0);
     BinNums.Zneg b0 ->
      _Z__of_N (BinNat._N__land (BinNums.Npos a0) (BinPos._Pos__pred_N b0))};
   BinNums.Zneg a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> BinNums.Zneg
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinNums.Npos b0)));
     BinNums.Zneg b0 ->
      _Z__of_N
        (BinNat._N__ldiff (BinPos._Pos__pred_N b0) (BinPos._Pos__pred_N a0))}}

_Z__lxor :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__lxor a b =
  case a of {
   BinNums.Z0 -> b;
   BinNums.Zpos a0 ->
    case b of {
     BinNums.Z0 -> a;
     BinNums.Zpos b0 -> _Z__of_N (BinPos._Pos__lxor a0 b0);
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
      _Z__of_N
        (BinNat._N__lxor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0))}}

_Z__eq_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z__eq_dec x y =
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

_Z__leb_spec0 :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z__leb_spec0 x y =
  Bool.iff_reflect (_Z__leb x y)

_Z__ltb_spec0 :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z__ltb_spec0 x y =
  Bool.iff_reflect (_Z__ltb x y)

_Z__sqrt_up :: BinNums.Z -> BinNums.Z
_Z__sqrt_up a =
  case _Z__compare BinNums.Z0 a of {
   Datatypes.Lt -> _Z__succ (_Z__sqrt (_Z__pred a));
   _ -> BinNums.Z0}

_Z__log2_up :: BinNums.Z -> BinNums.Z
_Z__log2_up a =
  case _Z__compare (BinNums.Zpos BinNums.Coq_xH) a of {
   Datatypes.Lt -> _Z__succ (_Z__log2 (_Z__pred a));
   _ -> BinNums.Z0}

_Z__Private_Div__Quot2Div__div :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__Private_Div__Quot2Div__div =
  _Z__quot

_Z__Private_Div__Quot2Div__modulo :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__Private_Div__Quot2Div__modulo =
  _Z__rem

_Z__lcm :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__lcm a b =
  _Z__abs (_Z__mul a (_Z__div b (_Z__gcd a b)))

_Z__eqb_spec :: BinNums.Z -> BinNums.Z -> Bool.Coq_reflect
_Z__eqb_spec x y =
  Bool.iff_reflect (_Z__eqb x y)

_Z__b2z :: Datatypes.Coq_bool -> BinNums.Z
_Z__b2z b =
  case b of {
   Datatypes.Coq_true -> BinNums.Zpos BinNums.Coq_xH;
   Datatypes.Coq_false -> BinNums.Z0}

_Z__setbit :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__setbit a n =
  _Z__lor a (_Z__shiftl (BinNums.Zpos BinNums.Coq_xH) n)

_Z__clearbit :: BinNums.Z -> BinNums.Z -> BinNums.Z
_Z__clearbit a n =
  _Z__ldiff a (_Z__shiftl (BinNums.Zpos BinNums.Coq_xH) n)

_Z__lnot :: BinNums.Z -> BinNums.Z
_Z__lnot a =
  _Z__pred (_Z__opp a)

_Z__ones :: BinNums.Z -> BinNums.Z
_Z__ones n =
  _Z__pred (_Z__shiftl (BinNums.Zpos BinNums.Coq_xH) n)

_Z__Private_Dec__max_case_strong :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                                    BinNums.Z -> () -> a1 -> a1) -> (() ->
                                    a1) -> (() -> a1) -> a1
_Z__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (_Z__max n m) __ (hl __);
   _ -> compat m (_Z__max n m) __ (hr __)}

_Z__Private_Dec__max_case :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                             BinNums.Z -> () -> a1 -> a1) -> a1 -> a1 -> a1
_Z__Private_Dec__max_case n m x x0 x1 =
  _Z__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z__Private_Dec__max_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z__Private_Dec__max_dec n m =
  _Z__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Z__Private_Dec__min_case_strong :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                                    BinNums.Z -> () -> a1 -> a1) -> (() ->
                                    a1) -> (() -> a1) -> a1
_Z__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (_Z__min n m) __ (hr __);
   _ -> compat n (_Z__min n m) __ (hl __)}

_Z__Private_Dec__min_case :: BinNums.Z -> BinNums.Z -> (BinNums.Z ->
                             BinNums.Z -> () -> a1 -> a1) -> a1 -> a1 -> a1
_Z__Private_Dec__min_case n m x x0 x1 =
  _Z__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z__Private_Dec__min_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z__Private_Dec__min_dec n m =
  _Z__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Z__max_case_strong :: BinNums.Z -> BinNums.Z -> (() -> a1) -> (() -> a1) ->
                       a1
_Z__max_case_strong n m x x0 =
  _Z__Private_Dec__max_case_strong n m (\x1 y _ x2 -> Logic.eq_rect __ x2 __)
    x x0

_Z__max_case :: BinNums.Z -> BinNums.Z -> a1 -> a1 -> a1
_Z__max_case n m x x0 =
  _Z__max_case_strong n m (\_ -> x) (\_ -> x0)

_Z__max_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z__max_dec =
  _Z__Private_Dec__max_dec

_Z__min_case_strong :: BinNums.Z -> BinNums.Z -> (() -> a1) -> (() -> a1) ->
                       a1
_Z__min_case_strong n m x x0 =
  _Z__Private_Dec__min_case_strong n m (\x1 y _ x2 -> Logic.eq_rect __ x2 __)
    x x0

_Z__min_case :: BinNums.Z -> BinNums.Z -> a1 -> a1 -> a1
_Z__min_case n m x x0 =
  _Z__min_case_strong n m (\_ -> x) (\_ -> x0)

_Z__min_dec :: BinNums.Z -> BinNums.Z -> Specif.Coq_sumbool
_Z__min_dec =
  _Z__Private_Dec__min_dec

