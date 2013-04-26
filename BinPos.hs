module BinPos where

import qualified Prelude
import qualified BinNums
import qualified Bool
import qualified Datatypes
import qualified Logic
import qualified Peano
import qualified Specif


__ :: any
__ = Prelude.error "Logical or arity value used"

type Pos__Coq_t = BinNums.Coq_positive

_Pos__succ :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__succ x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xO (_Pos__succ p);
   BinNums.Coq_xO p -> BinNums.Coq_xI p;
   BinNums.Coq_xH -> BinNums.Coq_xO BinNums.Coq_xH}

_Pos__add :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__add x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Pos__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xI (_Pos__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xO (_Pos__succ p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Pos__add p q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Pos__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xI p};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Pos__succ q);
     BinNums.Coq_xO q -> BinNums.Coq_xI q;
     BinNums.Coq_xH -> BinNums.Coq_xO BinNums.Coq_xH}}

_Pos__add_carry :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                   BinNums.Coq_positive
_Pos__add_carry x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Pos__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Pos__add_carry p q);
     BinNums.Coq_xH -> BinNums.Coq_xI (_Pos__succ p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xO (_Pos__add_carry p q);
     BinNums.Coq_xO q -> BinNums.Coq_xI (_Pos__add p q);
     BinNums.Coq_xH -> BinNums.Coq_xO (_Pos__succ p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xI q -> BinNums.Coq_xI (_Pos__succ q);
     BinNums.Coq_xO q -> BinNums.Coq_xO (_Pos__succ q);
     BinNums.Coq_xH -> BinNums.Coq_xI BinNums.Coq_xH}}

_Pos__pred_double :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__pred_double x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xI (BinNums.Coq_xO p);
   BinNums.Coq_xO p -> BinNums.Coq_xI (_Pos__pred_double p);
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Pos__pred :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__pred x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Coq_xO p;
   BinNums.Coq_xO p -> _Pos__pred_double p;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Pos__pred_N :: BinNums.Coq_positive -> BinNums.N
_Pos__pred_N x =
  case x of {
   BinNums.Coq_xI p -> BinNums.Npos (BinNums.Coq_xO p);
   BinNums.Coq_xO p -> BinNums.Npos (_Pos__pred_double p);
   BinNums.Coq_xH -> BinNums.N0}

data Pos__Coq_mask =
   Pos__IsNul
 | Pos__IsPos BinNums.Coq_positive
 | Pos__IsNeg

_Pos__mask_rect :: a1 -> (BinNums.Coq_positive -> a1) -> a1 -> Pos__Coq_mask
                   -> a1
_Pos__mask_rect f f0 f1 m =
  case m of {
   Pos__IsNul -> f;
   Pos__IsPos x -> f0 x;
   Pos__IsNeg -> f1}

_Pos__mask_rec :: a1 -> (BinNums.Coq_positive -> a1) -> a1 -> Pos__Coq_mask
                  -> a1
_Pos__mask_rec =
  _Pos__mask_rect

_Pos__succ_double_mask :: Pos__Coq_mask -> Pos__Coq_mask
_Pos__succ_double_mask x =
  case x of {
   Pos__IsNul -> Pos__IsPos BinNums.Coq_xH;
   Pos__IsPos p -> Pos__IsPos (BinNums.Coq_xI p);
   Pos__IsNeg -> Pos__IsNeg}

_Pos__double_mask :: Pos__Coq_mask -> Pos__Coq_mask
_Pos__double_mask x =
  case x of {
   Pos__IsPos p -> Pos__IsPos (BinNums.Coq_xO p);
   x0 -> x0}

_Pos__double_pred_mask :: BinNums.Coq_positive -> Pos__Coq_mask
_Pos__double_pred_mask x =
  case x of {
   BinNums.Coq_xI p -> Pos__IsPos (BinNums.Coq_xO (BinNums.Coq_xO p));
   BinNums.Coq_xO p -> Pos__IsPos (BinNums.Coq_xO (_Pos__pred_double p));
   BinNums.Coq_xH -> Pos__IsNul}

_Pos__pred_mask :: Pos__Coq_mask -> Pos__Coq_mask
_Pos__pred_mask p =
  case p of {
   Pos__IsPos q ->
    case q of {
     BinNums.Coq_xH -> Pos__IsNul;
     _ -> Pos__IsPos (_Pos__pred q)};
   _ -> Pos__IsNeg}

_Pos__sub_mask :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                  Pos__Coq_mask
_Pos__sub_mask x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Pos__double_mask (_Pos__sub_mask p q);
     BinNums.Coq_xO q -> _Pos__succ_double_mask (_Pos__sub_mask p q);
     BinNums.Coq_xH -> Pos__IsPos (BinNums.Coq_xO p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Pos__succ_double_mask (_Pos__sub_mask_carry p q);
     BinNums.Coq_xO q -> _Pos__double_mask (_Pos__sub_mask p q);
     BinNums.Coq_xH -> Pos__IsPos (_Pos__pred_double p)};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xH -> Pos__IsNul;
     _ -> Pos__IsNeg}}

_Pos__sub_mask_carry :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                        Pos__Coq_mask
_Pos__sub_mask_carry x y =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Pos__succ_double_mask (_Pos__sub_mask_carry p q);
     BinNums.Coq_xO q -> _Pos__double_mask (_Pos__sub_mask p q);
     BinNums.Coq_xH -> Pos__IsPos (_Pos__pred_double p)};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Pos__double_mask (_Pos__sub_mask_carry p q);
     BinNums.Coq_xO q -> _Pos__succ_double_mask (_Pos__sub_mask_carry p q);
     BinNums.Coq_xH -> _Pos__double_pred_mask p};
   BinNums.Coq_xH -> Pos__IsNeg}

_Pos__sub :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__sub x y =
  case _Pos__sub_mask x y of {
   Pos__IsPos z -> z;
   _ -> BinNums.Coq_xH}

_Pos__mul :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__mul x y =
  case x of {
   BinNums.Coq_xI p -> _Pos__add y (BinNums.Coq_xO (_Pos__mul p y));
   BinNums.Coq_xO p -> BinNums.Coq_xO (_Pos__mul p y);
   BinNums.Coq_xH -> y}

_Pos__iter :: BinNums.Coq_positive -> (a1 -> a1) -> a1 -> a1
_Pos__iter n f x =
  case n of {
   BinNums.Coq_xI n' -> f (_Pos__iter n' f (_Pos__iter n' f x));
   BinNums.Coq_xO n' -> _Pos__iter n' f (_Pos__iter n' f x);
   BinNums.Coq_xH -> f x}

_Pos__pow :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__pow x y =
  _Pos__iter y (_Pos__mul x) BinNums.Coq_xH

_Pos__square :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__square p =
  case p of {
   BinNums.Coq_xI p0 -> BinNums.Coq_xI (BinNums.Coq_xO
    (_Pos__add (_Pos__square p0) p0));
   BinNums.Coq_xO p0 -> BinNums.Coq_xO (BinNums.Coq_xO (_Pos__square p0));
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Pos__div2 :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__div2 p =
  case p of {
   BinNums.Coq_xI p0 -> p0;
   BinNums.Coq_xO p0 -> p0;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Pos__div2_up :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__div2_up p =
  case p of {
   BinNums.Coq_xI p0 -> _Pos__succ p0;
   BinNums.Coq_xO p0 -> p0;
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Pos__size_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat
_Pos__size_nat p =
  case p of {
   BinNums.Coq_xI p0 -> Datatypes.S (_Pos__size_nat p0);
   BinNums.Coq_xO p0 -> Datatypes.S (_Pos__size_nat p0);
   BinNums.Coq_xH -> Datatypes.S Datatypes.O}

_Pos__size :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__size p =
  case p of {
   BinNums.Coq_xI p0 -> _Pos__succ (_Pos__size p0);
   BinNums.Coq_xO p0 -> _Pos__succ (_Pos__size p0);
   BinNums.Coq_xH -> BinNums.Coq_xH}

_Pos__compare_cont :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                      Datatypes.Coq_comparison -> Datatypes.Coq_comparison
_Pos__compare_cont x y r =
  case x of {
   BinNums.Coq_xI p ->
    case y of {
     BinNums.Coq_xI q -> _Pos__compare_cont p q r;
     BinNums.Coq_xO q -> _Pos__compare_cont p q Datatypes.Gt;
     BinNums.Coq_xH -> Datatypes.Gt};
   BinNums.Coq_xO p ->
    case y of {
     BinNums.Coq_xI q -> _Pos__compare_cont p q Datatypes.Lt;
     BinNums.Coq_xO q -> _Pos__compare_cont p q r;
     BinNums.Coq_xH -> Datatypes.Gt};
   BinNums.Coq_xH ->
    case y of {
     BinNums.Coq_xH -> r;
     _ -> Datatypes.Lt}}

_Pos__compare :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                 Datatypes.Coq_comparison
_Pos__compare x y =
  _Pos__compare_cont x y Datatypes.Eq

_Pos__min :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__min p p' =
  case _Pos__compare p p' of {
   Datatypes.Gt -> p';
   _ -> p}

_Pos__max :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__max p p' =
  case _Pos__compare p p' of {
   Datatypes.Gt -> p;
   _ -> p'}

_Pos__eqb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             Datatypes.Coq_bool
_Pos__eqb p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Pos__eqb p0 q0;
     _ -> Datatypes.Coq_false};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xO q0 -> _Pos__eqb p0 q0;
     _ -> Datatypes.Coq_false};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xH -> Datatypes.Coq_true;
     _ -> Datatypes.Coq_false}}

_Pos__leb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             Datatypes.Coq_bool
_Pos__leb x y =
  case _Pos__compare x y of {
   Datatypes.Gt -> Datatypes.Coq_false;
   _ -> Datatypes.Coq_true}

_Pos__ltb :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             Datatypes.Coq_bool
_Pos__ltb x y =
  case _Pos__compare x y of {
   Datatypes.Lt -> Datatypes.Coq_true;
   _ -> Datatypes.Coq_false}

_Pos__sqrtrem_step :: (BinNums.Coq_positive -> BinNums.Coq_positive) ->
                      (BinNums.Coq_positive -> BinNums.Coq_positive) ->
                      (Datatypes.Coq_prod BinNums.Coq_positive Pos__Coq_mask)
                      -> Datatypes.Coq_prod BinNums.Coq_positive
                      Pos__Coq_mask
_Pos__sqrtrem_step f g p =
  case p of {
   Datatypes.Coq_pair s y ->
    case y of {
     Pos__IsPos r ->
      let {s' = BinNums.Coq_xI (BinNums.Coq_xO s)} in
      let {r' = g (f r)} in
      case _Pos__leb s' r' of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (BinNums.Coq_xI s)
        (_Pos__sub_mask r' s');
       Datatypes.Coq_false -> Datatypes.Coq_pair (BinNums.Coq_xO s)
        (Pos__IsPos r')};
     _ -> Datatypes.Coq_pair (BinNums.Coq_xO s)
      (_Pos__sub_mask (g (f BinNums.Coq_xH)) (BinNums.Coq_xO (BinNums.Coq_xO
        BinNums.Coq_xH)))}}

_Pos__sqrtrem :: BinNums.Coq_positive -> Datatypes.Coq_prod
                 BinNums.Coq_positive Pos__Coq_mask
_Pos__sqrtrem p =
  case p of {
   BinNums.Coq_xI p0 ->
    case p0 of {
     BinNums.Coq_xI p1 ->
      _Pos__sqrtrem_step (\x -> BinNums.Coq_xI x) (\x -> BinNums.Coq_xI x)
        (_Pos__sqrtrem p1);
     BinNums.Coq_xO p1 ->
      _Pos__sqrtrem_step (\x -> BinNums.Coq_xO x) (\x -> BinNums.Coq_xI x)
        (_Pos__sqrtrem p1);
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH (Pos__IsPos
      (BinNums.Coq_xO BinNums.Coq_xH))};
   BinNums.Coq_xO p0 ->
    case p0 of {
     BinNums.Coq_xI p1 ->
      _Pos__sqrtrem_step (\x -> BinNums.Coq_xI x) (\x -> BinNums.Coq_xO x)
        (_Pos__sqrtrem p1);
     BinNums.Coq_xO p1 ->
      _Pos__sqrtrem_step (\x -> BinNums.Coq_xO x) (\x -> BinNums.Coq_xO x)
        (_Pos__sqrtrem p1);
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH (Pos__IsPos
      BinNums.Coq_xH)};
   BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH Pos__IsNul}

_Pos__sqrt :: BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__sqrt p =
  Datatypes.fst (_Pos__sqrtrem p)

_Pos__gcdn :: Datatypes.Coq_nat -> BinNums.Coq_positive ->
              BinNums.Coq_positive -> BinNums.Coq_positive
_Pos__gcdn n a b =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S n0 ->
    case a of {
     BinNums.Coq_xI a' ->
      case b of {
       BinNums.Coq_xI b' ->
        case _Pos__compare a' b' of {
         Datatypes.Eq -> a;
         Datatypes.Lt -> _Pos__gcdn n0 (_Pos__sub b' a') a;
         Datatypes.Gt -> _Pos__gcdn n0 (_Pos__sub a' b') b};
       BinNums.Coq_xO b0 -> _Pos__gcdn n0 a b0;
       BinNums.Coq_xH -> BinNums.Coq_xH};
     BinNums.Coq_xO a0 ->
      case b of {
       BinNums.Coq_xI p -> _Pos__gcdn n0 a0 b;
       BinNums.Coq_xO b0 -> BinNums.Coq_xO (_Pos__gcdn n0 a0 b0);
       BinNums.Coq_xH -> BinNums.Coq_xH};
     BinNums.Coq_xH -> BinNums.Coq_xH}}

_Pos__gcd :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__gcd a b =
  _Pos__gcdn (Peano.plus (_Pos__size_nat a) (_Pos__size_nat b)) a b

_Pos__ggcdn :: Datatypes.Coq_nat -> BinNums.Coq_positive ->
               BinNums.Coq_positive -> Datatypes.Coq_prod
               BinNums.Coq_positive
               (Datatypes.Coq_prod BinNums.Coq_positive BinNums.Coq_positive)
_Pos__ggcdn n a b =
  case n of {
   Datatypes.O -> Datatypes.Coq_pair BinNums.Coq_xH (Datatypes.Coq_pair a b);
   Datatypes.S n0 ->
    case a of {
     BinNums.Coq_xI a' ->
      case b of {
       BinNums.Coq_xI b' ->
        case _Pos__compare a' b' of {
         Datatypes.Eq -> Datatypes.Coq_pair a (Datatypes.Coq_pair
          BinNums.Coq_xH BinNums.Coq_xH);
         Datatypes.Lt ->
          case _Pos__ggcdn n0 (_Pos__sub b' a') a of {
           Datatypes.Coq_pair g p ->
            case p of {
             Datatypes.Coq_pair ba aa -> Datatypes.Coq_pair g
              (Datatypes.Coq_pair aa (_Pos__add aa (BinNums.Coq_xO ba)))}};
         Datatypes.Gt ->
          case _Pos__ggcdn n0 (_Pos__sub a' b') b of {
           Datatypes.Coq_pair g p ->
            case p of {
             Datatypes.Coq_pair ab bb -> Datatypes.Coq_pair g
              (Datatypes.Coq_pair (_Pos__add bb (BinNums.Coq_xO ab)) bb)}}};
       BinNums.Coq_xO b0 ->
        case _Pos__ggcdn n0 a b0 of {
         Datatypes.Coq_pair g p ->
          case p of {
           Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair g
            (Datatypes.Coq_pair aa (BinNums.Coq_xO bb))}};
       BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
        (Datatypes.Coq_pair a BinNums.Coq_xH)};
     BinNums.Coq_xO a0 ->
      case b of {
       BinNums.Coq_xI p ->
        case _Pos__ggcdn n0 a0 b of {
         Datatypes.Coq_pair g p0 ->
          case p0 of {
           Datatypes.Coq_pair aa bb -> Datatypes.Coq_pair g
            (Datatypes.Coq_pair (BinNums.Coq_xO aa) bb)}};
       BinNums.Coq_xO b0 ->
        case _Pos__ggcdn n0 a0 b0 of {
         Datatypes.Coq_pair g p -> Datatypes.Coq_pair (BinNums.Coq_xO g) p};
       BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH
        (Datatypes.Coq_pair a BinNums.Coq_xH)};
     BinNums.Coq_xH -> Datatypes.Coq_pair BinNums.Coq_xH (Datatypes.Coq_pair
      BinNums.Coq_xH b)}}

_Pos__ggcd :: BinNums.Coq_positive -> BinNums.Coq_positive ->
              Datatypes.Coq_prod BinNums.Coq_positive
              (Datatypes.Coq_prod BinNums.Coq_positive BinNums.Coq_positive)
_Pos__ggcd a b =
  _Pos__ggcdn (Peano.plus (_Pos__size_nat a) (_Pos__size_nat b)) a b

_Pos__coq_Nsucc_double :: BinNums.N -> BinNums.N
_Pos__coq_Nsucc_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos BinNums.Coq_xH;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xI p)}

_Pos__coq_Ndouble :: BinNums.N -> BinNums.N
_Pos__coq_Ndouble n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinNums.Coq_xO p)}

_Pos__lor :: BinNums.Coq_positive -> BinNums.Coq_positive ->
             BinNums.Coq_positive
_Pos__lor p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Coq_xI (_Pos__lor p0 q0);
     BinNums.Coq_xO q0 -> BinNums.Coq_xI (_Pos__lor p0 q0);
     BinNums.Coq_xH -> p};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Coq_xI (_Pos__lor p0 q0);
     BinNums.Coq_xO q0 -> BinNums.Coq_xO (_Pos__lor p0 q0);
     BinNums.Coq_xH -> BinNums.Coq_xI p0};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.Coq_xI q0;
     _ -> q}}

_Pos__land :: BinNums.Coq_positive -> BinNums.Coq_positive -> BinNums.N
_Pos__land p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Pos__coq_Nsucc_double (_Pos__land p0 q0);
     BinNums.Coq_xO q0 -> _Pos__coq_Ndouble (_Pos__land p0 q0);
     BinNums.Coq_xH -> BinNums.Npos BinNums.Coq_xH};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Pos__coq_Ndouble (_Pos__land p0 q0);
     BinNums.Coq_xO q0 -> _Pos__coq_Ndouble (_Pos__land p0 q0);
     BinNums.Coq_xH -> BinNums.N0};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.N0;
     _ -> BinNums.Npos BinNums.Coq_xH}}

_Pos__ldiff :: BinNums.Coq_positive -> BinNums.Coq_positive -> BinNums.N
_Pos__ldiff p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Pos__coq_Ndouble (_Pos__ldiff p0 q0);
     BinNums.Coq_xO q0 -> _Pos__coq_Nsucc_double (_Pos__ldiff p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xO p0)};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Pos__coq_Ndouble (_Pos__ldiff p0 q0);
     BinNums.Coq_xO q0 -> _Pos__coq_Ndouble (_Pos__ldiff p0 q0);
     BinNums.Coq_xH -> BinNums.Npos p};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xO q0 -> BinNums.Npos BinNums.Coq_xH;
     _ -> BinNums.N0}}

_Pos__lxor :: BinNums.Coq_positive -> BinNums.Coq_positive -> BinNums.N
_Pos__lxor p q =
  case p of {
   BinNums.Coq_xI p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Pos__coq_Ndouble (_Pos__lxor p0 q0);
     BinNums.Coq_xO q0 -> _Pos__coq_Nsucc_double (_Pos__lxor p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xO p0)};
   BinNums.Coq_xO p0 ->
    case q of {
     BinNums.Coq_xI q0 -> _Pos__coq_Nsucc_double (_Pos__lxor p0 q0);
     BinNums.Coq_xO q0 -> _Pos__coq_Ndouble (_Pos__lxor p0 q0);
     BinNums.Coq_xH -> BinNums.Npos (BinNums.Coq_xI p0)};
   BinNums.Coq_xH ->
    case q of {
     BinNums.Coq_xI q0 -> BinNums.Npos (BinNums.Coq_xO q0);
     BinNums.Coq_xO q0 -> BinNums.Npos (BinNums.Coq_xI q0);
     BinNums.Coq_xH -> BinNums.N0}}

_Pos__shiftl_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                    BinNums.Coq_positive
_Pos__shiftl_nat p n =
  Peano.nat_iter n (\x -> BinNums.Coq_xO x) p

_Pos__shiftr_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                    BinNums.Coq_positive
_Pos__shiftr_nat p n =
  Peano.nat_iter n _Pos__div2 p

_Pos__shiftl :: BinNums.Coq_positive -> BinNums.N -> BinNums.Coq_positive
_Pos__shiftl p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Pos__iter n0 (\x -> BinNums.Coq_xO x) p}

_Pos__shiftr :: BinNums.Coq_positive -> BinNums.N -> BinNums.Coq_positive
_Pos__shiftr p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Pos__iter n0 _Pos__div2 p}

_Pos__testbit_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat ->
                     Datatypes.Coq_bool
_Pos__testbit_nat p n =
  case p of {
   BinNums.Coq_xI p0 ->
    case n of {
     Datatypes.O -> Datatypes.Coq_true;
     Datatypes.S n' -> _Pos__testbit_nat p0 n'};
   BinNums.Coq_xO p0 ->
    case n of {
     Datatypes.O -> Datatypes.Coq_false;
     Datatypes.S n' -> _Pos__testbit_nat p0 n'};
   BinNums.Coq_xH ->
    case n of {
     Datatypes.O -> Datatypes.Coq_true;
     Datatypes.S n0 -> Datatypes.Coq_false}}

_Pos__testbit :: BinNums.Coq_positive -> BinNums.N -> Datatypes.Coq_bool
_Pos__testbit p n =
  case p of {
   BinNums.Coq_xI p0 ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos n0 -> _Pos__testbit p0 (_Pos__pred_N n0)};
   BinNums.Coq_xO p0 ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_false;
     BinNums.Npos n0 -> _Pos__testbit p0 (_Pos__pred_N n0)};
   BinNums.Coq_xH ->
    case n of {
     BinNums.N0 -> Datatypes.Coq_true;
     BinNums.Npos p0 -> Datatypes.Coq_false}}

_Pos__iter_op :: (a1 -> a1 -> a1) -> BinNums.Coq_positive -> a1 -> a1
_Pos__iter_op op p a =
  case p of {
   BinNums.Coq_xI p0 -> op a (_Pos__iter_op op p0 (op a a));
   BinNums.Coq_xO p0 -> _Pos__iter_op op p0 (op a a);
   BinNums.Coq_xH -> a}

_Pos__to_nat :: BinNums.Coq_positive -> Datatypes.Coq_nat
_Pos__to_nat x =
  _Pos__iter_op Peano.plus x (Datatypes.S Datatypes.O)

_Pos__of_nat :: Datatypes.Coq_nat -> BinNums.Coq_positive
_Pos__of_nat n =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S x ->
    case x of {
     Datatypes.O -> BinNums.Coq_xH;
     Datatypes.S n0 -> _Pos__succ (_Pos__of_nat x)}}

_Pos__of_succ_nat :: Datatypes.Coq_nat -> BinNums.Coq_positive
_Pos__of_succ_nat n =
  case n of {
   Datatypes.O -> BinNums.Coq_xH;
   Datatypes.S x -> _Pos__succ (_Pos__of_succ_nat x)}

_Pos__eq_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                Specif.Coq_sumbool
_Pos__eq_dec x y =
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

_Pos__peano_rect :: a1 -> (BinNums.Coq_positive -> a1 -> a1) ->
                    BinNums.Coq_positive -> a1
_Pos__peano_rect a f p =
  let {
   f2 = _Pos__peano_rect (f BinNums.Coq_xH a) (\p0 x ->
          f (_Pos__succ (BinNums.Coq_xO p0)) (f (BinNums.Coq_xO p0) x))}
  in
  case p of {
   BinNums.Coq_xI q -> f (BinNums.Coq_xO q) (f2 q);
   BinNums.Coq_xO q -> f2 q;
   BinNums.Coq_xH -> a}

_Pos__peano_rec :: a1 -> (BinNums.Coq_positive -> a1 -> a1) ->
                   BinNums.Coq_positive -> a1
_Pos__peano_rec =
  _Pos__peano_rect

data Pos__PeanoView =
   Pos__PeanoOne
 | Pos__PeanoSucc BinNums.Coq_positive Pos__PeanoView

_Pos__coq_PeanoView_rect :: a1 -> (BinNums.Coq_positive -> Pos__PeanoView ->
                            a1 -> a1) -> BinNums.Coq_positive ->
                            Pos__PeanoView -> a1
_Pos__coq_PeanoView_rect f f0 p p0 =
  case p0 of {
   Pos__PeanoOne -> f;
   Pos__PeanoSucc p1 p2 -> f0 p1 p2 (_Pos__coq_PeanoView_rect f f0 p1 p2)}

_Pos__coq_PeanoView_rec :: a1 -> (BinNums.Coq_positive -> Pos__PeanoView ->
                           a1 -> a1) -> BinNums.Coq_positive ->
                           Pos__PeanoView -> a1
_Pos__coq_PeanoView_rec =
  _Pos__coq_PeanoView_rect

_Pos__peanoView_xO :: BinNums.Coq_positive -> Pos__PeanoView ->
                      Pos__PeanoView
_Pos__peanoView_xO p q =
  case q of {
   Pos__PeanoOne -> Pos__PeanoSucc BinNums.Coq_xH Pos__PeanoOne;
   Pos__PeanoSucc p0 q0 -> Pos__PeanoSucc (_Pos__succ (BinNums.Coq_xO p0))
    (Pos__PeanoSucc (BinNums.Coq_xO p0) (_Pos__peanoView_xO p0 q0))}

_Pos__peanoView_xI :: BinNums.Coq_positive -> Pos__PeanoView ->
                      Pos__PeanoView
_Pos__peanoView_xI p q =
  case q of {
   Pos__PeanoOne -> Pos__PeanoSucc (_Pos__succ BinNums.Coq_xH)
    (Pos__PeanoSucc BinNums.Coq_xH Pos__PeanoOne);
   Pos__PeanoSucc p0 q0 -> Pos__PeanoSucc (_Pos__succ (BinNums.Coq_xI p0))
    (Pos__PeanoSucc (BinNums.Coq_xI p0) (_Pos__peanoView_xI p0 q0))}

_Pos__peanoView :: BinNums.Coq_positive -> Pos__PeanoView
_Pos__peanoView p =
  case p of {
   BinNums.Coq_xI p0 -> _Pos__peanoView_xI p0 (_Pos__peanoView p0);
   BinNums.Coq_xO p0 -> _Pos__peanoView_xO p0 (_Pos__peanoView p0);
   BinNums.Coq_xH -> Pos__PeanoOne}

_Pos__coq_PeanoView_iter :: a1 -> (BinNums.Coq_positive -> a1 -> a1) ->
                            BinNums.Coq_positive -> Pos__PeanoView -> a1
_Pos__coq_PeanoView_iter a f p q =
  case q of {
   Pos__PeanoOne -> a;
   Pos__PeanoSucc p0 q0 -> f p0 (_Pos__coq_PeanoView_iter a f p0 q0)}

_Pos__eqb_spec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                  Bool.Coq_reflect
_Pos__eqb_spec x y =
  Bool.iff_reflect (_Pos__eqb x y)

_Pos__switch_Eq :: Datatypes.Coq_comparison -> Datatypes.Coq_comparison ->
                   Datatypes.Coq_comparison
_Pos__switch_Eq c c' =
  case c' of {
   Datatypes.Eq -> c;
   x -> x}

_Pos__mask2cmp :: Pos__Coq_mask -> Datatypes.Coq_comparison
_Pos__mask2cmp p =
  case p of {
   Pos__IsNul -> Datatypes.Eq;
   Pos__IsPos p0 -> Datatypes.Gt;
   Pos__IsNeg -> Datatypes.Lt}

_Pos__leb_spec0 :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                   Bool.Coq_reflect
_Pos__leb_spec0 x y =
  Bool.iff_reflect (_Pos__leb x y)

_Pos__ltb_spec0 :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                   Bool.Coq_reflect
_Pos__ltb_spec0 x y =
  Bool.iff_reflect (_Pos__ltb x y)

_Pos__Private_Dec__max_case_strong :: BinNums.Coq_positive ->
                                      BinNums.Coq_positive ->
                                      (BinNums.Coq_positive ->
                                      BinNums.Coq_positive -> () -> a1 -> a1)
                                      -> (() -> a1) -> (() -> a1) -> a1
_Pos__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (_Pos__max n m) __ (hl __);
   _ -> compat m (_Pos__max n m) __ (hr __)}

_Pos__Private_Dec__max_case :: BinNums.Coq_positive -> BinNums.Coq_positive
                               -> (BinNums.Coq_positive ->
                               BinNums.Coq_positive -> () -> a1 -> a1) -> a1
                               -> a1 -> a1
_Pos__Private_Dec__max_case n m x x0 x1 =
  _Pos__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Pos__Private_Dec__max_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              Specif.Coq_sumbool
_Pos__Private_Dec__max_dec n m =
  _Pos__Private_Dec__max_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Pos__Private_Dec__min_case_strong :: BinNums.Coq_positive ->
                                      BinNums.Coq_positive ->
                                      (BinNums.Coq_positive ->
                                      BinNums.Coq_positive -> () -> a1 -> a1)
                                      -> (() -> a1) -> (() -> a1) -> a1
_Pos__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (_Pos__min n m) __ (hr __);
   _ -> compat n (_Pos__min n m) __ (hl __)}

_Pos__Private_Dec__min_case :: BinNums.Coq_positive -> BinNums.Coq_positive
                               -> (BinNums.Coq_positive ->
                               BinNums.Coq_positive -> () -> a1 -> a1) -> a1
                               -> a1 -> a1
_Pos__Private_Dec__min_case n m x x0 x1 =
  _Pos__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Pos__Private_Dec__min_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                              Specif.Coq_sumbool
_Pos__Private_Dec__min_dec n m =
  _Pos__Private_Dec__min_case n m (\x y _ h0 -> h0) Specif.Coq_left
    Specif.Coq_right

_Pos__max_case_strong :: BinNums.Coq_positive -> BinNums.Coq_positive -> (()
                         -> a1) -> (() -> a1) -> a1
_Pos__max_case_strong n m x x0 =
  _Pos__Private_Dec__max_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Pos__max_case :: BinNums.Coq_positive -> BinNums.Coq_positive -> a1 -> a1 ->
                  a1
_Pos__max_case n m x x0 =
  _Pos__max_case_strong n m (\_ -> x) (\_ -> x0)

_Pos__max_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                 Specif.Coq_sumbool
_Pos__max_dec =
  _Pos__Private_Dec__max_dec

_Pos__min_case_strong :: BinNums.Coq_positive -> BinNums.Coq_positive -> (()
                         -> a1) -> (() -> a1) -> a1
_Pos__min_case_strong n m x x0 =
  _Pos__Private_Dec__min_case_strong n m (\x1 y _ x2 ->
    Logic.eq_rect __ x2 __) x x0

_Pos__min_case :: BinNums.Coq_positive -> BinNums.Coq_positive -> a1 -> a1 ->
                  a1
_Pos__min_case n m x x0 =
  _Pos__min_case_strong n m (\_ -> x) (\_ -> x0)

_Pos__min_dec :: BinNums.Coq_positive -> BinNums.Coq_positive ->
                 Specif.Coq_sumbool
_Pos__min_dec =
  _Pos__Private_Dec__min_dec

