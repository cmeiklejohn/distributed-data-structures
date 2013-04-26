{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module Counters where

import qualified Prelude
import qualified Compare_dec
import qualified Datatypes
import qualified List
import qualified Logic
import qualified OrderedType
import qualified OrdersEx
import qualified Peano
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

type Nat_as_Legacy_OT__Coq_t = Datatypes.Coq_nat

_Nat_as_Legacy_OT__compare :: Nat_as_Legacy_OT__Coq_t ->
                              Nat_as_Legacy_OT__Coq_t -> OrderedType.Compare
                              Datatypes.Coq_nat
_Nat_as_Legacy_OT__compare x y =
  case Datatypes.coq_CompareSpec2Type (Compare_dec.nat_compare x y) of {
   Datatypes.CompEqT -> OrderedType.EQ;
   Datatypes.CompLtT -> OrderedType.LT;
   Datatypes.CompGtT -> OrderedType.GT}

_Nat_as_Legacy_OT__eq_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                             Specif.Coq_sumbool
_Nat_as_Legacy_OT__eq_dec =
  OrdersEx._Nat_as_OT__eq_dec

type ClockMap__Raw__Coq_key = Datatypes.Coq_nat

type ClockMap__Raw__Coq_t elt =
  Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt)

_ClockMap__Raw__empty :: ClockMap__Raw__Coq_t a1
_ClockMap__Raw__empty =
  Datatypes.Coq_nil

_ClockMap__Raw__is_empty :: (ClockMap__Raw__Coq_t a1) -> Datatypes.Coq_bool
_ClockMap__Raw__is_empty l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_true;
   Datatypes.Coq_cons x x0 -> Datatypes.Coq_false}

_ClockMap__Raw__mem :: ClockMap__Raw__Coq_key -> (ClockMap__Raw__Coq_t 
                       a1) -> Datatypes.Coq_bool
_ClockMap__Raw__mem k s =
  case s of {
   Datatypes.Coq_nil -> Datatypes.Coq_false;
   Datatypes.Coq_cons p l ->
    case p of {
     Datatypes.Coq_pair k' e ->
      case _Nat_as_Legacy_OT__eq_dec k k' of {
       Specif.Coq_left -> Datatypes.Coq_true;
       Specif.Coq_right -> _ClockMap__Raw__mem k l}}}

data ClockMap__Raw__R_mem elt =
   ClockMap__Raw__R_mem_0 (ClockMap__Raw__Coq_t elt)
 | ClockMap__Raw__R_mem_1 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt))
 | ClockMap__Raw__R_mem_2 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt)) 
 Datatypes.Coq_bool (ClockMap__Raw__R_mem elt)

_ClockMap__Raw__coq_R_mem_rect :: ClockMap__Raw__Coq_key ->
                                  ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                                  ((ClockMap__Raw__Coq_t a1) ->
                                  Datatypes.Coq_nat -> a1 ->
                                  (Datatypes.Coq_list
                                  (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                  -> () -> () -> () -> a2) ->
                                  ((ClockMap__Raw__Coq_t a1) ->
                                  Datatypes.Coq_nat -> a1 ->
                                  (Datatypes.Coq_list
                                  (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                  -> () -> () -> () -> Datatypes.Coq_bool ->
                                  (ClockMap__Raw__R_mem a1) -> a2 -> a2) ->
                                  (ClockMap__Raw__Coq_t a1) ->
                                  Datatypes.Coq_bool -> (ClockMap__Raw__R_mem
                                  a1) -> a2
_ClockMap__Raw__coq_R_mem_rect k f f0 f1 s b r =
  case r of {
   ClockMap__Raw__R_mem_0 s0 -> f s0 __;
   ClockMap__Raw__R_mem_1 s0 k' _x l -> f0 s0 k' _x l __ __ __;
   ClockMap__Raw__R_mem_2 s0 k' _x l res r0 ->
    f1 s0 k' _x l __ __ __ res r0
      (_ClockMap__Raw__coq_R_mem_rect k f f0 f1 l res r0)}

_ClockMap__Raw__coq_R_mem_rec :: ClockMap__Raw__Coq_key ->
                                 ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                                 ((ClockMap__Raw__Coq_t a1) ->
                                 Datatypes.Coq_nat -> a1 ->
                                 (Datatypes.Coq_list
                                 (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                 -> () -> () -> () -> a2) ->
                                 ((ClockMap__Raw__Coq_t a1) ->
                                 Datatypes.Coq_nat -> a1 ->
                                 (Datatypes.Coq_list
                                 (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                 -> () -> () -> () -> Datatypes.Coq_bool ->
                                 (ClockMap__Raw__R_mem a1) -> a2 -> a2) ->
                                 (ClockMap__Raw__Coq_t a1) ->
                                 Datatypes.Coq_bool -> (ClockMap__Raw__R_mem
                                 a1) -> a2
_ClockMap__Raw__coq_R_mem_rec k =
  _ClockMap__Raw__coq_R_mem_rect k

_ClockMap__Raw__mem_rect :: ClockMap__Raw__Coq_key -> ((ClockMap__Raw__Coq_t
                            a1) -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                            a1) -> Datatypes.Coq_nat -> a1 ->
                            (Datatypes.Coq_list
                            (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                            -> () -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                            a1) -> Datatypes.Coq_nat -> a1 ->
                            (Datatypes.Coq_list
                            (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                            -> () -> () -> a2 -> a2) -> (ClockMap__Raw__Coq_t
                            a1) -> a2
_ClockMap__Raw__mem_rect k f1 f0 f s =
  Logic.eq_rect_r
    (case s of {
      Datatypes.Coq_nil -> Datatypes.Coq_false;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair k' e ->
         case _Nat_as_Legacy_OT__eq_dec k k' of {
          Specif.Coq_left -> Datatypes.Coq_true;
          Specif.Coq_right -> _ClockMap__Raw__mem k l}}})
    (let {f2 = f1 s} in
     let {f3 = f0 s} in
     let {f4 = f s} in
     case s of {
      Datatypes.Coq_nil -> f2 __;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair t0 e ->
         let {f5 = f4 t0 e l __} in
         let {
          f6 = \_ _ ->
           let {hrec = _ClockMap__Raw__mem_rect k f1 f0 f l} in f5 __ __ hrec}
         in
         let {f7 = f3 t0 e l __} in
         case _Nat_as_Legacy_OT__eq_dec k t0 of {
          Specif.Coq_left -> f7 __ __;
          Specif.Coq_right -> f6 __ __}}}) (_ClockMap__Raw__mem k s)

_ClockMap__Raw__mem_rec :: ClockMap__Raw__Coq_key -> ((ClockMap__Raw__Coq_t
                           a1) -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                           a1) -> Datatypes.Coq_nat -> a1 ->
                           (Datatypes.Coq_list
                           (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                           -> () -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                           a1) -> Datatypes.Coq_nat -> a1 ->
                           (Datatypes.Coq_list
                           (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                           -> () -> () -> a2 -> a2) -> (ClockMap__Raw__Coq_t
                           a1) -> a2
_ClockMap__Raw__mem_rec k =
  _ClockMap__Raw__mem_rect k

_ClockMap__Raw__coq_R_mem_correct :: ClockMap__Raw__Coq_key ->
                                     (ClockMap__Raw__Coq_t a1) ->
                                     Datatypes.Coq_bool ->
                                     ClockMap__Raw__R_mem a1
_ClockMap__Raw__coq_R_mem_correct x x0 res =
  let {princ = \x1 -> _ClockMap__Raw__mem_rect x1} in
  unsafeCoerce princ x (\y _ z _ ->
    Logic.eq_rect_r Datatypes.Coq_false (ClockMap__Raw__R_mem_0 y) z)
    (\y y0 y1 y2 _ _ _ z _ ->
    Logic.eq_rect_r Datatypes.Coq_true (ClockMap__Raw__R_mem_1 y y0 y1 y2) z)
    (\y y0 y1 y2 _ _ _ y6 z _ ->
    Logic.eq_rect_r (_ClockMap__Raw__mem x y2) (ClockMap__Raw__R_mem_2 y y0
      y1 y2 (_ClockMap__Raw__mem x y2) (y6 (_ClockMap__Raw__mem x y2) __)) z)
    x0 res __

_ClockMap__Raw__find :: ClockMap__Raw__Coq_key -> (ClockMap__Raw__Coq_t 
                        a1) -> Datatypes.Coq_option a1
_ClockMap__Raw__find k s =
  case s of {
   Datatypes.Coq_nil -> Datatypes.None;
   Datatypes.Coq_cons p s' ->
    case p of {
     Datatypes.Coq_pair k' x ->
      case _Nat_as_Legacy_OT__eq_dec k k' of {
       Specif.Coq_left -> Datatypes.Some x;
       Specif.Coq_right -> _ClockMap__Raw__find k s'}}}

data ClockMap__Raw__R_find elt =
   ClockMap__Raw__R_find_0 (ClockMap__Raw__Coq_t elt)
 | ClockMap__Raw__R_find_1 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt))
 | ClockMap__Raw__R_find_2 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt)) 
 (Datatypes.Coq_option elt) (ClockMap__Raw__R_find elt)

_ClockMap__Raw__coq_R_find_rect :: ClockMap__Raw__Coq_key ->
                                   ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                                   ((ClockMap__Raw__Coq_t a1) ->
                                   Datatypes.Coq_nat -> a1 ->
                                   (Datatypes.Coq_list
                                   (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                   -> () -> () -> () -> a2) ->
                                   ((ClockMap__Raw__Coq_t a1) ->
                                   Datatypes.Coq_nat -> a1 ->
                                   (Datatypes.Coq_list
                                   (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                   -> () -> () -> () -> (Datatypes.Coq_option
                                   a1) -> (ClockMap__Raw__R_find a1) -> a2 ->
                                   a2) -> (ClockMap__Raw__Coq_t a1) ->
                                   (Datatypes.Coq_option a1) ->
                                   (ClockMap__Raw__R_find a1) -> a2
_ClockMap__Raw__coq_R_find_rect k f f0 f1 s o r =
  case r of {
   ClockMap__Raw__R_find_0 s0 -> f s0 __;
   ClockMap__Raw__R_find_1 s0 k' x s' -> f0 s0 k' x s' __ __ __;
   ClockMap__Raw__R_find_2 s0 k' x s' res r0 ->
    f1 s0 k' x s' __ __ __ res r0
      (_ClockMap__Raw__coq_R_find_rect k f f0 f1 s' res r0)}

_ClockMap__Raw__coq_R_find_rec :: ClockMap__Raw__Coq_key ->
                                  ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                                  ((ClockMap__Raw__Coq_t a1) ->
                                  Datatypes.Coq_nat -> a1 ->
                                  (Datatypes.Coq_list
                                  (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                  -> () -> () -> () -> a2) ->
                                  ((ClockMap__Raw__Coq_t a1) ->
                                  Datatypes.Coq_nat -> a1 ->
                                  (Datatypes.Coq_list
                                  (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                  -> () -> () -> () -> (Datatypes.Coq_option
                                  a1) -> (ClockMap__Raw__R_find a1) -> a2 ->
                                  a2) -> (ClockMap__Raw__Coq_t a1) ->
                                  (Datatypes.Coq_option a1) ->
                                  (ClockMap__Raw__R_find a1) -> a2
_ClockMap__Raw__coq_R_find_rec k =
  _ClockMap__Raw__coq_R_find_rect k

_ClockMap__Raw__find_rect :: ClockMap__Raw__Coq_key -> ((ClockMap__Raw__Coq_t
                             a1) -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                             a1) -> Datatypes.Coq_nat -> a1 ->
                             (Datatypes.Coq_list
                             (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                             -> () -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                             a1) -> Datatypes.Coq_nat -> a1 ->
                             (Datatypes.Coq_list
                             (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                             -> () -> () -> a2 -> a2) ->
                             (ClockMap__Raw__Coq_t a1) -> a2
_ClockMap__Raw__find_rect k f1 f0 f s =
  Logic.eq_rect_r
    (case s of {
      Datatypes.Coq_nil -> Datatypes.None;
      Datatypes.Coq_cons p s' ->
       case p of {
        Datatypes.Coq_pair k' x ->
         case _Nat_as_Legacy_OT__eq_dec k k' of {
          Specif.Coq_left -> Datatypes.Some x;
          Specif.Coq_right -> _ClockMap__Raw__find k s'}}})
    (let {f2 = f1 s} in
     let {f3 = f0 s} in
     let {f4 = f s} in
     case s of {
      Datatypes.Coq_nil -> f2 __;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair t0 e ->
         let {f5 = f4 t0 e l __} in
         let {
          f6 = \_ _ ->
           let {hrec = _ClockMap__Raw__find_rect k f1 f0 f l} in
           f5 __ __ hrec}
         in
         let {f7 = f3 t0 e l __} in
         case _Nat_as_Legacy_OT__eq_dec k t0 of {
          Specif.Coq_left -> f7 __ __;
          Specif.Coq_right -> f6 __ __}}}) (_ClockMap__Raw__find k s)

_ClockMap__Raw__find_rec :: ClockMap__Raw__Coq_key -> ((ClockMap__Raw__Coq_t
                            a1) -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                            a1) -> Datatypes.Coq_nat -> a1 ->
                            (Datatypes.Coq_list
                            (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                            -> () -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                            a1) -> Datatypes.Coq_nat -> a1 ->
                            (Datatypes.Coq_list
                            (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                            -> () -> () -> a2 -> a2) -> (ClockMap__Raw__Coq_t
                            a1) -> a2
_ClockMap__Raw__find_rec k =
  _ClockMap__Raw__find_rect k

_ClockMap__Raw__coq_R_find_correct :: ClockMap__Raw__Coq_key ->
                                      (ClockMap__Raw__Coq_t a1) ->
                                      (Datatypes.Coq_option a1) ->
                                      ClockMap__Raw__R_find a1
_ClockMap__Raw__coq_R_find_correct x x0 res =
  let {princ = \x1 -> _ClockMap__Raw__find_rect x1} in
  unsafeCoerce princ x (\y _ z _ ->
    Logic.eq_rect_r Datatypes.None (ClockMap__Raw__R_find_0 y) z)
    (\y y0 y1 y2 _ _ _ z _ ->
    Logic.eq_rect_r (Datatypes.Some y1) (ClockMap__Raw__R_find_1 y y0 y1 y2)
      z) (\y y0 y1 y2 _ _ _ y6 z _ ->
    Logic.eq_rect_r (_ClockMap__Raw__find x y2) (ClockMap__Raw__R_find_2 y y0
      y1 y2 (_ClockMap__Raw__find x y2) (y6 (_ClockMap__Raw__find x y2) __))
      z) x0 res __

_ClockMap__Raw__add :: ClockMap__Raw__Coq_key -> a1 -> (ClockMap__Raw__Coq_t
                       a1) -> ClockMap__Raw__Coq_t a1
_ClockMap__Raw__add k x s =
  case s of {
   Datatypes.Coq_nil -> Datatypes.Coq_cons (Datatypes.Coq_pair k x)
    Datatypes.Coq_nil;
   Datatypes.Coq_cons p l ->
    case p of {
     Datatypes.Coq_pair k' y ->
      case _Nat_as_Legacy_OT__eq_dec k k' of {
       Specif.Coq_left -> Datatypes.Coq_cons (Datatypes.Coq_pair k x) l;
       Specif.Coq_right -> Datatypes.Coq_cons (Datatypes.Coq_pair k' y)
        (_ClockMap__Raw__add k x l)}}}

data ClockMap__Raw__R_add elt =
   ClockMap__Raw__R_add_0 (ClockMap__Raw__Coq_t elt)
 | ClockMap__Raw__R_add_1 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt))
 | ClockMap__Raw__R_add_2 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt)) 
 (ClockMap__Raw__Coq_t elt) (ClockMap__Raw__R_add elt)

_ClockMap__Raw__coq_R_add_rect :: ClockMap__Raw__Coq_key -> a1 ->
                                  ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                                  ((ClockMap__Raw__Coq_t a1) ->
                                  Datatypes.Coq_nat -> a1 ->
                                  (Datatypes.Coq_list
                                  (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                  -> () -> () -> () -> a2) ->
                                  ((ClockMap__Raw__Coq_t a1) ->
                                  Datatypes.Coq_nat -> a1 ->
                                  (Datatypes.Coq_list
                                  (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                  -> () -> () -> () -> (ClockMap__Raw__Coq_t
                                  a1) -> (ClockMap__Raw__R_add a1) -> a2 ->
                                  a2) -> (ClockMap__Raw__Coq_t a1) ->
                                  (ClockMap__Raw__Coq_t a1) ->
                                  (ClockMap__Raw__R_add a1) -> a2
_ClockMap__Raw__coq_R_add_rect k x f f0 f1 s t r =
  case r of {
   ClockMap__Raw__R_add_0 s0 -> f s0 __;
   ClockMap__Raw__R_add_1 s0 k' y l -> f0 s0 k' y l __ __ __;
   ClockMap__Raw__R_add_2 s0 k' y l res r0 ->
    f1 s0 k' y l __ __ __ res r0
      (_ClockMap__Raw__coq_R_add_rect k x f f0 f1 l res r0)}

_ClockMap__Raw__coq_R_add_rec :: ClockMap__Raw__Coq_key -> a1 ->
                                 ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                                 ((ClockMap__Raw__Coq_t a1) ->
                                 Datatypes.Coq_nat -> a1 ->
                                 (Datatypes.Coq_list
                                 (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                 -> () -> () -> () -> a2) ->
                                 ((ClockMap__Raw__Coq_t a1) ->
                                 Datatypes.Coq_nat -> a1 ->
                                 (Datatypes.Coq_list
                                 (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                 -> () -> () -> () -> (ClockMap__Raw__Coq_t
                                 a1) -> (ClockMap__Raw__R_add a1) -> a2 ->
                                 a2) -> (ClockMap__Raw__Coq_t a1) ->
                                 (ClockMap__Raw__Coq_t a1) ->
                                 (ClockMap__Raw__R_add a1) -> a2
_ClockMap__Raw__coq_R_add_rec k x =
  _ClockMap__Raw__coq_R_add_rect k x

_ClockMap__Raw__add_rect :: ClockMap__Raw__Coq_key -> a1 ->
                            ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                            ((ClockMap__Raw__Coq_t a1) -> Datatypes.Coq_nat
                            -> a1 -> (Datatypes.Coq_list
                            (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                            -> () -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                            a1) -> Datatypes.Coq_nat -> a1 ->
                            (Datatypes.Coq_list
                            (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                            -> () -> () -> a2 -> a2) -> (ClockMap__Raw__Coq_t
                            a1) -> a2
_ClockMap__Raw__add_rect k x f1 f0 f s =
  Logic.eq_rect_r
    (case s of {
      Datatypes.Coq_nil -> Datatypes.Coq_cons (Datatypes.Coq_pair k x)
       Datatypes.Coq_nil;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair k' y ->
         case _Nat_as_Legacy_OT__eq_dec k k' of {
          Specif.Coq_left -> Datatypes.Coq_cons (Datatypes.Coq_pair k x) l;
          Specif.Coq_right -> Datatypes.Coq_cons (Datatypes.Coq_pair k' y)
           (_ClockMap__Raw__add k x l)}}})
    (let {f2 = f1 s} in
     let {f3 = f0 s} in
     let {f4 = f s} in
     case s of {
      Datatypes.Coq_nil -> f2 __;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair t0 e ->
         let {f5 = f4 t0 e l __} in
         let {
          f6 = \_ _ ->
           let {hrec = _ClockMap__Raw__add_rect k x f1 f0 f l} in
           f5 __ __ hrec}
         in
         let {f7 = f3 t0 e l __} in
         case _Nat_as_Legacy_OT__eq_dec k t0 of {
          Specif.Coq_left -> f7 __ __;
          Specif.Coq_right -> f6 __ __}}}) (_ClockMap__Raw__add k x s)

_ClockMap__Raw__add_rec :: ClockMap__Raw__Coq_key -> a1 ->
                           ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                           ((ClockMap__Raw__Coq_t a1) -> Datatypes.Coq_nat ->
                           a1 -> (Datatypes.Coq_list
                           (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                           -> () -> () -> a2) -> ((ClockMap__Raw__Coq_t 
                           a1) -> Datatypes.Coq_nat -> a1 ->
                           (Datatypes.Coq_list
                           (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                           -> () -> () -> a2 -> a2) -> (ClockMap__Raw__Coq_t
                           a1) -> a2
_ClockMap__Raw__add_rec k x =
  _ClockMap__Raw__add_rect k x

_ClockMap__Raw__coq_R_add_correct :: ClockMap__Raw__Coq_key -> a1 ->
                                     (ClockMap__Raw__Coq_t a1) ->
                                     (ClockMap__Raw__Coq_t a1) ->
                                     ClockMap__Raw__R_add a1
_ClockMap__Raw__coq_R_add_correct x x0 x1 res =
  _ClockMap__Raw__add_rect x x0 (\y _ z _ ->
    Logic.eq_rect_r (Datatypes.Coq_cons (Datatypes.Coq_pair x x0)
      Datatypes.Coq_nil) (ClockMap__Raw__R_add_0 y) z)
    (\y y0 y1 y2 _ _ _ z _ ->
    Logic.eq_rect_r (Datatypes.Coq_cons (Datatypes.Coq_pair x x0) y2)
      (ClockMap__Raw__R_add_1 y y0 y1 y2) z) (\y y0 y1 y2 _ _ _ y6 z _ ->
    Logic.eq_rect_r (Datatypes.Coq_cons (Datatypes.Coq_pair y0 y1)
      (_ClockMap__Raw__add x x0 y2)) (ClockMap__Raw__R_add_2 y y0 y1 y2
      (_ClockMap__Raw__add x x0 y2) (y6 (_ClockMap__Raw__add x x0 y2) __)) z)
    x1 res __

_ClockMap__Raw__remove :: ClockMap__Raw__Coq_key -> (ClockMap__Raw__Coq_t 
                          a1) -> ClockMap__Raw__Coq_t a1
_ClockMap__Raw__remove k s =
  case s of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons p l ->
    case p of {
     Datatypes.Coq_pair k' x ->
      case _Nat_as_Legacy_OT__eq_dec k k' of {
       Specif.Coq_left -> l;
       Specif.Coq_right -> Datatypes.Coq_cons (Datatypes.Coq_pair k' x)
        (_ClockMap__Raw__remove k l)}}}

data ClockMap__Raw__R_remove elt =
   ClockMap__Raw__R_remove_0 (ClockMap__Raw__Coq_t elt)
 | ClockMap__Raw__R_remove_1 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt))
 | ClockMap__Raw__R_remove_2 (ClockMap__Raw__Coq_t elt) Datatypes.Coq_nat 
 elt (Datatypes.Coq_list (Datatypes.Coq_prod Datatypes.Coq_nat elt)) 
 (ClockMap__Raw__Coq_t elt) (ClockMap__Raw__R_remove elt)

_ClockMap__Raw__coq_R_remove_rect :: ClockMap__Raw__Coq_key ->
                                     ((ClockMap__Raw__Coq_t a1) -> () -> a2)
                                     -> ((ClockMap__Raw__Coq_t a1) ->
                                     Datatypes.Coq_nat -> a1 ->
                                     (Datatypes.Coq_list
                                     (Datatypes.Coq_prod Datatypes.Coq_nat
                                     a1)) -> () -> () -> () -> a2) ->
                                     ((ClockMap__Raw__Coq_t a1) ->
                                     Datatypes.Coq_nat -> a1 ->
                                     (Datatypes.Coq_list
                                     (Datatypes.Coq_prod Datatypes.Coq_nat
                                     a1)) -> () -> () -> () ->
                                     (ClockMap__Raw__Coq_t a1) ->
                                     (ClockMap__Raw__R_remove a1) -> a2 ->
                                     a2) -> (ClockMap__Raw__Coq_t a1) ->
                                     (ClockMap__Raw__Coq_t a1) ->
                                     (ClockMap__Raw__R_remove a1) -> a2
_ClockMap__Raw__coq_R_remove_rect k f f0 f1 s t r =
  case r of {
   ClockMap__Raw__R_remove_0 s0 -> f s0 __;
   ClockMap__Raw__R_remove_1 s0 k' x l -> f0 s0 k' x l __ __ __;
   ClockMap__Raw__R_remove_2 s0 k' x l res r0 ->
    f1 s0 k' x l __ __ __ res r0
      (_ClockMap__Raw__coq_R_remove_rect k f f0 f1 l res r0)}

_ClockMap__Raw__coq_R_remove_rec :: ClockMap__Raw__Coq_key ->
                                    ((ClockMap__Raw__Coq_t a1) -> () -> a2)
                                    -> ((ClockMap__Raw__Coq_t a1) ->
                                    Datatypes.Coq_nat -> a1 ->
                                    (Datatypes.Coq_list
                                    (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                    -> () -> () -> () -> a2) ->
                                    ((ClockMap__Raw__Coq_t a1) ->
                                    Datatypes.Coq_nat -> a1 ->
                                    (Datatypes.Coq_list
                                    (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                    -> () -> () -> () ->
                                    (ClockMap__Raw__Coq_t a1) ->
                                    (ClockMap__Raw__R_remove a1) -> a2 -> a2)
                                    -> (ClockMap__Raw__Coq_t a1) ->
                                    (ClockMap__Raw__Coq_t a1) ->
                                    (ClockMap__Raw__R_remove a1) -> a2
_ClockMap__Raw__coq_R_remove_rec k =
  _ClockMap__Raw__coq_R_remove_rect k

_ClockMap__Raw__remove_rect :: ClockMap__Raw__Coq_key ->
                               ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                               ((ClockMap__Raw__Coq_t a1) ->
                               Datatypes.Coq_nat -> a1 -> (Datatypes.Coq_list
                               (Datatypes.Coq_prod Datatypes.Coq_nat a1)) ->
                               () -> () -> () -> a2) ->
                               ((ClockMap__Raw__Coq_t a1) ->
                               Datatypes.Coq_nat -> a1 -> (Datatypes.Coq_list
                               (Datatypes.Coq_prod Datatypes.Coq_nat a1)) ->
                               () -> () -> () -> a2 -> a2) ->
                               (ClockMap__Raw__Coq_t a1) -> a2
_ClockMap__Raw__remove_rect k f1 f0 f s =
  Logic.eq_rect_r
    (case s of {
      Datatypes.Coq_nil -> Datatypes.Coq_nil;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair k' x ->
         case _Nat_as_Legacy_OT__eq_dec k k' of {
          Specif.Coq_left -> l;
          Specif.Coq_right -> Datatypes.Coq_cons (Datatypes.Coq_pair k' x)
           (_ClockMap__Raw__remove k l)}}})
    (let {f2 = f1 s} in
     let {f3 = f0 s} in
     let {f4 = f s} in
     case s of {
      Datatypes.Coq_nil -> f2 __;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair t0 e ->
         let {f5 = f4 t0 e l __} in
         let {
          f6 = \_ _ ->
           let {hrec = _ClockMap__Raw__remove_rect k f1 f0 f l} in
           f5 __ __ hrec}
         in
         let {f7 = f3 t0 e l __} in
         case _Nat_as_Legacy_OT__eq_dec k t0 of {
          Specif.Coq_left -> f7 __ __;
          Specif.Coq_right -> f6 __ __}}}) (_ClockMap__Raw__remove k s)

_ClockMap__Raw__remove_rec :: ClockMap__Raw__Coq_key ->
                              ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                              ((ClockMap__Raw__Coq_t a1) -> Datatypes.Coq_nat
                              -> a1 -> (Datatypes.Coq_list
                              (Datatypes.Coq_prod Datatypes.Coq_nat a1)) ->
                              () -> () -> () -> a2) -> ((ClockMap__Raw__Coq_t
                              a1) -> Datatypes.Coq_nat -> a1 ->
                              (Datatypes.Coq_list
                              (Datatypes.Coq_prod Datatypes.Coq_nat a1)) ->
                              () -> () -> () -> a2 -> a2) ->
                              (ClockMap__Raw__Coq_t a1) -> a2
_ClockMap__Raw__remove_rec k =
  _ClockMap__Raw__remove_rect k

_ClockMap__Raw__coq_R_remove_correct :: ClockMap__Raw__Coq_key ->
                                        (ClockMap__Raw__Coq_t a1) ->
                                        (ClockMap__Raw__Coq_t a1) ->
                                        ClockMap__Raw__R_remove a1
_ClockMap__Raw__coq_R_remove_correct x x0 res =
  let {princ = \x1 -> _ClockMap__Raw__remove_rect x1} in
  unsafeCoerce princ x (\y _ z _ ->
    Logic.eq_rect_r Datatypes.Coq_nil (ClockMap__Raw__R_remove_0 y) z)
    (\y y0 y1 y2 _ _ _ z _ ->
    Logic.eq_rect_r y2 (ClockMap__Raw__R_remove_1 y y0 y1 y2) z)
    (\y y0 y1 y2 _ _ _ y6 z _ ->
    Logic.eq_rect_r (Datatypes.Coq_cons (Datatypes.Coq_pair y0 y1)
      (_ClockMap__Raw__remove x y2)) (ClockMap__Raw__R_remove_2 y y0 y1 y2
      (_ClockMap__Raw__remove x y2) (y6 (_ClockMap__Raw__remove x y2) __)) z)
    x0 res __

_ClockMap__Raw__elements :: (ClockMap__Raw__Coq_t a1) -> ClockMap__Raw__Coq_t
                            a1
_ClockMap__Raw__elements m =
  m

_ClockMap__Raw__fold :: (ClockMap__Raw__Coq_key -> a1 -> a2 -> a2) ->
                        (ClockMap__Raw__Coq_t a1) -> a2 -> a2
_ClockMap__Raw__fold f m acc =
  case m of {
   Datatypes.Coq_nil -> acc;
   Datatypes.Coq_cons p m' ->
    case p of {
     Datatypes.Coq_pair k e -> _ClockMap__Raw__fold f m' (f k e acc)}}

data ClockMap__Raw__R_fold elt a =
   ClockMap__Raw__R_fold_0 (ClockMap__Raw__Coq_key -> elt -> a -> a) 
 (ClockMap__Raw__Coq_t elt) a
 | ClockMap__Raw__R_fold_1 (ClockMap__Raw__Coq_key -> elt -> a -> a) 
 (ClockMap__Raw__Coq_t elt) a Datatypes.Coq_nat elt (Datatypes.Coq_list
                                                    (Datatypes.Coq_prod
                                                    Datatypes.Coq_nat 
                                                    elt)) a (ClockMap__Raw__R_fold
                                                            elt a)

_ClockMap__Raw__coq_R_fold_rect :: (() -> (ClockMap__Raw__Coq_key -> a1 -> ()
                                   -> ()) -> (ClockMap__Raw__Coq_t a1) -> ()
                                   -> () -> a2) -> (() ->
                                   (ClockMap__Raw__Coq_key -> a1 -> () -> ())
                                   -> (ClockMap__Raw__Coq_t a1) -> () ->
                                   Datatypes.Coq_nat -> a1 ->
                                   (Datatypes.Coq_list
                                   (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                   -> () -> () -> (ClockMap__Raw__R_fold 
                                   a1 ()) -> a2 -> a2) ->
                                   (ClockMap__Raw__Coq_key -> a1 -> a3 -> a3)
                                   -> (ClockMap__Raw__Coq_t a1) -> a3 -> a3
                                   -> (ClockMap__Raw__R_fold a1 a3) -> a2
_ClockMap__Raw__coq_R_fold_rect f f0 f1 m acc a r =
  case r of {
   ClockMap__Raw__R_fold_0 f2 m0 acc0 -> unsafeCoerce f __ f2 m0 acc0 __;
   ClockMap__Raw__R_fold_1 f2 m0 acc0 k e m' res r0 ->
    unsafeCoerce f0 __ f2 m0 acc0 k e m' __ res r0
      (_ClockMap__Raw__coq_R_fold_rect f f0 f2 m' (f2 k e acc0) res r0)}

_ClockMap__Raw__coq_R_fold_rec :: (() -> (ClockMap__Raw__Coq_key -> a1 -> ()
                                  -> ()) -> (ClockMap__Raw__Coq_t a1) -> ()
                                  -> () -> a2) -> (() ->
                                  (ClockMap__Raw__Coq_key -> a1 -> () -> ())
                                  -> (ClockMap__Raw__Coq_t a1) -> () ->
                                  Datatypes.Coq_nat -> a1 ->
                                  (Datatypes.Coq_list
                                  (Datatypes.Coq_prod Datatypes.Coq_nat a1))
                                  -> () -> () -> (ClockMap__Raw__R_fold 
                                  a1 ()) -> a2 -> a2) ->
                                  (ClockMap__Raw__Coq_key -> a1 -> a3 -> a3)
                                  -> (ClockMap__Raw__Coq_t a1) -> a3 -> a3 ->
                                  (ClockMap__Raw__R_fold a1 a3) -> a2
_ClockMap__Raw__coq_R_fold_rec f f0 f1 m acc a r =
  _ClockMap__Raw__coq_R_fold_rect f f0 f1 m acc a r

_ClockMap__Raw__fold_rect :: (() -> (ClockMap__Raw__Coq_key -> a1 -> () ->
                             ()) -> (ClockMap__Raw__Coq_t a1) -> () -> () ->
                             a2) -> (() -> (ClockMap__Raw__Coq_key -> a1 ->
                             () -> ()) -> (ClockMap__Raw__Coq_t a1) -> () ->
                             Datatypes.Coq_nat -> a1 -> (Datatypes.Coq_list
                             (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                             -> a2 -> a2) -> (ClockMap__Raw__Coq_key -> a1 ->
                             a3 -> a3) -> (ClockMap__Raw__Coq_t a1) -> a3 ->
                             a2
_ClockMap__Raw__fold_rect f0 f f1 m acc =
  Logic.eq_rect_r
    (case m of {
      Datatypes.Coq_nil -> acc;
      Datatypes.Coq_cons p m' ->
       case p of {
        Datatypes.Coq_pair k e -> _ClockMap__Raw__fold f1 m' (f1 k e acc)}})
    (let {f2 = unsafeCoerce f0 __ f1 m acc} in
     let {f3 = unsafeCoerce f __ f1 m acc} in
     case m of {
      Datatypes.Coq_nil -> f2 __;
      Datatypes.Coq_cons p l ->
       case p of {
        Datatypes.Coq_pair t0 e ->
         let {f4 = f3 t0 e l __} in
         let {hrec = _ClockMap__Raw__fold_rect f0 f f1 l (f1 t0 e acc)} in
         f4 hrec}}) (_ClockMap__Raw__fold f1 m acc)

_ClockMap__Raw__fold_rec :: (() -> (ClockMap__Raw__Coq_key -> a1 -> () -> ())
                            -> (ClockMap__Raw__Coq_t a1) -> () -> () -> a2)
                            -> (() -> (ClockMap__Raw__Coq_key -> a1 -> () ->
                            ()) -> (ClockMap__Raw__Coq_t a1) -> () ->
                            Datatypes.Coq_nat -> a1 -> (Datatypes.Coq_list
                            (Datatypes.Coq_prod Datatypes.Coq_nat a1)) -> ()
                            -> a2 -> a2) -> (ClockMap__Raw__Coq_key -> a1 ->
                            a3 -> a3) -> (ClockMap__Raw__Coq_t a1) -> a3 ->
                            a2
_ClockMap__Raw__fold_rec f f0 f1 m acc =
  _ClockMap__Raw__fold_rect f f0 f1 m acc

_ClockMap__Raw__coq_R_fold_correct :: (ClockMap__Raw__Coq_key -> a1 -> a2 ->
                                      a2) -> (ClockMap__Raw__Coq_t a1) -> a2
                                      -> a2 -> ClockMap__Raw__R_fold 
                                      a1 a2
_ClockMap__Raw__coq_R_fold_correct x0 x1 x2 res =
  let {princ = \x x3 -> _ClockMap__Raw__fold_rect x x3} in
  unsafeCoerce princ (\_ y0 y1 y2 _ z _ ->
    Logic.eq_rect_r y2 (ClockMap__Raw__R_fold_0 y0 y1 y2) z)
    (\_ y0 y1 y2 y3 y4 y5 _ y7 z _ ->
    Logic.eq_rect_r (_ClockMap__Raw__fold y0 y5 (y0 y3 y4 y2))
      (ClockMap__Raw__R_fold_1 y0 y1 y2 y3 y4 y5
      (_ClockMap__Raw__fold y0 y5 (y0 y3 y4 y2))
      (y7 (_ClockMap__Raw__fold y0 y5 (y0 y3 y4 y2)) __)) z) x0 x1 x2 res __

_ClockMap__Raw__check :: (a1 -> a1 -> Datatypes.Coq_bool) ->
                         ClockMap__Raw__Coq_key -> a1 ->
                         (ClockMap__Raw__Coq_t a1) -> Datatypes.Coq_bool
_ClockMap__Raw__check cmp k e m' =
  case _ClockMap__Raw__find k m' of {
   Datatypes.Some e' -> cmp e e';
   Datatypes.None -> Datatypes.Coq_false}

_ClockMap__Raw__submap :: (a1 -> a1 -> Datatypes.Coq_bool) ->
                          (ClockMap__Raw__Coq_t a1) -> (ClockMap__Raw__Coq_t
                          a1) -> Datatypes.Coq_bool
_ClockMap__Raw__submap cmp m m' =
  _ClockMap__Raw__fold (\k e b ->
    Datatypes.andb (_ClockMap__Raw__check cmp k e m') b) m Datatypes.Coq_true

_ClockMap__Raw__equal :: (a1 -> a1 -> Datatypes.Coq_bool) ->
                         (ClockMap__Raw__Coq_t a1) -> (ClockMap__Raw__Coq_t
                         a1) -> Datatypes.Coq_bool
_ClockMap__Raw__equal cmp m m' =
  Datatypes.andb (_ClockMap__Raw__submap cmp m m')
    (_ClockMap__Raw__submap (\e' e -> cmp e e') m' m)

_ClockMap__Raw__map :: (a1 -> a2) -> (ClockMap__Raw__Coq_t a1) ->
                       ClockMap__Raw__Coq_t a2
_ClockMap__Raw__map f m =
  case m of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons p m' ->
    case p of {
     Datatypes.Coq_pair k e -> Datatypes.Coq_cons (Datatypes.Coq_pair k
      (f e)) (_ClockMap__Raw__map f m')}}

_ClockMap__Raw__mapi :: (ClockMap__Raw__Coq_key -> a1 -> a2) ->
                        (ClockMap__Raw__Coq_t a1) -> ClockMap__Raw__Coq_t 
                        a2
_ClockMap__Raw__mapi f m =
  case m of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons p m' ->
    case p of {
     Datatypes.Coq_pair k e -> Datatypes.Coq_cons (Datatypes.Coq_pair k
      (f k e)) (_ClockMap__Raw__mapi f m')}}

_ClockMap__Raw__combine_l :: (ClockMap__Raw__Coq_t a1) ->
                             (ClockMap__Raw__Coq_t a2) ->
                             ClockMap__Raw__Coq_t
                             (Datatypes.Coq_prod (Datatypes.Coq_option a1)
                             (Datatypes.Coq_option a2))
_ClockMap__Raw__combine_l m m' =
  _ClockMap__Raw__mapi (\k e -> Datatypes.Coq_pair (Datatypes.Some e)
    (_ClockMap__Raw__find k m')) m

_ClockMap__Raw__combine_r :: (ClockMap__Raw__Coq_t a1) ->
                             (ClockMap__Raw__Coq_t a2) ->
                             ClockMap__Raw__Coq_t
                             (Datatypes.Coq_prod (Datatypes.Coq_option a1)
                             (Datatypes.Coq_option a2))
_ClockMap__Raw__combine_r m m' =
  _ClockMap__Raw__mapi (\k e' -> Datatypes.Coq_pair
    (_ClockMap__Raw__find k m) (Datatypes.Some e')) m'

_ClockMap__Raw__fold_right_pair :: (a1 -> a2 -> a3 -> a3) ->
                                   (Datatypes.Coq_list
                                   (Datatypes.Coq_prod a1 a2)) -> a3 -> a3
_ClockMap__Raw__fold_right_pair f l i =
  List.fold_right (\p -> f (Datatypes.fst p) (Datatypes.snd p)) i l

_ClockMap__Raw__combine :: (ClockMap__Raw__Coq_t a1) -> (ClockMap__Raw__Coq_t
                           a2) -> ClockMap__Raw__Coq_t
                           (Datatypes.Coq_prod (Datatypes.Coq_option a1)
                           (Datatypes.Coq_option a2))
_ClockMap__Raw__combine m m' =
  let {l = _ClockMap__Raw__combine_l m m'} in
  let {r = _ClockMap__Raw__combine_r m m'} in
  _ClockMap__Raw__fold_right_pair _ClockMap__Raw__add l r

_ClockMap__Raw__at_least_left :: (Datatypes.Coq_option a1) ->
                                 (Datatypes.Coq_option a2) ->
                                 Datatypes.Coq_option
                                 (Datatypes.Coq_prod
                                 (Datatypes.Coq_option a1)
                                 (Datatypes.Coq_option a2))
_ClockMap__Raw__at_least_left o o' =
  case o of {
   Datatypes.Some e -> Datatypes.Some (Datatypes.Coq_pair o o');
   Datatypes.None -> Datatypes.None}

_ClockMap__Raw__at_least_right :: (Datatypes.Coq_option a1) ->
                                  (Datatypes.Coq_option a2) ->
                                  Datatypes.Coq_option
                                  (Datatypes.Coq_prod
                                  (Datatypes.Coq_option a1)
                                  (Datatypes.Coq_option a2))
_ClockMap__Raw__at_least_right o o' =
  case o' of {
   Datatypes.Some e -> Datatypes.Some (Datatypes.Coq_pair o o');
   Datatypes.None -> Datatypes.None}

_ClockMap__Raw__at_least_one :: (Datatypes.Coq_option a1) ->
                                (Datatypes.Coq_option a2) ->
                                Datatypes.Coq_option
                                (Datatypes.Coq_prod (Datatypes.Coq_option a1)
                                (Datatypes.Coq_option a2))
_ClockMap__Raw__at_least_one o o' =
  case o of {
   Datatypes.Some e -> Datatypes.Some (Datatypes.Coq_pair o o');
   Datatypes.None ->
    case o' of {
     Datatypes.Some e -> Datatypes.Some (Datatypes.Coq_pair o o');
     Datatypes.None -> Datatypes.None}}

_ClockMap__Raw__option_cons :: ClockMap__Raw__Coq_key ->
                               (Datatypes.Coq_option a1) ->
                               (Datatypes.Coq_list
                               (Datatypes.Coq_prod ClockMap__Raw__Coq_key a1))
                               -> Datatypes.Coq_list
                               (Datatypes.Coq_prod ClockMap__Raw__Coq_key a1)
_ClockMap__Raw__option_cons k o l =
  case o of {
   Datatypes.Some e -> Datatypes.Coq_cons (Datatypes.Coq_pair k e) l;
   Datatypes.None -> l}

_ClockMap__Raw__map2 :: ((Datatypes.Coq_option a1) -> (Datatypes.Coq_option
                        a2) -> Datatypes.Coq_option a3) ->
                        (ClockMap__Raw__Coq_t a1) -> (ClockMap__Raw__Coq_t
                        a2) -> Datatypes.Coq_list
                        (Datatypes.Coq_prod ClockMap__Raw__Coq_key a3)
_ClockMap__Raw__map2 f m m' =
  let {m0 = _ClockMap__Raw__combine m m'} in
  let {
   m1 = _ClockMap__Raw__map (\p -> f (Datatypes.fst p) (Datatypes.snd p)) m0}
  in
  _ClockMap__Raw__fold_right_pair _ClockMap__Raw__option_cons m1
    Datatypes.Coq_nil

_ClockMap__Raw__at_least_one_then_f :: ((Datatypes.Coq_option a1) ->
                                       (Datatypes.Coq_option a2) ->
                                       Datatypes.Coq_option a3) ->
                                       (Datatypes.Coq_option a1) ->
                                       (Datatypes.Coq_option a2) ->
                                       Datatypes.Coq_option a3
_ClockMap__Raw__at_least_one_then_f f o o' =
  case o of {
   Datatypes.Some e -> f o o';
   Datatypes.None ->
    case o' of {
     Datatypes.Some e -> f o o';
     Datatypes.None -> Datatypes.None}}

type ClockMap__E__Coq_t = Datatypes.Coq_nat

_ClockMap__E__compare :: ClockMap__E__Coq_t -> ClockMap__E__Coq_t ->
                         OrderedType.Compare Datatypes.Coq_nat
_ClockMap__E__compare x y =
  case Datatypes.coq_CompareSpec2Type (Compare_dec.nat_compare x y) of {
   Datatypes.CompEqT -> OrderedType.EQ;
   Datatypes.CompLtT -> OrderedType.LT;
   Datatypes.CompGtT -> OrderedType.GT}

_ClockMap__E__eq_dec :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                        Specif.Coq_sumbool
_ClockMap__E__eq_dec =
  OrdersEx._Nat_as_OT__eq_dec

type ClockMap__Coq_key = Datatypes.Coq_nat

type ClockMap__Coq_slist elt =
  ClockMap__Raw__Coq_t elt
  -- singleton inductive, whose constructor was Build_slist
  
_ClockMap__slist_rect :: ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                         (ClockMap__Coq_slist a1) -> a2
_ClockMap__slist_rect f s =
  f s __

_ClockMap__slist_rec :: ((ClockMap__Raw__Coq_t a1) -> () -> a2) ->
                        (ClockMap__Coq_slist a1) -> a2
_ClockMap__slist_rec =
  _ClockMap__slist_rect

_ClockMap__this :: (ClockMap__Coq_slist a1) -> ClockMap__Raw__Coq_t a1
_ClockMap__this s =
  s

type ClockMap__Coq_t elt = ClockMap__Coq_slist elt

_ClockMap__empty :: ClockMap__Coq_t a1
_ClockMap__empty =
  _ClockMap__Raw__empty

_ClockMap__is_empty :: (ClockMap__Coq_t a1) -> Datatypes.Coq_bool
_ClockMap__is_empty m =
  _ClockMap__Raw__is_empty (_ClockMap__this m)

_ClockMap__add :: ClockMap__Coq_key -> a1 -> (ClockMap__Coq_t a1) ->
                  ClockMap__Coq_t a1
_ClockMap__add x e m =
  _ClockMap__Raw__add x e (_ClockMap__this m)

_ClockMap__find :: ClockMap__Coq_key -> (ClockMap__Coq_t a1) ->
                   Datatypes.Coq_option a1
_ClockMap__find x m =
  _ClockMap__Raw__find x (_ClockMap__this m)

_ClockMap__remove :: ClockMap__Coq_key -> (ClockMap__Coq_t a1) ->
                     ClockMap__Coq_t a1
_ClockMap__remove x m =
  _ClockMap__Raw__remove x (_ClockMap__this m)

_ClockMap__mem :: ClockMap__Coq_key -> (ClockMap__Coq_t a1) ->
                  Datatypes.Coq_bool
_ClockMap__mem x m =
  _ClockMap__Raw__mem x (_ClockMap__this m)

_ClockMap__map :: (a1 -> a2) -> (ClockMap__Coq_t a1) -> ClockMap__Coq_t a2
_ClockMap__map f m =
  _ClockMap__Raw__map f (_ClockMap__this m)

_ClockMap__mapi :: (ClockMap__Coq_key -> a1 -> a2) -> (ClockMap__Coq_t 
                   a1) -> ClockMap__Coq_t a2
_ClockMap__mapi f m =
  _ClockMap__Raw__mapi f (_ClockMap__this m)

_ClockMap__map2 :: ((Datatypes.Coq_option a1) -> (Datatypes.Coq_option 
                   a2) -> Datatypes.Coq_option a3) -> (ClockMap__Coq_t 
                   a1) -> (ClockMap__Coq_t a2) -> ClockMap__Coq_t a3
_ClockMap__map2 f m m' =
  _ClockMap__Raw__map2 f (_ClockMap__this m) (_ClockMap__this m')

_ClockMap__elements :: (ClockMap__Coq_t a1) -> Datatypes.Coq_list
                       (Datatypes.Coq_prod ClockMap__Coq_key a1)
_ClockMap__elements m =
  _ClockMap__Raw__elements (_ClockMap__this m)

_ClockMap__cardinal :: (ClockMap__Coq_t a1) -> Datatypes.Coq_nat
_ClockMap__cardinal m =
  Datatypes.length (_ClockMap__this m)

_ClockMap__fold :: (ClockMap__Coq_key -> a1 -> a2 -> a2) -> (ClockMap__Coq_t
                   a1) -> a2 -> a2
_ClockMap__fold f m i =
  _ClockMap__Raw__fold f (_ClockMap__this m) i

_ClockMap__equal :: (a1 -> a1 -> Datatypes.Coq_bool) -> (ClockMap__Coq_t 
                    a1) -> (ClockMap__Coq_t a1) -> Datatypes.Coq_bool
_ClockMap__equal cmp m m' =
  _ClockMap__Raw__equal cmp (_ClockMap__this m) (_ClockMap__this m')

_ClockMapFacts__eqb :: Datatypes.Coq_nat -> Datatypes.Coq_nat ->
                       Datatypes.Coq_bool
_ClockMapFacts__eqb x y =
  case _ClockMap__E__eq_dec x y of {
   Specif.Coq_left -> Datatypes.Coq_true;
   Specif.Coq_right -> Datatypes.Coq_false}

_ClockMapFacts__coq_In_dec :: (ClockMap__Coq_t a1) -> ClockMap__Coq_key ->
                              Specif.Coq_sumbool
_ClockMapFacts__coq_In_dec m x =
  let {b = _ClockMap__mem x m} in
  case b of {
   Datatypes.Coq_true -> Specif.Coq_left;
   Datatypes.Coq_false -> Specif.Coq_right}

_ClockMapFacts__option_map :: (a1 -> a2) -> (Datatypes.Coq_option a1) ->
                              Datatypes.Coq_option a2
_ClockMapFacts__option_map f o =
  case o of {
   Datatypes.Some a -> Datatypes.Some (f a);
   Datatypes.None -> Datatypes.None}

coq_Clock_merge :: (Datatypes.Coq_option Datatypes.Coq_nat) ->
                   (Datatypes.Coq_option Datatypes.Coq_nat) ->
                   Datatypes.Coq_option Datatypes.Coq_nat
coq_Clock_merge n1 n2 =
  case n1 of {
   Datatypes.Some n1' ->
    case n2 of {
     Datatypes.Some n2' -> Datatypes.Some (Peano.max n1' n2');
     Datatypes.None -> Datatypes.Some n1'};
   Datatypes.None -> n2}

coq_G_Counter :: ClockMap__Coq_t Datatypes.Coq_nat
coq_G_Counter =
  _ClockMap__empty

coq_G_Counter_init :: ClockMap__Coq_t Datatypes.Coq_nat
coq_G_Counter_init =
  coq_G_Counter

coq_G_Counter_incr :: ClockMap__Coq_key -> (ClockMap__Coq_t
                      Datatypes.Coq_nat) -> ClockMap__Coq_t Datatypes.Coq_nat
coq_G_Counter_incr actor clocks =
  case _ClockMap__find actor clocks of {
   Datatypes.Some count -> _ClockMap__add actor (Datatypes.S count) clocks;
   Datatypes.None -> _ClockMap__add actor (Datatypes.S Datatypes.O) clocks}

coq_G_Counter_reveal :: (ClockMap__Coq_t Datatypes.Coq_nat) ->
                        Datatypes.Coq_nat
coq_G_Counter_reveal clocks =
  _ClockMap__fold (\key elt acc -> Peano.plus acc elt) clocks Datatypes.O

coq_G_Counter_merge :: (ClockMap__Coq_t Datatypes.Coq_nat) ->
                       (ClockMap__Coq_t Datatypes.Coq_nat) -> ClockMap__Coq_t
                       Datatypes.Coq_nat
coq_G_Counter_merge c1 c2 =
  _ClockMap__map2 coq_Clock_merge c2 c1

coq_PN_Counter :: Datatypes.Coq_prod (ClockMap__Coq_t Datatypes.Coq_nat)
                  (ClockMap__Coq_t Datatypes.Coq_nat)
coq_PN_Counter =
  Datatypes.Coq_pair coq_G_Counter coq_G_Counter

coq_PN_Counter_init :: Datatypes.Coq_prod (ClockMap__Coq_t Datatypes.Coq_nat)
                       (ClockMap__Coq_t Datatypes.Coq_nat)
coq_PN_Counter_init =
  coq_PN_Counter

coq_PN_Counter_incr :: ClockMap__Coq_key -> (Datatypes.Coq_prod
                       (ClockMap__Coq_t Datatypes.Coq_nat)
                       (ClockMap__Coq_t Datatypes.Coq_nat)) ->
                       Datatypes.Coq_prod (ClockMap__Coq_t Datatypes.Coq_nat)
                       (ClockMap__Coq_t Datatypes.Coq_nat)
coq_PN_Counter_incr actor clocks =
  Datatypes.Coq_pair (coq_G_Counter_incr actor (Datatypes.fst clocks))
    (Datatypes.snd clocks)

coq_PN_Counter_decr :: ClockMap__Coq_key -> (Datatypes.Coq_prod
                       (ClockMap__Coq_t Datatypes.Coq_nat)
                       (ClockMap__Coq_t Datatypes.Coq_nat)) ->
                       Datatypes.Coq_prod (ClockMap__Coq_t Datatypes.Coq_nat)
                       (ClockMap__Coq_t Datatypes.Coq_nat)
coq_PN_Counter_decr actor clocks =
  Datatypes.Coq_pair (Datatypes.fst clocks)
    (coq_G_Counter_incr actor (Datatypes.snd clocks))

coq_PN_Counter_reveal :: (Datatypes.Coq_prod
                         (ClockMap__Coq_t Datatypes.Coq_nat)
                         (ClockMap__Coq_t Datatypes.Coq_nat)) ->
                         Datatypes.Coq_nat
coq_PN_Counter_reveal clocks =
  Peano.minus (coq_G_Counter_reveal (Datatypes.fst clocks))
    (coq_G_Counter_reveal (Datatypes.snd clocks))

coq_PN_Counter_merge :: (Datatypes.Coq_prod
                        (ClockMap__Coq_t Datatypes.Coq_nat)
                        (ClockMap__Coq_t Datatypes.Coq_nat)) ->
                        (Datatypes.Coq_prod
                        (ClockMap__Coq_t Datatypes.Coq_nat)
                        (ClockMap__Coq_t Datatypes.Coq_nat)) ->
                        Datatypes.Coq_prod
                        (ClockMap__Coq_t Datatypes.Coq_nat)
                        (ClockMap__Coq_t Datatypes.Coq_nat)
coq_PN_Counter_merge c1 c2 =
  Datatypes.Coq_pair
    (coq_G_Counter_merge (Datatypes.fst c1) (Datatypes.fst c2))
    (coq_G_Counter_merge (Datatypes.snd c1) (Datatypes.snd c2))

