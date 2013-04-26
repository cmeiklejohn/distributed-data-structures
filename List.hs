module List where

import qualified Prelude
import qualified Datatypes
import qualified Logic
import qualified Specif


__ :: any
__ = Prelude.error "Logical or arity value used"

hd :: a1 -> (Datatypes.Coq_list a1) -> a1
hd default0 l =
  case l of {
   Datatypes.Coq_nil -> default0;
   Datatypes.Coq_cons x l0 -> x}

hd_error :: (Datatypes.Coq_list a1) -> Datatypes.Coq_option a1
hd_error l =
  case l of {
   Datatypes.Coq_nil -> Specif.error;
   Datatypes.Coq_cons x l0 -> Specif.value x}

tl :: (Datatypes.Coq_list a1) -> Datatypes.Coq_list a1
tl l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons a m -> m}

destruct_list :: (Datatypes.Coq_list a1) -> Specif.Coq_sumor
                 (Specif.Coq_sigT a1 (Datatypes.Coq_list a1))
destruct_list l =
  Datatypes.list_rect Specif.Coq_inright (\a tail iHtail -> Specif.Coq_inleft
    (Specif.Coq_existT a tail)) l

in_dec :: (a1 -> a1 -> Specif.Coq_sumbool) -> a1 -> (Datatypes.Coq_list 
          a1) -> Specif.Coq_sumbool
in_dec h a l =
  Datatypes.list_rec Specif.Coq_right (\a0 l0 iHl ->
    let {s = h a0 a} in
    case s of {
     Specif.Coq_left -> Specif.Coq_left;
     Specif.Coq_right -> iHl}) l

nth :: Datatypes.Coq_nat -> (Datatypes.Coq_list a1) -> a1 -> a1
nth n l default0 =
  case n of {
   Datatypes.O ->
    case l of {
     Datatypes.Coq_nil -> default0;
     Datatypes.Coq_cons x l' -> x};
   Datatypes.S m ->
    case l of {
     Datatypes.Coq_nil -> default0;
     Datatypes.Coq_cons x t -> nth m t default0}}

nth_ok :: Datatypes.Coq_nat -> (Datatypes.Coq_list a1) -> a1 ->
          Datatypes.Coq_bool
nth_ok n l default0 =
  case n of {
   Datatypes.O ->
    case l of {
     Datatypes.Coq_nil -> Datatypes.Coq_false;
     Datatypes.Coq_cons x l' -> Datatypes.Coq_true};
   Datatypes.S m ->
    case l of {
     Datatypes.Coq_nil -> Datatypes.Coq_false;
     Datatypes.Coq_cons x t -> nth_ok m t default0}}

nth_in_or_default :: Datatypes.Coq_nat -> (Datatypes.Coq_list a1) -> a1 ->
                     Specif.Coq_sumbool
nth_in_or_default n l d =
  Datatypes.list_rec (\n0 -> Specif.Coq_right) (\a l0 iHl n0 ->
    case n0 of {
     Datatypes.O -> Specif.Coq_left;
     Datatypes.S n1 ->
      Specif.sumbool_rec (\_ -> Specif.Coq_left) (\_ -> Specif.Coq_right)
        (iHl n1)}) l n

nth_error :: (Datatypes.Coq_list a1) -> Datatypes.Coq_nat -> Specif.Exc a1
nth_error l n =
  case n of {
   Datatypes.O ->
    case l of {
     Datatypes.Coq_nil -> Specif.error;
     Datatypes.Coq_cons x l0 -> Specif.value x};
   Datatypes.S n0 ->
    case l of {
     Datatypes.Coq_nil -> Specif.error;
     Datatypes.Coq_cons a l0 -> nth_error l0 n0}}

nth_default :: a1 -> (Datatypes.Coq_list a1) -> Datatypes.Coq_nat -> a1
nth_default default0 l n =
  case nth_error l n of {
   Datatypes.Some x -> x;
   Datatypes.None -> default0}

remove :: (a1 -> a1 -> Specif.Coq_sumbool) -> a1 -> (Datatypes.Coq_list 
          a1) -> Datatypes.Coq_list a1
remove eq_dec x l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons y tl0 ->
    case eq_dec x y of {
     Specif.Coq_left -> remove eq_dec x tl0;
     Specif.Coq_right -> Datatypes.Coq_cons y (remove eq_dec x tl0)}}

last :: (Datatypes.Coq_list a1) -> a1 -> a1
last l d =
  case l of {
   Datatypes.Coq_nil -> d;
   Datatypes.Coq_cons a l0 ->
    case l0 of {
     Datatypes.Coq_nil -> a;
     Datatypes.Coq_cons a0 l1 -> last l0 d}}

removelast :: (Datatypes.Coq_list a1) -> Datatypes.Coq_list a1
removelast l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons a l0 ->
    case l0 of {
     Datatypes.Coq_nil -> Datatypes.Coq_nil;
     Datatypes.Coq_cons a0 l1 -> Datatypes.Coq_cons a (removelast l0)}}

exists_last :: (Datatypes.Coq_list a1) -> Specif.Coq_sigT
               (Datatypes.Coq_list a1) a1
exists_last l =
  Datatypes.list_rect (\_ -> Prelude.error "absurd case") (\a l0 iHl _ ->
    case l0 of {
     Datatypes.Coq_nil -> Specif.Coq_existT Datatypes.Coq_nil a;
     Datatypes.Coq_cons a0 l1 ->
      case iHl __ of {
       Specif.Coq_existT l' s ->
        Logic.eq_rect_r
          (Datatypes.app l' (Datatypes.Coq_cons s Datatypes.Coq_nil))
          (Specif.Coq_existT (Datatypes.Coq_cons a l') s) (Datatypes.Coq_cons
          a0 l1)}}) l __

count_occ :: (a1 -> a1 -> Specif.Coq_sumbool) -> (Datatypes.Coq_list 
             a1) -> a1 -> Datatypes.Coq_nat
count_occ eq_dec l x =
  case l of {
   Datatypes.Coq_nil -> Datatypes.O;
   Datatypes.Coq_cons y tl0 ->
    let {n = count_occ eq_dec tl0 x} in
    case eq_dec y x of {
     Specif.Coq_left -> Datatypes.S n;
     Specif.Coq_right -> n}}

rev :: (Datatypes.Coq_list a1) -> Datatypes.Coq_list a1
rev l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons x l' ->
    Datatypes.app (rev l') (Datatypes.Coq_cons x Datatypes.Coq_nil)}

rev_append :: (Datatypes.Coq_list a1) -> (Datatypes.Coq_list a1) ->
              Datatypes.Coq_list a1
rev_append l l' =
  case l of {
   Datatypes.Coq_nil -> l';
   Datatypes.Coq_cons a l0 -> rev_append l0 (Datatypes.Coq_cons a l')}

rev' :: (Datatypes.Coq_list a1) -> Datatypes.Coq_list a1
rev' l =
  rev_append l Datatypes.Coq_nil

list_eq_dec :: (a1 -> a1 -> Specif.Coq_sumbool) -> (Datatypes.Coq_list 
               a1) -> (Datatypes.Coq_list a1) -> Specif.Coq_sumbool
list_eq_dec eq_dec l l' =
  Datatypes.list_rect (\l'0 ->
    case l'0 of {
     Datatypes.Coq_nil -> Specif.Coq_left;
     Datatypes.Coq_cons a l0 -> Specif.Coq_right}) (\a l0 x l'0 ->
    case l'0 of {
     Datatypes.Coq_nil -> Specif.Coq_right;
     Datatypes.Coq_cons a0 l1 ->
      Specif.sumbool_rec (\_ ->
        Logic.eq_rec_r a0
          (Specif.sumbool_rec (\_ -> Logic.eq_rec_r l1 Specif.Coq_left l0)
            (\_ -> Specif.Coq_right) (x l1)) a) (\_ -> Specif.Coq_right)
        (eq_dec a a0)}) l l'

map :: (a1 -> a2) -> (Datatypes.Coq_list a1) -> Datatypes.Coq_list a2
map f l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons a t -> Datatypes.Coq_cons (f a) (map f t)}

flat_map :: (a1 -> Datatypes.Coq_list a2) -> (Datatypes.Coq_list a1) ->
            Datatypes.Coq_list a2
flat_map f l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons x t -> Datatypes.app (f x) (flat_map f t)}

fold_left :: (a1 -> a2 -> a1) -> (Datatypes.Coq_list a2) -> a1 -> a1
fold_left f l a0 =
  case l of {
   Datatypes.Coq_nil -> a0;
   Datatypes.Coq_cons b t -> fold_left f t (f a0 b)}

fold_right :: (a2 -> a1 -> a1) -> a1 -> (Datatypes.Coq_list a2) -> a1
fold_right f a0 l =
  case l of {
   Datatypes.Coq_nil -> a0;
   Datatypes.Coq_cons b t -> f b (fold_right f a0 t)}

list_power :: (Datatypes.Coq_list a1) -> (Datatypes.Coq_list a2) ->
              Datatypes.Coq_list
              (Datatypes.Coq_list (Datatypes.Coq_prod a1 a2))
list_power l l' =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_cons Datatypes.Coq_nil
    Datatypes.Coq_nil;
   Datatypes.Coq_cons x t ->
    flat_map (\f ->
      map (\y -> Datatypes.Coq_cons (Datatypes.Coq_pair x y) f) l')
      (list_power t l')}

existsb :: (a1 -> Datatypes.Coq_bool) -> (Datatypes.Coq_list a1) ->
           Datatypes.Coq_bool
existsb f l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_false;
   Datatypes.Coq_cons a l0 -> Datatypes.orb (f a) (existsb f l0)}

forallb :: (a1 -> Datatypes.Coq_bool) -> (Datatypes.Coq_list a1) ->
           Datatypes.Coq_bool
forallb f l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_true;
   Datatypes.Coq_cons a l0 -> Datatypes.andb (f a) (forallb f l0)}

filter :: (a1 -> Datatypes.Coq_bool) -> (Datatypes.Coq_list a1) ->
          Datatypes.Coq_list a1
filter f l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons x l0 ->
    case f x of {
     Datatypes.Coq_true -> Datatypes.Coq_cons x (filter f l0);
     Datatypes.Coq_false -> filter f l0}}

find :: (a1 -> Datatypes.Coq_bool) -> (Datatypes.Coq_list a1) ->
        Datatypes.Coq_option a1
find f l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.None;
   Datatypes.Coq_cons x tl0 ->
    case f x of {
     Datatypes.Coq_true -> Datatypes.Some x;
     Datatypes.Coq_false -> find f tl0}}

partition :: (a1 -> Datatypes.Coq_bool) -> (Datatypes.Coq_list a1) ->
             Datatypes.Coq_prod (Datatypes.Coq_list a1)
             (Datatypes.Coq_list a1)
partition f l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_pair Datatypes.Coq_nil
    Datatypes.Coq_nil;
   Datatypes.Coq_cons x tl0 ->
    case partition f tl0 of {
     Datatypes.Coq_pair g d ->
      case f x of {
       Datatypes.Coq_true -> Datatypes.Coq_pair (Datatypes.Coq_cons x g) d;
       Datatypes.Coq_false -> Datatypes.Coq_pair g (Datatypes.Coq_cons x d)}}}

split :: (Datatypes.Coq_list (Datatypes.Coq_prod a1 a2)) ->
         Datatypes.Coq_prod (Datatypes.Coq_list a1) (Datatypes.Coq_list a2)
split l =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_pair Datatypes.Coq_nil
    Datatypes.Coq_nil;
   Datatypes.Coq_cons p tl0 ->
    case p of {
     Datatypes.Coq_pair x y ->
      case split tl0 of {
       Datatypes.Coq_pair g d -> Datatypes.Coq_pair (Datatypes.Coq_cons x g)
        (Datatypes.Coq_cons y d)}}}

combine :: (Datatypes.Coq_list a1) -> (Datatypes.Coq_list a2) ->
           Datatypes.Coq_list (Datatypes.Coq_prod a1 a2)
combine l l' =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons x tl0 ->
    case l' of {
     Datatypes.Coq_nil -> Datatypes.Coq_nil;
     Datatypes.Coq_cons y tl' -> Datatypes.Coq_cons (Datatypes.Coq_pair x y)
      (combine tl0 tl')}}

list_prod :: (Datatypes.Coq_list a1) -> (Datatypes.Coq_list a2) ->
             Datatypes.Coq_list (Datatypes.Coq_prod a1 a2)
list_prod l l' =
  case l of {
   Datatypes.Coq_nil -> Datatypes.Coq_nil;
   Datatypes.Coq_cons x t ->
    Datatypes.app (map (\y -> Datatypes.Coq_pair x y) l') (list_prod t l')}

firstn :: Datatypes.Coq_nat -> (Datatypes.Coq_list a1) -> Datatypes.Coq_list
          a1
firstn n l =
  case n of {
   Datatypes.O -> Datatypes.Coq_nil;
   Datatypes.S n0 ->
    case l of {
     Datatypes.Coq_nil -> Datatypes.Coq_nil;
     Datatypes.Coq_cons a l0 -> Datatypes.Coq_cons a (firstn n0 l0)}}

skipn :: Datatypes.Coq_nat -> (Datatypes.Coq_list a1) -> Datatypes.Coq_list
         a1
skipn n l =
  case n of {
   Datatypes.O -> l;
   Datatypes.S n0 ->
    case l of {
     Datatypes.Coq_nil -> Datatypes.Coq_nil;
     Datatypes.Coq_cons a l0 -> skipn n0 l0}}

seq :: Datatypes.Coq_nat -> Datatypes.Coq_nat -> Datatypes.Coq_list
       Datatypes.Coq_nat
seq start len =
  case len of {
   Datatypes.O -> Datatypes.Coq_nil;
   Datatypes.S len0 -> Datatypes.Coq_cons start
    (seq (Datatypes.S start) len0)}

coq_Forall_rect :: a2 -> (a1 -> (Datatypes.Coq_list a1) -> () -> a2) ->
                   (Datatypes.Coq_list a1) -> a2
coq_Forall_rect h h' l =
  Datatypes.list_rect (\_ -> h) (\a l0 iHl _ -> h' a l0 __) l __

