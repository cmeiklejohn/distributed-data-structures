module Datatypes where

import qualified Prelude

__ :: any
__ = Prelude.error "Logical or arity value used"

type Empty_set = () -- empty inductive

coq_Empty_set_rect :: Empty_set -> a1
coq_Empty_set_rect e =
  Prelude.error "absurd case"

coq_Empty_set_rec :: Empty_set -> a1
coq_Empty_set_rec =
  coq_Empty_set_rect

data Coq_unit =
   Coq_tt

unit_rect :: a1 -> Coq_unit -> a1
unit_rect f u =
  f

unit_rec :: a1 -> Coq_unit -> a1
unit_rec =
  unit_rect

data Coq_bool =
   Coq_true
 | Coq_false

bool_rect :: a1 -> a1 -> Coq_bool -> a1
bool_rect f f0 b =
  case b of {
   Coq_true -> f;
   Coq_false -> f0}

bool_rec :: a1 -> a1 -> Coq_bool -> a1
bool_rec =
  bool_rect

andb :: Coq_bool -> Coq_bool -> Coq_bool
andb b1 b2 =
  case b1 of {
   Coq_true -> b2;
   Coq_false -> Coq_false}

orb :: Coq_bool -> Coq_bool -> Coq_bool
orb b1 b2 =
  case b1 of {
   Coq_true -> Coq_true;
   Coq_false -> b2}

implb :: Coq_bool -> Coq_bool -> Coq_bool
implb b1 b2 =
  case b1 of {
   Coq_true -> b2;
   Coq_false -> Coq_true}

xorb :: Coq_bool -> Coq_bool -> Coq_bool
xorb b1 b2 =
  case b1 of {
   Coq_true ->
    case b2 of {
     Coq_true -> Coq_false;
     Coq_false -> Coq_true};
   Coq_false -> b2}

negb :: Coq_bool -> Coq_bool
negb b =
  case b of {
   Coq_true -> Coq_false;
   Coq_false -> Coq_true}

eq_true_rect :: a1 -> Coq_bool -> a1
eq_true_rect f b =
  f

eq_true_rec :: a1 -> Coq_bool -> a1
eq_true_rec f b =
  eq_true_rect f b

eq_true_rec_r :: Coq_bool -> a1 -> a1
eq_true_rec_r b h =
  h

eq_true_rect_r :: Coq_bool -> a1 -> a1
eq_true_rect_r b h =
  h

data Coq_nat =
   O
 | S Coq_nat

nat_rect :: a1 -> (Coq_nat -> a1 -> a1) -> Coq_nat -> a1
nat_rect f f0 n =
  case n of {
   O -> f;
   S n0 -> f0 n0 (nat_rect f f0 n0)}

nat_rec :: a1 -> (Coq_nat -> a1 -> a1) -> Coq_nat -> a1
nat_rec =
  nat_rect

data Coq_option a =
   Some a
 | None

option_rect :: (a1 -> a2) -> a2 -> (Coq_option a1) -> a2
option_rect f f0 o =
  case o of {
   Some x -> f x;
   None -> f0}

option_rec :: (a1 -> a2) -> a2 -> (Coq_option a1) -> a2
option_rec =
  option_rect

option_map :: (a1 -> a2) -> (Coq_option a1) -> Coq_option a2
option_map f o =
  case o of {
   Some a -> Some (f a);
   None -> None}

data Coq_sum a b =
   Coq_inl a
 | Coq_inr b

sum_rect :: (a1 -> a3) -> (a2 -> a3) -> (Coq_sum a1 a2) -> a3
sum_rect f f0 s =
  case s of {
   Coq_inl x -> f x;
   Coq_inr x -> f0 x}

sum_rec :: (a1 -> a3) -> (a2 -> a3) -> (Coq_sum a1 a2) -> a3
sum_rec =
  sum_rect

data Coq_prod a b =
   Coq_pair a b

prod_rect :: (a1 -> a2 -> a3) -> (Coq_prod a1 a2) -> a3
prod_rect f p =
  case p of {
   Coq_pair x x0 -> f x x0}

prod_rec :: (a1 -> a2 -> a3) -> (Coq_prod a1 a2) -> a3
prod_rec =
  prod_rect

fst :: (Coq_prod a1 a2) -> a1
fst p =
  case p of {
   Coq_pair x y -> x}

snd :: (Coq_prod a1 a2) -> a2
snd p =
  case p of {
   Coq_pair x y -> y}

prod_uncurry :: ((Coq_prod a1 a2) -> a3) -> a1 -> a2 -> a3
prod_uncurry f x y =
  f (Coq_pair x y)

prod_curry :: (a1 -> a2 -> a3) -> (Coq_prod a1 a2) -> a3
prod_curry f p =
  case p of {
   Coq_pair x y -> f x y}

data Coq_list a =
   Coq_nil
 | Coq_cons a (Coq_list a)

list_rect :: a2 -> (a1 -> (Coq_list a1) -> a2 -> a2) -> (Coq_list a1) -> a2
list_rect f f0 l =
  case l of {
   Coq_nil -> f;
   Coq_cons y l0 -> f0 y l0 (list_rect f f0 l0)}

list_rec :: a2 -> (a1 -> (Coq_list a1) -> a2 -> a2) -> (Coq_list a1) -> a2
list_rec =
  list_rect

length :: (Coq_list a1) -> Coq_nat
length l =
  case l of {
   Coq_nil -> O;
   Coq_cons y l' -> S (length l')}

app :: (Coq_list a1) -> (Coq_list a1) -> Coq_list a1
app l m =
  case l of {
   Coq_nil -> m;
   Coq_cons a l1 -> Coq_cons a (app l1 m)}

data Coq_comparison =
   Eq
 | Lt
 | Gt

comparison_rect :: a1 -> a1 -> a1 -> Coq_comparison -> a1
comparison_rect f f0 f1 c =
  case c of {
   Eq -> f;
   Lt -> f0;
   Gt -> f1}

comparison_rec :: a1 -> a1 -> a1 -> Coq_comparison -> a1
comparison_rec =
  comparison_rect

coq_CompOpp :: Coq_comparison -> Coq_comparison
coq_CompOpp r =
  case r of {
   Eq -> Eq;
   Lt -> Gt;
   Gt -> Lt}

data CompareSpecT =
   CompEqT
 | CompLtT
 | CompGtT

coq_CompareSpecT_rect :: (() -> a1) -> (() -> a1) -> (() -> a1) ->
                         Coq_comparison -> CompareSpecT -> a1
coq_CompareSpecT_rect f f0 f1 c c0 =
  case c0 of {
   CompEqT -> f __;
   CompLtT -> f0 __;
   CompGtT -> f1 __}

coq_CompareSpecT_rec :: (() -> a1) -> (() -> a1) -> (() -> a1) ->
                        Coq_comparison -> CompareSpecT -> a1
coq_CompareSpecT_rec =
  coq_CompareSpecT_rect

coq_CompareSpec2Type :: Coq_comparison -> CompareSpecT
coq_CompareSpec2Type c =
  case c of {
   Eq -> CompEqT;
   Lt -> CompLtT;
   Gt -> CompGtT}

type CompSpecT a = CompareSpecT

coq_CompSpec2Type :: a1 -> a1 -> Coq_comparison -> CompSpecT a1
coq_CompSpec2Type x y c =
  coq_CompareSpec2Type c

data Coq_identity a =
   Coq_identity_refl

identity_rect :: a1 -> a2 -> a1 -> a2
identity_rect a f y =
  f

identity_rec :: a1 -> a2 -> a1 -> a2
identity_rec a f y =
  identity_rect a f y

type ID = () -> () -> ()

id :: a1 -> a1
id x =
  x

