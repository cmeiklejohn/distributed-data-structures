Require Import Coq.FSets.FMaps.
Require Import Coq.FSets.FSets.
Require Import Coq.Arith.Arith.
Require Import Coq.Structures.OrdersEx.
Require Import Coq.MSets.MSetList.

(* Taken from LambdaJS to provide backwards compatibility b/t Ordered/OrderedType *)
Module UOT_to_OrderedTypeLegacy (UOT:OrderedType) <:
  (IsEqOrig UOT) <: OrderedType.OrderedType.

  Definition t := UOT.t.
  Definition lt := UOT.lt.
  Definition eq := UOT.eq.
  Definition eq_refl := let (r, _, _) := UOT.eq_equiv in r.
  Definition eq_sym := let (_, s, _) := UOT.eq_equiv in s.
  Definition eq_trans := let (_, _, t) := UOT.eq_equiv in t.

  Lemma lt_trans : forall x y z : t, UOT.lt x y -> UOT.lt y z -> UOT.lt x z.
  Proof. destruct UOT.lt_strorder as [i t]. apply t. Qed.

  Lemma lt_not_eq : forall x y : t, UOT.lt x y -> ~ UOT.eq x y.
  Proof. destruct UOT.lt_strorder as [i t]. intros. intro. rewrite H0 in *.
         exact (i y H). Qed.

  Definition compare (x y : t) : OrderedType.Compare UOT.lt UOT.eq x y :=
    match (CompareSpec2Type (UOT.compare_spec x y)) with
      | CompLtT l => OrderedType.LT l
      | CompEqT e => OrderedType.EQ e
      | CompGtT g => OrderedType.GT g
    end.

  Definition eq_dec := UOT.eq_dec.
  Definition eq_equiv := UOT.eq_equiv.
  Definition lt_strorder := UOT.lt_strorder.
  Definition lt_compat := UOT.lt_compat.
End UOT_to_OrderedTypeLegacy.

Module Nat_as_Legacy_OT := UOT_to_OrderedTypeLegacy (Nat_as_OT).

(* Map of clocks, which are nat -> nat. *)

Module ClockMap := FMapWeakList.Make (Nat_as_Legacy_OT).
Module ClockMapFacts := FMapFacts.Facts (ClockMap).

(* Grow only counter, representing a vector of clocks which are nats. *)

(* Initialize an empty G_Counter. *)
Definition G_Counter_init := ClockMap.empty nat.

(* Increment a G_Counter for a particular actor. *)
Definition G_Counter_incr actor clocks :=
  match ClockMap.find actor clocks with
    | None => ClockMap.add actor 1 clocks
    | Some count => (ClockMap.add actor (S count) clocks)
  end.

(* Reveal the current value of a G_Counter. *)
Definition G_Counter_reveal clocks :=
  ClockMap.fold (fun key elt acc => (plus acc elt)) clocks 0.

(* Merge two G_Counters *)
Definition G_Counter_max (n1 n2 : option nat) :=
  match n1, n2 with
    | None, None => None
    | Some n, None => Some n
    | None, Some n => Some n
    | Some n1', Some n2' => Some (max n1' n2')
  end.

Lemma G_Counter_max_comm : forall n1 n2, 
  G_Counter_max n1 n2 = G_Counter_max n2 n1.
Proof.
  intros. destruct n1; destruct n2; auto.
  simpl. f_equal. apply Max.max_comm.
Qed.

Lemma G_Counter_max_idempotent : forall n1, 
  G_Counter_max n1 n1 = n1.
Proof.
  intros. destruct n1; auto; simpl.
  f_equal. apply Max.max_idempotent.
Qed.

Lemma G_Counter_max_assoc : forall n1 n2 n3, 
  G_Counter_max n1 (G_Counter_max n2 n3) = G_Counter_max (G_Counter_max n1 n2) n3.
Proof.
  intros. destruct n1; destruct n2; destruct n3; auto.
  unfold G_Counter_max. f_equal. apply Max.max_assoc.
Qed.

Definition G_Counter_merge c1 c2 :=
  ClockMap.map2 G_Counter_max c2 c1.

Theorem G_Counter_merge_comm : forall c1 c2,
  ClockMap.Equal (G_Counter_merge c1 c2) (G_Counter_merge c2 c1).
Proof.
  intros; unfold G_Counter_merge.
  unfold ClockMap.Equal; intro.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  apply G_Counter_max_comm.
Qed.

Theorem G_Counter_merge_idempotent : forall c1,
  ClockMap.Equal (G_Counter_merge c1 c1) c1.
Proof.
  intros; unfold G_Counter_merge.
  unfold ClockMap.Equal; intro.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  apply G_Counter_max_idempotent.
Qed.

Theorem G_Counter_merge_assoc : forall c1 c2 c3,
  ClockMap.Equal 
    (G_Counter_merge c1 (G_Counter_merge c2 c3))
    (G_Counter_merge (G_Counter_merge c1 c2) c3).
Proof.
  intros; unfold G_Counter_merge.
  unfold ClockMap.Equal; intro.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  repeat rewrite <- G_Counter_max_assoc; reflexivity.
Qed.