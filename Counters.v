Require Import Coq.FSets.FMaps.
Require Import Coq.FSets.FSets.
Require Import Coq.Arith.Arith.
Require Import Coq.Structures.OrdersEx.
Require Import Coq.MSets.MSetList.

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

Module ClockMap := FMapList.Make (Nat_as_Legacy_OT).

(* Grow only counter, representing a vector of clocks which are nats. *)

Definition G_Counter_init := ClockMap.empty nat.

Eval compute in ClockMap.find 1 (ClockMap.empty nat).

Function G_Counter_incr actor clocks :=
  match ClockMap.find actor clocks with
    | None => ClockMap.add actor 1 clocks
    | Some count => (ClockMap.add actor (S count) (ClockMap.remove actor clocks))
  end.

Eval compute in ClockMap.find 1 (G_Counter_incr 1 (G_Counter_init)).

Eval compute in ClockMap.find 2 (G_Counter_incr 1 (G_Counter_init)).