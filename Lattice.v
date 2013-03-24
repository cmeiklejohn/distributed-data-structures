Require Export SfLib.
Require Export Max.
Require Export Min.

(** Bounded join-semilattices. *)

Module Lattice.

(** Boolean lattice. *)

Inductive lbool : Type :=
  LBool : bool -> lbool.

Hint Constructors lbool.

Definition lbool_merge lb1 lb2 :=
  match (lb1, lb2) with
    (LBool b1, LBool b2) => (LBool (orb b1 b2))
  end.

Theorem lbool_merge_assoc : forall (b1 b2 b3 : bool),
  lbool_merge (lbool_merge (LBool b1) (LBool b2)) (LBool b3) = 
    lbool_merge (LBool b3) (lbool_merge (LBool b1) (LBool b2)).
Proof with eauto.
  induction b1; induction b2; induction b3...
Qed.

Theorem lbool_merge_comm : forall (b1 b2 : bool),
  lbool_merge (LBool b1) (LBool b2) = lbool_merge (LBool b2) (LBool b1).
Proof with eauto.
  induction b1; induction b2...
Qed.

(** Max value lattice *)

Inductive lmax : Type :=
  LMax : nat -> lmax.

Hint Constructors lmax.

Definition lmax_merge lm1 lm2 :=
  match (lm1, lm2) with
    (LMax n1, LMax n2) => (LMax (max n1 n2))
  end.

Theorem lmax_merge_assoc : forall (n1 n2 n3 : nat),
  lmax_merge (lmax_merge (LMax n1) (LMax n2)) (LMax n3) =
    lmax_merge (LMax n3) (lmax_merge (LMax n1) (LMax n2)).
Proof with eauto.
  induction n1; induction n2; induction n3; try unfold lmax_merge; try eauto;
  rewrite max_comm; reflexivity.
Qed.

Theorem lmax_merge_comm : forall (n1 n2 : nat),
  lmax_merge (LMax n1) (LMax n2) = lmax_merge (LMax n2) (LMax n1).
Proof with eauto.
  induction n1; induction n2...
  try unfold lmax_merge; rewrite max_comm...
Qed.

(** Min value lattice *)

Inductive lmin : Type :=
  LMin : nat -> lmin.

Hint Constructors lmin.

Definition lmin_merge lm1 lm2 :=
  match (lm1, lm2) with
    (LMin n1, LMin n2) => (LMin (min n1 n2))
  end.

Theorem lmin_merge_assoc : forall (n1 n2 n3 : nat),
  lmin_merge (lmin_merge (LMin n1) (LMin n2)) (LMin n3) =
    lmin_merge (LMin n3) (lmin_merge (LMin n1) (LMin n2)).
Proof with eauto.
  induction n1; induction n2; induction n3; try unfold lmin_merge; try eauto;
  rewrite min_comm; reflexivity.
Qed.

Theorem lmin_merge_comm : forall (n1 n2 : nat),
  lmin_merge (LMin n1) (LMin n2) = lmin_merge (LMin n2) (LMin n1).
Proof with eauto.
  induction n1; induction n2...
  try unfold lmin_merge; rewrite min_comm...
Qed.

End Lattice.