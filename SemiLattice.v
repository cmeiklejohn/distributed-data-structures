Require Export SfLib.
Require Export Max.
Require Export Min.
Require Export Sets.Ensembles.

(** Bounded join-semilattices. *)

Module SemiLattice.

(** Boolean lattice. *)

Inductive lbool : Type :=
  LBool : bool -> lbool.

Hint Constructors lbool.

Definition lbool_merge lb1 lb2 :=
  match (lb1, lb2) with
    (LBool b1, LBool b2) => (LBool (orb b1 b2))
  end.

Theorem lbool_merge_assoc : forall (lb1 lb2 lb3 : lbool),
  lbool_merge (lbool_merge lb1 lb2) lb3 =
    lbool_merge lb3 (lbool_merge lb1 lb2).
Proof with eauto.
  induction lb1; induction lb2; induction lb3...
  unfold lbool_merge; unfold orb.
  destruct b; simpl; destruct b1; try destruct b0; try reflexivity.
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

Theorem lmax_merge_assoc : forall (lm1 lm2 lm3 : lmax),
  lmax_merge (lmax_merge lm1 lm2) lm3 =
    lmax_merge lm3 (lmax_merge lm1 lm2).
Proof with eauto.
  induction lm1; induction lm2; induction lm3; try unfold lmax_merge; try eauto;
  rewrite max_comm; reflexivity.
Qed.

Theorem lmax_merge_comm : forall (lm1 lm2 : lmax),
  lmax_merge lm1 lm2 = lmax_merge lm2 lm1.
Proof with eauto.
  induction lm1; induction lm2...
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

Theorem lmin_merge_assoc : forall (lm1 lm2 lm3 : lmin),
  lmin_merge (lmin_merge lm1 lm2) lm3 =
    lmin_merge lm3 (lmin_merge lm1 lm2).
Proof with eauto.
  induction lm1; induction lm2; induction lm3; try unfold lmin_merge; try eauto;
  rewrite min_comm; reflexivity.
Qed.

Theorem lmin_merge_comm : forall (lm1 lm2 : lmin),
  lmin_merge lm1 lm2 = lmin_merge lm2 lm1.
Proof with eauto.
  induction lm1; induction lm2...
  try unfold lmin_merge; rewrite min_comm...
Qed.

End SemiLattice.
