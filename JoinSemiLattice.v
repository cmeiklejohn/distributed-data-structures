Require Export SfLib.
Require Export Max.
Require Export Min.
Require Export Sets.Ensembles.

Set Implicit Arguments.

Module JoinSemiLattice.

Inductive lbool : Type :=
  | LBoolBottom : lbool
  | LBoolValue  : bool -> lbool.

Hint Constructors lbool.

Definition lbool_merge lb1 lb2 :=
  match (lb1, lb2) with
    | (LBoolBottom, LBoolBottom) => LBoolBottom
    | (LBoolBottom, LBoolValue b2) => (LBoolValue b2)
    | (LBoolValue b1, LBoolBottom) => (LBoolValue b1)
    | (LBoolValue b1, LBoolValue b2) => (LBoolValue (orb b1 b2))
  end.

Theorem lbool_merge_assoc : forall (lb1 lb2 lb3 : lbool),
  lbool_merge (lbool_merge lb1 lb2) lb3 =
    lbool_merge lb3 (lbool_merge lb1 lb2).
Proof with eauto.
  destruct lb3; destruct lb2; destruct lb1...
    destruct b; destruct b0...
    destruct b; destruct b0...
    destruct b; destruct b0...
      destruct b1...
      destruct b1...
      destruct b1...
      destruct b1...
Qed.

Theorem lbool_merge_comm : forall (lb1 lb2 : lbool),
  lbool_merge lb1 lb2 = lbool_merge lb2 lb1.
Proof with eauto.
  induction lb1; induction lb2...
    destruct b; destruct b0...
Qed.

Theorem lbool_merge_idemp : forall (lb : lbool),
  lbool_merge lb lb = lb.
Proof with eauto.
  induction lb... destruct b...
Qed.

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

Theorem lmax_merge_idemp : forall (lm : lmax),
  lmax_merge lm lm = lm.
Proof with eauto.
  induction lm; unfold lmax_merge; simpl; rewrite max_idempotent...
Qed.

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

Theorem lmin_merge_idemp : forall (lm : lmin),
  lmin_merge lm lm = lm.
Proof with eauto.
  induction lm; unfold lmin_merge; simpl; rewrite min_idempotent...
Qed.

End JoinSemiLattice.
