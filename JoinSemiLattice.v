Require Export SfLib.
Require Export Max.
Require Export Min.
Require Export Sets.Ensembles.

Module JoinSemiLattice.

Inductive lbool : Type :=
  | LBoolValue  : bool -> lbool.

Hint Constructors lbool.

Definition lbool_reveal lb :=
  match lb with
    | LBoolValue b => b
  end.

Definition lbool_merge lb1 lb2 :=
  match (lb1, lb2) with
    | (LBoolValue b1, LBoolValue b2) => (LBoolValue (orb b1 b2))
  end.

Theorem lbool_merge_assoc : forall (lb1 lb2 lb3 : lbool),
  lbool_merge (lbool_merge lb1 lb2) lb3 =
    lbool_merge lb3 (lbool_merge lb1 lb2).
Proof with eauto.
  destruct lb1; destruct lb2; destruct lb3...
    destruct b; destruct b0; destruct b1...
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
  | LMaxValue  : forall (n : nat), lmax.

Hint Constructors lmax.

Definition lmax_reveal lm :=
  match lm with
    | LMaxValue n => n
  end.

Definition lmax_merge lm1 lm2 :=
  match (lm1, lm2) with
    | (LMaxValue n1, LMaxValue n2) => (LMaxValue (max n1 n2))
  end.

Theorem lmax_merge_assoc : forall (lm1 lm2 lm3 : lmax),
  lmax_merge (lmax_merge lm1 lm2) lm3 =
    lmax_merge lm3 (lmax_merge lm1 lm2).
Proof with eauto.
  destruct lm1; destruct lm2; destruct lm3; eauto;
    unfold lmax_merge; rewrite max_comm...
Qed.

Theorem lmax_merge_comm : forall (lm1 lm2 : lmax),
  lmax_merge lm1 lm2 = lmax_merge lm2 lm1.
Proof with eauto.
  destruct lm1; destruct lm2...
    try unfold lmax_merge; rewrite max_comm...
Qed.

Theorem lmax_merge_idemp : forall (lm : lmax),
  lmax_merge lm lm = lm.
Proof with eauto.
  destruct lm; unfold lmax_merge...
    rewrite max_idempotent...
Qed.

End JoinSemiLattice.
