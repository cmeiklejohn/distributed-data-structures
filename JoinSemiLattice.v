(**
   Bounded join-semilattices over natural numbers.

   Logic and Lattices in Distributed Programming
   by Conway, Marczak, Alvaro, Hellerstein, Maier.
   http://db.cs.berkeley.edu/papers/UCB-lattice-tr.pdf
**)

Require Export Max.
Require Export Min.
Require Export Sets.Ensembles.

Module JoinSemiLattice.

(* Boolean lattice, values advance from false -> true *)
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

(* Proofs regarding the merge operation follows ACI *)
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

(* Max-value lattice, values monotonically advance upwards. *)
Inductive lmax_nat : Type :=
  | LMaxNatValue  : forall (n : nat), lmax_nat.

Hint Constructors lmax_nat.

Definition lmax_nat_reveal lm :=
  match lm with
    | LMaxNatValue n => n
  end.

Definition lmax_nat_merge lm1 lm2 :=
  match (lm1, lm2) with
    | (LMaxNatValue n1, LMaxNatValue n2) => (LMaxNatValue (max n1 n2))
  end.

(* Proofs regarding the merge operation follows ACI *)
Theorem lmax_nat_merge_assoc : forall (lm1 lm2 lm3 : lmax_nat),
  lmax_nat_merge (lmax_nat_merge lm1 lm2) lm3 =
    lmax_nat_merge lm3 (lmax_nat_merge lm1 lm2).
Proof with eauto.
  destruct lm1; destruct lm2; destruct lm3; eauto;
    unfold lmax_nat_merge; rewrite max_comm...
Qed.

Theorem lmax_nat_merge_comm : forall (lm1 lm2 : lmax_nat),
  lmax_nat_merge lm1 lm2 = lmax_nat_merge lm2 lm1.
Proof with eauto.
  destruct lm1; destruct lm2...
    try unfold lmax_nat_merge; rewrite max_comm...
Qed.

Theorem lmax_nat_merge_idemp : forall (lm : lmax_nat),
  lmax_nat_merge lm lm = lm.
Proof with eauto.
  destruct lm; unfold lmax_nat_merge...
    rewrite max_idempotent...
Qed.

(* Min-value lattice, values monotonically advance downwards. *)
Inductive lmin_nat : Type :=
  | LMinNatValue  : forall (n : nat), lmin_nat.

Hint Constructors lmin_nat.

Definition lmin_nat_reveal lm :=
  match lm with
    | LMinNatValue n => n
  end.

Definition lmin_nat_merge lm1 lm2 :=
  match (lm1, lm2) with
    | (LMinNatValue n1, LMinNatValue n2) => (LMinNatValue (min n1 n2))
  end.

(* Proofs regarding the merge operation follows ACI *)
Theorem lmin_nat_merge_assoc : forall (lm1 lm2 lm3 : lmin_nat),
  lmin_nat_merge (lmin_nat_merge lm1 lm2) lm3 =
    lmin_nat_merge lm3 (lmin_nat_merge lm1 lm2).
Proof with eauto.
  destruct lm1; destruct lm2; destruct lm3; eauto;
    unfold lmin_nat_merge; rewrite min_comm...
Qed.

Theorem lmin_nat_merge_comm : forall (lm1 lm2 : lmin_nat),
  lmin_nat_merge lm1 lm2 = lmin_nat_merge lm2 lm1.
Proof with eauto.
  destruct lm1; destruct lm2...
    try unfold lmin_nat_merge; rewrite min_comm...
Qed.

Theorem lmin_nat_merge_idemp : forall (lm : lmin_nat),
  lmin_nat_merge lm lm = lm.
Proof with eauto.
  destruct lm; unfold lmin_nat_merge...
    rewrite min_idempotent...
Qed.

End JoinSemiLattice.
