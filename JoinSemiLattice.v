Require Export SfLib.
Require Export Max.
Require Export Min.
Require Export Sets.Ensembles.

Module JoinSemiLattice.

Inductive lbool : Type :=
  | LBoolBottom : lbool
  | LBoolValue  : bool -> lbool.

Hint Constructors lbool.

Definition lbool_reveal lb :=
  match lb with
    | LBoolBottom  => false
    | LBoolValue b => b
  end.

Definition lbool_merge lb1 lb2 :=
  match (lb1, lb2) with
    | (LBoolBottom, LBoolBottom)     => LBoolBottom
    | (LBoolBottom, LBoolValue b2)   => (LBoolValue b2)
    | (LBoolValue b1, LBoolBottom)   => (LBoolValue b1)
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

Inductive number :=
  | NumberNat : nat -> number.

Inductive lmax : Type :=
  | LMaxBottom : lmax
  | LMaxValue  : number -> lmax.

Eval compute in LMaxBottom.
Eval compute in LMaxValue (NumberNat 2).

(* Definition lmax_reveal (lm : lmax) : number := *)
(*   match lm with *)
(*     | LMaxBottom =>  *)
(*     | LMaxValue n => n *)
(*   end. *)

Definition lmax_merge (lm1 lm2 : lmax) :=
  match (lm1, lm2) with
    | (LMaxBottom, LMaxBottom)     => LMaxBottom
    | (LMaxBottom, LMaxValue n)    => LMaxBottom
    | (LMaxValue n, LMaxBottom)    => LMaxBottom
    | (LMaxValue n1, LMaxValue n2) =>
      match (n1, n2) with
        | (NumberNat nn1, NumberNat nn2) => LMaxValue (NumberNat (max nn1 nn2))
      end 
  end.

(* Theorem lmax_merge_assoc : forall (lm1 lm2 lm3 : lmax), *)
(*   lmax_merge (lmax_merge lm1 lm2) lm3 = *)
(*     lmax_merge lm3 (lmax_merge lm1 lm2). *)
(* Proof with eauto. *)
(*   destruct lm1; destruct lm2; destruct lm3; eauto; *)
(*     unfold lmax_merge; rewrite max_comm... *)
(* Qed. *)

(* Theorem lmax_merge_comm : forall (lm1 lm2 : lmax), *)
(*   lmax_merge lm1 lm2 = lmax_merge lm2 lm1. *)
(* Proof with eauto. *)
(*   destruct lm1; destruct lm2... *)
(*     try unfold lmax_merge; rewrite max_comm... *)
(* Qed. *)

(* Theorem lmax_merge_idemp : forall (lm : lmax), *)
(*   lmax_merge lm lm = lm. *)
(* Proof with eauto. *)
(*   destruct lm; unfold lmax_merge... *)
(*     rewrite max_idempotent... *)
(* Qed. *)

End JoinSemiLattice.
