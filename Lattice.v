Require Export SfLib.

Module Lattice.

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
Proof. 
  induction b1; induction b2; induction b3; auto.
Qed.

Theorem lbool_merge_comm : forall (b1 b2 : bool),
  lbool_merge (LBool b1) (LBool b2) = lbool_merge (LBool b2) (LBool b1).
Proof. 
  induction b1; induction b2; auto.
Qed.

End Lattice.