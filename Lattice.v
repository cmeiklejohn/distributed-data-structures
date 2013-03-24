Require Export SfLib.

Module Lattice.

Inductive lbool : Type :=
  LBool : bool -> lbool.

Definition lbool_merge lb1 lb2 :=
  match (lb1, lb2) with
    (LBool b1, LBool b2) => (LBool (orb b1 b2))
  end.

Theorem lbool_merge_comm : forall (b1 b2 : bool),
  lbool_merge (LBool b1) (LBool b2) = lbool_merge (LBool b1) (LBool b2).
Proof. reflexivity. Qed.

End Lattice.