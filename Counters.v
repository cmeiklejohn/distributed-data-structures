Require Export SfLib.
Require Export Sets.Ensembles.

(** Grow-only counters, similar to a vector clock. *)
Module G_Counter.

(** Actor, which is of type nat. *)
Definition Actor := nat.

(** Clock, representing an actor and count. *)
Inductive Clock : Type :=
  In_Clock : Actor -> nat -> Clock.

Hint Constructors Clock.

(** Grow only counter, representing a vector of clocks. *)
Inductive G_Counter : Type :=
  In_G_Counter : forall (x : Type), Ensemble x -> G_Counter.

Hint Constructors G_Counter.

(** Tests for constructor code. *)
Eval compute in Singleton Clock (In_Clock 1 1).

Eval compute in In_Clock 1 1.

Eval compute in In_G_Counter Clock (Singleton Clock (In_Clock 1 1)).

Eval compute in Singleton Clock (In_Clock 1 1).

(** Add increment, merge and reveal functions. *)

End G_Counter.

(** Positive/negative counters, a composition of two G_Counters. *)
Module PN_Counter.

End PN_Counter.