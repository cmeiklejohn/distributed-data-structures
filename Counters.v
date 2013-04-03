Require Export SfLib.
Require Export Sets.Ensembles.

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

(** Add increment, merge and reveal functions for G_Counter. *)

(** Positive/negative counters, a composition of two G_Counters. *)
Inductive PN_Counter : Type :=
  In_PN_Counter : forall (x : Type), G_Counter -> G_Counter -> PN_Counter.

(** Tests for constructor code. *)
Eval compute in Singleton Clock (In_Clock 1 1).

Eval compute in In_Clock 1 1.

Eval compute in In_G_Counter Clock (Singleton Clock (In_Clock 1 1)).

Eval compute in Singleton Clock (In_Clock 1 1).

Eval compute in In_PN_Counter Clock 
                              (In_G_Counter Clock (Singleton Clock (In_Clock 1 1)))                                      (In_G_Counter Clock (Singleton Clock (In_Clock 2 1))).

(** Add increment, decrement, merge and reveal functions for PN_Counter. *)