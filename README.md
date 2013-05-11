# Distributed Data Structures

## Overview

An attempt at modeling various Lattices and CRDTs in Coq and Haskell.

## Files

* ```JoinSemiLattice.v```: An experiment in modeling JoinSemiLattices
  over naturals as outlined by Conway.
* ```Counters.v```: Work in modeling CvRDTs, specifically G-Counters and
  PN-Counters as outlined by Shapiro.

## Outstanding issues

* The current record definiton for CvRDTs does not support multiple
  update functions.  This needs to somehow be adapted for allowing
  multiple update functions to exist for a given CvRDT.  Not sure how to
  do this.
* How do we use these records for defining and overall signature for
  CvRDTS?

## References

* Conway, Marczak, Alvaro, Hellerstein, Maier, [_Logic and Lattices in Distributed Programming_](http://db.cs.berkeley.edu/papers/UCB-lattice-tr.pdf)
* Shapiro, Preguiça, Baquero, Zawirski, [_A comprehensive study of Convergent and Commutative Replicated Data Types_](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf)

## Copyright

Copyright (C) 2013 Christopher Meiklejohn.
