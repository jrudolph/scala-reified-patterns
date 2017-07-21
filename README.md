# Reified Patterns For Scala

Pattern matching is popular in Scala and other functional programming languages. In Scala,
pattern matches are compiled down to efficient bytecode. This is good for execution speed,
especially when the pattern match is successful. However, when a pattern match fails, the
only information you get is that a candidate value did not match.

This project aims at providing a reified structure of a pattern match, an ADT, to inspect
the structure of a pattern at runtime. This can be used to execute (or better 'interpret')
a pattern match piecewise to be able to provide better diagnostics.

Aside from that, it is an interesting (meta) programming challenge to model the
existing pattern matches to an ADT.