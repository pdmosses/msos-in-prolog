# Modular structural operational semantics (MSOS) in Prolog

> INCOMPLETE DRAFT

Modular SOS (MSOS) is a variant of conventional Structural Operational Semantics 
(SOS). Using MSOS, the transition rules for each construct of a programming
language can be given incrementally, once and for all, and do not need
reformulation when further constructs are added to the language. 
MSOS thus provides an exceptionally high degree of modularity in language
descriptions, removing a shortcoming of the original SOS framework.

The file [run.pro] illustrates how MSOS rules for the dynamic semantics of a
simple imperative programming language can be written as Prolog clauses. The
language and the rules are from [a paper about MSOS] published in 2004. 
The Prolog clauses correspond closely to the MSOS rules specified in the paper.

Running programs using the Prolog clauses tests whether the MSOS rules specify
the expected behaviour. For example, running the [test2] program is expected to
terminate with the value 32 stored in the variable bound to the identifier `a`;
the following output from the query `run('test2.txt')` checks that this is the
case.

[run.pro]: run.pro

[a paper about MSOS]: https://pdmosses.github.io/papers/Mosses2004MSOS/

[test2]: test2.txt
