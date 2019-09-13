---
title: MSOS in Prolog
---

# Modular structural operational semantics (MSOS) in Prolog

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
terminate with the value `32` stored in the variable bound to identifier `b`;
the following output from the query `parsef_run('test2.txt')` shows that this
is indeed the case:
```
?- parsef_run('test2.txt').

const n = 5;
var a := 0;
var b := 1;
while n > a do {b := 2*b; a := a+1}

--- [rho=[],sigma=[],sigma+=[loc(1)=5,loc(2)=32],epsilon+=[]] --->*

    nil
true .
```
(`b` was bound to location `loc(2)`, and `sigma+` is the final store.)

# Specifying MSOS in Prolog

The MSOS specified in Prolog in this website relates to the following paper:

- *Modular Structural Operational Semantics (MSOS)*  
  Peter D. Mosses  
  *J. Log. Algebr. Program.* 60-61 (2004), pp. 195–228  
  DOI: [10.1016/j.jlap.2004.03.008](https://doi.org/10.1016/j.jlap.2004.03.008)   
  Preprint: [Mosses2004MSOS](https://pdmosses.github.io/papers/Mosses2004MSOS/)

The Prolog code was originally made available at `http://www.brics.dk/~pdm/JLAP-MSOS.pl`,
and subsequently at `http://cs.swansea.ac.uk/~cspdm/JLAP-MSOS.pl`.
It has now been updated and tested with SWI-Prolog 8.0.

The accompanying web pages assume familiarity with the above paper, and with
elementary Prolog programming:

- [Illustrative language syntax](syntax) uses a definite clause grammar (DCG)
  to specify the syntax of illustrative programming language constructs.

- [Illustrative language semantics](semantics) uses a a representation of
  MSOS rules as Prolog clauses to specify the semantics of illustrative
  programming language constructs.

- [Labels and computations](msos) defines Prolog predicates corresponding to
  steps and computations in MSOS, independently of specified languages.

- [User interface](run) defines the Prolog predicates for parsing and running
  programs in specified languages.

- [Tests](tests) provides a few (very) simple test programs.

# Executing MSOS in Prolog

`['run.pro']` loads the Prolog code for parsing and running programs.
The Prolog code embedded in the web pages is available at
<https://github.com/pdmosses/msos-in-prolog>.
The rule numbers shown in the code correspond to those in the above paper,
to support comparison.

## Parsing and running programs

`parsef_run(F)` parses file `F` as `prog(T)`, then calls `run(T)`.

`parse_run(S)` parses string `S` as `prog(T)`, then calls `run(T)`.

`parsef_run(F,N)` parses file `F` as `prog(T)`, then calls `run(T,N)`.

`parse_run(S,N)` parses string `S` as `prog(T)`, then calls `run(T,N)`.

## Parsing programs

`parsef_prog(F,T)` parses file `F` as `prog(T)`.

`parse_prog(S,T)` parses string `S` as `prog(T)`.

`parsef_prog(F)` parses file `F` as `prog(_)`.

`parse_prog(S)` parses string `S` as `prog(_)`.

## Running parsed programs

`run(T)` starts a computation with initial state `T`
and initial label specified by `init_label`.
If the computation terminates, it prints the final state.

`run(T, N)` starts a computation with initial state `T`
and initial label specified by `init_label`.
If the computation terminates in `N` or fewer steps,
it prints the final state, otherwise it prints the
state after the `N`th step.

## Printing program trees

`print_tree(T)` pretty-prints the tree `T`.
All the parsing queries pretty-print the resulting tree `T` by default.

`no_pretty` turns printing off, `pretty` turns it back on.


Please report any issues with parsing or running programs to
[the author](mailto:p.d.mosses@swansea.ac.uk).

[run.pro]: /run.pro

[test2]: /test2.txt

[a paper about MSOS]: https://pdmosses.github.io/papers/Mosses2004MSOS/
