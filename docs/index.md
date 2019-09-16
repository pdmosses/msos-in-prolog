---
layout: default
title: MSOS in Prolog
nav_order: 0
---

# Modular structural operational semantics (MSOS) in Prolog

Modular SOS (MSOS) is a variant of conventional Structural Operational Semantics
(SOS). Using MSOS, the transition rules for each construct of a programming
language can be given incrementally, once and for all, and do not need
reformulation when further constructs are added to the language.
MSOS thus provides an exceptionally high degree of modularity in language
descriptions, removing a shortcoming of the original SOS framework.

This repository illustrates how MSOS rules for the dynamic semantics of a
simple imperative programming language can be written as Prolog clauses. The
language and the rules are from [a paper about MSOS] published in 2004.
The Prolog clauses correspond closely to the MSOS rules specified in the paper.

Running programs using the Prolog clauses tests whether the MSOS rules specify
the expected behaviour. For example, running the [`tests/2.txt`] program is
expected to terminate with the value `32` stored in the variable bound to `b`;
the result of the appropriate Prolog query confirms that expectation:
```
?- parsef_run('../tests/2.txt').

const n = 5;
var a := 0;
var b := 1;
while n > a do {b := 2*b; a := a+1}

--- [rho=[],sigma=[],sigma+=[loc(1)=5,loc(2)=32],epsilon+=[]] --->*

    nil
true .
```
(`b` was bound to location `loc(2)`, and `sigma+` is the final store.)

## Specifying MSOS in Prolog

The MSOS specified in Prolog in this website is from the following paper:

- *Modular Structural Operational Semantics (MSOS)*  
  Peter D. Mosses  
  *J. Log. Algebr. Program.* 60-61 (2004), pp. 195–228  
  DOI: [10.1016/j.jlap.2004.03.008](https://doi.org/10.1016/j.jlap.2004.03.008)   
  Preprint: [Mosses2004MSOS](https://pdmosses.github.io/papers/Mosses2004MSOS/)

The Prolog code was originally made available as a single file at 
`http://www.brics.dk/~pdm/JLAP-MSOS.pl`, and subsequently at 
`http://cs.swansea.ac.uk/~cspdm/JLAP-MSOS.pl`. It has now been updated, split
into smaller files, and tested with SWI-Prolog 8.0. To obtain it, download or
clone the [msos-in-prolog repository].

The accompanying web pages assume familiarity with the above paper, and with
elementary Prolog programming:

- [Illustrative language syntax](syntax) uses a definite clause grammar (DCG)
  to specify the syntax of illustrative programming language constructs.

- [Illustrative language semantics](semantics) uses a straightforward
  representation of MSOS rules as Prolog clauses to specify the semantics of
  illustrative programming language constructs. The rule numbers shown in the
  code correspond to those in the above paper, to facilitate comparison.

- [Labels and computations](msos) defines Prolog predicates corresponding to
  steps and computations in MSOS, independently of specified languages.

- [User interface](run) defines Prolog predicates for parsing and running
  programs in specified languages.

- [Tests](tests) provides a few (very) simple test programs.

Note that for conciseness (and portability), module interfaces are not specified
at all; moreover, the code does not attempt to exemplify Prolog best practice.

## Executing MSOS in Prolog

The file `run.pro` loads the Prolog code for parsing and running programs.
Using SWI-Prolog from the command line:
```
cd .../code
swipl -l run.pro
```
Using the Eclipse plugin PDT, the working directory should be set to `.../code`
in the Prolog console before consulting `run.pro`.

### Parsing and running programs

`parsef_run(F)` parses file `F` as `prog(T)`, then calls `run(T)`.

`parse_run(S)` parses string `S` as `prog(T)`, then calls `run(T)`.

`parsef_run(F,N)` parses file `F` as `prog(T)`, then calls `run(T,N)`.

`parse_run(S,N)` parses string `S` as `prog(T)`, then calls `run(T,N)`.

### Parsing programs

`parsef_prog(F,T)` parses file `F` as `prog(T)`.

`parse_prog(S,T)` parses string `S` as `prog(T)`.

`parsef_prog(F)` parses file `F` as `prog(_)`.

`parse_prog(S)` parses string `S` as `prog(_)`.

### Running parsed programs

`run(T)` starts a computation with initial state `T`
and initial label specified by `init_label`.
If the computation terminates, it prints the final state.

`run(T, N)` starts a computation with initial state `T`
and initial label specified by `init_label`.
If the computation terminates in `N` or fewer steps,
it prints the final state, otherwise it prints the
state after the `N`th step.

### Printing program trees

`print_tree(T)` pretty-prints the tree `T`.
All the parsing queries pretty-print the resulting tree `T` by default.

`no_pretty` turns printing off, `pretty` turns it back on.

Please report any issues with parsing or running programs to
[the author](mailto:p.d.mosses@swansea.ac.uk).

## Related projects

- [Prolog MSOS Tool](https://pdmosses.github.io/prolog-msos-tool):
  Generation of Prolog interpreters from MSDF (a meta-language for specifying
  MSOS of programming languages), including lecture notes.

- [CBS](https://plancomps.github.io/CBS-beta/):
  A framework for component-based specification of programming languages using
  MSOS.

[msos-in-prolog repository]: https://github.com/pdmosses/msos-in-prolog

[`run.pro`]: https://github.com/pdmosses/msos-in-prolog/blob/master/code/run.pro

[`tests/2.txt`]: https://github.com/pdmosses/msos-in-prolog/blob/master/tests/2.txt

[a paper about MSOS]: https://pdmosses.github.io/papers/Mosses2004MSOS/
