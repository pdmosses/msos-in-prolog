---
title: MSOS in Prolog
nav_exclude: true
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
the expected behaviour. For example, running the [test2.txt] program is expected to
terminate with the value `32` stored in the variable bound to identifier `a`;
the following output from the query `parsef_run('test2.txt')` shows that this
is the case:
```
?- parsef_run('test2.txt').

const n = 5;
var a := 0; 
var b := 1;
while n > a do {b := 2*b; a := a+1}


    program
    ( seq
      ( const
        ( x(n),
          5 ),
        seq
        ( var
          ( x(a),
            0 ),
          seq
          ( var
            ( x(b),
              1 ),
            while
            ( app
              ( x(n),
                >,
                x(a) ),
              seq
              ( assign
                ( x(b),
                  app
                  ( 2,
                    *,
                    x(b) ) ),
                assign
                ( x(a),
                  app
                  ( x(a),
                    +,
                    1 ) ) ) ) ) ) ) )

--- [rho=[],sigma=[],sigma+=[loc(1)=5,loc(2)=32],epsilon+=[]] --->*

    nil
true .
```



[run.pro]: run

[test2.txt]: test2.txt

[a paper about MSOS]: https://pdmosses.github.io/papers/Mosses2004MSOS/

[test2]: test2.txt
