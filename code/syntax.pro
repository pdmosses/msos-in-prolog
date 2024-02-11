/*

This is an update of the original version to use left-recursive non-terminals,
adding `table` directives to suspend the corresponding goals.

# Illustrative language syntax

[Prolog code](https://github.com/pdmosses/msos-in-prolog/blob/master/code/syntax.pro)

## Table 1. [Concrete] abstract syntax of some illustrative constructs

Truth-values `T`
*/
t(tt) --> "tt".
t(ff) --> "ff".
/*

Numbers `N`
*/
n(N)          --> digit(D0), digits(D), {number_chars(N, [D0|D])}.
digits([D|T]) --> digit(D), !, digits(T).
digits([])    --> [].
digit(D)      --> [D], {code_type(D, digit)}.
/*

Identifiers `x(I)`
*/
id(I) --> [C], {code_type(C, alpha), name(I, [C])}.
/*

Binary ops `BOP`
*/
bop(+) --> "+".
bop(-) --> "-".
bop(*) --> "*".
bop(<) --> "<".
bop(=) --> "=".
bop(>) --> ">".
/*

Constants `CON`
*/
con(T) --> lex(t(T)).
con(N) --> lex(n(N)).
/*

Expressions `E`
*/
:- table exp/3.
exp(CON)             --> con(CON).
exp(x(I))            --> lex(id(I)).
exp(E)               --> lex("("), exp(E), lex(")").
exp(app(E0,BOP,E1))  --> exp(E0), lex(bop(BOP)), exp(E1).
exp(let(D,E))        --> lex("let"), dec(D), lex("in"), exp(E).
/*

Commands `C`
*/
:- table com/3.
com(nil)             --> lex("nil").
com(assign(x(I),E))  --> lex(id(I)), lex(":="), exp(E).
com(C)               --> lex("{"), com(C), lex("}").
com(seq(C0,C1))      --> com(C0), lex(";"), com(C1).
com(seq(D,C))        --> dec(D), lex(";"), com(C).
com(if(E,C0,C1))     --> lex("if"), exp(E), lex("then"),
                         com(C0), lex("else"), com(C1).
com(while(E,C))      --> lex("while"), exp(E), lex("do"), com(C).
/*

Declarations `D`
*/
:- table dec/3.
dec(const(x(I),E))   --> lex("const"), lex(id(I)), lex("="), exp(E).
dec(var(x(I),E))     --> lex("var"), lex(id(I)), lex(":="), exp(E).
dec(D)               --> lex("{"), dec(D), lex("}").
dec(seq(D0, D1))     --> dec(D0), lex(";"), dec(D1).
/*


Programs `P`
*/
prog(program(C))     --> layout_star, com(C).
/*

Layout
*/
lex(LS)     --> LS, layout_star.
layout_star --> [C], {code_type(C,space)}, layout_star.
layout_star --> [].
