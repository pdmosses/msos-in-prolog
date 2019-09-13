---
title: Illustrative language syntax
nav_order: 1
---

# Illustrative language syntax

## Table 1. [Concrete] abstract syntax of some illustrative constructs

Truth-values `T`
```prolog
t(tt) --> "tt".
t(ff) --> "ff".
```

Numbers `N`
```prolog
n(N)          --> digit(D0), digits(D), {number_chars(N, [D0|D])}.
digits([D|T]) --> digit(D), !, digits(T).
digits([])    --> [].
digit(D)      --> [D], {code_type(D, digit)}.
```

Identifiers `x(I)`
```prolog
id(I) --> [C], {code_type(C, alpha), name(I, [C])}.
```

Binary ops `BOP`
```prolog
bop(+) --> "+".
bop(-) --> "-".
bop(*) --> "*".
bop(<) --> "<".
bop(=) --> "=".
bop(>) --> ">".
```

Constants `CON`
```prolog
con(T) --> lex(t(T)).
con(N) --> lex(n(N)).
```

Expressions `E`
```prolog
exp1(CON)            --> con(CON).
exp1(x(I))           --> lex(id(I)).
exp1(E)              --> lex("("), exp(E), lex(")").
exp(E)               --> exp1(E).
exp(app(E0,BOP,E1))  --> exp1(E0), lex(bop(BOP)), exp(E1).
exp(let(D,E))        --> lex("let"), dec(D), lex("in"), exp(E).
```

Commands `C`
```prolog
com1(nil)            --> lex("nil").
com1(assign(x(I),E)) --> lex(id(I)), lex(":="), exp(E).
com1(C)              --> lex("{"), com(C), lex("}").
com(C)               --> com1(C).
com(seq(C0,C1))      --> com1(C0), lex(";"), com(C1).
com(seq(D,C))        --> dec(D), lex(";"), com(C).
com(if(E,C0,C1))     --> lex("if"), exp(E), lex("then"),
                         com(C0), lex("else"), com(C1).
com(while(E,C))      --> lex("while"), exp(E), lex("do"), com(C).
```

Declarations `D`
```prolog
dec1(const(x(I),E))  --> lex("const"), lex(id(I)), lex("="), exp(E).
dec1(var(x(I),E))    --> lex("var"), lex(id(I)), lex(":="), exp(E).
dec1(D)              --> lex("{"), dec(D), lex("}").
dec(D)               --> dec1(D).
dec(seq(D0, D1))     --> dec1(D0), lex(";"), dec(D1).
```


Programs `P`
```prolog
prog(program(C))     --> layout_star, com(C).
```

Layout
```prolog
lex(LS)     --> LS, layout_star.
layout_star --> [C], {code_type(C,space)}, layout_star.
layout_star --> [].
```
