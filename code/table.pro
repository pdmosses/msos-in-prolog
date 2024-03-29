/*

This is a copy of `all.pro`, updated to use left-recursive non-terminals,
adding `table` directives to suspend the corresponding goals.

# User interface

[Prolog code](https://github.com/pdmosses/msos-in-prolog/blob/master/code/run.pro)

*/
/*
Scanner-less parsing with DCGs:
*/
:- set_prolog_flag(double_quotes, chars).



/*


## Transition relations

Labelled transition `S ---X---> S1`

*/
:- op(750, xfy, (---)).
:- op(800, xfx, (--->)).

:- discontiguous (--->)/2.
/*

Unlabelled transition `S ------> S1`
*/
:- op(800, xfy, (------>)).

:- discontiguous (------>)/2.
/*

Final states
*/
:- discontiguous final/1.
/*

*/
:- op(800, xfx, (--->*)).

:- op(800, xfx, (>*)).


/*

## Labels

Records are represented by lists of equations:
- unprimed index: `I=A`  (initial value of label component)
- "primed" index: `I+=A` (final value of label component)

*/
:- op(700, xfx, (+=)).
/*
The predicates `readable` and `writable` could be derived,
but simpler to specify them for each index `I`.

*/
:- discontiguous readable/1, writable/1.
/*

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
exp(CON)            --> con(CON).
exp(x(I))           --> lex(id(I)).
exp(E)              --> lex("("), exp(E), lex(")").
exp(app(E0,BOP,E1)) --> exp(E0), lex(bop(BOP)), exp(E1).
exp(let(D,E))       --> lex("let"), dec(D), lex("in"), exp(E).
/*

Commands `C`
*/
:- table com/3.
com(nil)            --> lex("nil").
com(assign(x(I),E)) --> lex(id(I)), lex(":="), exp(E).
com(C)              --> lex("{"), com(C), lex("}").
com(seq(C0,C1))     --> com(C0), lex(";"), com(C1).
com(seq(D,C))       --> dec(D), lex(";"), com(C).
com(if(E,C0,C1))    --> lex("if"), exp(E), lex("then"),
                        com(C0), lex("else"), com(C1).
com(while(E,C))     --> lex("while"), exp(E), lex("do"), com(C).
/*

Declarations `D`
*/
:- table dec/3.
dec(const(x(I),E))  --> lex("const"), lex(id(I)), lex("="), exp(E).
dec(var(x(I),E))    --> lex("var"), lex(id(I)), lex(":="), exp(E).
dec(D)              --> lex("{"), dec(D), lex("}").
dec(seq(D0, D1))    --> dec(D0), lex(";"), dec(D1).
/*


Programs `P`
*/
prog(program(C))    --> layout_star, com(C).
/*

Layout
*/
lex(LS)     --> LS, layout_star.
layout_star --> [C], {code_type(C,space)}, layout_star.
layout_star --> [].

/*
# Labels and computations

[Prolog code](https://github.com/pdmosses/msos-in-prolog/blob/master/code/msos.pro)


## Table 10. Configurations for MSOS

A computed value in this language is either a constant value `con(G)`,
`nil`, or an environment `env(G)`.
*/
final(G) :- con(G); G=nil; env(G).

con(tt).
con(ff).
con(N) :- integer(N).

env([]).
env([_I=_DV|R]) :- env(R).
/*
(For full modularity, the above clauses could be deferred until the values that
they introduce are needed.)

## Table 11. Label components

To start with, the only label component required is an environment `rho`.
*/
readable(rho).
/*

## Table 12. MSOS rules for arithmetic expressions

*/
app(E0,BOP,E1) ---X---> app(E0_,BOP,E1) :-                         %(18)
        E0 ---X---> E0_.

app(CON0,BOP,E1) ---X---> app(CON0,BOP,E1_) :-                     %(19)
        con(CON0), E1 ---X---> E1_.

app(N0,'+',N1) ------> N :- N is N0 + N1.                          %(20+)
app(N0,'-',N1) ------> N :- N is N0 - N1, N >= 0.                  %(20-)
app(N0,'*',N1) ------> N :- N is N0 * N1.                          %(20*)
app(N0,'<',N1) ------> T :- N0  <  N1 -> T=tt; T=ff.               %(20<)
app(N0,'=',N1) ------> T :- N0 =:= N1 -> T=tt; T=ff.               %(20=)
app(N0,'>',N1) ------> T :- N0  >  N1 -> T=tt; T=ff.               %(20>)

x(I) ---U---> CON :-
        select(rho=R,U,_DOTS), member(I=CON,R), con(CON), unobs(U).%(21)
/*

## Table 13. MSOS rules for declarations

Declarations compute environments.
*/
let(D,E) ---X---> let(D_,E) :-                                     %(22)
        D ---X---> D_.

let(R0,E) ---X---> let(R0,E_) :-                                   %(23)
        env(R0), select(rho=R1,X,DOTS), override(R0,R1,R),
        E ---[rho=R|DOTS]---> E_.

let(R0,CON) ------> CON :-                                         %(24)
        env(R0), con(CON).

const(x(I),E) ---X---> const(x(I),E_) :-                           %(25)
        E ---X---> E_.

const(x(I),CON) ------> [I=CON] :- con(CON).                       %(26)

seq(D0,D1) ---X---> seq(D0_,D1) :-                                 %(27)
        D0 ---X---> D0_.

seq(R0,D1) ---X---> seq(R0,D1_) :-                                 %(28)
        env(R0), select(rho=R1,X,DOTS), override(R0,R1,R),
        D1 ---[rho=R|DOTS]---> D1_.

seq(R0,R1) ------> R :- env(R0), env(R1), override(R1,R0,R).       %(29)
/*

## Table 14. MSOS rules for commands

(The rules that are commented-out below involve the AST constructor `seq(_,_)`
which is overloaded. It would have been better to eliminate the overloading.)
*/
% seq(C0,C1) ---X---> seq(C0_,C1) :-                               %(30)
%       C0 ---X---> C0_.

seq(nil,C1) ------> C1.                                            %(31)

% seq(D,C) ---X---> seq(D_,C) :-                                   %(32)
%       D ---X---> D_.

% seq(R0,C) ---X---> seq(R0,C_) :-                                 %(33)
%       env(R0), select(rho=R1,X,DOTS), override(R0,R1,R),
%       C ---[rho=R|DOTS]---> C_.

seq(R0,nil) ------> nil :- env(R0).                                %(34)

if(E,C0,C1) ---X---> if(E_,C0,C1) :-                               %(35)
        E ---X---> E_.

if(tt,C0,_C1) ------> C0.                                          %(36)

if(ff,_C0,C1) ------> C1.                                          %(37)

while(E,C) ------> if(E,seq(C,while(E,C)),nil).                    %(38)
/*

## Table 15. MSOS rules for variables

Label components now include a mutable store `sigma`. When a variable identifier
`I` is bound to a location `L` in the store, it evaluates to the value `CON`
currently stored at that location.
*/
readable(sigma). writable(sigma).

x(I) ---U---> CON :-                                               %(42)
        select(rho=R,U,U1), select(sigma=S,U1,_DOTS),
        member(I=L,R), member(L=CON,S), unobs(U).

assign(x(I),E) ---X---> assign(x(I),E_) :-                         %(43)
        E ---X---> E_.

assign(x(I),CON) ---X---> nil :-                                   %(44)
        con(CON), select(rho=R,X,X1), member(I=L,R), loc(L),
        select(sigma=S,X1,X2), select(sigma+=S_,X2,DOTS),
        override([L=CON],S,S_),
        unobs([rho=R,sigma=S,sigma+=S|DOTS]).

var(x(I),E) ---X---> var(x(I),E_) :-                               %(45)
        E ---X---> E_.

var(x(I),CON) ---X---> [I=L] :-                                    %(46)
        con(CON), select(sigma=S,X,X1), select(sigma+=S_,X1,DOTS),
        fresh(S,L), override([L=CON],S,S_),
        unobs([sigma=S,sigma+=S|DOTS]).
/*

## Table 18. MSOS of dynamic errors

The writable label component `epsilon` is either `[]` or `[err]`. Program
execution terminates as soon as a step has `epsilon` non-`[]`.
*/
writable(epsilon).

app(N0,'-',N1) ---X---> stuck :-                                   %(53)
        integer(N0), integer(N1), N0 < N1,
        select(epsilon+=[err],X,DOTS),
        unobs([epsilon+=[]|DOTS]).

program(C) ---X---> program(C_) :-                                 %(54)
        C ---X---> C_, member(epsilon+=[],X).

program(C) ---X---> nil :-                                         %(55)
        C ---X---> _C_, member(epsilon+=L,X), L\=[].

program(nil) ------> nil.                                          %(56)
/*

The only non-modular part of the specification, depending on all the required
label components:
*/
init_label([rho=[],sigma=[],sigma+=_S,epsilon+=_L]).
/*

## Finite mappings for environments and stores

`override(M1,M2,M)` when `M` represents `M2[M1]`
*/
override(M,[],M).
override([],M,M).
override([X=Y|M1],M2,[X=Y|M]) :- delete(M2,X=_,M3), override(M1,M3,M).
/*

`fresh(ST, L)` when `L` is not in dom(`ST`)
*/
fresh(ST, loc(N1)) :- length(ST, N), N1 is N+1.

loc(loc(N)) :- integer(N).
/*

## Computations

Unobservable steps `S ---U---> S1`
*/
S ---U---> S1 :-
        S ------> S1, unobs(U).
/*

Unbounded computation `S ---X--->* F`
*/
F ---U--->* F :- final(F), unobs(U).

S ---X--->* F :-
        pre_comp(X, X1),
        S ---X1---> S1,
        mid_comp(X1, X2),
        S1 ---X2--->* F,
        post_comp(X1, X2, X).
/*

Bounded computation (up to N steps) `S ---X---N>*  SN`
*/
F ---U---_N>* F :- final(F), unobs(U).

S ---X---N>* SN :-
        N =:= 0 -> SN = S ;
        pre_comp(X, X1),
        S ---X1---> S1,
        mid_comp(X1, X2),
        N1 is N - 1,
        S1 ---X2---N1>* SN,
        post_comp(X1, X2, X).
/*

`pre_comp(X, X1)` sets readable components of `X1`
*/
pre_comp([I=C|L], X1) :-
        select(I=C, X1, L1), !,
        pre_comp(L, L1).

pre_comp([I+=_C|L], X1) :-
        select(I+=_, X1, L1), !,
        pre_comp(L, L1).

pre_comp([], []).
/*

`mid_comp(X1, X2)` sets readable components of `X2`
*/
mid_comp([I=C1|L1], X2) :-
        ( changeable(I) ->
            select(I+=_, X2, L2) ;
            select(I=C1, X2, L2) ), !,
        mid_comp(L1, L2).

mid_comp([I+=C1|L1], X2) :-
        ( changeable(I) ->
            select(I=C1, X2, L2) ;
            select(I+=_, X2, L2) ), !,
        mid_comp(L1, L2).

mid_comp([], []).
/*

`post_comp(X1, X2, X)` sets writable components of `X`
*/
post_comp([_=_|L1], X2, X) :-
        post_comp(L1, X2, X).

post_comp([I+=C1|L1], X2, X) :-
        member(I+=C2, X2),
        ( changeable(I) ->
            member(I+=C2, X) ;
          append(C1, C2, C),
          member(I+=C, X) ), !,
        post_comp(L1, X2, X).

post_comp([], _X2, _X).
/*

`unobs(L)` when `L` has no observable effects
*/
unobs([I=C|L]) :-
        \+ writable(I) -> unobs(L) ;
        select(I+=C, L, L1), unobs(L1).

unobs([I+=C|L]) :-
        \+ readable(I) -> C = [], unobs(L) ;
        select(I=C, L, L1), unobs(L1).

unobs([]).

readable(dummy). writable(dummy).

changeable(I) :- readable(I), writable(I).

/*
# Illustrative language semantics

[Prolog code](https://github.com/pdmosses/msos-in-prolog/blob/master/code/semantics.pro)

## Parsing and running programs

`run(T)` starts a computation with initial state `T`
and initial label specified by `init_label`.
If the computation terminates, it prints the final state.
*/
run(T) :-
        init_label(X),
        T ---X--->* F, nl, nl,
        write('--- '), print(X), write(' --->*'),
        nl, print_nl_tree(F).
/*

`run(T, N)` starts a computation with initial state `T`
and initial label specified by `init_label`.
If the computation terminates in `N` or fewer steps,
it prints the final state, otherwise it prints the
state after the `N`th step.
*/
run(T, N) :-
        init_label(X),
        T ---X---N>* SN, nl, nl,
        write('--- '), print(X), write(' --- '),
        print(N), write(' >*'), nl, print_nl_tree(SN).
/*

`..._run` both parses and runs programs.
`no_pretty` turns off the printing of the initial state.
*/
parse_run(S, N) :-
        parse_prog(S, T), !,
        run(T, N).

parse_run(S) :-
        parse_prog(S, T), !,
        run(T).

parsef_run(F, N) :-
        parsef_prog(F, T), !,
        run(T, N).

parsef_run(F) :-
        parsef_prog(F, T), !,
        run(T).
/*

`parse(S, T)` parses string `S` as `T`, then
optionally pretty-prints `T`.
*/
parse(S, T) :-
        phrase(T, S), !,
        opt_pretty(T).
/*

`parse_prog(S, T)` parses string `S` as `prog(T)`, then
optionally pretty-prints `T`.
*/
parse_prog(S, T) :-
        phrase(prog(T), S), !,
        opt_pretty(T).

parse_prog(S) :-
        phrase(prog(T), S), !,
        opt_pretty(T).
/*

`parsef` forms the string from a file.
*/
parsef(F, T) :-
        see(F), read_chars(S), seen, !,
        parse(S, T).

parsef_prog(F, T) :-
        see(F), read_chars(S), seen, !,
        parse_prog(S, T).

parsef_prog(F) :-
        see(F), read_chars(S), seen, !,
        parse_prog(S).
/*

`read_chars(Cs)` sets `Cs` to the list of characters read
from the current input stream
*/
read_chars(Cs) :-
	get_char(C), nl, show(C), read_rest(C, Cs), nl.

read_rest(end_of_file, []) :- !.
read_rest(C, [C|Cs]) :- get_char(C1), show(C1), read_rest(C1, Cs).

show(end_of_file) :- !.
show(C) :- write(C).
/*

## Pretty printing programs

*/
:- set_prolog_flag(pretty, true).
pretty    :- set_prolog_flag(pretty, true).
no_pretty :- set_prolog_flag(pretty, false).

:- set_prolog_flag(blank, true).
blank :- set_prolog_flag(blank, true).
lined :- set_prolog_flag(blank, false).

opt_pretty(T) :-
        (   current_prolog_flag(pretty, true)
        ->  print_nl_tree(T), !
        ;   true
        ).

print_nl_tree(T) :- print_nl_tree(T, 0).

print_nl_tree(T, N) :-
        indent(N), print_tree(T, N).
/*

`print_tree(T, N)` assumes already at column `2N`,
prints `T` in columns `>= 2N`, ending at column `>= 2N`.

*/
print_tree(T) :-
        print_tree(T, 0).

print_tree(T, N) :-
        (   atomic(T)
        ->  write(T)
        ;   T = []
        ->  write('[ ]')
        ;   is_list(T)
        ->  write('[ '), N1 is N+1,
            print_list(T, N1),
            write(' ]')
        ;   T =.. [F, T1], atomic(T1)
        ->  write(F), write('('), write(T1), write(')')
        ;   T = (T1=T2), atomic(T1), atomic(T2)
        ->  write(T1), write(' = '), write(T2)
        ;   T = (T1=T2), atomic(T1)
        ->  write(T1), write(' = '), N1 is N+1,
            print_nl_tree(T2, N1)
        ;   T =.. [F|L]
        ->  write(F), indent(N),
            write('( '), N1 is N+1,
            print_list(L, N1), write(' )')
        ).
/*

`print_list(L, N)` assumes already at column `2N`,
prints elements of `L` on separate lines starting at column `2N`,
terminating each line except the last with a comma.
*/
print_list([], _N).

print_list([H], N) :-
        print_tree(H, N).

print_list([H|L], N) :-
        print_tree(H, N), write(','), indent(N),
        print_list(L, N).

indent(N) :-
        (   N =< 0 -> nl, tab(4)
        ;   N1 is N-1, indent(N1), indent
        ).
/*

`indent` may be redefined, e.g. to `write('    ')`
*/
indent :-
        (   current_prolog_flag(blank, true) -> write('  ')
        ;   write('| ')
        ).
