/*
# Illustrative language semantics

[Prolog code](https://github.com/pdmosses/msos-in-prolog/blob/master/code/semantics.pro)

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

app(N0,'+',N1) ------> N :-
        con(N0), con(N1), N is N0 + N1.                            %(20+)
app(N0,'-',N1) ------> N :-
        con(N0), con(N1), N is N0 - N1, N >= 0.                    %(20-)
app(N0,'*',N1) ------> N :-
        con(N0), con(N1), N is N0 * N1.                            %(20*)
app(N0,'<',N1) ------> T :-
        con(N0), con(N1), N0  <  N1 -> T=tt; T=ff.                 %(20<)
app(N0,'=',N1) ------> T :-
        con(N0), con(N1), N0 =:= N1 -> T=tt; T=ff.                 %(20=)
app(N0,'>',N1) ------> T :-
        con(N0), con(N1), N0  >  N1 -> T=tt; T=ff.                 %(20>)

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
