---
title: Labels and computations
nav_order: 3
---

# Labels and computations

## Finite mappings for environments and stores

`override(M1,M2,M)` when `M` represents `M2[M1]`
```prolog
override(M,[],M).
override([],M,M).
override([X=Y|M1],M2,[X=Y|M]) :- delete(M2,X=_,M3), override(M1,M3,M).
```

`fresh(ST, L)` when `L` is not in dom(`ST`)
```prolog
fresh(ST, loc(N1)) :- length(ST, N), N1 is N+1.

loc(loc(N)) :- integer(N).
```

## Transition relations

Labelled transition `S ---X---> S1`

```prolog
:- op(750, xfy, (---)).
:- op(800, xfx, (--->)).

:- discontiguous (--->)/2.
```

Unlabelled transition `S ------> S1`
```prolog
:- op(800, xfy, (------>)).

:- discontiguous (------>)/2.
```

Final states
```prolog
:- discontiguous final/1.
```

## Labels

Records are represented by lists of equations:
- unprimed index: `I=A`  (initial value of label component)
- "primed" index: `I+=A` (final value of label component)

```prolog
:- op(700, xfx, (+=)).
```
The predicates `readable` and `writable` could be derived,
but simpler to specify them for each index `I`.

```prolog
:- discontiguous readable/1, writable/1.
```

## Computations

Unobservable steps `S ---U---> S1`
```prolog
S ---U---> S1 :-
        S ------> S1, unobs(U).
```

Unbounded computation `S ---X--->* F`
```prolog
:- op(800, xfx, (--->*)).

F ---U--->* F :- final(F), unobs(U).

S ---X--->* F :-
        pre_comp(X, X1),
        S ---X1---> S1,
        mid_comp(X1, X2),
        S1 ---X2--->* F,
        post_comp(X1, X2, X).
```

Bounded computation (up to N steps) `S ---X---N>*  SN`
```prolog
:- op(800, xfx, (>*)).

F ---U---_N>* F :- final(F), unobs(U).

S ---X---N>* SN :-
        N =:= 0 -> SN = S ;
        pre_comp(X, X1),
        S ---X1---> S1,
        mid_comp(X1, X2),
        N1 is N - 1,
        S1 ---X2---N1>* SN,
        post_comp(X1, X2, X).
```

`pre_comp(X, X1)` sets readable components of `X1`
```prolog
pre_comp([I=C|L], X1) :-
        select(I=C, X1, L1), !,
        pre_comp(L, L1).

pre_comp([I+=_C|L], X1) :-
        select(I+=_, X1, L1), !,
        pre_comp(L, L1).

pre_comp([], []).
```

`mid_comp(X1, X2)` sets readable components of `X2`
```prolog
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
```

`post_comp(X1, X2, X)` sets writable components of `X`
```prolog
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
```

`unobs(L)` when `L` has no observable effects
```prolog
unobs([I=C|L]) :-
        \+ writable(I) -> unobs(L) ;
        select(I+=C, L, L1), unobs(L1).

unobs([I+=C|L]) :-
        \+ readable(I) -> C = [], unobs(L) ;
        select(I=C, L, L1), unobs(L1).

unobs([]).

readable(dummy). writable(dummy).

changeable(I) :- readable(I), writable(I).
```
