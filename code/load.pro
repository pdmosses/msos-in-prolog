
/*
# Prolog directives

## Scanner-less parsing with DCGs:
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

## Computations
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
:- discontiguous readable/1, writable/1.
/*

# Loading other files

*/
:- include('syntax.pro').
:- include('msos.pro').
:- include('semantics.pro').
