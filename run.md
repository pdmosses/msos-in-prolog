---
title: Running MSOS in Prolog
---

# Running MSOS in Prolog

`['run.pro']` loads the Prolog code for parsing and running programs.

## Parsing and running programs

`parsef_run(F)`:
parse the text in file `F` as `prog(T)`, then `run(T)`.

`parse_run(S)`:
parse the string `S` as `prog(T)`, then `run(T)`.

`parsef_run(F,N)`:
parse the text in file `F` as `prog(T)`, then `run(T,N)`.

`parse_run(S,N)`:
parse the string `S` as `prog(T)`, then `run(T,N)`.

## Parsing programs

`parsef_prog(F,T)`:
parses the text in file `F` as `prog(T)`.

`parse_prog(S,T)`:
parses string `S` as `prog(T)`.

`parsef_prog(F)`:
parses the text in file `F` as `prog(_)`.

`parse_prog(S)`:
parses string `S` as `prog(_)`.

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
