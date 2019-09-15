/*
# User interface

[Prolog code](https://github.com/pdmosses/msos-in-prolog/blob/master/code/run.pro)

*/
:- include('load.pro').
/*

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
