:- use_module(library(main)).
:- use_module(library(optparse)).
:- use_module(src/abstree).
:- use_module(src/typing).

:- initialization(main, main).

main(Argv) :-
    opt_spe.

b(X, TYPE) :-
    abstree(X, TERM),!,
    write(TERM),
    format("~n"),
    inferType(TERM, TYPE).