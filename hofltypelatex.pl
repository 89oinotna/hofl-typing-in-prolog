:- use_module(src/abstree).
:- use_module(src/typing).

b(X, TYPE) :-
    abstree(X, TERM),!,
    write(TERM),
    format("~n"),
    inferType(TERM, TYPE).