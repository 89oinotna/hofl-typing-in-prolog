:- use_module(src/abstree).
:- use_module(src/typing).

b(X, R) :-
    abstree(X, T),!,
    write(T),
    format("~n"),
    inferType(T, R).