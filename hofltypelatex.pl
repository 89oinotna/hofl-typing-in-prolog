:- use_module(abstree).
:- use_module(typing).

b(X, R) :-
    abstree(X, T),
    infer(T, R).