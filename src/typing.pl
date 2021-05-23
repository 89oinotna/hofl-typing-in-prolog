:- module(typing, [inferType/2]).
info(X, T) :-
	write(X),
    format(" : "),
    write(T),
    format("~n").

inferType(X, T) :-
    inferType([], X, T).
inferType(_, X, _) :- atom(X).
inferType(ENV,i(X), T) :-
    info(i(X), T),
    nth0(_, ENV, (X, T)),
    info(i(X), T).
inferType(_, N, int) :- 
    number(N),
    info(N, int).
inferType(ENV, op(O, E1, E2), int) :-
    info(op(O, E1, E2), int),
    inferType(ENV,E1, int),
    inferType(ENV,E2, int),
    info(op(O, E1, E2), int).
inferType(ENV, if(C, E1, E2), T) :-
    info(if(C, E1, E2), T),
    inferType(ENV,C, int),
    inferType(ENV,E1, T),
    inferType(ENV,E2, T),
    info(if(C, E1, E2), T).
inferType(ENV, pair(E1, E2), (T1, T2)) :-
    info( pair(E1, E2), T),
    inferType(ENV,E1, T1),
    inferType(ENV,E2, T2),
    info( pair(E1, E2), T).
inferType(ENV, fst(P), T) :-
    info(fst(P), T),
    inferType(ENV,P, (T, _)),
    info(fst(P), T).
inferType(ENV, snd(P), T) :-
    info(snd(P), T),
    inferType(ENV,P, (_,T)),
    info(snd(P), T).
inferType(ENV, abs(X, B), [TIde, TB]) :-
    info(abs(X, B), [TIde, TB]),
    inferType(ENV, X, TIde),
    inferType([(X, TIde)|ENV], B, TB),
    info(abs(X, B), [TIde, TB]).
inferType(ENV, app(E1, E2), T0) :-
    info(app(E1, E2), T0),
    inferType(ENV,E1, [T2, T0]),
    inferType(ENV,E2, T2),
    info(app(E1, E2), T0).
inferType(ENV, rec(X, B), T) :-
    info(rec(X, B), T),
    inferType(ENV,X, T),
    inferType([(X, T)|ENV], B, T),
    info(rec(X, B), T).

