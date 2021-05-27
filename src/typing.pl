:- module(typing, [inferType/2]).
:- use_module(latex).


inferType(X, T) :-
    inferType([], term(X, tau(0)), T).



inferType(ENV, term(var(X), tau(I)), T) :-
    /*pre(term(var(X), tau(I))),*/
    nth0(_, ENV, (X, T))/*,
    after(term(var(X), tau(I)), T)*/.



inferType(ENV, term(bin_op(O, E1, E2), tau(I)), int) :-
    info(term(bin_op(O, E1, E2), tau(I)), int),
    I1 is I +1,
    I2 is I1+1,
    inferType(ENV,term(E1, tau(I1)), int),
    inferType(ENV,term(E2, tau(I2)), int),
    info(term(bin_op(O, E1, E2), tau(I)), int).

inferType(ENV, term(if(C, E1, E2), tau(I)), T) :-
    info(if(C, E1, E2), T),
    inferType(ENV,term(C, tau(I)), int),
    inferType(ENV,term(E1, tau(I)), T),
    inferType(ENV,term(E2, tau(I)), T),
    info(if(C, E1, E2), T).

inferType(ENV, term(pair(E1, E2), tau(I)), (T1, T2)) :-
    T=(T1, T2),
    info( pair(E1, E2), T),
    inferType(ENV,term(E1, T1), T1),
    inferType(ENV, term(E2, T2), T2),
    info( pair(E1, E2), T).

inferType(ENV, term(fst(P), tau(I)), T) :-
    info(fst(P), T),
    inferType(ENV, term(P,(T,_)), (T, _)),
    info(fst(P), T).

inferType(ENV, term(snd(P), tau(I)), T) :-
    info(snd(P), T),
    inferType(ENV, term(P, (_,T)), (_,T)),
    info(snd(P), T).

inferType(ENV, term(abst(X, B), tau(I)), [TIde, TB]) :-
    info(term(abst(X, B), tau(I)), [TIde, TB]),
    I1 is I+1,
    I2 is I1+1,
    inferType(ENV, term(X, tau(I1)), TIde),
    inferType([(X, TIde)|ENV], term(B, tau(I2)), TB),
    info(abst(X, B), [TIde, TB]).

inferType(ENV, term(app(E1, E2), tau(I)), T0) :-
    info(app(E1, E2), T0),
    inferType(ENV, term(E1, [T2, T0]), [T2, T0]),
    inferType(ENV, term(E2, T2), T2),
    info(app(E1, E2), T0).

inferType(ENV, term(rec(X, B), tau(I)), T) :-
    info(rec(X, B), T),
    inferType(ENV, term(X, T), T),
    inferType([(X, T)|ENV], term(B, T), T),
    info(rec(X, B), T).

inferType(_, term(X, _), _) :- atom(X).

inferType(ENV, term(N, tau(I)), T) :-
    info(term(N, tau(I)), int),
    number(N),
    info(term(N, int), int).