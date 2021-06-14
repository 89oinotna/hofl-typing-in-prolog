:- module(typing, [inferType/3]).
:- use_module(latex).

inferType(X, T, TypedTerm) :- 
    inferType([], X, T, TypedTerm).

inferType(_, N, int, typed(N, int)) :-
    number(N).

inferType(ENV, bin_op(OP, E1, E2), int, typed(bin_op(OP, TT1, TT2),int)) :-
    inferType(ENV, E1, int, TT1),
    inferType(ENV, E2, int, TT2).

inferType(ENV, cond(C, E1, E2), T, typed(cond(TTC, TT1, TT2), T)) :- 
    inferType(ENV, C, int, TTC),
    inferType(ENV, E1, T, TT1),
    inferType(ENV, E2, T, TT2).

inferType(ENV, pair(E1, E2), (T1, T2), typed(pair(TT1, TT2), (T1, T2))) :-
    inferType(ENV, E1, T1, TT1),
    inferType(ENV, E2, T2, TT2).

inferType(ENV, fst(E), T1, typed(fst(TT1), T1)) :-
    inferType(ENV, E, (T1, _), TT1).

inferType(ENV, snd(E), T2, typed(snd(TT2), T2)) :-
    inferType(ENV, E, (_, T2), TT2).

inferType(ENV, abst(X, B), [TA, TB], typed(abst(X, TTB), [TA, TB])) :-
    inferType([(X, TA)|ENV], B, TB, TTB).

inferType(ENV, app(E1, E2), T1, typed(app(TT, TT0), T1)) :-
    inferType(ENV, E2, T0, TT0),
    inferType(ENV, E1, [T0, T1], TT).

inferType(ENV, rec(X, B), T, typed(rec(X, TT), T)) :-
    inferType([(X, T)|ENV], B, T, TT).

inferType(ENV, id(X), T, typed(id(X), T)) :- 
    nth0(_, ENV, (X, T)).
/*.
% entry point
inferType(X, T) :-
    %pre(term(X, tau(I)), T),
    inferType([], term(X, tau(0)), T).%,
    %post(term(X, tau(I)), T).

inferType(ENV, term(id(X), tau(I)), T) :-
    
    nth0(_, ENV, (X, T, Y)),
    pre(term(id(X), tau(I)), Y),
    post(term(id(X), tau(I)), T).

inferType(ENV, term(bin_op(O, E1, E2), tau(I)), int) :-
    pre(term(bin_op(O, E1, E2), tau(I)), int),
    I1 is I +1,
    I2 is I1+1,
    inferType(ENV,term(E1, tau(I1)), int),
    inferType(ENV,term(E2, tau(I2)), int),
    post(term(bin_op(O, E1, E2), tau(I)), int).

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
    I1 is I+1,
    I2 is I1+1,
    pre(term(abst(X, B), tau(I)), [tau(I1), tau(I2)]),
    inferType([(X, TIde, tau(I1))|ENV], term(B, tau(I2)), TB),
    post(term(abst(X, B), [tau(I1), tau(I2)]), [TIde, TB]).

inferType(ENV, term(app(E1, E2), tau(I)), T0) :-
    info(app(E1, E2), T0),
    inferType(ENV, term(E1, [T2, T0]), [T2, T0]),
    inferType(ENV, term(E2, T2), T2),
    info(app(E1, E2), T0).

inferType(ENV, term(rec(X, B), tau(I)), T) :-
    pre(term(rec(X, B), tau(I)), T),
    inferType(ENV, term(X, tau(I)), T),
    inferType([(X, T, tau(I))|ENV], term(B, tau(I)), T),
    post(term(rec(X, B), tau(I)), T).

inferType(ENV, term(N, tau(I)), T) :-
    pre(term(N, tau(I)), int),
    number(N),
    post(term(N, int), int).

inferType(_, term(X, _), _) :- atom(X).*/

