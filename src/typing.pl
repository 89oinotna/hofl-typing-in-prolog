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

inferType(ENV, pair(E1, E2), tpair(T1, T2), typed(pair(TT1, TT2), tpair(T1, T2))) :-
    inferType(ENV, E1, T1, TT1),
    inferType(ENV, E2, T2, TT2).

inferType(ENV, fst(E), T1, typed(fst(TT1), T1)) :-
    inferType(ENV, E, tpair(T1, _), TT1).

inferType(ENV, snd(E), T2, typed(snd(TT2), T2)) :-
    inferType(ENV, E, tpair(_, T2), TT2).

inferType(ENV, abst(X, B), tfun(TA, TB), typed(abst(X, TTB), tfun(TA, TB))) :-
    inferType([(X, TA)|ENV], B, TB, TTB).

inferType(ENV, app(E1, E2), T1, typed(app(TT, TT0), T1)) :-
    inferType(ENV, E2, T0, TT0),
    inferType(ENV, E1, tfun(T0, T1), TT).

inferType(ENV, rec(X, B), T, typed(rec(X, TT), T)) :-
    inferType([(X, T)|ENV], B, T, TT).

inferType(ENV, id(X), T, typed(id(X), T)) :- 
    nth0(_, ENV, (X, T)).
