:- module(canonical,[canonic/2, is_canonic/1]).
:- use_module(freevars).
:- use_module(substitutions).
:- use_module(typing).

% we assign semantics only to terms that are:well-formed and closed

canonic(pair(E1, E2), pair(E1, E2)) :-
    inferType(E1, T1, TT1),
    inferType(E2, T2, TT2),
    fv(E1, []),
    fv(E2, []).

canonic(abst(X, B), C) :-
    inferType(abst(X, B), T, TT),
    fv(abst(X, B), []).

canonic(bin_op(OP, E1, E2), C) :- 
    canonic(E1, C1),
    number(C1),
    canonic(E2, C2),
    number(C2),
    opp(OP, C1, C2, C).

canonic(cond(T, E1, E2), C) :-
    canonic(T, 0),!,
    canonic(E1, C).

canonic(cond(T, E1, E2), C) :-
    canonic(T, C0),!,
    canonic(E2, C).

canonic(fst(E), C) :-
    inferType(E, (_, _), TT),
    pair(E1, E2) = E,
    canonic(E1, C).

canonic(snd(E), C) :-
    inferType(E, (_, _), TT),
    (E1, E2) = E,
    canonic(E2, C).

canonic(rec(X, B), C) :-
    subst(B, rec(X, B), X, R),
    canonic(R, C).

canonic(app(E1, E2), C) :-
    canonic(E1, abst(X, B)),
    subst(B, E2, X, R),
    canonic(R, C).

canonic(N, N) :- number(N).

opp(OP, E1, E2, R) :-
    (mul = OP,
        R is E1 * E2 );
    (plus = OP,
        R is E1 + E2);
    (minus = OP,
        R is E1 - E2).

is_canonic(pair(E1, E2)) :-
    inferType(E1, T1, TT1),
    inferType(E2, T2, TT2),
    fv(E1, []),
    fv(E2, []).

is_canonic(abst(X, B)) :-
    inferType(abst(X, B), T, TT),
    fv(abst(X, B), []).

is_canonic(N) :- number(N).
