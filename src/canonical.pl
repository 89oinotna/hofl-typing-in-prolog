:- module(canonical,[canonic/2, is_canonic/1]).
:- use_module(freevars).
:- use_module(substitutions).
:- use_module(typing).

% we assign semantics only to terms that are:well-formed and closed

canonic(pair(E1, E2), pair(E1, E2)) :-
    inferType(E1, _, _),
    inferType(E2, _, _),
    fv(E1, []),
    fv(E2, []).

canonic(abst(X, B), abst(X, B)) :-
    inferType(abst(X, B), tfun(_, _), _), !,
    fv(abst(X, B), []).

canonic(bin_op(OP, E1, E2), C) :- 
    canonic(E1, C1),
    number(C1),
    canonic(E2, C2),
    number(C2),
    opp(OP, C1, C2, C).

canonic(cond(T, E1, _), C) :-
    canonic(T, 0),!,
    canonic(E1, C).

canonic(cond(T, _, E2), C) :-
    canonic(T, _),!,
    canonic(E2, C).

canonic(fst(pair(E1, E2)), C) :-
    inferType(pair(E1, E2), tpair(_, _), _),
    canonic(E1, C).

canonic(snd(pair(E1, E2)), C) :-
    inferType(pair(E1, E2), tpair(_, _), _),
    canonic(E2, C).

canonic(rec(X, B), C) :-
    subst(B, rec(X, B), X, R),
    canonic(R, C).

canonic(app(E1, E2), C) :-
    canonic(E1, abst(X, B)),!,
    subst(B, E2, X, R),!,
    canonic(R, C).

canonic(N, N) :- number(N).

opp(OP, E1, E2, R) :-
    (mul = OP,
        R is E1 * E2 );
    (plus = OP,
        R is E1 + E2);
    (minus = OP,
        R is E1 - E2).

% only to check if a term is already canonic

is_canonic(pair(E1, E2)) :-
    inferType(E1, _, _),
    inferType(E2, _, _),
    fv(E1, []),
    fv(E2, []).

is_canonic(abst(X, B)) :-
    inferType(abst(X, B), tfun(_, _), _), !,
    fv(abst(X, B), []).

is_canonic(N) :- number(N).
