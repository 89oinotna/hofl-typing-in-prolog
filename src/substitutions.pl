:- module(substitutions,[subst/4]).
:- use_module(freevars).


/* 
to apply capture avoiding substitutions

fresh names are created by concatenation of all the free variables 
and the variable we were substituting

first argument is the term, 
second argument is the term we are inserting, 
third argument is the variable, 
fourth argument is the result.
*/

subst(N, _, _, R) :- number(N), R = N.

% subst(id(Y), T, Y, T) :- number(T).
% subst(id(Y), T, Y, id(T)) :- atom(T).
subst(id(Y), T, Y, T).
subst(id(Y), _, _, id(Y)).

subst(bin_op(O, E1, E2), T, X, bin_op(O, R1, R2)) :-
    subst(E1, T, X, R1),
    subst(E2, T, X, R2).

subst(cond(C, E1, E2), T, X, cond(R1, R2, R3)) :-
    subst(C, T, X, R1),
    subst(E1, T, X, R2),
    subst(E2, T, X, R3).

subst(pair(E1, E2), T, X, pair(R1, R2)) :-
    subst(E1, T, X, R1),
    subst(E2, T, X, R2).

subst(fst(E), T, X, fst(R)) :-
    subst(E, T, X, R).

subst(snd(E), T, X, snd(R)) :-
    subst(E, T, X, R).

subst(app(E1, E2), T, X, app(R1, R2)) :-
    subst(E1, T, X, R1),
    subst(E2, T, X, R2).

subst(abst(Y, B), T, X, abst(Z, R)) :-
    fv(abst(Y, B), S0),
    fv(T, S1),
    union(S0, S1, S01),
    union(S01, [X], S2),
    atomic_list_concat(S2, X1),
    atom_concat(X, X1, Z),  
    subst(B, id(Z), Y, R0),
    subst(R0, T, X, R).

subst(rec(Y, B), T, X, rec(Z, R)) :-
    fv(rec(Y, B), S0),
    fv(T, S1),
    union(S0, S1, S01),
    union(S01, [X], S2),
    atomic_list_concat(S2, X1),
    atom_concat(X, X1, Z), 
    subst(B, id(Z), Y, R0),
    subst(R0, T, X, R).