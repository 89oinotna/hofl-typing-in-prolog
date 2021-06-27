:- module(freevars, [fv/2]).

% V is the resulting set
fv(rec(X, B), V) :-
    fv(B, V0),
    subtract(V0, [X], V). 

fv(app(T1, T2), V) :-
    fv(T1, V1),
    fv(T2, V2),
    union(V1, V2, V).
    
fv(abst(X, B), V) :-
    fv(B, VB),
    subtract(VB, [X], V).    

fv(fst(T), V) :-
    fv(T, V). 

fv(snd(T), V) :-
    fv(T, V).   

fv(pair(T0, T1), V) :-
    fv(T0, V0),
    fv(T1, V1),
    union(V0, V1, V).

fv(cond(C, E1, E2), V) :-
    fv(C, V0),
    fv(E1, V1),
    fv(E2, V2),
    union(V0, V1, V01),
    union(V01, V2, V).

fv(bin_op(_, E1, E2), V) :- 
    fv(E1, V1),
    fv(E2, V2),
    union(V1, V2, V).

fv(id(X), V) :- atom(X), list_to_set([X], V). 

fv(N, V) :- number(N), list_to_set([], V).
