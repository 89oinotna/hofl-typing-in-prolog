subst(_, _, n(Y), n(Y)).
subst(T, X, i(X), T).
subst(_, X, i(Y), i(Y)) :-
    \+ X==Y.
subst(T, X, op(O, E1, E2), op(O, R1, R2)) :-
    subst(T, X, E1, R1),
    subst(T, X, E2, R2).
subst(T, X, if(C, E1, E2), if(R0, R1, R2)) :-
	subst(T, X, C, R0),
    subst(T, X, E1, R1),
    subst(T, X, E2, R2).
subst(T, X, pair(E1, E2), pair(R1, R2)) :-
    subst(T, X, E1, R1),
    subst(T, X, E2, R2).
subst(T, X, fst(E), R) :-
    subst(T, X, E, R).
subst(T, X, snd(E), R) :-
    subst(T, X, E, R).
subst(T, X, app(E1, E2), app(R1, R2)) :-
    subst(T, X, E1, R1),
    subst(T, X, E2, R2).

ttype(i(X), _) :- atom(X).
ttype(n(X), int) :- number(X), write("int").
ttype(op(_, E1, E2), int) :- 
    ttype(E1, int),
    ttype(E2, int).
ttype(if(C, E1, E2), X) :-
    ttype(C, int),
    ttype(E1, X),
    ttype(E2, X).
ttype(pair(E1, E2), (X, Y)) :-
    ttype(E1, X),
    ttype(E2, Y).
ttype(fst(P), X) :-
    ttype(P, (X, _)).
ttype(snd(P), Y) :-
    ttype(P, (_,Y)).
ttype(abs(X, B), [IDET, BT]) :-
    atom(X),
    ttype(X, IDET),
    ttype(B, BT).
ttype(app(E0, E1), Y) :-
    ttype(E0, [X, Y]),
    ttype(E1, X).
ttype(rec(X, B), T) :-
    atom(X),
    ttype(X, T),
    ttype(B, T).
