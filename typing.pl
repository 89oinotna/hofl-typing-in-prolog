

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
