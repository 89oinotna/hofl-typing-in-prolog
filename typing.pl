ttype(_, X, T) :- atom(X).
ttype(Γ,i(X), T) :-
    nth0(I, Γ, (X, T)).
ttype(Γ, n(_), int).
ttype(Γ, op(_, E1, E2), int) :- 
    ttype(Γ,E1, int),
    ttype(Γ,E2, int).
ttype(Γ, if(C, E1, E2), X) :-
    ttype(Γ,C, int),
    ttype(Γ,E1, X),
    ttype(Γ,E2, X).
ttype(Γ, pair(E1, E2), (X, Y)) :-
    ttype(Γ,E1, X),
    ttype(Γ,E2, Y).
ttype(Γ, fst(P), X) :-
    ttype(Γ,P, (X, _)).
ttype(Γ, snd(P), Y) :-
    ttype(Γ,P, (_,Y)).
ttype(Γ, abs(X, B), [IDET, BT]) :-
    ttype(Γ, X, IDET),
    ttype([(X, IDET)|Γ],B, BT).
ttype(Γ, app(E0, E1), Y) :-
    ttype(Γ,E1, X),
    ttype(Γ,E0, [X, Y]).
ttype(Γ, rec(X, B), T) :-
    atom(X),
    ttype(Γ,X, T),
    ttype(Γ,B, T).
ttype(X, T) :-
    ttype([], X, T).
