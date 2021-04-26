ttype(_, X, _) :- atom(X).
ttype(Γ,i(X), T) :-
    nth0(_, Γ, (X, T)).
ttype(_, N, int) :- number(N).
ttype(Γ, op(_, E1, E2), int) :- 
    ttype(Γ,E1, int),
    ttype(Γ,E2, int).
ttype(Γ, if(C, E1, E2), T) :-
    ttype(Γ,C, int),
    ttype(Γ,E1, T),
    ttype(Γ,E2, T).
ttype(Γ, pair(E1, E2), (T1, T2)) :-
    ttype(Γ,E1, T1),
    ttype(Γ,E2, T2).
ttype(Γ, fst(P), T) :-
    ttype(Γ,P, (T, _)).
ttype(Γ, snd(P), T) :-
    ttype(Γ,P, (_,T)).
ttype(Γ, abs(X, B), [TIde, TB]) :-
    ttype(Γ, X, TIde),
    ttype([(X, TIde)|Γ], B, TB).
ttype(Γ, app(E1, E2), T0) :-
    ttype(Γ,E2, T2),
    ttype(Γ,E1, [T2, T0]).
ttype(Γ, rec(X, B), T) :-
    ttype(Γ,X, T),
    ttype([(X, T)|Γ], B, T).
ttype(X, T) :-
    ttype([], X, T).
