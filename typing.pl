info(X, T) :-
	write(X),
    format(" : "),
    write(T),
    format("~n").

ttype(_, X, _) :- atom(X).
ttype(Γ,i(X), T) :-
    info(i(X), T),
    nth0(_, Γ, (X, T)),
    info(i(X), T).
ttype(_, N, int) :- 
    number(N),
    info(N, int).
ttype(Γ, op(O, E1, E2), int) :-
    info(op(O, E1, E2), int),
    ttype(Γ,E1, int),
    ttype(Γ,E2, int),
    info(op(O, E1, E2), int).
ttype(Γ, if(C, E1, E2), T) :-
    info(if(C, E1, E2), T),
    ttype(Γ,C, int),
    ttype(Γ,E1, T),
    ttype(Γ,E2, T),
    info(if(C, E1, E2), T).
ttype(Γ, pair(E1, E2), (T1, T2)) :-
    info( pair(E1, E2), T),
    ttype(Γ,E1, T1),
    ttype(Γ,E2, T2),
    info( pair(E1, E2), T).
ttype(Γ, fst(P), T) :-
    info(fst(P), T),
    ttype(Γ,P, (T, _)),
    info(fst(P), T).
ttype(Γ, snd(P), T) :-
    info(snd(P), T),
    ttype(Γ,P, (_,T)),
    info(snd(P), T).
ttype(Γ, abs(X, B), [TIde, TB]) :-
    info(abs(X, B), [TIde, TB]),
    ttype(Γ, X, TIde),
    ttype([(X, TIde)|Γ], B, TB),
    info(abs(X, B), [TIde, TB]).
ttype(Γ, app(E1, E2), T0) :-
    info(app(E1, E2), T0),
    ttype(Γ,E1, [T2, T0]),
    ttype(Γ,E2, T2),
    info(app(E1, E2), T0).
ttype(Γ, rec(X, B), T) :-
    info(rec(X, B), T),
    ttype(Γ,X, T),
    ttype([(X, T)|Γ], B, T),
    info(rec(X, B), T).
ttype(X, T) :-
    ttype([], X, T).
