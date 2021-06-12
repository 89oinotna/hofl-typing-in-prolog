:- module(parser, [parse/2]).

parse(TOKENS, T) :- phrase(sterm(T), TOKENS).

sterm(X) --> expr(X).

expr(Y) --> term(X), expr1(X, Y).

expr1(X, R) --> ["+"], term(T), {Y=bin_op(plus, X, T)}, expr1(Y, R).
expr1(X, R) --> ["-"], term(T), {Y=bin_op(minus, X, T)}, expr1(Y, R).
expr1(X, R) --> {R=X}.

term(Y) --> pterm(X), term1(X, Y).

term1(X, R) --> ["*"], pterm(F), {Y=bin_op(mul, X, F)}, term1(Y, R).
term1(X, R) --> {R=X}.

pterm(X) --> application(X).
pterm(X) --> atom(X).

application(B) --> atom(A), application1(A, B).
application1(A, B) --> atom(E), {Y=app(A, E)}, application1(Y, B).
application1(A, B) --> {A=B}.

atom(rec(X, B)) --> ["rec"],!, argument(X), ["."], sterm(B).

atom(pair(X, Y)) --> ["("], sterm(X), [","], sterm(Y), [")"].

atom(fst(X)) --> ["fst"],!,["("], sterm(X), [")"].

atom(snd(Y)) --> ["snd"],!,["("], sterm(Y), [")"].

atom(cond(C, B1, B2)) --> ["if"],!, sterm(C), ["then"], sterm(B1), ["else"], sterm(B2).

atom(X) --> ["("], sterm(X), [")"].
atom(N) --> [I], {number_string(N, I)}.
atom(V) --> variable(V).
atom(X) --> abstraction(X).

abstraction(abst(X, B)) --> ["\\"],!, argument(X), ["."], sterm(B).

variable(id(X)) --> [Y], {atom_string(X, Y), \+number_string(_, Y), \+member(Y, [".", "+", "-", "*", "(", ")", "\\", ","])}.
argument(X) --> [Y], {atom_string(X, Y), \+number_string(_, Y), \+member(Y, [".", "+", "-", "*", "(", ")", "\\", ","])}.

