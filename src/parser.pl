:- module(parser, [parse/2]).

parse(TOKENS, T) :- phrase(preterm(T), TOKENS).

preterm([]) --> [].

preterm(X) --> expr(X).

preterm(abs(X, B)) --> ["\\"], variable(X), ["."], preterm(B).

preterm(app(F, A)) --> preterm(F), expr(A).

preterm(rec(X, B)) --> ["rec"], variable(X), ["."], preterm(B).

preterm(pair(X, Y)) --> ["("], preterm(X), [","], preterm(Y), [")"].

preterm(fst(X)) --> ["fst("], preterm(X), [")"].

preterm(snd(Y)) --> ["snd("], preterm(Y), [")"].

preterm(cond(C, B1, B2)) --> ["if"], preterm(C), ["then"], preterm(B1), ["else"], preterm(B2).

variable(var(X)) --> [Y], {atom_string(X, Y), \+number_string(_, Y), \+member(Y, [".", "+", "-", "*", "(", ")", "\\", ","])}.

expr(Y) --> term(X), expr1(X, Y).

expr1(X, R) --> ["+"], term(T), {Y=bin_op(plus, X, T)}, expr1(Y, R).
expr1(X, R) --> ["-"], term(T), {Y=bin_op(minus, X, T)}, expr1(Y, R).
expr1(X, R) --> {R=X}.

term(Y) --> factor(X), term1(X, Y).

term1(X, R) --> ["*"], factor(F), {Y=bin_op(mul, X, F)}, term1(Y, R).
term1(X, R) --> {R=X}.


factor(X) --> ["("], preterm(X), [")"].
factor(N) --> [I], {number_string(N, I)}.
factor(V) --> variable(V).


