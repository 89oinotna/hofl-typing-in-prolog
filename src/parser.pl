:- module(parser, [parse/2]).

preterm(X) --> expr(X).

preterm(abs(X, B)) --> ["\\"], variable(X), ["."], preterm(B).

preterm(app(F, A)) --> preterm(F), expr(A).

preterm(rec(X, B)) --> ["rec"], variable(X), ["."], preterm(B).

preterm(pair(X, Y)) --> ["("], preterm(X), [","], preterm(Y), [")"].

preterm(fst(X)) --> ["fst("], preterm(X), [")"].

preterm(snd(Y)) --> ["snd("], preterm(Y), [")"].

preterm(cond(C, B1, B2)) --> ["if"], preterm(C), ["then"], preterm(B1), ["else"], preterm(B2).

variable(var(X)) --> [Y], {atom_string(X, Y), \+number_string(_, Y), \+member(Y, ['.', '+', '-', '*', '(', ')', '\\', ','])}.

expr(bin_op(plus, L, R)) --> term(L), ["+"], expr(R).
expr(bin_op(minus, L, R)) --> term(L), ["-"], expr(R).

expr(X) --> term(X).

term(bin_op(mul, L, R)) --> factor(L), ["*"], term(R).
term(X) --> factor(X).

/*factor(X) --> preterm(X).*/
factor(X) --> ["("], preterm(X), [")"].
factor(N) --> [I], {number_string(N, I)}.
factor(V) --> variable(V).