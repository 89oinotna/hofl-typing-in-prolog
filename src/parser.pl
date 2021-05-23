:- module(parser, [parse/2]).



preterm(abs(X, B)) --> ["\\"], variable(X), ["."], preterm(B),!.

preterm(rec(X, B)) --> ["rec"], variable(X), ["."], preterm(B).

preterm(pair(X, Y)) --> ["("], preterm(X), [","], preterm(Y), [")"].

preterm(fst(X)) --> ["fst("], preterm(X), [")"].

preterm(snd(Y)) --> ["snd("], preterm(Y), [")"].

preterm(cond(C, B1, B2)) --> ["if"], preterm(C), ["then"], preterm(B1), ["else"], preterm(B2).

preterm(var(X)) -->[Y], {atom_string(X, Y), \+number_string(_, Y)}.

preterm(X) --> expr(X).

preterm(app(F, A)) --> application(F), preterm(A).

preterm([]) --> [].

application(app(F, A)) --> preterm(F), preterm(A).

variable(var(X)) --> [Y], {atom_string(X, Y), \+number_string(_, Y)}.

expr(bin_op(plus, L, R)) --> term(L), ["+"], expr(R).
expr(bin_op(minus, L, R)) --> term(L), ["-"], expr(R).
expr(X) --> term(X).

term(bin_op(mul, L, R)) --> factor(L), ["*"], term(R).
term(X) --> factor(X).

/*factor(X) --> preterm(X).*/
factor(X) --> ["("], preterm(X), [")"].
factor(N) --> [I], {number_string(N, I)}.
factor(V) --> variable(V).
factor([]) --> [].





