:- module(latex, [info/2, pre/2, post/2]).

info(X, T) :-
	write(X),
    /*format(" : "),
    write(T),*/
    format("~n").

pre(term(abst(X, B), T0), [T1|T2] ) :-
    Y = [T1|T2],
    write(term(abst(X, B), T0)),
    format("~n ~t "),
    write(T0),
    format(" = "),
    write(Y),
    write(", "),
    write(cap(X)), %cappuccio ^
    format(" = "),
    write(T1),
    format("~n").

pre(term(bin_op(O, E1, E2), tau(I)), int):-
    write(term(bin_op(O, E1, E2), tau(I))),
    format("~n ~t "),
    write(tau(I)),
    format(" = "),
    write(int),
    write(", "),
    write(E1),
    format(" = "),
    write(int),
    write(", "),
    write(E2),
    format(" = "),
    write(int),
    format("~n").

pre(term(X, TAU), T) :-
    write(term(X, TAU)),
    format("~n ~t "),
    write(TAU),
    format(" = "),
    write(T),
    write(" "),
    
    format("~n").

post(term(X, TAU), T) :-
    var(T),
    write(term(X, TAU)),
    format("~n ~t "),
    write(TAU),    
    format("~n").

post(term(X, TAU), T) :-
    write(term(X, TAU)),
    format("~n ~t "),
    write(TAU),
    format(" = "),
    write(T),
    write(" "),
    
    format("~n").