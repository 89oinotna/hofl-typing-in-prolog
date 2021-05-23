:- module(abstree, [tree/2]).
:- use_module(lexer).
:- use_module(parser).

tree(INPUT, T) :- 
    tokenize(INPUT, TOKENS),
    parse(TOKENS, T).
