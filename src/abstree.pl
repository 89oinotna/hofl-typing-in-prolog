:- module(abstree, [abstree/2]).
:- use_module(lexer).
:- use_module(parser).

abstree(INPUT, T) :- 
    tokenize(INPUT, TOKENS),
    parse(TOKENS, T).
