:- module(lexer, [tokenize/2]).

tokenize(INPUT, TOKENS) :- 
    string_chars(INPUT, Chars),
    phrase(tokens(TOKENS), Chars).

tokens([T|Ts]) --> whitespace, token(T), whitespace, tokens(Ts).
tokens([]) --> [].

whitespace --> [S], { char_type(S, space) }, whitespace.
whitespace --> [].

token(S) --> [C], { symbol(C, S)}.
token(Ws) --> word(Wl), {string_chars(Ws, Wl)}.

symbol(C, S) :-
    member(C, ['.', '+', '-', '*', '(', ')', '\\', ',']),
    string_chars(S, [C]).
    
word([C|Cs]) --> wl(C), wr(Cs).
wl(C) --> [C], {char_type(C, alnum)}.
wr([C|Cs]) --> [C], {char_type(C, alnum)}, wr(Cs).
wr([]) --> [].


    