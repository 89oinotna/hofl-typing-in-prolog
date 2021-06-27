:- module(latex, [get_latex/2]).

% to get the latex typing of the term
get_latex(TT, String) :-
    get_latex(0, TT, String).

%% this helps with the instantiation as Tau for uninstantiated prolog variables in the term
type_to_write( NTAU, T, RNTAU, tau(RNTAU)) :-
    var(T),
    RNTAU is NTAU +1,
    T = tau(RNTAU).

type_to_write( NTAU, tpair(T1, T2), RNTAU1, tpair(WT1, WT2)) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    type_to_write(RNTAU, T2, RNTAU1, WT2).

type_to_write( NTAU, tfun(TA, TB), RNTAU1, tfun(WTA, WTB)) :-
    type_to_write( NTAU, TA, RNTAU, WTA),
    type_to_write(RNTAU, TB, RNTAU1, WTB).

type_to_write( NTAU, T,  NTAU, T) :-
    \+var(T).

% gets the type as string
get_type(tfun(TA, TB), String) :-
    get_type(TA, S1),
    get_type(TB, S2),
    swritef(String, "%w \\\\rightarrow %w", [S1, S2]).

get_type(tpair(T1, T2), String) :-
    get_type(T1, S1),
    get_type(T2, S2),
    swritef(String, "( %w * %w )", [S1, S2]).

get_type(tau(N), String) :-
    swritef(String, "\\\\tau_{%w}", [N]).

get_type(T, String) :-
    swritef(String, "%w", [T]).

/*get_latex(NTAU, typed(abst(X, B), tfun(TA, TB)), String) :- 
    type_to_write(NTAU, TA, RNTAU, WTA),
    type_to_write(RNTAU, TB, RNTAU1, WTB),
    write("\\underbracket{"),
        write("\\lambda"),
        format("\\underbracket{~p}_{\\mathsf{", X),
        get_type(WTA, String),
        write("}} . "),
        get_latex(RNTAU1, B, String),
        %write("\\underbracket{"),
         %   get_latex(B),
          %  write("}_{\\mathsf{"),
          %  write(TB),
          %  write("}}"),
    write("}_{\\mathsf{"),
    get_type( tfun(WTA, WTB), String),
    write("}}").*/

get_latex(NTAU, typed(abst(X, B), tfun(TA, TB)), String) :- 
        type_to_write(NTAU, TA, RNTAU, WTA),
        type_to_write(RNTAU, TB, RNTAU1, WTB),
        get_type(WTA, S1),
        get_latex(RNTAU1, B, S2),
        get_type( tfun(WTA, WTB), S3),
        swritef(String, "\\underbracket{\\\\lambda\\underbracket{ %w }_{\\mathsf{%w}} . %w}_{\\mathsf{%w}}", [X, S1, S2, S3]).

get_latex( _, typed(N, int), String) :-
    number(N),
    swritef(String, "\\underbracket{%w}_{\\mathsf{int}}", [N]).

get_latex( NTAU, typed(bin_op(OP, TT1, TT2),int), String) :-
    get_latex( NTAU, TT1, S1),
    (
        mul = OP -> SOP = "*" ;
        plus = OP -> SOP = "+";
        minus = OP -> SOP = "-"
    ),
    get_latex( NTAU, TT2, S2),
    swritef(String, "\\underbracket{%w %w %w}_{\\mathsf{int}}", [S1, SOP, S2]).


get_latex( NTAU,typed(cond(TTC, TT1, TT2), T), String) :- 
    type_to_write( NTAU, T, RNTAU, WT),
    get_latex(RNTAU,TTC, S1),
    get_latex(RNTAU,TT1, S2),
    get_latex(RNTAU,TT2, S3),
    get_type(WT, S4),
    swritef(String, "\\underbracket{ if %w then %w else %w}_{\\mathsf{%w}}", [S1, S2, S3, S4]).

get_latex( NTAU,typed(pair(TT1, TT2), tpair(T1, T2)), String) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    type_to_write(RNTAU, T2, RNTAU1, WT2),
    get_latex(RNTAU1,TT1, S1),
    get_latex(RNTAU1,TT2, S2),
    get_type(tpair(WT1, WT2), S3),
    swritef(String, "\\underbracket{(%w, %w)}_{\\mathsf{%w}}", [S1, S2, S3]).

get_latex( NTAU,typed(fst(TT1), T1), String) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    get_latex(RNTAU, TT1, S1),
    get_type(WT1, S2),
    swritef(String, "\\underbracket{fst(%w)}_{\\mathsf{%w}}", [S1, S2]).   

get_latex( NTAU,typed(snd(TT2), T2), String) :-
    type_to_write( NTAU, T2, RNTAU, WT2),
    get_latex(RNTAU,TT2, S1),
    get_type(WT2, S2),
    swritef(String, "\\underbracket{snd(%w)}_{\\mathsf{%w}}", [S1, S2]).    

get_latex( NTAU,typed(app(TT, TT0), T1), String) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    get_latex(RNTAU,TT, S1),
    get_latex(RNTAU,TT0,S2),
    get_type(WT1,S3),
    swritef(String, "\\underbracket{%w %w}_{\\mathsf{%w}}", [S1, S2, S3]).            

/*get_latex( NTAU, typed(rec(X, TT), T), String) :-
    type_to_write( NTAU, T, RNTAU, WT),
    write("\\underbracket{rec "),
        format("\\underbracket{~p}_{\\mathsf{", X),
        get_type(WT),
        write("}} . "),
        get_latex(RNTAU, TT),
        %write("\\underbracket{"),
         %   get_latex(RNTAU, TT),
         %   write("}_{\\mathsf{"),
          %  get_type(T),
           % write("}}"),
    write("}_{\\mathsf{"),
    get_type(WT),
    write("}}").*/

get_latex( NTAU, typed(rec(X, TT), T), String) :-
    type_to_write( NTAU, T, RNTAU, WT),
    get_type(WT, S1),
    get_latex(RNTAU, TT, S2),
    get_type(WT, S3),
    swritef(String, "\\underbracket{rec \\underbracket{%w}_{\\mathsf{%w}} . %w}_{\\mathsf{%w}}", [X, S1, S2, S3]).

get_latex( NTAU,typed(id(X), T), String) :- 
    type_to_write( NTAU, T, _, WT),
    get_type(WT, S1),
    swritef(String, "\\underbracket{%w}_{\\mathsf{%w}}", [X, S1]).

