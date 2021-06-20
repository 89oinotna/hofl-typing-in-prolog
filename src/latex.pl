:- module(latex, [write_latex/2]).

write_latex(TT, String) :-
    write_latex(0, TT, String),
    write(String).

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


write_type(tfun(TA, TB), String) :-
    write_type(TA, S1),
    %write(" \\rightarrow "),
    write_type(TB, S2),
    swritef(String, "%w \\rightarrow %w", [S1, S2]).

write_type(tpair(T1, T2), String) :-
    %write("( "),
    write_type(T1, S1),
   % write(" * "),
    write_type(T2, S2),
   % write(" )"),
    swritef(String, "( %w * %w )", [S1, S2]).

write_type(tau(N), String) :-
    %format("\\tau_{~p}", N).
    swritef(String, "\\tau_{%w}", [N]).

write_type(T, String) :-
    %write(T).
    swritef(String, "%w", [T]).

/*write_latex(NTAU, typed(abst(X, B), tfun(TA, TB)), String) :- 
    type_to_write(NTAU, TA, RNTAU, WTA),
    type_to_write(RNTAU, TB, RNTAU1, WTB),
    write("\\underbracket{"),
        write("\\lambda"),
        format("\\underbracket{~p}_{\\mathsf{", X),
        write_type(WTA, String),
        write("}} . "),
        write_latex(RNTAU1, B, String),
        %write("\\underbracket{"),
         %   write_latex(B),
          %  write("}_{\\mathsf{"),
          %  write(TB),
          %  write("}}"),
    write("}_{\\mathsf{"),
    write_type( tfun(WTA, WTB), String),
    write("}}").*/

write_latex(NTAU, typed(abst(X, B), tfun(TA, TB)), String) :- 
        type_to_write(NTAU, TA, RNTAU, WTA),
        type_to_write(RNTAU, TB, RNTAU1, WTB),
        write_type(WTA, S1),
        write_latex(RNTAU1, B, S2),
        write_type( tfun(WTA, WTB), S3),
        swritef(String, "\\underbracket{\\lambda\\underbracket{ %w }_{\\mathsf{%w}} . %w}_{\\mathsf{%w}}", [X, S1, S2, S3]).

write_latex( NTAU, typed(N, int), String) :-
    number(N),
    %format("\\underbracket{~d}_{\\mathsf{int}}", N).
    swritef(String, "\\underbracket{%w}_{\\mathsf{int}}", [N]).

write_latex( NTAU, typed(bin_op(OP, TT1, TT2),int), String) :-
    %write("\\underbracket{"),
    write_latex( NTAU, TT1, S1),
    % write(OP),
    (
        mul = OP -> SOP = "*" ;
        plus = OP -> SOP = "+";
        minus = OP -> SOP = "-"
    ),
    write_latex( NTAU, TT2, S2),
    %write("}_{\\mathsf{int}}").
    swritef(String, "\\underbracket{%w %w %w}_{\\mathsf{int}}", [S1, SOP, S2]).


write_latex( NTAU,typed(cond(TTC, TT1, TT2), T), String) :- 
    type_to_write( NTAU, T, RNTAU, WT),
    %write("\\underbracket{ if "),
    write_latex(RNTAU,TTC, S1),
    %write(" then "),
    write_latex(RNTAU,TT1, S2),
    %write(" else "),
    write_latex(RNTAU,TT2, S3),
    %write("}_{\\mathsf{"),
    write_type(WT, S4),
    %write("}}").
    swritef(String, "\\underbracket{ if %w then %w else %w}_{\\mathsf{%w}}", [S1, S2, S3, S4]).

write_latex( NTAU,typed(pair(TT1, TT2), tpair(T1, T2)), String) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    type_to_write(RNTAU, T2, RNTAU1, WT2),
    %write("\\underbracket{("),
    write_latex(RNTAU1,TT1, S1),
    %write(", "),
    write_latex(RNTAU1,TT2, S2),
    %write(")}_{\\mathsf{"),
    write_type(tpair(WT1, WT2), S3),
    %write("}}").
    swritef(String, "\\underbracket{(%w, %w)}_{\\mathsf{%w}}", [S1, S2, S3]).

write_latex( NTAU,typed(fst(TT1), T1), String) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    %write("\\underbracket{fst("),
    write_latex(RNTAU, TT1, S1),
    %write(")}_{\\mathsf{"),
    write_type(WT1, S2),
    %write("}}").
    swritef(String, "\\underbracket{fst(%w)}_{\\mathsf{%w}}", [S1, S2]).   

write_latex( NTAU,typed(snd(TT2), T2), String) :-
    type_to_write( NTAU, T2, RNTAU, WT2),
    %write("\\underbracket{snd("),
    write_latex(RNTAU,TT2, S1),
    %write(")}_{\\mathsf{"),
    write_type(WT2, S2),
    %write("}}").
    swritef(String, "\\underbracket{snd(%w)}_{\\mathsf{%w}}", [S1, S2]).    

write_latex( NTAU,typed(app(TT, TT0), T1), String) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    %write("\\underbracket{"),
    write_latex(RNTAU,TT, S1),
    %write(" "),
    write_latex(RNTAU,TT0,S2),
    %write("}_{\\mathsf{"),
    write_type(WT1,S3),
    %write("}}").
    swritef(String, "\\underbracket{%w %w}_{\\mathsf{%w}}", [S1, S2, S3]).            

/*write_latex( NTAU, typed(rec(X, TT), T), String) :-
    type_to_write( NTAU, T, RNTAU, WT),
    write("\\underbracket{rec "),
        format("\\underbracket{~p}_{\\mathsf{", X),
        write_type(WT),
        write("}} . "),
        write_latex(RNTAU, TT),
        %write("\\underbracket{"),
         %   write_latex(RNTAU, TT),
         %   write("}_{\\mathsf{"),
          %  write_type(T),
           % write("}}"),
    write("}_{\\mathsf{"),
    write_type(WT),
    write("}}").*/

write_latex( NTAU, typed(rec(X, TT), T), String) :-
    type_to_write( NTAU, T, RNTAU, WT),
    write_type(WT, S1),
    write_latex(RNTAU, TT, S2),
    write_type(WT, S3),
    swritef(String, "\\underbracket{rec \\underbracket{%w}_{\\mathsf{%w}} . %w\\underbracket{%w}_{\\mathsf{%w}}}_{\\mathsf{%w}}", [X, S1, S2, S3]).

write_latex( NTAU,typed(id(X), T), String) :- 
    type_to_write( NTAU, T, RNTAU, WT),
    %write("\\underbracket{"),
    %write(X),
    %write("}_{\\mathsf{"),
    write_type(WT, S1),
    %write("}}").
    swritef(String, "\\underbracket{%w}_{\\mathsf{%w}}", [X, S1]).

/*write_latex( NTAU,typed(Term, Type)) :-
    write("\\underbracket{"),
    write(Term),
    %write_latex(Term),
    write("}_{\\mathsf{"),
    write(Type),
    write("}}").*/


/*\[\underbracket{
    \underbracket{x}_{
        \mathit{int}} - \underbracket{1}_{\mathit{int}}
    }_{\mathsf{int}}
\]*/