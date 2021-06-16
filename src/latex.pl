:- module(latex, [write_latex/1]).

write_latex(TT) :-
    write_latex(0, TT).

type_to_write( NTAU, T,  NTAU, T) :-
    \+var(T).

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

write_type(tfun(TA, TB)) :-
    write_type(TA),
    write(" \\rightarrow "),
    write_type(TB).

write_type(tpair(T1, T2)) :-
    write("( "),
    write_type(T1),
    write(", "),
    write_type(T2),
    write(" )").

write_type(tau(N)) :-
    format("\\tau_{~p}", N).

write_type(T) :-
    write(T).

write_latex(NTAU, typed(abst(X, B), tfun(TA, TB))) :- 
    type_to_write(NTAU, TA, RNTAU, WTA),
    type_to_write(RNTAU, TB, RNTAU1, WTB),
    write("\\underbracket{"),
        write("\\lambda"),
        format("\\underbracket{~p}_{\\mathsf{", X),
        write_type(WTA),
        write("}} . "),
        write_latex(RNTAU1, B),
        /*write("\\underbracket{"),
            write_latex(B),
            write("}_{\\mathsf{"),
            write(TB),
            write("}}"),*/
    write("}_{\\mathsf{"),
    write_type( tfun(WTA, WTB)),
    write("}}").

write_latex( NTAU, typed(N, int)) :-
    number(N),
    format("\\underbracket{~d}_{\\mathsf{int}}", N).

write_latex( NTAU, typed(bin_op(OP, TT1, TT2),int)) :-
    write("\\underbracket{"),
    write_latex( NTAU, TT1),
    write(OP),
    write_latex( NTAU, TT2),
    write("}_{\\mathsf{int}}").

write_latex( NTAU,typed(cond(TTC, TT1, TT2), T)) :- 
    type_to_write( NTAU, T, RNTAU, WT),
    write("\\underbracket{ if "),
    write_latex(RNTAU,TTC),
    write(" then "),
    write_latex(RNTAU,TT1),
    write(" else "),
    write_latex(RNTAU,TT2),
    write("}_{\\mathsf{"),
    write_type(WT),
    write("}}").

write_latex( NTAU,typed(pair(TT1, TT2), tpair(T1, T2))) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    type_to_write(RNTAU, T2, RNTAU1, WT2),
    write("\\underbracket{("),
    write_latex(RNTAU1,TT1),
    write(", "),
    write_latex(RNTAU1,TT2),
    write(")}_{\\mathsf{"),
    write_type(tpair(WT1, WT2)),
    write("}}").

write_latex( NTAU,typed(fst(TT1), T1)) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    write("\\underbracket{fst("),
    write_latex(RNTAU, TT1),
    write(")}_{\\mathsf{"),
    write_type(WT1),
    write("}}").

write_latex( NTAU,typed(snd(TT2), T2)) :-
    type_to_write( NTAU, T2, RNTAU, WT2),
    write("\\underbracket{snd("),
    write_latex(RNTAU,TT2),
    write(")}_{\\mathsf{"),
    write_type(WT2),
    write("}}").

write_latex( NTAU,typed(app(TT, TT0), T1)) :-
    type_to_write( NTAU, T1, RNTAU, WT1),
    write("\\underbracket{"),
    write_latex(RNTAU,TT),
    write(" "),
    write_latex(RNTAU,TT0),
    write("}_{\\mathsf{"),
    write_type(WT1),
    write("}}").

write_latex( NTAU, typed(rec(X, TT), T)) :-
    type_to_write( NTAU, T, RNTAU, WT),
    write("\\underbracket{rec "),
        format("\\underbracket{~p}_{\\mathsf{", X),
        write_type(WT),
        write("}} . "),
        write_latex(RNTAU, TT),
        /*write("\\underbracket{"),
            write_latex(RNTAU, TT),
            write("}_{\\mathsf{"),
            write_type(T),
            write("}}"),*/
    write("}_{\\mathsf{"),
    write_type(WT),
    write("}}").

write_latex( NTAU,typed(id(X), T)) :- 
    type_to_write( NTAU, T, RNTAU, WT),
    write("\\underbracket{"),
    write(X),
    write("}_{\\mathsf{"),
    write_type(WT),
    write("}}").

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