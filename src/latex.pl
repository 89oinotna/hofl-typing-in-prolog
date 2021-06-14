:- module(latex, [write_latex/1]).

write_latex(typed(abst(X, B), [TA | TB])) :- 
    write("\\underbracket{"),
        write("\\lambda"),
        format("\\underbracket{~p}_{\\mathsf{~p}} . ", [X, TA]),
        write_latex(B),
        /*write("\\underbracket{"),
            write_latex(B),
            write("}_{\\mathsf{"),
            write(TB),
            write("}}"),*/
    write("}_{\\mathsf{"),
    write([TA | TB]),
    write("}}").

write_latex(typed(N, int)) :-
    number(N),
    format("\\underbracket{~d}_{\\mathsf{int}}", N).

write_latex(typed(bin_op(OP, TT1, TT2),int)) :-
    write("\\underbracket{"),
    write_latex(TT1),
    write(OP),
    write_latex(TT2),
    write("}_{\\mathsf{int}}").

write_latex(typed(cond(TTC, TT1, TT2), T)) :- 
    write(" if \\underbracket{"),
    write_latex(TTC),
    write(" then "),
    write_latex(TT1),
    write(" else "),
    write_latex(TT2),
    write("}_{\\mathsf{"),
    write(T),
    write("}}").

write_latex(typed(pair(TT1, TT2), (T1, T2))) :-
    write("\\underbracket{("),
    write_latex(TT1),
    write(", "),
    write_latex(TT2),
    write(")}_{\\mathsf{"),
    write((T1, T2)),
    write("}}").

write_latex(typed(fst(TT1), T1)) :-
    write("\\underbracket{fst("),
    write_latex(TT1),
    write(")}_{\\mathsf{"),
    write(T1),
    write("}}").

write_latex(typed(snd(TT2), T2)) :-
    write("\\underbracket{snd("),
    write_latex(TT2),
    write(")}_{\\mathsf{"),
    write(T2),
    write("}}").

write_latex(typed(app(TT, TT0), T1)) :-
    write("\\underbracket{"),
    write_latex(TT),
    write(" "),
    write_latex(TT0),
    write("}_{\\mathsf{"),
    write(T1),
    write("}}").

write_latex(typed(rec(X, TT), T)) :-
    write("\\underbracket{rec "),
        format("\\underbracket{~p}_{\\mathsf{~p}} . ", [X, T]),
        write("\\underbracket{"),
            write_latex(TT),
            write("}_{\\mathsf{"),
            write(T),
            write("}}"),
    write(")}_{\\mathsf{"),
    write(T),
    write("}}").

write_latex(typed(id(X), T)) :- 
    write("\\underbracket{"),
    write(X),
    write("}_{\\mathsf{"),
    write(T),
    write("}}").

write_latex(typed(Term, Type)) :-
    write("\\underbracket{"),
    write(Term),
    %write_latex(Term),
    write("}_{\\mathsf{"),
    write(Type),
    write("}}").


/*\[\underbracket{
    \underbracket{x}_{
        \mathit{int}} - \underbracket{1}_{\mathit{int}}
    }_{\mathsf{int}}
\]*/