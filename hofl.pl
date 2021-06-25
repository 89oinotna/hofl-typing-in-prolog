:- use_module(library(main)).
:- use_module(library(optparse)).
:- use_module(src/abstree).
:- use_module(src/typing).
:- use_module(src/canonical).
:- use_module(src/latex).
%:- style_check(-singleton).

:- initialization(main, main).

main(Argv) :-
    opts_spec(OptsSpec),
    opt_parse(OptsSpec, Argv, Opts, PositionalArgs),
    (
        member(help(true), Opts) -> (opt_help(OptsSpec, Help), write("Usage: swipl hofl.pl <?opts> ?term \n"), write(Help));
        
        member(mode(canonical), Opts) -> opts_canonical(Opts, PositionalArgs);

        member(mode(typing), Opts) -> opts_type(Opts, PositionalArgs);

        (opt_help(OptsSpec, Help),write("Usage: swipl hofl.pl <?opts> ?term"), write(Help))
    )    
    . 

opts_canonical(Opts, PositionalArgs) :-
    (
        member(infile(IF), Opts) -> (
            \+var(IF) -> read_file_to_string(IF, T, []);
            [T|_] = PositionalArgs
        )
    ),
    abstree(T, TERM),!,
    write(TERM),
    canonic(TERM, C),
    write(C).
    %(member(outfile(OF), Opts) -> (\+var(OF), write_file(OF, C))).
    

opts_type(Opts, PositionalArgs) :-
    (
        member(infile(IF), Opts) -> (
            \+var(IF) -> read_file_to_string(IF, T, []);
            [T|_] = PositionalArgs
        )
    ),
    abstree(T, TERM),!,
    
    inferType(TERM, _, TypedTerm),
    write(TypedTerm),
    member(outfile(OF), Opts) -> (
        (\+var(OF), get_latex(TypedTerm, String), write_file(OF, String));
        (get_latex(TypedTerm, String), format("~n"), write(String))
    )
    . 

write_file(F, T) :-
    open(F,write,OS),
    writeln(OS, "\\documentclass[10pt]{article}"),
    writeln(OS, "\\usepackage{mathtools}"),
    writeln(OS, "\\begin{document}"),
    writeln(OS, "\\["),
    write(OS,T),
    writeln(OS, "\\]"),
    writeln(OS,"\\end{document}"),
    close(OS).


ttype(X, TYPE, TypeTerm) :-
    abstree(X, TERM),!,
    write(TERM),
    format("~n"),
    inferType(TERM, TYPE, TypeTerm).

llatex(X) :-
    abstree(X, TERM),!,
    write(TERM),
    format("~n"),
    inferType(TERM, _, TypedTerm),
    write(TypedTerm),
    format("~n"),
    get_latex(TypedTerm, S),
    write(S).

get_canonical(T, C) :- 
    abstree(T, TERM),
    canonic(TERM, C).

opts_spec(
    [ [opt(mode), type(atom), default('SCAN'),
        shortflags([m]),   longflags(['mode'] ),
        help([ 'Modes are:'
             , '  canonical: derive the canonical form'
             , '  typing: assigns types to a term'])]

    , [opt(outfile), meta('OFILE'), type(atom),
        shortflags([o]),  longflags(['output-file']),
        help('write output (typing) to OFILE')]

    , [opt(infile),  meta('IFILE'), type(atom),
        shortflags([f]), help('read term from IFILE')]

    , [opt(help),  type(boolean), default(false), 
        longflags(['help'] ),shortflags([h]), help('Help')]
    
    ]).