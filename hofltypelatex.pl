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
        member(help(true), Opts) -> (opt_help(OptsSpec, Help), write(Help));
        
        member(mode(canonical), Opts) -> opts_canonical(Opts, PositionalArgs);

        member(mode(typing), Opts) -> opts_type(Opts, PositionalArgs)
        
    
        ); (opt_help(OptsSpec, Help), write(Help))
    . 

opts_canonical(Opts, PositionalArgs) :-
    (
        member(infile(IF), Opts) -> read_file_to_string(IF, T, []);
        ([T|Tail] = PositionalArgs, write(T))
    ),
    get_canonical(T, C),
    (
        member(outfile(OF), Opts) -> write_file(OF, C);
        write(C)
    ).

opts_type(Opts, PositionalArgs) :-
    (
        member(infile(IF), Opts) -> (
            \+var(IF) -> read_file_to_string(IF, T, []);
            [T|Tail] = PositionalArgs
        )
    ),
    abstree(T, TERM),!,
    
    inferType(TERM, TYPE, TypedTerm),

    (
        member(outfile(OF), Opts) -> (write_latex(TypedTerm, String), write_file(OF, String));
        (write_latex(TypedTerm, String), write(String))
    ).

write_file(F, T) :-
    open(F,write,OS),
    write(OS,T),
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
    inferType(TERM, TYPE, TypedTerm),
    write(TypedTerm),
    format("~n"),
    write_latex(TypedTerm).

get_canonical(T, C) :- 
    abstree(T, TERM),
    canonic(TERM, C).

opts_spec(
    [ [opt(mode), type(atom), default('SCAN'),
        shortflags([m]),   longflags(['mode'] ),
        help([ 'Modes are:'
             , '  CANONICAL: derive the canonical form'
             , '  TYPING: assigns types to a term'])]

    , [opt(outfile), meta('OFILE'), type(atom),
        shortflags([o]),  longflags(['output-file']),
        help('write output to OFILE')]

    , [opt(infile),  meta('IFILE'), type(atom),
        shortflags([f]), help('read term from IFILE')]

    , [opt(help),  type(boolean), default(false), 
        shortflags([h]), help('Help')]
    , [opt(latex),  type(boolean), default(false), 
        shortflags([l]), help('Write latex typing')]
    ]).