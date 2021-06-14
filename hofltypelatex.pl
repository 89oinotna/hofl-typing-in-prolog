:- use_module(library(main)).
:- use_module(library(optparse)).
:- use_module(src/abstree).
:- use_module(src/typing).
:- use_module(src/canonical).
:- use_module(src/latex).

:- initialization(main, main).

main(Argv) :-
    opt_help(OptsSpec, HelpText),
    opt_parse(OptsSpec, Argv, Opts, PositionalArgs)
    . 

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

OptsSpec =
    [ [opt(mode), type(atom), default('SCAN'),
        shortflags([m]),   longflags(['mode'] ),
        help([ 'data gathering mode, one of'
             , '  SCAN: do this'
             , '  READ: do that'
             , '  MAKE: fabricate some numbers'
             , '  WAIT: don''t do anything'])]

    , [opt(cache), type(boolean), default(true),
        shortflags([r]),   longflags(['rebuild-cache']),
        help('rebuild cache in each iteration')]

    , [opt(threshold), type(float), default(0.1),
        shortflags([t,h]),  longflags(['heisenberg-threshold']),
        help('heisenberg threshold')]

    , [opt(depth), meta('K'), type(integer), default(3),
        shortflags([i,d]),longflags([depths,iters]),
        help('stop after K iterations')]

    , [opt(distances), default([1,2,3,5]),
        longflags([distances]),
        help('initial prolog term')]

    , [opt(outfile), meta('FILE'), type(atom),
        shortflags([o]),  longflags(['output-file']),
        help('write output to FILE')]

    , [opt(label), type(atom), default('REPORT'),
        shortflags([l]), longflags([label]),
        help('report label')]

    , [opt(verbose),  meta('V'), type(integer), default(2),
        shortflags([v]),  longflags([verbosity]),
        help('verbosity level, 1 <= V <= 3')]

    , [opt(path), default('/some/file/path/')]
    ].