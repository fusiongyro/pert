#!/opt/local/bin/swipl -q -t main -s

:- module(main, [main/0]).

:- use_module(library(optparse)).

:- use_module(parser).
:- use_module(graph).

process(Filename) :-
    parser:parse_file(Filename, Project),
    graph:output_graph(Project).

main :-
    optparse:opt_arguments([], _, Args),
    maplist(process, Args).
