:- object(main).

:- public(process/1).
process(Filename) :-
    csvParser::parse_file(Filename, Project),
    Project::pert(Pert),
    Pert::output_graph.

:- public(main/0).
main :-
    optparse:opt_arguments([], _, Args),
    meta::map(process, Args).

:- end_object.
