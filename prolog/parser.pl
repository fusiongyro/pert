/**<module> Parses CSV files containing projects. */
:- module(parser, [parse_file/2]).

:- use_module(library(csv)).
:- use_module(library(lambda)).

:- use_module(project).

% The input file is CSV with the following fields:
%
%   Name, Pessimistic, Most Likely, Optimistic, Dependencies
%
% The name is assumed to be a string (will be treated as an atom).
% The three cost columns that follow must be numeric in nature.
% The dependencies column is to be a comma-separated list of names.

%! parse_file(+Filename, +Project) is semidet.
%    Parse Filename to CSV and convert to a Project.
parse_file(Filename, project(Activities, Dependencies)) :-
    csv_read_file(Filename, Rows),
    maplist(row_to_activity, Rows, Activities),
    rows_to_dependencies(Rows, Dependencies).

% row_to_activity(?Row, ?Activity) is det.
%   Convert between rows and activity tuples.
row_to_activity(
	row(Name, Pessimistic, MostLikely, Optimistic, _),
	activity(Name, Pessimistic, MostLikely, Optimistic)).

% rows_to_dependencies(+Rows, +Dependencies) is det.
%   Convert between CSV rows and dependency relations.
rows_to_dependencies([], []).
rows_to_dependencies([row(_,_,_,_,'')|Rows], Deps) :- 
    !, rows_to_dependencies(Rows, Deps).
rows_to_dependencies([row(Name, _,_,_, DependencyValue)|Rows], Deps) :-
    atomic_list_concat(Dependencies, ',', DependencyValue),
    setof(Name depends_on Dep, member(Dep, Dependencies), ConstructedDeps),
    rows_to_dependencies(Rows, MoreDeps),
    append(ConstructedDeps, MoreDeps, Deps).
