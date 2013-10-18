:- object(csvParser).

:- public(parse_file/2).
:- mode(parse_file(+filename, -project), zero_or_one).
:- info(parse_file/2, [comment is 'Parse a CSV file']).
parse_file(Filename, P) :-
    % Read the file
    csv:csv_read_file(Filename, Rows1),

    % Parse the CSV data
    %discard_header(Rows0, Rows1),
    meta::map(row_to_activity, Rows1, Activities),
    rows_to_dependencies(Rows1, Dependencies),

    % Generate the project
    project::new(P),
    meta::map([A]>>(P::add_activity(A)), Activities),
    meta::map([D]>>(P::add_dependency(D)), Dependencies).
    
:- private(discard_header/2).
:- info(discard_header/2, [comment is 'Discards the first row if it smells like a header']).
discard_header([Row|Rest], Rest) :-
    Row =.. [row,X|_],
    \+ number(X), !.
discard_header(_, _).

% row_to_activity(?Row, ?Activity) is det.
%   Convert between rows and activity tuples.
:- private(row_to_activity/2).
row_to_activity(
	row(Name, Pessimistic, MostLikely, Optimistic, _),
	activity(Name, Pessimistic, MostLikely, Optimistic)).
row_to_activity(
	row(Name, Time, _),
	activity(Name, Time)).

% rows_to_dependencies(+Rows, +Dependencies) is det.
%   Convert between CSV rows and dependency relations.
:- private(rows_to_dependencies/2).
rows_to_dependencies([], []).
rows_to_dependencies([row(_,_,_,_,'')|Rows], Deps) :- 
    !, rows_to_dependencies(Rows, Deps).
rows_to_dependencies([row(_,_,'')|Rows], Deps) :- 
    !, rows_to_dependencies(Rows, Deps).
rows_to_dependencies([row(Name, _,_,_, DependencyValue)|Rows], Deps) :-
    atomic_list_concat(Dependencies, ',', DependencyValue),
    setof(Name depends_on Dep, list::member(Dep, Dependencies), ConstructedDeps),
    rows_to_dependencies(Rows, MoreDeps),
    list::append(ConstructedDeps, MoreDeps, Deps).
rows_to_dependencies([row(Name, _, DependencyValue)|Rows], Deps) :-
    atomic_list_concat(Dependencies, ',', DependencyValue),
    setof(Name depends_on Dep, list::member(Dep, Dependencies), ConstructedDeps),
    rows_to_dependencies(Rows, MoreDeps),
    list::append(ConstructedDeps, MoreDeps, Deps).

:- end_object.
