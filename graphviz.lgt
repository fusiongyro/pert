:- object(graphviz,
	  imports(dependencyTracking),
	  instantiates(class),
	  specializes(object)).

:- info([
       comment is 'Creates GraphViz representations of dependencies.',
       author is 'Daniel Lyons',
       date is 2013/10/10]).

:- public(write_dot/2).
:- mode(write_dot(+filename, +labeller), one).
:- info(write_dot/2, [
    comment is 'Writes our dependencies to a GraphViz format file, using the supplied labeller.']).
write_dot(Filename, Labeller) :-
    once(phrase(graph(Labeller), Codes)),
    open(Filename, write, Stream, [close_on_abort(true)]),
    format(Stream, '~s', [Codes]),
    close(Stream).

% output_graph(-Project) is det.
%    Write the Project to the current output stream in GraphViz format.
:- public(output_dot/1).
output_dot(Labeller) :-
    once(phrase(graph(Labeller), Codes)),
    format('~s', [Codes]).


:- private(nl//0).
nl --> "\n".

% graph(-Project) is det.
%    Generates a Graphviz input file for the given Project.
:- public(graph//1).
graph(Labeller) -->
    "digraph G {", nl,
    "  rankdir=LR;", nl, nl,
    
    nodes(Labeller), nl,
    dependencies(Labeller), nl,
    "}", nl.

% activity_nodes(-Project) is det.
%    Generates the node list in Graphviz format.
nodes(Labeller) -->
    {	names(Names)	},
    activities(Labeller, Names).

:- public(names/1).
names(Names) :-
    findall(Name, ::(Name depends_on _; _ depends_on Name), Names1),
    sort(Names1, Names).

:- public(all_dependencies/1).
all_dependencies(Deps) :-
    setof(X depends_on Y, ::(X depends_on Y), Deps).

% dependency_nodes(-Project) is det.
%    Generates the dependency edges in Graphviz format.
dependencies(Labeller) -->
    {	all_dependencies(Dependencies)	},
    dependencies(Labeller, Dependencies).

% activities(-Project, -Names) is det.
%    Generates the Graphviz nodes for the given list of names.
activities(_, []) --> [].
activities(Labeller, [A|As]) --> activity(Labeller, A), nl, activities(Labeller, As).

properties([]) --> [].
properties([P|Rest]) --> "[", properties_inner([P|Rest]), "]".
properties_inner([P]) --> property(P).
properties_inner([P1,P2|Rest]) --> property(P1), ", ", properties_inner([P2|Rest]).

property(X=Y) --> dcg_basics:atom(X), "=", dcg_basics:atom(Y).

quoted(Atom) --> "\"", dcg_basics:atom(Atom), "\"".

% activity(-Project, Name) is det.
%    Generates a Graphviz node for the given activity under the given project.
activity(Labeller, Activity) --> 
    {
	Labeller::node_label(Activity, Label),
	Labeller::node_attrs(Activity, Attrs)
    },
    " ", quoted(Activity), " ", properties([shape=rect, label=Label | Attrs]), ";".

% dependencies(-Project, -Dependencies) is det.
%    Generate a styled edge for each pair of nodes expressing a dependency.
dependencies(_, []) --> [].
dependencies(Labeller, [Dependency|Rest]) -->
    dependency(Labeller, Dependency), dependencies(Labeller, Rest).

dependency(Labeller, A depends_on B) -->
    {	Labeller::edge_attrs(A depends_on B, Attrs)	},
    "  ", quoted(B), " -> ", quoted(A), " ", properties(Attrs), ";", nl.

:- end_object.
