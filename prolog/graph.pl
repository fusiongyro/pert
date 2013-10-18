% Writes graphs of projects.
:- module(graph, [write_graph/2, output_graph/1]).

:- use_module(library(dcg/basics)).

:- use_module(project).
:- use_module(pert).

% graph(-Project) is det.
%    Generates a Graphviz input file for the given Project.
graph(Project) -->
    "digraph G {\n",
    "  rankdir=LR;\n\n",
    activity_nodes(Project),
    "\n",
    dependency_nodes(Project),
    "}\n".

% activity_nodes(-Project) is det.
%    Generates the node list in Graphviz format.
activity_nodes(Project) -->
    {	activity_names(Project, Names)	},
    activities(Project, Names).

% dependency_nodes(-Project) is det.
%    Generates the dependency edges in Graphviz format.
dependency_nodes(Project) -->
    {	all_dependencies(Project, Dependencies)	},
    dependencies(Project, Dependencies).

% activities(-Project, -Names) is det.
%    Generates the Graphviz nodes for the given list of names.
activities(_, []) --> [].
activities(Project, [A|As]) --> activity(Project, A), activities(Project, As).

% activity(-Project, Name) is det.
%    Generates a Graphviz node for the given activity under the given project.
activity(Project, A) -->
    { critical_path_element(expected, Project, A) },
    "  \"", atom(A), "\" [shape=rect, style=bold, label=",
    critical_path_element_label(Project, A),
    "];\n".
activity(Project, A) -->
    { \+ critical_path_element(expected, Project, A) },
    "  \"", atom(A), "\" [shape=rect, label=",
    element_label(Project, A),
    "];\n".

% activity_property(-Project, -Property, -Activity) is det.
%   Generates a formatted number for the given property of the given
%   activity under the given project.
activity_property(Project, Property, Activity) -->
    {
	call(Property, expected, Project, Activity, Value),
	format(atom(ValueFormatted), '~2f', [Value])
    },
    atom(ValueFormatted).

% element_label(-Project, -Activity) is det.
%    Generates a generic label for the given element, with the full
%    complement of early/late start/finish.
element_label(Project, A) -->
    "<<TABLE BORDER=\"0\"><TR>",
    "<TD>", activity_property(Project, early_start, A), "</TD>",
    "<TD></TD>",
    "<TD>", activity_property(Project, early_finish, A), "</TD></TR>",
    "<TR><TD></TD><TD>\\N</TD><TD></TD></TR>",
    "<TR>",
    "<TD>", activity_property(Project, late_start, A), "</TD>",
    "<TD></TD>",
    "<TD>", activity_property(Project, late_finish, A), "</TD>",
    "</TR></TABLE>>".

% critical_path_element_label(-Project, -Name) is det.
%   Generate a node label for a critical path element, containing just
%   start/finish. Items on the critical path have the same early and
%   late start/finish, so there's no need to repeat them.
critical_path_element_label(Project, A) -->
    "<<TABLE BORDER=\"0\"><TR>",
    "<TD>", activity_property(Project, early_start, A), "</TD>",
    "<TD>\\N</TD>",
    "<TD>", activity_property(Project, early_finish, A), "</TD>",
    "</TR></TABLE>>".

% dependencies(-Project, -Dependencies) is det.
%    Generate a styled edge for each pair of nodes expressing a dependency.
dependencies(_, []) --> [].
dependencies(Project, [A depends_on B|Rest]) -->
    { critical_path_element(expected, Project, A), critical_path_element(expected, Project, B) },
    "  \"", atom(B), "\" -> \"", atom(A), "\" [style=bold];\n",
    dependencies(Project, Rest).
dependencies(Project, [A depends_on B|Rest]) -->
    { (\+ critical_path_element(expected, Project, A) ; \+ critical_path_element(expected, Project, B)), ! },
    "  \"", atom(B), "\" -> \"", atom(A), "\";\n",
    dependencies(Project, Rest).

% write_graph(-Project, -Filename) is semidet.
%    Write the Project onto Filename in Graphviz format.
write_graph(Project, Filename) :-
    once(phrase(graph(Project), Codes)),
    open(Filename, write, Stream, [close_on_abort(true)]),
    format(Stream, '~s', [Codes]),
    close(Stream).

% output_graph(-Project) is det.
%    Write the Project to the current output stream in GraphViz format.
output_graph(Project) :-
    once(phrase(graph(Project), Codes)),
    format('~s', [Codes]).
