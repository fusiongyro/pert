:- object(pert,
	  implements([timeReceiver, labeller]),
	  imports(dependencyTracking),
	  instantiates(class),
	  specializes(object)).

:- info([
       comment is 'Implements Program Evaluation and Review Technique (PERT).',
       author is 'Daniel Lyons',
       date is 2013/10/10]).

% P U B L I C    A P I
:- public(early/2).
:- mode(early(?atom, ?timeRange), zero_or_more).
:- mode(early(+atom, ?timeRange), zero_or_one).
:- info(early/2, [
    comment is 'Unify Activity with the Start-Finish time range for the early case.',
    arguments is ['Activity'-'activity label', 'Time'-'time range'],
    argnames is ['Activity', 'Start-Finish']]).
early(Activity, Start-Finish) :-
    activity(Activity),
    early_start(Activity, Start),
    early_finish(Activity, Finish).

:- public(late/2).
:- mode(late(?atom, ?timeRange), zero_or_more).
:- mode(late(+atom, ?timeRange), zero_or_one).
:- info(late/2, [
    comment is 'Unify Activity with the Start-Finish time range for the late case.',
    arguments is ['Activity'-'activity label', 'Time'-'time range'],
    argnames is ['Activity', 'Start-Finish']]).
late(Activity, Start-Finish) :-
    activity(Activity),
    late_start(Activity, Start),
    late_finish(Activity, Finish).

:- public(slack/2).
:- mode(slack(+atom, ?number), zero_or_one).
:- info(slack/2, [
    comment is 'Unify Activity with the Slack time permitted for this activity. May be zero.',
    arguments is ['Activity'-'activity label', 'Slack'-'slack time'],
    argnames is ['Activity', 'Slack']]).
slack(Activity, Slack) :-
    early_finish(Activity, EF),
    late_finish(Activity, LF),
    Slack is LF - EF.

:- public(on_critical_path/1).
:- mode(on_critical_path(+atom), zero_or_one).
:- info(on_critical_path/1, [
    comment is 'Succeeds if Activity is on the critical path, meaning the slack time is zero.',
    arguments is ['Activity'-'activity label'],
    argnames is ['Activity']]).
on_critical_path(Activity) :-
    activity(Activity),
    slack(Activity, Slack),
    Slack =:= 0.

:- public(write_graph/1).
:- mode(write_graph(+filename), one).
:- info(write_graph/1, [
    comment is 'Writes the PERT output to a GraphViz file with the specified name.',
    arguments is ['Filename'-filename],
    argnames is ['Filename']]).
write_graph(Filename) :-
    self(Self),
    graphviz::new(Graph),
    Self::send_dependencies(Graph),
    Graph::write_dot(Filename, Self).

:- public(output_graph/0).
:- mode(output_graph, one).
:- info(output_graph/0, [
    comment is 'Writes PERT output in GraphViz format to standard output.']).
output_graph :-
    self(Self),
    graphviz::new(Graph),
    Self::send_dependencies(Graph),
    Graph::output_dot(Self).

% P R O T O C O L S

% timeReceiver
add_time(Activity, Time) :-
    ::assertz(time(Activity, Time)).

% labeller
node_label(Activity, Label) :-
    activity(Activity),
    (on_critical_path(Activity)
    	-> 	(phrase(critical_node_label(Activity), Label0), atom_codes(Label, Label0))
    	; 	(phrase(noncritical_node_label(Activity), Label0), atom_codes(Label, Label0))).

node_attrs(Activity, [style=bold]) 	:- on_critical_path(Activity).
node_attrs(Activity, []) 			:- activity(Activity), \+ on_critical_path(Activity).

edge_attrs(A depends_on B, [style=bold]) :-
    on_critical_path(A), on_critical_path(B).
edge_attrs(A depends_on B, []) :-
      \+ on_critical_path(A)
    ; \+ on_critical_path(B).

% element_label(-Project, -Activity) is det.
%    Generates a generic label for the given element, with the full
%    complement of early/late start/finish.
noncritical_node_label(A) -->
    {	early(A, EarlyStart-EarlyFinish), late(A, LateStart-LateFinish)	},
    "<<TABLE BORDER=\"0\"><TR>",
    "<TD>", decimal(EarlyStart), "</TD>",
    "<TD></TD>",
    "<TD>", decimal(EarlyFinish), "</TD></TR>",
    "<TR><TD></TD><TD>\\N</TD><TD></TD></TR>",
    "<TR>",
    "<TD>", decimal(LateStart), "</TD>",
    "<TD></TD>",
    "<TD>", decimal(LateFinish), "</TD>",
    "</TR></TABLE>>".

% critical_path_element_label(-Project, -Name) is det.
%   Generate a node label for a critical path element, containing just
%   start/finish. Items on the critical path have the same early and
%   late start/finish, so there's no need to repeat them.
critical_node_label(A) -->
    {	early(A, Start-Finish)	},
    "<<TABLE BORDER=\"0\"><TR>",
    "<TD>", decimal(Start), "</TD>",
    "<TD>\\N</TD>",
    "<TD>", decimal(Finish), "</TD>",
    "</TR></TABLE>>".

decimal(D) -->
    {	format(atom(Formatted), '~2f', [D])	},
    dcg_basics:atom(Formatted).

% P R I V A T E   A P I
:- public(early_start/2).
early_start(Activity, Cached) :-
    cache(early_start(Activity, Cached)),
    !.
early_start(Activity, 0) :-
    activity(Activity),
    \+ has_predecessors(Activity),
    ::asserta(cache(early_start(Activity, 0))).
early_start(Activity, ES) :-
    predecessors(Activity, Predecessors),
    self(Self),
    meta::map([X,Y]>>(Self::early_finish(X,Y)), Predecessors, FinishingTimes),
    list::max(FinishingTimes, ES),
    ::asserta(cache(early_start(Activity, ES))).

:- public(early_finish/2).
early_finish(Activity, Cached) :-
    cache(early_finish(Activity, Cached)),
    !.
early_finish(Activity, EF) :-
    early_start(Activity, ES),
    ::time(Activity, Duration),
    EF is ES + Duration,
    ::asserta(cache(early_finish(Activity, EF))).

:- private(last_finish/1).
last_finish(Cached) :-
    cache(last_finish(Cached)),
    !.
last_finish(Time) :-
    early_finish(_, Time),
    \+ (early_finish(_, Time2), Time2 > Time),
    !,
    ::asserta(cache(last_finish(Time))).

:- private(late_start/2).
late_start(Activity, Cached) :-
    cache(late_start(Activity, Cached)),
    !.
late_start(Activity, LS) :-
    late_finish(Activity, LF),
    ::time(Activity, ET),
    LS is LF - ET,
    ::asserta(cache(late_start(Activity, LS))).

:- private(late_finish/2).
late_finish(Activity, Cached) :-
    cache(late_finish(Activity, Cached)),
    !.
late_finish(Activity, Last) :-
    activity(Activity),
    \+ has_successors(Activity),
    ::last_finish(Last),
    ::asserta(cache(late_finish(Activity, Last))).
late_finish(Activity, LF) :-
    activity(Activity),
    successors(Activity, Successors),
    self(Self),
    meta::map([X,Y]>>(Self::late_start(X,Y)), Successors, Starts),
    list::min(Starts, LF),
    ::asserta(cache(late_finish(Activity, LF))).

:- private(predecessors/2).
:- mode(predecessors(+atom, -list), zero_or_one).
predecessors(Activity, Predecessors) :-
    setof(A, ::(Activity depends_on A), Predecessors).

:- private(successors/2).
:- mode(successors(+atom, -list), zero_or_one).
successors(Activity, Successors) :-
    setof(A, ::(A depends_on Activity), Successors).

:- private(has_predecessors/1).
:- mode(has_predecessors(+atom), zero_or_one).
has_predecessors(Activity) 	:- predecessors(Activity, [_|_]).

:- private(has_successors/1).
:- mode(has_successors(+atom), zero_or_one).
has_successors(Activity) 	:- successors(Activity, [_|_]).

% P R I V A T E   H E L P E R S
:- public(activity/1).
%:- mode(activity(?atom), zero_or_more).
%:- mode(activity(+atom), zero_or_one).
activity(Activity) :-
    ::time(Activity,_).

% stored facts
% DO NOT USE THESE! THEY ARE PUBLIC FOR TECHNICAL REASONS ONLY!
:- public(time/2).
:- dynamic time/2.

% caches
% DO NOT USE THESE! THEY ARE PUBLIC FOR TECHNICAL REASONS!
:- public(cache/1).
:- dynamic cache/1.

:- end_object.
