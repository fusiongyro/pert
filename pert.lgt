:- info([
       comment is 'Implements Program Evaluation and Review Technique (PERT).',
       author is 'Daniel Lyons',
       date is 2013/1010]).

:- object(pert,
	  implements([timeReceiver, dependencyReceiver, dependencyProvider, labeller]),
	  instantiates(class),
	  specializes(object)).

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
on_critical_path(Activity) :- slack(Activity, 0.0).

% P R O T O C O L S

% timeReceiver
add_time(Activity, Time) :-
    ::assertz(time(Activity, Time)).

% dependencyReceiver
add_dependency(X depends_on Y) :-
    ::assertz(X depends_on Y).

% dependencyProvider
send_dependencies(Receiver) :-
    findall(X depends_on Y, ::(X depends_on Y), Dependencies),
    meta::map(Receiver::add_dependency, Dependencies).

% labeller
node_label(Activity, Label) :-
    on_critical_path(Activity)
    	-> 	phrase(critical_node_label(Activity), Label)
    	; 	phrase(noncritical_node_label(Activity), Label).

node_attrs(Activity, [bold=true]) 	:- on_critical_path(Activity).
node_attrs(Activity, []) 			:- \+ on_critical_path(Activity).

edge_attrs(A depends_on B, [bold=true]) :-
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
early_start(Activity, 0) :-
    activity(Activity),
    \+ has_predecessors(Activity),
    ::asserta(early_start(Activity, 0)).
early_start(Activity, ES) :-
    predecessors(Activity, Predecessors),
    meta::map(early_finish, Predecessors, FinishingTimes),
    list::max(FinishingTimes, ES),
    ::asserta(early_start(Activity, ES)).

early_finish(Activity, EF) :-
    early_start(Activity, ES),
    ::time(Activity, Duration),
    EF is ES + Duration,
    ::asserta(early_finish(Activity, EF)).

late_start(Activity, LS) :-
    late_finish(Activity, LF),
    ::time(Activity, ET),
    LS is LF - ET,
    ::asserta(late_start(Activity, LS)).

last_finish(Time) :-
    early_finish(_, Time),
    \+ (early_finish(_, Time2), Time2 > Time),
    !,
    ::asserta(last_finish(Time)).

late_finish(Activity, Last) :-
    activity(Activity),
    \+ has_successors(Activity),
    ::last_finish(Last),
    ::asserta(late_finish(Activity, Last)).
late_finish(Activity, LF) :-
    activity(Activity),
    successors(Activity, Successors),
    meta::map(late_start, Successors, Starts),
    list::min(Starts, LF),
    ::asserta(late_finish(Activity, LF)).

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
    setof(Activity, ::time(Activity,_), Activities),
    !,
    list::member(Activity, Activities).

% stored facts
% DO NOT USE THESE! THEY ARE PUBLIC FOR TECHNICAL REASONS ONLY!
:- public(time/2).
:- public((depends_on)/2).
:- dynamic time/2, (depends_on)/2.

% caches
% DO NOT USE THESE! THEY ARE PUBLIC FOR TECHNICAL REASONS!
:- public(early_start/2). 	:- public(early_finish/2).
:- public(late_start/2). 	:- public(late_finish/2).
:- public(last_finish/1).
:- dynamic early_start/2, early_finish/2, late_start/2, late_finish/2, last_finish/1.

:- end_object.
