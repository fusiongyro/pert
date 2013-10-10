:- object(pert,
	  implements([timeReceiver, dependencyReceiver, dependencyProvider, labeller]),
	  instantiates(class),
	  specializes(object)).


% P U B L I C    A P I
:- public(early/2).
:- mode(early(?atom, ?timeRange), zero_or_more).
:- mode(early(+atom, ?timeRange), zero_or_one).
early(Activity, Start-Finish) :-
    activity(Activity),
    early_start(Activity, Start),
    early_finish(Activity, Finish).

:- public(late/2).
:- mode(late(?atom, ?timeRange), zero_or_more).
:- mode(late(+atom, ?timeRange), zero_or_one).
late(Activity, Start-Finish) :-
    activity(Activity),
    late_start(Activity, Start),
    late_finish(Activity, Finish).

:- public(slack/2).
:- mode(slack(+atom, ?number), zero_or_one).
slack(Activity, Slack) :-
    early_finish(Activity, EF),
    late_finish(Activity, LF),
    Slack is LF - EF.

:- public(critical_path_element/1).
:- mode(critical_path_element(+atom), zero_or_one).
critical_path_element(Activity) :- slack(Activity, 0.0).

% P R O T O C O L S

% timeReceiver
add_time(Activity, Time) :- ::assertz(time(Activity, Time)).

% dependencyReceiver
add_dependency(X depends_on Y) :- ::assertz(X depends_on Y).

% dependencyProvider
send_dependencies(Receiver) :-
    findall(X depends_on Y, ::(X depends_on Y), Dependencies),
    list::map(Receiver::add_dependency, Dependencies).

% labeller
node_label(Activity, Activity). % FIXME: stub
node_attrs(_, []). % FIXME: stub
edge_attrs(_ depends_on _, []). % FIXME: stub

% P R I V A T E   A P I
early_start(Activity, 0) :-
    activity(Activity),
    \+ has_predecessors(Activity),
    ::asserta(early_start(Activity, 0)).
early_start(Activity, ES) :-
    predecessors(Activity, Predecessors),
    list::map(early_finish, Predecessors, FinishingTimes),
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
    list::map(late_start, Successors, Starts),
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
