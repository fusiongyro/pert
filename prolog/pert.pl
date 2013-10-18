% Calculate PERT for a given project.
:- module(pert, [
	      expected_time/3,
	      early_start/4,
	      early_finish/4,
	      late_start/4,
	      late_finish/4,
	      last_finish/3,
	      slack/4,
	      critical_path_element/3]).

% PERT = Program Evaluation Review Technique

:- use_module(library(lambda)).
:- use_module(library(dcg/basics)).

:- use_module(utility).
:- use_module(project).
:- use_module(gaussian).

% Expected time calculation
expected_time(Project, Activity, ET) :-
    optimistic_time(Project, Activity, Opt),
    most_likely_time(Project, Activity, ML),
    pessimistic_time(Project, Activity, Pess),
    ET is (Opt + ML * 4 + Pess) / 6.

% P E R T   F U N C T I O N S

% early_start(?Activity, ?Time) is det.
%   Returns the earliest time to start this activity.
early_start(_, Project, Activity, 0) :-
    activity(Project, Activity),
    \+ has_predecessors(Project, Activity).
early_start(Type, Project, Activity, ES) :-
    predecessors(Project, Activity, Predecessors),
    maplist(early_finish(Type, Project), Predecessors, FinishingTimes),
    max(FinishingTimes, ES).

% early_finish(?Activity, ?Time) is det.
%   Returns the earliest time to finish this activity.
early_finish(Type, Project, Activity, EF) :-
    early_start(Type, Project, Activity, ES),
    time(Type, Project, Activity, Duration),
    EF is ES + Duration.

% late_start(?Activity, ?Time) is det.
%   Returns the latest time to start this activity.
late_start(Type, Project, Activity, LS) :-
    late_finish(Type, Project, Activity, LF),
    time(Type, Project, Activity, ET),
    LS is LF - ET.

% last_finish(+Time) is det.
%   Returns the latest time any activity finishes.
last_finish(Type, Project, Time) :-
    early_finish(Type, Project, _, Time),
    \+ (early_finish(Type, Project, _, Time2), Time2 > Time), !.

% late_finish(?Activity, ?Time) is det.
%   Returns the latest time this activity can finish.
late_finish(Type, Project, Activity, Last) :-
    activity(Project, Activity),
    \+ has_successors(Project, Activity),
    last_finish(Type, Project, Last).
late_finish(Type, Project, Activity, LF) :-
    activity(Project, Activity),
    successors(Project, Activity, Successors),
    maplist(late_start(Type, Project), Successors, Starts),
    min(Starts, LF).

% slack(-Activity, ?Slack) is det.
% slack(?Activity, ?Slack) is multi.
%   Returns the slack time between the early start and late start.
slack(Type, Project, Activity, Slack) :-
    early_finish(Type, Project, Activity, EF),
    late_finish(Type, Project, Activity, LF),
    Slack is LF - EF.

% critical_path_element(?Activity) is semidet.
%   True if Activity belongs to the critical path.
critical_path_element(Type, Project, Activity) :- slack(Type, Project, Activity, 0.0).


% S T A T I S T I C A L    F U N C T I O N S
deviation_time(Project, Activity, Time) :-
    optimistic_time(Project, Activity, Optimistic),
    pessimistic_time(Project, Activity, Pessimistic),
    Time is (Optimistic - Pessimistic) / 6.

simulated_time(Project, Activity, SimulatedTime) :-
    expected_time(Project, Activity, ExpectedTime),
    deviation_time(Project, Activity, DeviationTime),
    gaussian(ExpectedTime, DeviationTime, SimulatedTime).

%! time(-Type:atom, -Project:project, ?Activity:atom, ?Time:number) is det.
%    Unifies an activity name Activity with Time for the selected time
%    Type (optimistic, pessimistic, most_likely, expected, simulated), under Project.
time(pessimistic, Project, Activity, Time) 	:- pessimistic_time(Project, Activity, Time).
time(most_likely, Project, Activity, Time) 	:- most_likely_time(Project, Activity, Time).
time(optimistic, Project, Activity, Time) 	:- optimistic_time(Project, Activity, Time).
time(expected, Project, Activity, Time)	:- expected_time(Project, Activity, Time).
time(simulated, Project, Activity, Time)	:- simulated_time(Project, Activity, Time).
