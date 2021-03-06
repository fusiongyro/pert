:- object(project,
	  implements(timeProvider),
	  imports(dependencyTracking),
	  instantiates(class),
	  specializes(object)).

:- info([
       comment is 'Represents a project containing activity/2 or activity/4 tuples and a dependency graph between them.',
       author is 'Daniel Lyons',
       date is 2013/10/10]).

:- public(activity/2).
:- dynamic activity/2.

:- public(activity/4).
:- dynamic activity/4.

:- public(add_activity/1).
:- mode(add_activity(+activity), one).
:- info(add_activity/1, [
	comment is 'Adds an activity to this project',
	arguments is ['Activity'-activity],
	argnames is ['Activity']]).
add_activity(activity(A, T)) :-
    ::assertz(activity(A, T)).
add_activity(activity(A, Pes, ML, Opt)) :-
    ::assertz(activity(A, Pes, ML, Opt)).

% Note that the dependency tracking is provided by the
% dependencyTracking category.

send_times(Dest) :- send_times(expected, Dest).

:- public(send_times/2).
send_times(Type, Dest) :-
    findall(activity(A, Time), ::activity(A, Time), Activities2),
    findall(activity(A, T1, T2, T3), ::activity(A, T1, T2, T3), Activities4),
    list::append(Activities2, Activities4, Activities),
    self(Self),
    meta::map([Time]>>(Self::send_time(Type, Time, Dest)), Activities).

:- public(pert/1).
pert(Pert) :-
    pert::new(Pert),
    send_times(Pert),
    ::send_dependencies(Pert).

:- public(pert/2).
pert(Type, Pert) :-
    pert::new(Pert),
    send_times(Type, Pert),
    ::send_dependencies(Pert).

:- private(send_time/3).
send_time(Type, Activity, Dest) :-
    Type::time(Activity, Time),
    Activity::name(Name),
    Dest::add_time(Name, Time).

:- end_object.

