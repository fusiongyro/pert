:- object(simulation,
	  instantiates(class),
	  specializes(object)).

% database
:- public(time/5).
:- dynamic time/5.

:- public(total_time/1).
:- dynamic total_time/1.

:- public(simulate/3).
:- mode(simulate(+timeType, +project, +iterations), one).
:- info(simulate/3, [
    comment is 'Performs Iterations of a simulation with time type Type and Project',
    arguments is ['TimeType'-timetype, 'Project'-project, 'Iterations'-integer],
    argnames is ['TimeType', 'Project', 'Iterations']]).
simulate(_, _, 0).
simulate(Type, Project, N) :-
    N0 is N-1,
    simulate(Type, Project),
    simulate(Type, Project, N0).

:- private(simulate/2).
simulate(Type, Project) :-
    Project::pert(Type, Pert),
    gather_from(Pert).

:- private(gather_from/1).
gather_from(Pert) :-
    % get the last time
    Pert::last_finish(TotalTime),
    ::assertz(total_time(TotalTime)),
    
    % get the activities
    findall(time(Activity, EarlyStart, EarlyFinish, LateStart, LateFinish),
	    (Pert::early(Activity, EarlyStart-EarlyFinish),
	     Pert::late(Activity, LateStart-LateFinish)),
	    Activities),
    meta::map(assertz, Activities).

:- end_object.
