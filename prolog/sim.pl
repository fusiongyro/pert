:- module(sim, [
	      simulate/3,
	      simulate_expected_time/2,
	      simulate_random_time/2]).

:- use_module(pert).

% simulation_flags(TimeType)
%   TimeType = [simulated, expected, most_likely, optimistic, pessimistic]
simulation_type(simulation_flags(Type), Type).

% simulation(EarlyTimes, LateTimes, LastFinish)
%   EarlyTimes, LateTimes = time(Activity, Start-Finish)
%   LastFinish = 0..inf
blank_simulation(simulation([], [], 0)).

early_times(simulation(EarlyTimes, _, _), EarlyTimes).
late_times(simulation(_, LateTimes, _), LateTimes).
last_finish(simulation(_, _, LF), LF).

early_time(Sim, Activity, Time) :- early_times(Sim, Times), member(time(Activity, Time), Times).
late_time(Sim, Activity, Time) :- late_times(Sim, Times), member(time(Activity, Time), Times).

early_start(Sim, Activity, Time) :- early_time(Sim, Activity, Time-_).
early_finish(Sim, Activity, Time) :- early_time(Sim, Activity, _-Time).
late_start(Sim, Activity, Time) :- late_time(Sim, Activity, Time-_).
late_finish(Sim, Activity, Time) :- late_finish(Sim, Activity, _-Time).

add_early_time(simulation(EarlyTimes, LT, LF), Activity, Time,
	       simulation([time(Activity, Time)|EarlyTimes], LT, LF)).
add_late_time(simulation(ET, LateTimes, LF), Activity, Time,
	      simulation(ET, [time(Activity, Time)|LateTimes], LF)).
set_last_finish(simulation(ET, LT, _), LastFinish,
		simulation(ET, LT, LastFinish)).

%! simulate(SimulationFlags:simulation_flags, Project:project, Simulation:simulation) is det.
%    Performs the PERT simulation for Project using the specified simulation flags.
simulate(Flags, Project, Simulation) :-
    % Get all the activities
    project:activity_names(Project, Activities),

    % Make a blank simulation
    blank_simulation(Simulation0),

    % Perform the forward simulation (early times)
    forward_simulate(Flags, Project, Activities, Simulation0, Simulation1),

    % Calculate the last finish
    calculate_last_finish(Simulation1, Simulation2),
    
    % Flip the list and do the backwards simulation (late times).
    reverse(Activities, ReverseActivities),
    backward_simulate(Flags, Project, ReverseActivities, Simulation2, Simulation).

%! simulate_expected_time(Project, Simulation) is det.
%    Performs the PERT simulation using expected time.
%
%    Equivalent to simulate(simulation_flags(expected), Project, Simulation).
simulate_expected_time(Project, Simulation) :-
    simulate(simulation_flags(expected), Project, Simulation).

%! simulate_random_time(Project, Simulation) is det.
%    Performs the PERT simulation using simulated time.
%
%    Equivalent to simulate(simulation_flags(simulated), Project, Simulation).
simulate_random_time(Project, Simulation) :-
    simulate(simulation_flags(simulated), Project, Simulation).

forward_simulate(_, _, [], Sim, Sim).
forward_simulate(Flags, Project, [Activity|Rest], Sim0, Sim2) :-
    forward_simulate(Flags, Project, Activity, Sim0, Sim1),
    forward_simulate(Flags, Project, Rest, Sim1, Sim2).
forward_simulate(_, _, Activity, Sim, Sim) :-
    early_time(Sim, Activity, _).
forward_simulate(Flags, Project, Activity, Sim0, Sim1) :-
    \+ early_time(Sim0, Activity, _),
    simulation_type(Flags, Type),
    early_start(Type, Project, Activity, ES),
    early_finish(Type, Project, Activity, EF),
    add_early_time(Sim0, Activity, ES-EF, Sim1).

calculate_last_finish(Sim0, Sim1) :-
    late_finish(Sim0, _, LastFinish),
    \+ (late_finish(Sim0, _, LF2), LF2 > LastFinish),
    set_last_finish(Sim0, LastFinish, Sim1).

backward_simulate(_, _, [], Sim, Sim).
backward_simulate(Flags, Project, [Activity|Rest], Sim0, Sim2) :-
    backward_simulate(Flags, Project, Activity, Sim0, Sim1),
    backward_simulate(Flags, Project, Rest, Sim1, Sim2).
backward_simulate(_, _, Activity, Sim, Sim) :-
    late_time(Sim, Activity, _).
backward_simulate(Flags, Project, Activity, Sim0, Sim1) :-
    \+ late_time(Sim0, Activity, _),
    simulation_type(Flags, Type),
    late_start(Type, Project, Activity, ES),
    late_finish(Type, Project, Activity, EF),
    add_late_time(Sim0, Activity, ES-EF, Sim1).
