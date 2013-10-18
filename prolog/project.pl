/** <module> Project ADT

This module defines a Project abstract data type and a variety of
relations on it. The objective here is to insulate the ADT from
changes and permit Prolog-style access to its contets without
violating encapsulation (much).

@author Daniel Lyons
@license MIT
*/
:- module(project,
	 [op(500, xfx, depends_on),
	  activities/2,
	  activity_names/2,
	  activity/2,
	  pessimistic_time/3,
	  most_likely_time/3,
	  optimistic_time/3,
	  all_dependencies/2,
	  successors/3,
	  predecessors/3,
	  has_successors/2,
	  has_predecessors/2]).

% C O N V E N I E N C E   A C C E S S O R S

%! activities(+Project:project, -Activities:[activity]) is det.
%    Unifies the activities table for Project with Activities.
activities(project(Activities, _), Activities).

%! activity_names(+Project:project, -Names:[atom]) is det.
%   Returns the Names of activities in this Project.
activity_names(Project, Names) :-
    activities(Project, Activities),
    findall(Name, member(activity(Name, _, _, _), Activities), Names).

%! activity(+Project:project, ?Activity:activity/4) is det.
%   Unifies an activity name in Project with Activity.
activity(Project, A) :-
    activities(Project, Activities),
    member(activity(A, _, _, _), Activities).

%! pessimistic_time(+Project:project, ?Activity:atom, ?Time:number) is semidet.
%    Unifies the pessimistic time of Activity with Time, under Project.
pessimistic_time(project(Activities,_), Activity, Time) :-
    member(activity(Activity, Time, _, _), Activities).

%! most_likely_time(+Project:project, ?Activity:atom, ?Time:number) is semidet.
%    Unifies the most likely time of Activity with Time, under Project.
most_likely_time(project(Activities,_), Activity, Time) :-
    member(activity(Activity, _, Time, _), Activities).

%! optimistic_time(+Project:project, ?Activity:atom, ?Time:number) is det.
%    Unifies the optimistic time of Activity with Time, under Project.
optimistic_time(project(Activities,_), Activity, Time) :-
    member(activity(Activity, _, _, Time), Activities).

%! all_dependencies(+Project:project, ?Dependencies:[dependency]) is det.
%   Returns the dependencies list (using operator depends_on) for Project.
all_dependencies(project(_, Deps), Deps).

%! successors(+Project:project, ?Activity:atom, ?Successors:[atom]) is det.
%    Under Project, unifies the names of all the activities that are
%    successors of Activity with Successors.
successors(Project, Activity, Successors) :-
    all_dependencies(Project, Deps),
    setof(A, member(A depends_on Activity, Deps), Successors).

%! predecessors(+Project:project, ?Activity:atom, ?Predecessors:[atom]) is det.
%    Under Project, unifies the names of all the activities that are
%    predecessors of Activity with Predecessors.
predecessors(Project, Activity, Predecessors) :-
    all_dependencies(Project, Deps),
    setof(A, member(Activity depends_on A, Deps), Predecessors).

%! has_successors(+Project:project, +Activity:atom) is semidet.
%    True if Activity has successors in Project.
has_successors(Project, Activity) 	:- successors(Project, Activity, [_|_]).

%! has_predecessors(+Project, +Activity) is semidet.
%    True if Activity has predecessors in Project.
has_predecessors(Project, Activity) 	:- predecessors(Project, Activity, [_|_]).
