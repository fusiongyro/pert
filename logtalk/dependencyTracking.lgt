:- category(dependencyTracking,
	   implements([dependencyReceiver, dependencyProvider])).

:- info([
       comment is 'Enables dependency tracking as a mixin to an object.',
       author is 'Daniel Lyons',
       date is 2013/10/10]).

% Our data
:- public((depends_on)/2).
:- dynamic (depends_on)/2.

% P R O T O C O L S

% dependencyReceiver
add_dependency(X depends_on Y) :-
    ::assertz(X depends_on Y).

% dependencyProvider
send_dependencies(Receiver) :-
    findall(X depends_on Y, ::(X depends_on Y), Dependencies),
    meta::map(Receiver::add_dependency, Dependencies).

:- end_category.
