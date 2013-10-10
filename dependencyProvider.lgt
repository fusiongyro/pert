:- info([
       comment is 'Provides dependency information to a designated dependencyReceiver.',
       author is 'Daniel Lyons',
       date is 2013/10/10
       ]).
	       
:- protocol(dependencyProvider).

:- public(send_dependencies/1).
:- mode(send_dependencies(+dependencyReceiver), one).

:- end_protocol.
