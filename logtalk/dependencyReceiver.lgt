:- protocol(dependencyReceiver).

:- info([
       comment is 'Receives dependency information.',
       author is 'Daniel Lyons',
       date is 2013/10/10
       ]).

:- public(add_dependency/1).
:- mode(add_dependency(+dependency), one).

:- end_protocol.
