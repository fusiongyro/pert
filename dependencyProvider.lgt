:- protocol(dependencyProvider).

:- public(send_dependencies/1).
:- mode(send_dependencies(+dependencyReceiver), one).

:- end_protocol.
