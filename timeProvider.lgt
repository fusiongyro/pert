:- protocol(timeProvider).

:- public(sendTimes/1).
:- mode(sendTimes(+timeReceiver), one).

:- end_protocol.
