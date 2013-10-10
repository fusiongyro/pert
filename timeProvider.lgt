:- info([
       comment is 'Provides time information to a designated timeReceiver.',
       author is 'Daniel Lyons',
       date is 2013/10/10
       ]).
:- protocol(timeProvider).

:- public(sendTimes/1).
:- mode(sendTimes(+timeReceiver), one).

:- end_protocol.
