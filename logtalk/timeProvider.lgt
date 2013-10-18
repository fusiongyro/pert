:- protocol(timeProvider).

:- info([
       comment is 'Provides time information to a designated timeReceiver.',
       author is 'Daniel Lyons',
       date is 2013/10/10
       ]).

:- public(send_times/1).
:- mode(send_times(+timeReceiver), one).

:- end_protocol.
