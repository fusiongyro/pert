 :- info([
       comment is 'Receives time information.',
       author is 'Daniel Lyons',
       date is 2013/10/10
       ]).
:- protocol(timeReceiver).

:- public(add_time/2).
:- mode(add_time(+atom, +number), one).

:- end_protocol.
