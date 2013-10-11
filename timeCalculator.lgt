:- protocol(timeCalculator).

:- public(time/2).
:- mode(time(+activity, ?time), zero_or_one).
:- info(time/2, [
	    comment is 'Calculates the time this activity will take according to some formula.',
	    arguments is ['Activity'-activity, 'Time'-time],
	    argnames is ['Activity', 'Time']]).

:- end_protocol.
