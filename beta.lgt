:- object(beta).

:- public(beta/3).
:- mode(beta(+number, +number, -number), zero_or_one).
:- info(beta/3, [
    comment is 'Generates a Beta distribution variate.',
    arguments is ['Alpha'-number, 'Beta'-number, 'Value'-number],
    argnames is ['Alpha', 'Beta', 'Value']]).
% Code is again from Python.
beta(Alpha, Beta, Value) :-
    gamma::value(Alpha, 1.0, Y),
    (Y =:= 0
     	-> 	Value = 0.0
	;	(gamma::value(Beta, 1.0, Z),
		 Value is Y / (Y + Z))).

:- public(estimate_parameters/4).
:- mode(estimate_parameters(+number, +number, -number, -number), one).
:- info(estimate_parameters/4, [
    comment is 'Estimate beta parameters from normal parameters. Uses same formula as NTBETAPARAM from Excel.',
    arguments is ['Mean'-number, 'StandardDeviation'-number, 'Alpha'-number, 'Beta'-number],
    argnames is ['Mean', 'StandardDeviation', 'Alpha', 'Beta']]).
estimate_parameters(Mean, StandardDeviation, Alpha, Beta):-
    Alpha is Mean * (((Mean * (1 - Mean)) / StandardDeviation^2) - 1),
    Beta is (1 - Mean) * (((Mean * (1 - Mean)) / StandardDeviation^2) - 1).

:- end_object.

:- object(beta(_, _),
	 implements(timeCalculator)).

time(activity(_, Pessimistic, _, Optimistic), Time) :-
    % This is a stub
    beta(Pessimistic, Optimistic)::value(Time).

:- public(value/1).
:- mode(value(-value), zero_or_one).
value(Value) :-
    this(beta(Alpha, Beta)),
    beta::beta(Alpha, Beta, Value).

:- end_object.
