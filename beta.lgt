:- object(beta(_, _),
	 implements(timeCalculator)).

time(activity(_, Pessimistic, _, Optimistic), Time) :-
    % This is a stub
    beta(Pessimistic, Optimistic)::value(Time).

:- public(value/1).
:- mode(value(-value), zero_or_one).
value(Value) :-
    this(beta(Alpha, Beta)),
    gamma(Alpha, 1.0)::value(Y),
    (Y =:= 0
     	-> 	Value = 0.0
	;	(gamma(Beta, 1.0)::value(Z),
		 Value is Y / (Y + Z))).

:- end_object.
