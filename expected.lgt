:- object(expected,
	  implements(timeCalculator)).

:- public(time/2).
time(activity(_, Time), Time).
time(activity(_, Pessimistic, MostLikely, Optimistic), Time) :-
    Time is (Pessimistic + 4 * MostLikely + Optimistic) / 6.

:- end_object.
