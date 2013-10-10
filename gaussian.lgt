:- object(gaussian).

/**
Uses the Marsaglia polar method to convert a uniform random number in
the range 0.0-1.0 to a random number from the normal distribution
defined by its mean and standard deviation.
*/

% NOT REALLY PUBLIC!!
:- public(spare/3).
:- dynamic spare/3.

:- public(gaussian/3).
:- mode(gaussian(+number, +standard_deviation, -value), one).
:- info(gaussian/3, [
    comment is 'Unifies Value with a new randomly generated Value along the normal distribution defined by the specified Mean and StandardDeviation.',
    arguments is ['Mean'-number, 'StandardDeviation'-number, 'Value'-number]]).
gaussian(Mean, StandardDeviation, Value) :-
    % Interestingly, this algorithm generates two values per
    % iteration. The retract here finds a spare value produced on the
    % previous iteration and uses it to generate the second value.
    ::retract(spare(Mean, StandardDeviation, Spare)), !,
    Value is Spare * StandardDeviation + Mean.
gaussian(Mean, StandardDeviation, Value) :-
    % Making it to here implies that we had no spare value, so we
    % calculate both values now.
    random_point_in_unit_circle(U, V),
    area_squared(U, V, S),

    Mul is sqrt(-2.0 * log(S) / S),

    % Create the spare value for our use later.
    Spare is V * Mul,
    ::asserta(spare(Mean, StandardDeviation, Spare)),
    
    Value is Mean + StandardDeviation * U * Mul.

% Generate a random point within the unit circle
:- private(random_point_in_unit_circrle/2).
random_point_in_unit_circle(U, V) :-
    random::random(X0), random::random(Y0),
    X is X0 * 2 - 1,
    Y is Y0 * 2 - 1,
    area_squared(X, Y, S),
    ((S >= 1 ; S = 0)
    	-> random_point_in_unit_circle(U, V)
    	; (U = X, V = Y)).

% Calculate the square of the area.
:- private(area_squared/2).
area_squared(X, Y, S) :- S is X^2 + Y^2.

:- end_object.
