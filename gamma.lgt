:- object(gamma(_, _)).

:- initialization(precalculate).

:- public(log4/1).
:- public(sg_magicconst/1).
:- dynamic log4/1, sg_magicconst/1.

precalculate :-
    LOG4 is log(4), ::assertz(log4(LOG4)),
    SG_MAGICCONST is 1.0 + log(4.5), ::assertz(sg_magicconst(SG_MAGICCONST)).

:- public(value/1).
:- mode(value(-number), zero_or_one).
:- info(value/1, [
    comment is 'Implements a Gamma distribution.']).
% Code borrowed from Python's standard 'random' module:
%    http://hg.python.org/cpython/file/2.7/Lib/random.py
value(Value) :-
    this(gamma(Alpha, Beta)),
    % preconditions
    Alpha > 1, Beta > 0, !,

    % inside first conditional
    ::log4(LOG4), 
    Ainv is sqrt(2.0 * Alpha - 1.0),
    BBB is Alpha - LOG4,
    CCC is Alpha + Ainv,
    gamma_above_one(Alpha, Beta, Ainv, BBB, CCC, Value).
value(Value) :-
    this(gamma(Alpha, Beta)),
    % preconditions
    Alpha =:= 1.0, Beta > 0, !,
    gamma_at_one(Beta, Value).
value(Value) :-
    this(gamma(Alpha, Beta)),
    Alpha > 0, Beta > 0, !,
    gamma_below_one(Alpha, Beta, Value).

gamma_above_one(Alpha, Beta, Ainv, BBB, CCC, Value) :-
    ::sg_magicconst(SG_MAGICCONST),
    random::random(U1),
    (\+ (1e-7 =< U1, U1 =< 0.9999999)
     	-> 	gamma_above_one(Alpha, Beta, Ainv, BBB, CCC, Value)
	; 	(random::random(U20),
		 U2 is (1.0 - U20),
		 V is log(U1 / (1.0 - U1)) / Ainv,
		 X is Alpha * exp(V),
		 Z is U1 * U1 * U2,
		 R is BBB + CCC * V - X,
		 Threshold is R + SG_MAGICCONST - 4.5 * Z,
		 LogZ is log(Z),
		 ((Threshold >= 0.0 ; R >= LogZ)
		      ->	Value is X * Beta
		  	; 	gamma_above_one(Alpha, Beta, Ainv, BBB, CCC, Value)))).

gamma_at_one(Beta, Value) :-
    random::random(U),
    (U =< 1e-7
     	-> 	gamma_at_one(Beta, Value)
     	; 	Value is -log(U) * Beta).

gamma_below_one(Alpha, Beta, Value) :-
    random::random(U),
    B is (e + Alpha) / e,
    P is B * U,
    (P =< 1.0
	    ->	X is P ^ (1.0 / Alpha)
	    ;		X is -log((B-P)/Alpha)),
    random::random(U1),
    Threshold is X^(Alpha-1.0),
    ExpXMinus is exp(-X),
    (((P > 1, U1 =< Threshold) ; U1 =< ExpXMinus)
     	->	Value is X * Beta
     	;	gamma_below_one(Alpha, Beta, Value)).

:- end_object.
