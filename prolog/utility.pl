:- module(utility, [max/2, min/2]).

% U T I L I T Y   F U N C T I O N S

%! max(+List, ?Max) is det.
%   Returns the largest value in the list
max([Max], Max).
max([X,Y|Xs], Max) :- X > Y
    		      ->	max([X|Xs], Max)
    		      ;		max([Y|Xs], Max).

%! min(+List, ?Min) is det.
%   Returns the smallest value in the list
min([Min], Min).
min([X,Y|Xs], Min) :- X < Y
		      -> 	min([X|Xs], Min)
		      ; 	min([Y|Xs], Min).
