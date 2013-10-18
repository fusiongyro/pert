% The activity cost table
%		Activity			Pessimistic	Most Likely	Optimistic
activity(	fuselage,			40,			20,			15		).
activity(	wings,			60,			38,			25		).
activity(	tail,				30,			22,			18		).
activity(	landing_gear,		45,			28,			20		).
activity(	horizontal_stabilizer,	  5,			  2,			  1		).
activity(	vertical_stabilizer,	  5,			  2,			  1		).
activity(	wheels,			11,			  8,			  5		).
activity(	airfoil,			14,			  8,			  5		).

% The activity dependency graph
wings 			depends_on 	fuselage.
tail 				depends_on 	fuselage.
landing_gear 		depends_on 	fuselage.
airfoil 			depends_on 	wings.
horizontal_stabilizer 	depends_on 	tail.
vertical_stabilizer	depends_on 	tail.
wheels 			depends_on 	landing_gear.
