:- module(test_bags,
	  [ test_bags/0
	  ]).
:- use_module(library(plunit)).

test_bags :-
	run_tests([ bags
		  ]).

/** <module> Test findall, bagof, etc.

@tbd	Only tests new findnsols/4,5.  Other tests are in test.pl
*/

:- begin_tests(bags).

test(nsols, Lists == [ [1,2,3,4,5],
		       [6,7,8,9,10],
		       [11,12]
		     ]) :-
	findall(L, findnsols(5, I, between(1,12,I), L), Lists).
test(nsols_nested, Lists == [ [ [1,2,3,4,5],
				[6,7,8,9,10]
			      ],
			      [ [11,12]
			      ]
			    ]) :-
	findall(L1,
		findnsols(2, L,
			  findnsols(5, I, between(1,12,I), L),
			  L1),
		Lists).
test(nsols_commit, Lists == [ [ [1,2,3,4,5],
				[6,7,8,9,10]
			      ]
			    ]) :-
	findall(L1,
		( findnsols(2, L,
			    findnsols(5, I, between(1,12,I), L),
			    L1),
		  !
		),
		Lists).

:- end_tests(bags).
