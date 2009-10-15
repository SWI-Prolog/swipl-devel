:- module(test_ch_shift,
	  [ test_ch_shift/0
	  ]).

%%	test_ch_shift
%
%	Tests expansion of the local stack due to a clause with many
%	choicepoints.

test_ch_shift :-
	trim_stacks,
	make_or(10000, OR),
	asserta((t :- OR), Ref),
	once(t),
	erase(Ref).

make_or(0, a) :- !.
make_or(N, (G;a)) :-
	N2 is N - 1,
	make_or(N2, G).

a.
