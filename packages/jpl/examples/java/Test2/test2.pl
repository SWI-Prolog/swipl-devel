% jpl_test_fac(+N, -F) :-
%   F is the factorial of the positive integer N

jpl_test_fac(N, F) :-
	(   N == 1
	->  F = 1
	;   N2 is N-1,
	    jpl_call('Test2', fac, [N2], F2),
	    F is N*F2
	).
