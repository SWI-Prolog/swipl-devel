p( N, T) :-
	(	N > 1
	->	Nx is N-1,
		p( Nx, Tx),
		T = a(Tx,Tx)
	;	N == 1
	->	T = a
	).

