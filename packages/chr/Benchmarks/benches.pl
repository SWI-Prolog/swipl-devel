:- prolog_load_context(directory, Dir),
   working_directory(_, Dir).

benches :-
	bench(B),
	atom_concat(B, '.chr', File),
	style_check(-singleton),
	abolish(main,0),
	abolish(main,1),
	[File],
	(main;main;main;main),
	fail.
benches.

bench(bool).
bench(fib).
bench(fibonacci).
bench(leq).
bench(primes).
bench(ta).
bench(wfs).
bench(zebra).

cputime(Time) :-
	statistics(runtime, [_,Time]).
