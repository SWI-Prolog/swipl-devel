:- use_module(plunit).

:- begin_tests(read).

create_file(Tmp) :-
	tmp_file(plunit, Tmp),
	open(Tmp, write, Out),
	write(Out, 'hello(World).\n'),
	close(Out).

test(read, [ setup(create_file(Tmp)),
	     cleanup(delete_file(Tmp))
	   ]) :-
	read_file_to_terms(Tmp, Terms, []),
	Terms =@= [hello(_)].

:- end_tests(read).
