:- module(test_process,
	  [ test_process/0
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(process).

test_process :-
	run_tests([ process_create,
		    process_wait
		  ]).

read_process(In, Text) :-
	read_stream_to_codes(In, Codes),
	close(In),
	atom_codes(Text, Codes).

:- begin_tests(process_create, [sto(rational_trees)]).

test(echo, true) :-
	process_create(path(true), [], []).
test(null_input, Codes == []) :-
	process_create(path(cat), [], [stdin(null), stdout(pipe(Out))]),
	read_stream_to_codes(Out, Codes),
	close(Out).
test(null_output, true) :-
	process_create(path(echo), ['THIS IS AN ERROR'], [stdout(null)]).
test(null_error, true) :-
	process_create('/bin/sh',
		       ['-c', 'echo "THIS IS AN ERROR" 1>&2'],
		       [stderr(null)]).
test(read_error, X == 'error\n') :-
	process_create('/bin/sh',
		       ['-c', 'echo "error" 1>&2'],
		       [stderr(pipe(Out))]),
	read_process(Out, X).
test(echo, X == 'hello\n') :-
	process_create(path(echo), [hello],
		       [ stdout(pipe(Out))
		       ]),
	read_process(Out, X).
test(lwr, X == 'HELLO') :-
	process_create(path(tr), ['a-z', 'A-Z'],
		       [ stdin(pipe(In)),
			 stdout(pipe(Out))
		       ]),
	format(In, hello, []),
	close(In),
	read_process(Out, X).
test(cwd, true) :-
	process_create(path(pwd), [],
		       [ stdout(pipe(Out)),
			 cwd('/tmp')
		       ]),
	read_process(Out, CWD0),
	normalize_space(atom(CWD), CWD0),
	same_file(CWD, '/tmp').

:- end_tests(process_create).


:- begin_tests(process_wait, [sto(rational_trees)]).

test(wait_ok, X == exit(0)) :-
	process_create('/bin/sh', ['-c', 'exit 0'], [process(PID)]),
	process_wait(PID, X).
test(wait_ok, X == exit(42)) :-
	process_create('/bin/sh', ['-c', 'exit 42'], [process(PID)]),
	process_wait(PID, X).
test(kill_ok, X == killed(9)) :-
	process_create(path(sleep), [2], [process(PID)]),
	process_kill(PID, 9),
	process_wait(PID, X).

:- end_tests(process_wait).
