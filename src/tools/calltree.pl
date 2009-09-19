#!/home/jan/bin/pl -q -g true -t main -G4g -T4g -s

:- use_module(sexp).
:- use_module(library(debug)).

% :- debug(rtl).

process(File, Into) :-
	read_sexp_file(File, Terms, [comment(true)]),
	setup_call_cleanup(open(Into, write, Out),
			   terms(Terms, -, Out),
			   close(Out)).

terms([], _, _).
terms([comment(Comment)|T], _, Out) :-
	function_comment(Comment, Function), !,
	debug(rtl, 'Function ~w', [Function]),
	format(Out, '% ~q.~n', [function(Function)]),
	terms(T, Function, Out).
terms([[call_insn|Call]|T], Function, Out) :-
	calls(Call, File, Line, Callee), !,
	debug(rtl, '~w calls ~w at ~w:~d', [Function, Callee, File, Line]),
	format(Out, '~q.~n', [calls(Function, Callee, File, Line)]),
	terms(T, Function, Out).
terms([_|T], Function, Out) :-
	terms(T, Function, Out).

function_comment(Comment, Function) :-
	atomic_list_concat(['Function',Function|_], ' ', Comment).

calls([_, _, _, Where|Rest], File, Line, Callee) :-
	atom(Where),
	atomic_list_concat([File,LineTxt], ':', Where),
	atom_number(LineTxt, Line),
	(   sub_term(['symbol_ref:di', [Callee]|_], Rest)
	->  true
	;   sub_term(['reg/f:di', _, '[',_,']'], Rest)
	->  Callee = '<POINTER>'
	;   Callee = '<UNKNOWN>',
	    (	debugging(rtl)
	    ->	pp(Rest),
		abort
	    ;	put(user_error, '!')
	    )
	), !.

main :-
	current_prolog_flag(argv, Argv),
	append(_, [--,In,Out], Argv),
	format(user_error, 'Processing ~w ...', [In]),
	statistics(cputime, T0),
	catch(process(In, Out), E,
	      (	  print_message(error, E),
		  fail
	      )),
	statistics(cputime, T1),
	T is T1 - T0,
	format(user_error, '~3f sec~n', [T]).

