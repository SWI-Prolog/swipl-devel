:- module(utf8_test,
	  [ utf8_test/0
	  ]).
:- use_module(library(readutil)).
:- use_module(library(lists)).

:- dynamic
	test_dir/1.

:- prolog_load_context(directory, Dir),
   assert(test_dir(Dir)).

utf8_test :-
	test_dir(Dir),
	atom_concat(Dir, '/UTF-8-test.txt', File),
	length_test(File),
	peek_test(File).


		 /*******************************
		 *	    LENGTH TEST		*
		 *******************************/

length_test(File) :-
	open(File, read, In,
	     [ encoding(utf8)
	     ]),
	call_cleanup(test_lines(In), close(In)).

test_lines(In) :-
	line_count(In, LineNo),
	catch_messages(readline(In, Line), Messages),
	(   append(Before, "|", Line)
	->  length(Before, Len),
	    (	Len == 78
	    ->	utter(utf8(ok), 'OK:~d', [LineNo])
	    ;	utter(utf8(error), 'Wrong line:~d: ~w', [LineNo, Before])
	    ),
	    test_lines(In)
	;   append(Before, "*", Line),
	    length(Before, Len),
	    (   Len == 78
	    ->	utter(utf8(ok), 'Recovery ok:~d', [LineNo])
	    ;	utter(utf8(malformed), 'Bad recovery:~d (Len=~d) ~w',
		      [LineNo, Len, Line])
	    ),
	    (	Messages == []
	    ->	utter(utf8(malformed), 'Warning expected: ~d', [LineNo])
	    ;	utter(utf8(ok), 'Warning OK: ~d', [LineNo])
	    )
	->  test_lines(In)
	;   Line == end_of_file
	->  true
	;   utter(utf8(skipped), 'Skipped:~d: ~w', [LineNo, Line]),
	    test_lines(In)
	).


		 /*******************************
		 *	      PEEK TEST		*
		 *******************************/

peek_test(File) :-
	open(File, read, In,
	     [ encoding(utf8)
	     ]),
	call_cleanup(catch_messages(peek_all(In), _), close(In)).

peek_all(In) :-
	peek_code(In, C),
	get_code(In, C2),
	(   C == C2
	->  true
	;   utter(utf8(peek), 'Peek error: ~d != ~d', [C, C2])
	),
	(   C == -1
	->  true
	;   peek_all(In)
	).


		 /*******************************
		 *	     READ LINE		*
		 *******************************/

readline(In, Line) :-
	at_end_of_stream(In), !,
	Line = end_of_file.
readline(In, Line) :-
	get_code(In, C1),
	readline(C1, In, Line).

readline(10, _, []) :- !.
readline(-1, _, []) :- !.
readline(C, In, [C|T]) :-
	get_code(In, C2),
	readline(C2, In, T).

		 /*******************************
		 *	  MESSAGE TRICKS	*
		 *******************************/

catch_messages(Goal, Messages) :-
	nb_setval(messages, []),
	assert((user:message_hook(Msg, _, _) :- catch_message(Msg)),
	       Ref),
	Goal,
	collect_messages(Messages, Ref).
	
catch_message(Msg) :-
	nb_getval(messages, L0),
	duplicate_term(Msg, Copy),
	nb_linkval(messages, [Copy|L0]).

collect_messages(Messages, Ref) :-
	erase(Ref),
	nb_getval(messages, L),
%	nb_delete(messages),
	reverse(L, Messages).


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

:- dynamic
	uttering/1.

utter(Term, Fmt, Args) :-
	(   uttering(Term)
	->  print_message(informational, format(Fmt, Args))
	;   true
	).

utter(Term) :-
	assert(uttering(Term)).

:- utter(utf8(error)).
:- utter(utf8(malformed)).

