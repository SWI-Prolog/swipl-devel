/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2004-2010, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(utf8_test,
	  [ utf8_test/0
	  ]).
:- use_module(library(readutil)).
:- use_module(library(lists)).

:- dynamic
	test_dir/1.

:- prolog_load_context(directory, Dir),
   retractall(test_dir(_)),
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
	thread_self(Me),
	setup_call_cleanup(assert((user:message_hook(Msg, _, _) :-
				  	catch_message(Me, Msg)),
				  Ref),
			   once(Goal),
			   collect_messages(Messages, Ref)).

catch_message(Me, Msg) :-
	thread_self(Me), !,
	nb_getval(messages, L0),
	duplicate_term(Msg, Copy),
	nb_linkval(messages, [Copy|L0]).

collect_messages(Messages, Ref) :-
	erase(Ref),
	nb_getval(messages, L),
	nb_delete(messages),
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

