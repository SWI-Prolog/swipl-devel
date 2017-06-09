#!/home/jan/bin/swipl -q -g true -t main -G4g -T4g -s

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2013, University of Amsterdam
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

calls([_, _, _, _, Where|Rest], File, Line, Callee) :-
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
	current_prolog_flag(argv, [In,Out]),
	format(user_error, 'Processing ~w ...', [In]),
	statistics(cputime, T0),
	catch(process(In, Out), E,
	      (	  print_message(error, E),
		  fail
	      )),
	statistics(cputime, T1),
	T is T1 - T0,
	format(user_error, '~3f sec~n', [T]).

