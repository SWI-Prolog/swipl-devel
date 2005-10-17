/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(memfile_test,
	  [ test/0
	  ]).
:- asserta(user:file_search_path(foreign, '.')).

:- use_module(memfile).


		 /*******************************
		 *	      ACTION		*
		 *******************************/

wr_atom(Atom) :-
	new_memory_file(H),
	open_memory_file(H, write, Out),
	write(Out, Atom),
	close(Out),
	memory_file_to_atom(H, A2),
	size_memory_file(H, Size),
	atom_length(Atom, Size),
	A2 == Atom.

writemem(simple-1) :-
	wr_atom('Hello World').
writemem(wide-1) :-
	atom_codes(Atom, [97,98,1080,1081]),
	wr_atom(Atom).


		 /*******************************
		 *	    ACCESS ATOM		*
		 *******************************/

rd_atom(Atom) :-
	atom_to_memory_file(Atom, File),
	open_memory_file(File, read, In),
	read_to_codes(In, Codes),
	close(In),
	atom_codes(Atom, Codes).


read_to_codes(In, Codes) :-
	get_code(In, C0),
	read_to_codes(C0, In, Codes).

read_to_codes(-1, _, []).
read_to_codes(C0, In, [C0|T]) :-
	get_code(In, C1),
	read_to_codes(C1, In, T).

mematom(simple-1) :-
	rd_atom('Hello World').
mematom(wide-1) :-
	atom_codes(Atom, [97,98,1080,1081]),
	rd_atom(Atom).


		 /*******************************
		 *	      POSITION		*
		 *******************************/

position(pos-1) :-
	new_memory_file(MF),
	open_memory_file(MF, write, Out),
	format(Out, '~s', [[97, 254, 500]]),
	close(Out),
	size_memory_file(MF, CodeSize),
	open_memory_file(MF, read, In),
	get_code(In, _),
	get_code(In, _),
	utf8_position_memory_file(MF, Here, Size),
	CodeSize == 3,			% size in characters
	Here == 3,
	Size == 5.


		 /*******************************
		 *        TEST MAIN-LOOP	*
		 *******************************/

testset(writemem).
testset(mematom).
testset(position).

:- dynamic
	failed/1,
	blocked/2.

test :-
	retractall(failed(_)),
	retractall(blocked(_,_)),
	forall(testset(Set), runtest(Set)),
	report_blocked,
	report_failed.

report_blocked :-
	findall(Head-Reason, blocked(Head, Reason), L),
	(   L \== []
        ->  format('~nThe following tests are blocked:~n', []),
	    (	member(Head-Reason, L),
		format('    ~p~t~40|~w~n', [Head, Reason]),
		fail
	    ;	true
	    )
        ;   true
	).
report_failed :-
	findall(X, failed(X), L),
	length(L, Len),
	(   Len > 0
        ->  format('~n*** ~w tests failed ***~n', [Len]),
	    fail
        ;   format('~nAll tests passed~n', [])
	).

runtest(Name) :-
	format('Running test set "~w" ', [Name]),
	flush,
	functor(Head, Name, 1),
	nth_clause(Head, _N, R),
	clause(Head, _, R),
	(   catch(Head, Except, true)
	->  (   var(Except)
	    ->  put(.), flush
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(Head, Reason)),
		put(!), flush
	    ;   test_failed(R, Except)
	    )
	;   test_failed(R, fail)
	),
	fail.
runtest(_) :-
	format(' done.~n').
	
test_failed(R, Except) :-
	clause(ClHead, _, R),
	plain_head(ClHead, Head),
	functor(Head, Name, 1),
	arg(1, Head, TestName),
	clause_property(R, line_count(Line)),
	clause_property(R, file(File)),
	(   Except == failed
	->  format('~N~w:~d: Test ~w(~w) failed~n',
		   [File, Line, Name, TestName])
	;   message_to_string(Except, Error),
	    format('~N~w:~d: Test ~w(~w):~n~t~8|ERROR: ~w~n',
		   [File, Line, Name, TestName, Error])
	),
	assert(failed(Head)).

plain_head(_M:Head, Head) :- !.
plain_head(Head, Head).


blocked(Reason) :-
	throw(blocked(Reason)).

