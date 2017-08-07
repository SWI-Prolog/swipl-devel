/*  Part of SWI-Prolog

    Author:        Ulrich Neumerkel and Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(test_pio, [test_pio/0]).
:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(library(pio)).

test_pio :-
	run_tests([ phrase_from_file,
		    read_pending_input
		  ]).


:- begin_tests(phrase_from_file, []).

... --> [] | [_], ... .

seq([]) --> [].
seq([E|Es]) -->
	[E],
	seq(Es).

get_all(Codes) -->
    seq(Codes),
    (   \+ [_]
    ;   { \+ ground(Codes) }
    ),
    !.


cfc(Content,Tmp) :-
	tmp_file(plunit_pio,Tmp),
	open(Tmp,write,Out),
	format(Out,'~s',[Content]),
	close(Out).

df(Tmp) :-
	delete_file(Tmp).

test(null, [setup(cfc("",Null)),cleanup(df(Null)) ]) :-
	phrase_from_file([],Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), fail]) :-
	phrase_from_file("a",Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), nondet]) :-
	phrase_from_file(([]|"a"),Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), nondet]) :-
	phrase_from_file(("a"|[]),Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), nondet]) :-
	phrase_from_file(...,Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null))]) :-
	phrase_from_file(([],[],{true}),Null).


test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)) ]) :-
	phrase_from_file("aba",ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)) ]) :-
	phrase_from_file(("aca"|"aba"),ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), nondet]) :-
	phrase_from_file(("abx"|"aba"|"ada"),ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)) ]) :-
	phrase_from_file([A,_,A], ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), nondet ]) :-
	phrase_from_file(([A],...,[A]), ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), fail ]) :-
	phrase_from_file((...,[A,A],...), ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), fail ]) :-
	phrase_from_file((...,"c",...), ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), nondet ]) :-
	phrase_from_file((seq(Seq),...,seq(Seq)), ABA),
	Seq = [_|_].

test(abc_nodebug, [ Codes == `abc`,
		    setup(cfc("abc",ABC)), cleanup(df(ABC))
		  ]) :-
	setup_call_cleanup(
	    (	current_prolog_flag(debug, Old),
		set_prolog_flag(debug, false)
	    ),
	    phrase_from_file(get_all(Codes), ABC),
	    set_prolog_flag(debug, Old)).
test(abc_debug, [ Codes == `abc`,
		  setup(cfc("abc",ABC)), cleanup(df(ABC))
		]) :-
	setup_call_cleanup(
	    (	current_prolog_flag(debug, Old),
		set_prolog_flag(debug, true)
	    ),
	    phrase_from_file(get_all(Codes), ABC),
	    set_prolog_flag(debug, Old)).


:- end_tests(phrase_from_file).


:- begin_tests(read_pending_input, [sto(rational_trees)]).

test_pe(N, BF, Enc) :-
	tmp_file(plunit_pio, Tmp),
	call_cleanup(test_pe(N, BF, Enc, Tmp), delete_file(Tmp)).

test_pe(N, BF, Enc, Tmp) :-
	max_char(Enc, MaxChar),
	random_list(N, MaxChar, List),
	test_list(List, BF, Enc, Tmp).

test_list(List, BF, Enc, Tmp) :-
	save_list(Tmp, List, Enc),
	open(Tmp, read, In, [encoding(Enc), bom(false)]),
	set_stream(In, buffer_size(BF)),
	stream_to_lazy_list(In, Lazy),
	(   List = Lazy
	->  close(In)
	;   format('List: ~w~n', [List]),
	    (	last(Lazy, _)
	    ->	format('Lazy: ~w~n', [Lazy])
	    ;	format('Lazy: cannot materialize~n')
	    ),
	    close(In),
	    read_file_to_codes(Tmp, Codes, [encoding(Enc)]),
	    (	Codes == List
	    ->	format('File content ok~n')
	    ;	Codes == Lazy
	    ->	format('File content BAD, but read consistently~n')
	    ;	format('File content BAD, and read inconsistently~n')
	    ),
	    fail
	).

:- if(fail).
% Keep around to get a better indication of the error location
cmp_lists([], [], _).
cmp_lists([H|T1], [H|T2], C0) :- !,
	C1 is C0+1,
	cmp_lists(T1, T2, C1).
cmp_lists(L1, L2, C) :-
	format('~NCommon: ~d~nLeft: ~w~nRight: ~w~n', [C, L1, L2]).
:- endif.


max_char(ascii, 127).
max_char(octet, 255).
max_char(text, 0xfffff).		% Only if Locale is UTF-8!  How to test?
max_char(iso_latin_1, 255).
max_char(utf8, Max) :-
	(   current_prolog_flag(windows, true)
	->  Max = 0xffff		% UTF-16
	;   Max = 0xfffff
	).
max_char(unicode_le, 0xffff).
max_char(unicode_be, 0xffff).
max_char(wchar_t, Max) :-
	(   current_prolog_flag(windows, true)
	->  Max = 0xffff		% UTF-16
	;   Max = 0xfffff
	).


save_list(File, Codes, Enc) :-
	open(File, write, Out, [encoding(Enc)]),
	format(Out, '~s', [Codes]),
	close(Out).

%random_list(_, _, "hello\nworld\n") :- !. % debug
random_list(0, _, []) :- !.
random_list(N, Max, [H|T]) :-
	repeat,
	H is 1+random(Max-1),
	H \== 0'\r, !,
	N2 is N - 1,
	random_list(N2, Max, T).

test(ascii) :-
	test_pe(1000, 25, ascii).
test(octet) :-
	test_pe(1000, 25, octet).
test(iso_latin_1) :-
	test_pe(1000, 25, iso_latin_1).
test(utf8) :-
	test_pe(1000, 25, utf8).
test(unicode_le) :-
	test_pe(1000, 25, unicode_le).
test(unicode_be) :-
	test_pe(1000, 25, unicode_be).
test(wchar_t) :-
	test_pe(1000, 25, wchar_t).

:- end_tests(read_pending_input).
