/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, University of Amsterdam
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

:- module(test_code_type, [test_code_type/0]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(varnumbers)).

/** <module> Test Prolog text code_typeting primitives

This module is a Unit test for  Prolog code_type/2, etc.

@author	Jan Wielemaker
*/

test_code_type :-
	run_tests([ code_type,
		    code_type_unicode,
		    case_mapping
		  ]).

:- begin_tests(code_type, [sto(rational_trees)]).

test(code_type) :-
	assert_ct,
	gen,
	retractall(ct(_,_)).

:- end_tests(code_type).

/* The character classes are derived from the Unicode Character
 * Database at build time rather than from <wctype.h>, so the answers
 * below must hold on every platform and in every locale.  Several of
 * them do *not* hold for the C library: glibc denies that U+00A0 is
 * space, Darwin denies that U+0085 is, and both deny most of the
 * post-Unicode-6 additions.
 */

:- begin_tests(code_type_unicode).

test(white_space, all(C == [0x09,0x0a,0x0b,0x0c,0x0d,0x20,0x85,0xa0,
			    0x1680,0x2000,0x2001,0x2002,0x2003,0x2004,
			    0x2005,0x2006,0x2007,0x2008,0x2009,0x200a,
			    0x2028,0x2029,0x202f,0x205f,0x3000])) :-
	code_point(C),
	code_type(C, space).
test(nbsp_is_not_graph, [fail]) :-
	code_type(0xa0, graph).
test(nbsp_is_print) :-
	code_type(0xa0, print).
test(nel_is_cntrl) :-
	code_type(0x85, cntrl).
test(separator_is_not_cntrl, [fail]) :-
	code_type(0x2028, cntrl).
test(digit_is_ascii, all(C == [0'0,0'1,0'2,0'3,0'4,0'5,0'6,0'7,0'8,0'9])) :-
	code_point(C),
	code_type(C, digit).
test(arabic_digit_is_alnum) :-
	code_type(0x660, alnum),
	\+ code_type(0x660, digit),
	code_type(0x660, decimal).
test(cjk_is_alpha) :-
	code_type(0x4e2d, alpha),
	code_type(0x4e2d, prolog_atom_start).
					% csym/csymf are C identifiers, so
					% ASCII; Prolog has its own types
test(csym_is_ascii, all(C == [0'0,0'1,0'2,0'3,0'4,0'5,0'6,0'7,0'8,0'9,
			      0'A,0'B,0'C,0'D,0'E,0'F,0'G,0'H,0'I,0'J,0'K,
			      0'L,0'M,0'N,0'O,0'P,0'Q,0'R,0'S,0'T,0'U,0'V,
			      0'W,0'X,0'Y,0'Z,0'_,
			      0'a,0'b,0'c,0'd,0'e,0'f,0'g,0'h,0'i,0'j,0'k,
			      0'l,0'm,0'n,0'o,0'p,0'q,0'r,0's,0't,0'u,0'v,
			      0'w,0'x,0'y,0'z])) :-
	code_point(C),
	code_type(C, csym).
test(csymf_excludes_digits) :-
	code_type(0'a, csymf),
	code_type(0'_, csymf),
	\+ code_type(0'0, csymf),
	\+ code_type(0x00e9, csymf),
	code_type(0x00e9, prolog_atom_start).
test(roman_numerals_have_case) :-
	code_type(0x2160, upper),
	code_type(0x2170, lower),
	code_type(0x2160, upper(0x2170)).
test(non_bmp_is_classified) :-		% wint_t is 16 bits on Windows
	code_type(0x1d400, upper),
	code_type(0x1d400, alpha).
test(private_use_is_print) :-
	code_type(0xf8ff, print),
	code_type(0xf8ff, graph).
test(unassigned_is_classless, [fail]) :-
	member(T, [alnum,alpha,cntrl,digit,graph,lower,print,punct,space,upper]),
	code_type(0x0378, T).		% permanently unassigned

					% white is White_Space minus the
					% seven line terminators
test(white, all(C == [0x09,0x20,0xa0,
		      0x1680,0x2000,0x2001,0x2002,0x2003,0x2004,0x2005,
		      0x2006,0x2007,0x2008,0x2009,0x200a,
		      0x202f,0x205f,0x3000])) :-
	code_point(C),
	code_type(C, white).
					% end_of_line is the seven Unicode
					% line terminators
test(end_of_line, all(C == [0x0a,0x0b,0x0c,0x0d,0x85,0x2028,0x2029])) :-
	code_point(C),
	code_type(C, end_of_line).
test(space_is_white_plus_end_of_line, [fail]) :-
	code_point(C),
	(   code_type(C, space)
	->  \+ ( code_type(C, white)
		; code_type(C, end_of_line)
		)
	;   ( code_type(C, white)
	    ; code_type(C, end_of_line)
	    )
	).
test(white_and_end_of_line_are_disjoint, [fail]) :-
	code_point(C),
	code_type(C, white),
	code_type(C, end_of_line).
test(period_is_sentence_terminal) :-
	forall(member(C, [0'., 0'!, 0'?,	% ASCII
			  0x0589,		% ARMENIAN FULL STOP
			  0x061f,		% ARABIC QUESTION MARK
			  0x0964,		% DEVANAGARI DANDA
			  0x3002,		% IDEOGRAPHIC FULL STOP
			  0xff01]),		% FULLWIDTH EXCLAMATION MARK
	       code_type(C, period)),
	\+ code_type(0x002c, period).		% a comma is not a terminator
					% quote/0 holds for both sides of a
					% pair, quote/1 only for the opening
test(quote_covers_both_sides) :-
	forall(( code_point(C),
		 code_type(C, quote(Close))
	       ),
	       ( code_type(C, quote),
		 code_type(Close, quote)
	       )),
	code_type(0x00ab, quote),		% << is the opening
	code_type(0x00bb, quote),		% >> the closing
	code_type(0x00ab, quote(0x00bb)),
	\+ code_type(0x00bb, quote(_)).

/* The POSIX class invariants, over the whole of Unicode.  These are
 * what the generated ctype_to_flags[] table encodes; a mistake in the
 * class partition shows up here.
 */

test(graph_and_space_are_disjoint, [fail]) :-
	code_point(C),
	code_type(C, graph),
	code_type(C, space).
test(punct_is_graph_minus_alnum, [fail]) :-
	code_point(C),
	(   code_type(C, punct)
	->  \+ ( code_type(C, graph), \+ code_type(C, alnum) )
	;   code_type(C, graph), \+ code_type(C, alnum)
	).
test(print_is_graph_or_non_control_space, [fail]) :-
	code_point(C),
	(   code_type(C, print)
	->  \+ ( code_type(C, graph)
		; code_type(C, space), \+ code_type(C, cntrl)
		)
	;   ( code_type(C, graph)
	    ; code_type(C, space), \+ code_type(C, cntrl)
	    )
	).
test(white_implies_space, [fail]) :-
	code_point(C),
	code_type(C, white),
	\+ code_type(C, space).
test(case_implies_alpha, [fail]) :-
	code_point(C),
	( code_type(C, upper) ; code_type(C, lower) ),
	\+ code_type(C, alpha).
test(alpha_and_digit_imply_alnum, [fail]) :-
	code_point(C),
	( code_type(C, alpha) ; code_type(C, digit) ),
	\+ code_type(C, alnum).

:- end_tests(code_type_unicode).

/* Case conversion uses the Unicode *simple* case mapping, so it is
 * length preserving and, unlike towlower()/towupper(), the same in
 * every locale.
 */

:- begin_tests(case_mapping).

test(ascii) :-
	upcase_atom(abc, 'ABC'),
	downcase_atom('ABC', abc).
test(latin1) :-
	upcase_atom('caf\u00e9', 'CAF\u00c9').
test(sharp_s_is_length_preserving) :-
	upcase_atom('stra\u00dfe', U),
	atom_length(U, 6),
	U == 'STRA\u00dfE'.
test(turkish_i_is_not_applied) :-	% would be \u0130 under tr_TR in libc
	upcase_atom(istanbul, 'ISTANBUL').
test(roman_numerals) :-
	code_type(0x2160, to_upper(0x2170)),
	code_type(0x2170, to_lower(0x2160)).
test(non_bmp) :-			% wint_t is 16 bits on Windows
	atom_codes(A, [0x10428]),	% DESERET SMALL LETTER LONG I
	upcase_atom(A, U),
	atom_codes(U, [0x10400]).
test(titlecase_differs_from_upper) :-
	code_type(0x01c6, to_upper(_)),	% dz -> DZ, title Dz (U+01C5)
	char_type(X, to_lower('\u01c4')),
	X == '\u01c6'.
test(unmapped_is_identity) :-
	code_type(0x4e2d, to_lower(0x4e2d)),
	code_type(0x2200, to_upper(0x2200)).

:- end_tests(case_mapping).

%!	code_point(-C) is nondet.
%
%	Enumerate the code points code_type/2 accepts, i.e. all of
%	Unicode except the surrogates, which are not characters and
%	raise a type error.

code_point(C) :-
	(   between(0x0000, 0xd7ff, C)
	;   between(0xe000, 0x10ffff, C)
	).

:- thread_local ct/2.

test_range(0x0000, 0x0100).
test_range(0x0400, 0x0500).
%test_range(0, 0x1000).

assert_ct :-
	retractall(ct(_,_)),
	forall(( test_range(Low, High),
		 between(Low, High, C),
		 code_type(C, T)
	       ),
	       assertz(ct(C,T))).

gen_t(T) :-
	ct(_C,T0),
	(   atom(T0)
	->  T = T0
	;   functor(T0,F,A),
	    assertion(A==1),
	    T =.. [F, '$VAR'(0)]
	).

gen :-
	setof(T, gen_t(T), TL0),
	aggregate_all(max(U), test_range(_, U), Max),
	maplist(gen(Max), TL0).

t_code_type(Max,C, T) :-
	code_type(C, T),
	(   C > Max
	->  !,
	    fail
	;   test_range(Low, High),
	    between(Low, High, C)
	->  true
	).

gen(Max, T0) :-
	varnumbers(T0, T),
	(setof(C, t_code_type(Max,C,T), CL) -> true ; CL = []),
	(setof(C, ct(C,T), CL2) -> true ; CL2 = []),
	(   CL == CL2
	->  true
	;   ord_subtract(CL, CL2, Add),
	    ord_subtract(CL2, CL, Del),
	    format('ERROR: code_type ~p: Add: ~p, Del: ~p~n', [T, Add, Del]),
	    fail
	).
