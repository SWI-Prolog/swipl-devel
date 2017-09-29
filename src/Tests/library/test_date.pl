/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2006-2017, University of Amsterdam
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

:- module(test_date,
	  [ test_date/0
	  ]).
:- use_module(library(lists)).
:- use_module(library(plunit)).
:- use_module(library(date)).

:- dynamic
	error/1.

test_date :-
	non_unit_tests,
	run_tests([ parse_time,
		    format_time
		  ]).

non_unit_tests :-
	retractall(error(_)),
	(   catch((test_format, test_trip), E, true)
	->  (   var(E)
	    ->  \+ error(_)
	    ;	print_message(error, E),
		fail
	    )
	).


		 /*******************************
		 *      TIME_FORMAT/3 TESTS	*
		 *******************************/

ok(1152794050, '%a', 'Thu').
ok(1152794050, '%A', 'Thursday').
ok(1152794050, '%b', 'Jul').
ok(1152794050, '%B', 'July').
ok(1152794050, '%c', 'Thu Jul 13 14:34:10 2006').
ok(1152794050, '%p', ['P', 'PM']).
%ok(1152794050, '%P', 'pm').		% non-standard
ok(1152794050, '%x', '07/13/06').
ok(1152794050, '%X', '14:34:10').
ok(1152794050, '%C', '20').
ok(1152794050, '%d', '13').
ok(1152794050, '%D', '07/13/06').
ok(1152794050, '%e', '13').
%ok(1152794050, '%E', '').
ok(1152794050, '%F', '2006-07-13').
ok(1152794050, '%g', '06').
ok(1152794050, '%G', '2006').
ok(1152794050, '%V', '28').
ok(1152794050, '%h', 'Jul').
ok(1152794050, '%H', '14').
ok(1152794050, '%I', '02').
ok(1152794050, '%j', '194').
ok(1152794050, '%k', '14').
ok(1152794050, '%l', ' 2').
ok(1152794050, '%m', '07').
ok(1152794050, '%M', '34').
ok(1152794050, '%n', '\n').
%ok(1152794050, '%O', '').
ok(1152794050, '%r', '02:34:10 PM').
ok(1152794050, '%R', '14:34').
ok(1152794050, '%s', '1152794050').
ok(1152794050, '%S', '10').
ok(1152794050, '%t', '\t').
ok(1152794050, '%T', '14:34:10').
ok(1152794050, '%u', '4').
ok(1152794050, '%U', '28').
ok(1152794050, '%w', '4').
ok(1152794050, '%W', '28').
ok(1152794050, '%y', '06').
ok(1152794050, '%Y', '2006').
ok(1152794050, '%z', '+0200').
ok(1152794050, '%Z', 'CEST').
ok(1152794050, '%+', 'Thu Jul 13 14:34:10 2006').
ok(1152794050, '%%', '%').

%%	test_format/0
%
%	Extensively test the output of all supported formats.  We must
%	run this in the C locale to get reproducable answers.

test_format :-
	setlocale(time, OldLocale, 'C'),
	(   ok(Time, Fmt, Atom),
	    (	format_time(atom(A), Fmt, Time)
	    ->	(   (   A == Atom
		    ;	is_list(Atom),
			memberchk(A, Atom)
		    )
		->  true
		;   format('~q: got ~q, expected ~q~n', [Fmt, A, Atom])
		)
	    ;	format('format_time(~q, ~q, ~q) failed~n', [atom(_), Fmt, Time])
	    ),
	    fail
	;   true
	),
	setlocale(time, _, OldLocale).


		 /*******************************
		 *	  GENERAL TESTS		*
		 *******************************/

%	test_date(+Date, +Time, -FormatTests).

test_date(1970-1-1, 0:0:0.0,		% Epoch
	  [ '%s' = '0'
	  ]).
test_date(0-1-1, 0:0:0.0, []).		% Year 0
test_date(2000-1-1, 0:0:0.0, []).	% Year 2000
test_date(-10000-1-1, 0:0:0.0, []).	% Year 10,000BC
test_date(10000-1-1, 0:0:0.0, []).	% Year 10,000AD

%	test_trip/0
%
%	Run all round-trip tests and verify formats on them

test_trip :-
	forall(test_date(Date, Time, FormatTests),
	       test_trip(Date, Time, FormatTests)).

test_trip(Y-M-D, H:Min:S, FormatTests) :-
	Date = date(Y,M,D,H,Min,S,0,-,-),
	date_time_stamp(Date, Stamp),
	stamp_date_time(Stamp, Date2, 0),
	(   Date2 = Date
	->  true
	;   error('~q: Tripped as ~q', [Date, Date2])
	),
	(   member(Fmt = Val, FormatTests),
	    (	format_time(atom(A), Fmt, Stamp),
		A == Val
	    ->	true
	    ;	error('Format failed: ~q ~q ~q', [Date, Fmt, Val])
	    ),
	    fail
	;   true
	).


		 /*******************************
		 *	       KEEP		*
		 *******************************/

russian_day(A) :-
	setlocale(time, Old, 'ru_RU.utf8'),
	get_time(X),
	format_time(atom(A), '%A', X),
	setlocale(time, _, Old).

utc :-
	get_time(Stamp),
	stamp_date_time(Stamp, DateTime, 'UTC'),
	format_time(current_output, '%F %T %Z', DateTime).

		 /*******************************
		 *	      ERROR		*
		 *******************************/


error(Fmt, Args) :-
	assert(error(Fmt-Args)),
	format(user_error, Fmt, Args),
	nl(user_error).


		 /*******************************
		 *	     UNIT TESTS		*
		 *******************************/

:- begin_tests(parse_time).

test(iso_8601, T =:= 1165591784) :-
	parse_time('2006-12-08T17:29:44+02:00', iso_8601, T).
test(iso_8601, T =:= 1165591784) :-
	parse_time('20061208T172944+0200', iso_8601, T).
test(iso_8601, T =:= 1165591740) :-
	parse_time('2006-12-08T15:29Z', iso_8601, T).
test(iso_8601, T =:= 1165536000) :-
	parse_time('2006-12-08', iso_8601, T).
test(iso_8601, T =:= 1165536000) :-
	parse_time('20061208', iso_8601, T).
test(iso_8601, T =:= 1164844800) :-
	parse_time('2006-12', iso_8601, T).
test(iso_8601, T =:= 1165536000) :-
	parse_time('2006-W49-5', iso_8601, T).
test(iso_8601, T =:= 1165536000) :-
	parse_time('2006-342', iso_8601, T).

:- end_tests(parse_time).

:- begin_tests(format_time).

test(fraction) :-
	forall(member(T, [100000.0, 100000.9, 100000.99,
			  100000.999, 100000.9999]),
	       ( format_time(string(S), '%3f', T),
	         assertion(fok(S)))).
test(negfraction) :-
	forall(member(T, [-100000.0, -100000.9, -100000.99,
			  -100000.999, -100000.9999]),
	       ( format_time(string(S), '%3f', T),
	         assertion(fok(S)))).

fok("000").
fok("900").
fok("990").
fok("999").

:- end_tests(format_time).

:- begin_tests(timestamp_roundtrip).

test(roundtrip_local, T =:= T2) :-
	T = 1165591784,
	stamp_date_time(T, D, local),
	date_time_stamp(D, T2).
test(roundtrip_utc, T =:= T2) :-
	T = 1165591784,
	stamp_date_time(T, D, 'UTC'),
	date_time_stamp(D, T2).
test(roundtrip_epoch, T =:= T2) :-
	T = 0,
	stamp_date_time(T, D, 0),
	date_time_stamp(D, T2).

:- end_tests(timestamp_roundtrip).
