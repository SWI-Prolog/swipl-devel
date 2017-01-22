/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2013-2015, University of Amsterdam
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

:- module(test_locale,
	  [ test_locale/0
	  ]).
:- use_module(library(plunit)).

test_locale :-
	run_tests([ locale,
		    collation_key
		  ]).

setup_test_locale(Old, Test) :-
	current_locale(Old),
	locale_create(Test, default,
		      [ alias(test),
			decimal_point(.),
			thousands_sep(','),
			grouping([repeat(3)])
		      ]),
	set_locale(Test).
restore_locale(Old, Test) :-
	set_locale(Old),
	locale_destroy(Test).

%%	format_with_locale(?Out, +Format, +Args, +LocaleProps)
%
%	As format/3, but using the given temporary locale properties.

format_with_locale(Out, Fmt, Args, LocaleProps) :-
	current_locale(Old),
	locale_create(Tmp, Old, LocaleProps),
	setup_call_cleanup(
	    set_locale(Tmp),
	    format(Out, Fmt, Args),
	    set_locale(Old)).


:- begin_tests(locale,
	       [ setup(setup_test_locale(Old, Test)),
		 cleanup(restore_locale(Old, Test))
	       ]).

test(current, Alias == test) :-
	locale_property(current_locale, alias(Alias)).
test(property, DC == '.') :-
	locale_property(current_locale, decimal_point(DC)).
test(property, TS == ',') :-
	locale_property(current_locale, thousands_sep(TS)).
test(property, Group == [repeat(3)]) :-
	locale_property(current_locale, grouping(Group)).
test(create, TS == ' ') :-
	locale_create(New, current_locale, [thousands_sep(' ')]),
	locale_property(New, thousands_sep(TS)).

test(group, Atom == '0') :-
	format(atom(Atom), '~D', [0]).
test(group, Atom == '1,000') :-
	format(atom(Atom), '~D', [1000]).
test(group, Atom == '10.00') :-
	format(atom(Atom), '~2D', [1000]).
test(group, Atom == '1,000.00') :-
	format(atom(Atom), '~2D', [100000]).

test(group, Atom == '0') :-
	format(atom(Atom), '~:d', [0]).
test(group, Atom == '1,000') :-
	format(atom(Atom), '~:d', [1000]).
test(group, Atom == '10.00') :-
	format(atom(Atom), '~2:d', [1000]).
test(group, Atom == '1,000.00') :-
	format(atom(Atom), '~2:d', [100000]).

test(group, Atom == '0.00') :-
	format(atom(Atom), '~2:f', [0]).
test(group, Atom == '10.00') :-
	format(atom(Atom), '~2:f', [10]).
test(group, Atom == '1,000.00') :-
	format(atom(Atom), '~2:f', [1000]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '12,345,678,901,234,567,890.00') :-
	format(atom(Atom), '~2:f', [12345678901234567890]).
:-endif.

test(group, Atom == '0.00') :-
	format(atom(Atom), '~2:f', [-0]).
test(group, Atom == '-10.00') :-
	format(atom(Atom), '~2:f', [-10]).
test(group, Atom == '-1,000.00') :-
	format(atom(Atom), '~2:f', [-1000]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '-12,345,678,901,234,567,890.00') :-
	format(atom(Atom), '~2:f', [-12345678901234567890]).
:-endif.

test(group, Atom == '0.00') :-
	format(atom(Atom), '~2:f', [0.0]).
test(group, Atom == '10.00') :-
	format(atom(Atom), '~2:f', [10.0]).
test(group, Atom == '1,000.00') :-
	format(atom(Atom), '~2:f', [1000.0]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '0.00') :-
	format(atom(Atom), '~2:f', [0 rdiv 1]).
test(group, Atom == '1,234,567,890.00') :-
	format(atom(Atom), '~2:f', [1234567890 rdiv 1]).

test(group, Atom == '0.00') :-
	format(atom(Atom), '~2:f', [1 rdiv 1000]).
test(group, Atom == '0.01') :-
	format(atom(Atom), '~2:f', [12 rdiv 1000]).
test(group, Atom == '0.12') :-
	format(atom(Atom), '~2:f', [123 rdiv 1000]).
test(group, Atom == '1.23') :-
	format(atom(Atom), '~2:f', [1234 rdiv 1000]).

test(group, Atom == '0.01') :-
	format(atom(Atom), '~2:f', [6 rdiv 1000]).
test(group, Atom == '0.07') :-
	format(atom(Atom), '~2:f', [67 rdiv 1000]).
test(group, Atom == '0.68') :-
	format(atom(Atom), '~2:f', [678 rdiv 1000]).
test(group, Atom == '6.79') :-
	format(atom(Atom), '~2:f', [6789 rdiv 1000]).

test(group, Atom == '10.00') :-
	format(atom(Atom), '~2:f', [9999 rdiv 1000]).
:-endif.

test(group, Atom == '-0.00') :-
	format(atom(Atom), '~2:f', [-0.0]).
test(group, Atom == '-10.00') :-
	format(atom(Atom), '~2:f', [-10.0]).
test(group, Atom == '-1,000.00') :-
	format(atom(Atom), '~2:f', [-1000.0]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '0.00') :-
	format(atom(Atom), '~2:f', [-0 rdiv 1]).
test(group, Atom == '-1,234,567,890.00') :-
	format(atom(Atom), '~2:f', [-1234567890 rdiv 1]).

test(group, Atom == '-0.00') :-
	format(atom(Atom), '~2:f', [-1 rdiv 1000]).
test(group, Atom == '-0.01') :-
	format(atom(Atom), '~2:f', [-12 rdiv 1000]).
test(group, Atom == '-0.12') :-
	format(atom(Atom), '~2:f', [-123 rdiv 1000]).
test(group, Atom == '-1.23') :-
	format(atom(Atom), '~2:f', [-1234 rdiv 1000]).

test(group, Atom == '-0.01') :-
	format(atom(Atom), '~2:f', [-6 rdiv 1000]).
test(group, Atom == '-0.07') :-
	format(atom(Atom), '~2:f', [-67 rdiv 1000]).
test(group, Atom == '-0.68') :-
	format(atom(Atom), '~2:f', [-678 rdiv 1000]).
test(group, Atom == '-6.79') :-
	format(atom(Atom), '~2:f', [-6789 rdiv 1000]).

test(group, Atom == '-10.00') :-
	format(atom(Atom), '~2:f', [-9999 rdiv 1000]).
:-endif.

test(group, Atom == '0.00e+00') :-
	format(atom(Atom), '~2:e', [0]).
test(group, Atom == '1.00e+01') :-
	format(atom(Atom), '~2:e', [10]).
test(group, Atom == '1.00e+03') :-
	format(atom(Atom), '~2:e', [1000]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '1.23e+19') :-
	format(atom(Atom), '~2:e', [12345678901234567890]).
test(group, Atom == '9.88e+19') :-
	format(atom(Atom), '~2:e', [98765432109876543210]).
:-endif.

test(group, Atom == '0.00e+00') :-
	format(atom(Atom), '~2:e', [-0]).
test(group, Atom == '-1.00e+01') :-
	format(atom(Atom), '~2:e', [-10]).
test(group, Atom == '-1.00e+03') :-
	format(atom(Atom), '~2:e', [-1000]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '-1.23e+19') :-
	format(atom(Atom), '~2:e', [-12345678901234567890]).
test(group, Atom == '-9.88e+19') :-
	format(atom(Atom), '~2:e', [-98765432109876543210]).
:-endif.

test(group, Atom == '0.00e+00') :-
	format(atom(Atom), '~2:e', [0.0]).
test(group, Atom == '1.00e+01') :-
	format(atom(Atom), '~2:e', [10.0]).
test(group, Atom == '1.00e+03') :-
	format(atom(Atom), '~2:e', [1000.0]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '0.00e+00') :-
	format(atom(Atom), '~2:e', [0 rdiv 1]).
test(group, Atom == '1.23e+09') :-
	format(atom(Atom), '~2:e', [1234567890 rdiv 1]).

test(group, Atom == '1.00e-03') :-
	format(atom(Atom), '~2:e', [1 rdiv 1000]).
test(group, Atom == '1.20e-02') :-
	format(atom(Atom), '~2:e', [12 rdiv 1000]).
test(group, Atom == '1.23e-01') :-
	format(atom(Atom), '~2:e', [123 rdiv 1000]).
test(group, Atom == '1.23e+00') :-
	format(atom(Atom), '~2:e', [1234 rdiv 1000]).

test(group, Atom == '6.00e-03') :-
	format(atom(Atom), '~2:e', [6 rdiv 1000]).
test(group, Atom == '6.70e-02') :-
	format(atom(Atom), '~2:e', [67 rdiv 1000]).
test(group, Atom == '6.78e-01') :-
	format(atom(Atom), '~2:e', [678 rdiv 1000]).
test(group, Atom == '6.79e+00') :-
	format(atom(Atom), '~2:e', [6789 rdiv 1000]).

test(group, Atom == '1.00e+01') :-
	format(atom(Atom), '~2:e', [9999 rdiv 1000]).
:-endif.

test(group, Atom == '-0.00e+00') :-
	format(atom(Atom), '~2:e', [-0.0]).
test(group, Atom == '-1.00e+01') :-
	format(atom(Atom), '~2:e', [-10.0]).
test(group, Atom == '-1.00e+03') :-
	format(atom(Atom), '~2:e', [-1000.0]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '0.00e+00') :-
	format(atom(Atom), '~2:e', [-0 rdiv 1]).
test(group, Atom == '-1.23e+09') :-
	format(atom(Atom), '~2:e', [-1234567890 rdiv 1]).

test(group, Atom == '-1.00e-03') :-
	format(atom(Atom), '~2:e', [-1 rdiv 1000]).
test(group, Atom == '-1.20e-02') :-
	format(atom(Atom), '~2:e', [-12 rdiv 1000]).
test(group, Atom == '-1.23e-01') :-
	format(atom(Atom), '~2:e', [-123 rdiv 1000]).
test(group, Atom == '-1.23e+00') :-
	format(atom(Atom), '~2:e', [-1234 rdiv 1000]).

test(group, Atom == '-6.00e-03') :-
	format(atom(Atom), '~2:e', [-6 rdiv 1000]).
test(group, Atom == '-6.70e-02') :-
	format(atom(Atom), '~2:e', [-67 rdiv 1000]).
test(group, Atom == '-6.78e-01') :-
	format(atom(Atom), '~2:e', [-678 rdiv 1000]).
test(group, Atom == '-6.79e+00') :-
	format(atom(Atom), '~2:e', [-6789 rdiv 1000]).

test(group, Atom == '-1.00e+01') :-
	format(atom(Atom), '~2:e', [-9999 rdiv 1000]).
:-endif.

test(group, Atom == '1.00E+01') :-
	format(atom(Atom), '~2:E', [10]).

:-if(current_prolog_flag(bounded, false)).
test(group, Atom == '1.00E-03') :-
	format(atom(Atom), '~2:E', [1 rdiv 1000]).
:-endif.

test(group, Atom == '12345,67,89') :-
	format_with_locale(atom(Atom), '~:d', [123456789],
			   [ grouping([2,2])
			   ]).
test(group, Atom == '123,45,67.89') :-
	format_with_locale(atom(Atom), '~2:d', [123456789],
			   [ grouping([2,2])
			   ]).
test(group, Atom == '12,345,67,89') :-
	format_with_locale(atom(Atom), '~:d', [123456789],
			   [ grouping([2,2,repeat(3)])
			   ]).

test(group, Atom == '1.234.567,89') :-
	format_with_locale(atom(Atom), '~2:d', [123456789],
			   [ decimal_point(','),
			     thousands_sep('.')
			   ]).

% \u2009 is Unicode thin space

test(group, Atom == '1\u2009234\u2009567,89') :-
	format_with_locale(atom(Atom), '~2:d', [123456789],
			   [ decimal_point(','),
			     thousands_sep('\u2009')
			   ]).
test(group, Atom == '1.\u2009234.\u2009567,89') :-
	format_with_locale(atom(Atom), '~2:d', [123456789],
			   [ decimal_point(','),
			     thousands_sep('.\u2009')
			   ]).

test(group, Atom == '1\u2009234\u2009567,89') :-
	format_with_locale(atom(Atom), '~2:f', [1234567.89],
			   [ decimal_point(','),
			     thousands_sep('\u2009')
			   ]).
test(group, Atom == '1.\u2009234.\u2009567,89') :-
	format_with_locale(atom(Atom), '~2:f', [1234567.89],
			   [ decimal_point(','),
			     thousands_sep('.\u2009')
			   ]).

:- end_tests(locale).


:- begin_tests(collation_key).

test('WCSXFRM_BUFFER_OVERRUN', true) :-
	findall(C, between(32,1000,C), Codes),
	atom_codes(Atom, Codes),
	collation_key(Atom, _Key).

:- end_tests(collation_key).
