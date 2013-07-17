:- module(test_locale,
	  [ test_locale/0
	  ]).
:- use_module(library(plunit)).

test_locale :-
	run_tests([ locale
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
	format(atom(Atom), '~2:f', [0.0]).
test(group, Atom == '1,000.00') :-
	format(atom(Atom), '~2:f', [1000.0]).
test(group, Atom == '10.00') :-
	format(atom(Atom), '~2:f', [10.0]).
test(group, Atom == '1,000.00') :-
	format(atom(Atom), '~2:f', [1000.0]).


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
