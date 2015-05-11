:- module(test_scan_options,
	  [ test_scan_options/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(terms)).
:- use_module(library(apply)).

/** <module> Test option processing

This test suite deals with  built-in   option  processing. Note that, in
default mode, SWI-Prolog option processing ignores unknown options.
*/

test_scan_options :-
	run_tests([ scan_options,
		    dict_option
		  ]).

:- begin_tests(scan_options, []).

test(process, End == 1) :-
	numbervars(x(_,X,X), 0, End, [singletons(true)]).
test(implicit_true, End == 1) :-
	numbervars(x(_,X,X), 0, End, [singletons]).
test(no_option, End == 2) :-
	numbervars(x(_,X,X), 0, End, [unlikely(true)]).
test(bad_value_type, error(type_error(bool, 42))) :-
	numbervars(x(_,X,X), 0, _, [singletons(42)]).
test(bad_type, error(domain_error(numbervar_option, unlikely))) :-
	numbervars(x(_,X,X), 0, _, [unlikely]).
test(bad_type, error(domain_error(numbervar_option, f(a,b)))) :-
	numbervars(x(_,X,X), 0, _, [f(a,b)]).
test(bad_type, error(domain_error(numbervar_option, 1.3))) :-
	numbervars(x(_,X,X), 0, _, [1.3]).
test(instantiation, error(instantiation_error)) :-
	numbervars(x(_,X,X), 0, _, [_]).
test(instantiation, error(instantiation_error)) :-
	numbervars(x(_,X,X), 0, _, [singletons(true)|_]).
test(instantiation, error(type_error(list,1))) :-
	numbervars(x(_,X,X), 0, _, [singletons(true)|1]).

:- end_tests(scan_options).

:- begin_tests(dict_option, []).

test(process, End == 1) :-
	numbervars(x(_,X,X), 0, End, _{singletons:true}).
test(bad_value_type, error(type_error(bool, 42))) :-
	numbervars(x(_,X,X), 0, _, _{singletons:42}).
test(no_option, End == 2) :-
	numbervars(x(_,X,X), 0, End, _{unlikely:true}).

:- end_tests(dict_option).
