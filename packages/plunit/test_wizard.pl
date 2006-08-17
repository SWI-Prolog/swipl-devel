:- module(test_wizard,
	  [ make_tests/3,		% +Module, +File, +Out
	    make_test/3			% +Callable, -Module, -Test
	  ]).
:- use_module(library(time)).
:- use_module(library(lists)).
:- use_module(library(listing)).
:- use_module(library(readutil)).

/** <module> Test Generation Wizard

Tasks

	* Accumulate user queries
	* Suggest tests from user queries
*/

setting(max_time(5)).


		 /*******************************
		 *	 UNIT GENERATION	*
		 *******************************/

%%	make_tests(+Module, +File, +Out) is det.
%
%	Create tests from queries stored in File and write the tests for
%	Module to the stream Out.

make_tests(Module, File, Out) :-
	read_file_to_terms(File, Queries, []),
	findall(Test, (   member(Q, Queries),
			  make_test(Q, Module, Test)), Tests),
	(   Tests == []
	->  true
	;   format(Out, ':- begin_tests(~q).~n~n', [Module]),
	    maplist(portray_clause(Out), Tests),
	    format(Out, '~n:- end_tests(~q).~n', [Module])
	).


		 /*******************************
		 *	 TEST GENERATION	*
		 *******************************/

%%	make_test(+Query:callable, -Module, -Test:term) is det.
%
%	Generate a test from a query. Test   is  returned as a clause of
%	test/1  or  test/2  to  be   inserted  between  begin_tests  and
%	end_tests.

make_test(Query0, Module, (test(Name, Options) :- Query)) :-
	find_test_module(Query0, Module, Query),
	pred_name(Query, Name),
	setting(max_time(Max)),
	test_result(Module:Query, Max, Options).
	
%%	find_test_module(+QuerySpec, ?Module, -Query).
%
%	Find module to test from a query. Note that it is very common
%	for toplevel usage to rely on SWI-Prolog's DWIM.
%	
%	@tbd	What if multiple modules match?  We can select the
%		local one or ask the user.

find_test_module(Var, _, _) :-
	var(Var), !, fail.
find_test_module(M:Query, M0, Query) :- !,
	M0 = M.
find_test_module(Query, M, Query) :-
	current_predicate(_, M:Query),
	\+ predicate_property(M:Query, imported_from(_M2)).

%%	pred_name(+Callable, -Name) is det.
%
%	Suggest a name for the test. In   the  plunit framework the name
%	needs not be unique, so we simply take the predicate name.

pred_name(Callable, Name) :-
	strip_module(Callable, _, Term),
	functor(Term, Name, _Arity).

%%	test_result(+Callable, +Maxtime, -Result) is det.
%
%	Try running goal and get meaningful results.  Results are:
%	
%		* true(Templ == Var)
%		* fail
%		* all(Templ == Bindings)
%		* throws(Error)
%		* timeout

test_result(Callable, Maxtime, Result) :-
	term_variables(Callable, Vars),
	make_template(Vars, Templ),
	catch(call_with_time_limit(Maxtime,
				   findall(Templ-Det,
					   call_test(Callable, Det),
					   Bindings)),
	      E, true),
	(   var(E)
	->  success(Bindings, Templ, Result)
	;   error(E, Result)
	).

%%	success(+Bindings, +Templ, -Result) is det.
%
%	Create test-results from non-error cases.

success([], _, [fail]) :- !.
success([[]-true],  _, []) :- !.
success([S1-true],  Templ, [ true(Templ == S1) ]) :- !.
success([[]-false], _, [ nondet ]) :- !.
success([S1-false], Templ, [ true(Templ == S1), nondet ]) :- !.
success(ListDet, Templ, [all(Templ == List)]) :-
	strip_det(ListDet, List).

strip_det([], []).
strip_det([H-_|T0], [H|T]) :-
	strip_det(T0, T).

%%	error(+ErrorTerm, -Result)

error(Error0, [throws(Error)]) :-
	generalise_error(Error0, Error).


generalise_error(error(Formal, _), error(Formal, _)) :- !.
generalise_error(Term, Term).


%%	make_template(+Vars, -Template) is det.
%
%	Make a nice looking template

make_template([], []) :- !.
make_template([One], One) :- !.
make_template([One, Two], One-Two) :- !.
make_template(List, Vars) :-
	Vars =.. [v|List].

%%	call_test(:Goal, -Det) is nondet.
%
%	True if Goal succeeded.  Det is unified to =true= if Goal left
%	no choicepoints and =false= otherwise.

call_test(Goal, Det) :-
	Goal,
	deterministic(Det).


		 /*******************************
		 *	     COLLECT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Collect toplevel queries if the Prolog flag log_query_file points to the
name of a writeable  file.  The  file   is  opened  in  append-mode  for
exclusive write to allow for concurrent   operation from multiple Prolog
systems using the same logfile.

The file is written in  UTF-8   encoding  and  using ignore_ops(true) to
ensure it can be read.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	user:message_hook/3.

user:message_hook(toplevel_goal(Goal0, Bindings), _Level, _Lines) :-
	open_query_log(Out),
	bind_vars(Bindings),
	clean_goal(Goal0, Goal),
	call_cleanup(format(Out, '~W.~n', [Goal, [ numbervars(true),
						   quoted(true),
						   ignore_ops(true)
						 ]]), close(Out)),
	fail.
	
clean_goal(Var, _) :-
	var(Var), !, fail.
clean_goal(user:Goal, Goal) :- !.
clean_goal(Goal, Goal).

bind_vars([]).
bind_vars([Name=Var|T]) :-
	Var = '$VAR'(Name),
	bind_vars(T).

open_query_log(Out) :-
	current_prolog_flag(log_query_file, File),
	exists_file(File), !,
	open(File, append, Out,
	     [ encoding(utf8),
	       lock(write)
	     ]).
open_query_log(Out) :-
	current_prolog_flag(log_query_file, File),
	access_file(File, write), !,
	open(File, write, Out,
	     [ encoding(utf8),
	       lock(write),
	       bom(true)
	     ]),
	format(Out,
	       '/* SWI-Prolog query log.  This file contains all syntactically\n   \
	           correct queries issued in this directory.  It is used by the\n   \
		   test wizard to generate unit tests.\n\
		*/~n~n', []).
	
	
	
