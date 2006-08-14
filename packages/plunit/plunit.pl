/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(plunit,
	  [ set_test_options/1,		% +Options
	    begin_tests/1,		% +Name
	    end_tests/1,		% +Name
	    run_tests/0,		% Run all tests
	    run_tests/1			% Run named test-set
	  ]).
:- use_module(library(lists)).
:- use_module(library(option)).

:- initialization
   set_prolog_flag(test_options, []).

%%	set_test_options(+Options)
%
%	Specifies how to deal with test suites.  Defined options are:
%	
%		* load(+Load)
%		Whether or not the tests must be loaded.  Values are
%		=never=, =always=, =normal= (only if not optimised)
%		
%		* run(+When)
%		When the tests are run.  Values are =manual= or =make=.
%		
%	@tbd	Verify types	

set_test_options(Options) :-
	set_prolog_flag(test_options, Options).

%%	loading_tests
%
%	True if tests must be loaded.

loading_tests :-
	current_prolog_flag(test_options, Options),
	option(load(Load), Options, normal),
	(   Load == always
	->  true
	;   Load == normal,
	    \+ current_prolog_flag(optimise, true)
	).

		 /*******************************
		 *	      MODULE		*
		 *******************************/

:- dynamic
	loading_unit/3,			% Unit, Module, OldSource
	current_unit/4.			% Unit, Module, Context, Options
	
%%	begin_tests(+UnitName:atom) is det.
%%	begin_tests(+UnitName:atom, Options) is det.
%
%	Start a test-unit. UnitName is the  name   of  the test set. the
%	unit is ended by :- end_tests(UnitName).

begin_tests(Unit) :-
	begin_tests(Unit, []).

begin_tests(Unit, Options) :-
	unit_module(Unit, Name),
	(   current_module(Name),
	    \+ current_unit(_, Name, _, _)
	->  throw(error(permission_error(create, plunit, Unit),
			'Existing module'))
	;   source_location(File, Line),
	    begin_tests(Unit, Name, File:Line, Options)
	).

begin_tests(Unit, Name, File:Line, Options) :-
	'$set_source_module'(Context, Context),
	Supers = [Context],
	(   current_unit(Unit, Name, Supers, Options)
	->  true
	;   retractall(current_unit(Unit, Name, _, _)),
	    assert(current_unit(Unit, Name, Supers, Options)),
	    set_import_modules(Name, Supers)
	),
	'$set_source_module'(Old, Name),
	'$declare_module'(Name, File, Line),
	discontiguous(Name:'unit test'/4),
	asserta(loading_unit(Unit, Name, Old)).

set_import_modules(Module, Imports) :-
	findall(I, import_module(Module, I), IL),
	forall(member(I, IL), delete_import_module(Module, I)),
	forall(member(I, Imports), add_import_module(Module, I, end)).

%%	end_tests(+Name) is det.
%
%	Close a unit-test module.
%	
%	@tbd	Run tests/clean module?
%	@tbd	End of file?

end_tests(Unit) :-
	loading_unit(StartUnit, _, _), !,
	(   Unit == StartUnit
	->  once(retract(loading_unit(StartUnit, _, Old))),
	    '$set_source_module'(_, Old)
	;   throw(error(context_error(plunit_close(Unit, StartUnit)), _))
	).
end_tests(Unit) :-
	throw(error(context_error(plunit_close(Unit, -)), _)).

unit_module(Name, Module) :-
	atom_concat('plunit_', Name, Module).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

%%	expand_test(+Name, +Options, +Body, -Clause) is det.
%
%	@tbd	Verify options.

expand_test(Name, Options, Body, 'unit test'(Name, Line, Options, Body)) :-
	source_location(_File, Line).

%%	expand(+Term, -Clauses) is semidet.

expand(end_of_file, _) :-
	loading_unit(Unit, _, _), !,
	end_tests(Unit),		% warn?
	fail.
expand(_Term, []) :-
	\+ loading_tests.
expand((test(Name) :- Body), Clauses) :- !,
	expand_test(Name, [], Body, Clauses).
expand((test(Name, Options) :- Body), Clauses) :- !,
	expand_test(Name, Options, Body, Clauses).

:- multifile
	user:term_expansion/2.

user:term_expansion(Term, Expanded) :-
	loading_unit(_, _, _),
	expand(Term, Expanded).


		 /*******************************
		 *	  RUNNING TOPLEVEL	*
		 *******************************/

:- dynamic
	passed/5,			% Unit, Test, Line, Det, Time
	failed/4,			% Unit, Test, Line, Reason
	blocked/4.			% Unit, Test, Line, Reason

%%	run_tests is semidet.
%%	run_tests(+TestSet) is semidet.

run_tests :-
	cleanup,
	forall(current_test_set(Set),
	       run_unit(Set)),
	report.

run_tests(Set) :-
	cleanup,
	run_unit(Set),
	report.

run_unit([]) :- !.
run_unit([H|T]) :- !,
	run_unit(H),
	run_unit(T).
run_unit(Spec) :-
	print_message(informational, plunit(begin(Spec))),
	unit_from_spec(Spec, Unit, Tests, Module, _UnitOptions),
	forall((Module:'unit test'(Name, Line, Options, Body),
	       matching_test(Name, Tests)),
	       run_test(Unit, Name, Line, Options, Body)),
	print_message(informational, plunit(end(Spec))).

unit_from_spec(Unit, Unit, _, Module, Options) :-
	atom(Unit), !,
	current_unit(Unit, Module, _Supers, Options).
unit_from_spec(Unit:Tests, Unit, Tests, Module, Options) :-
	atom(Unit), !,
	current_unit(Unit, Module, _Supers, Options).

matching_test(X, X) :- !.
matching_test(Name, Set) :-
	is_list(Set),
	memberchk(Name, Set).

cleanup :-
	retractall(passed(_, _, _, _, _)),
	retractall(failed(_, _, _, _)),
	retractall(blocked(_, _, _, _)).


		 /*******************************
		 *	   RUNNING A TEST	*
		 *******************************/

%%	run_test(+Unit, +Name, +Line, +Options, +Body) is det.

run_test(Unit, Name, Line, Options, _Body) :-
	option(blocked(Reason), Options, Default),
	Reason \== Default, !,				% Blocked test
	assert(blocked(Unit, Name, Line, Reason)).
run_test(Unit, Name, Line, Options, Body) :-
	option(all(Answer), Options), !,		% all(Bindings)
	nondet_test(all(Answer), Unit, Name, Line, Options, Body).
run_test(Unit, Name, Line, Options, Body) :-
	option(set(Answer), Options), !,		% set(Bindings)
	nondet_test(set(Answer), Unit, Name, Line, Options, Body).
run_test(Unit, Name, Line, Options, Body) :-
	option(fail, Options), !,			% fail
	unit_module(Unit, Module),
	setup(Module, Options), !,
	statistics(cputime, T0),
	(   catch(Module:Body, E, true)
	->  (   var(E)
	    ->	statistics(cputime, T1),
		Time is T1 - T0,
		failure(Unit, Name, Line, Time),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E),
		cleanup(Module, Options)
	    )
	;   statistics(cputime, T1),
	    Time is T1 - T0,
	    success(Unit, Name, Line, true, Time),
	    cleanup(Module, Options)
	).
run_test(Unit, Name, Line, Options, Body) :-
	option(true(Cmp), Options),
	unit_module(Unit, Module),
	setup(Module, Options), !,			% true(Binding)
	statistics(cputime, T0),
	(   catch(call_test(Module:Body, Det), E, true)
	->  (   var(E)
	    ->	statistics(cputime, T1),
		Time is T1 - T0,
		(   catch(Cmp, _, fail)			% tbd: error
		->  success(Unit, Name, Line, Det, Time)
		;   failure(Unit, Name, Line, wrong_answer)
		),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E),
		cleanup(Module, Options)
	    )
	;   failure(Unit, Name, Line, failed),
	    cleanup(Module, Options)
	).
run_test(Unit, Name, Line, Options, Body) :-
	unit_module(Unit, Module),
	setup(Module, Options), !,			% true
	statistics(cputime, T0),
	(   catch(call_test(Module:Body, Det), E, true)
	->  (   var(E)
	    ->	statistics(cputime, T1),
		Time is T1 - T0,
		success(Unit, Name, Line, Det, Time),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E),
		cleanup(Module, Options)
	    )
	;   failure(Unit, Name, Line, failed),
	    cleanup(Module, Options)
	).
run_test(_Unit, _Name, _Line, _Options, _Body).
	
%%	non_det_test(+Expected, +Unit, +Name, +Line, +Options, +Body)
%
%	Run tests on non-deterministic predicates.

nondet_test(Expected, Unit, Name, Line, Options, Body) :-
	unit_module(Unit, Module),
	setup(Module, Options), !,
	result_vars(Expected, Vars),
	statistics(cputime, T0),
	(   catch(findall(Vars, Module:Body, Bindings), E, true)
	->  (   var(E)
	    ->	statistics(cputime, T1),
		Time is T1 - T0,
	        (   nondet_compare(Expected, Bindings, Unit, Name, Line)
		->  success(Unit, Name, Line, true, Time)
		;   failure(Unit, Name, Line, wrong_answer)
		),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E),
		cleanup(Module, Options)
	    )
	).

%%	result_vars(+Expected, -Vars) is det.
%	
%	Create a term v(V1, ...) containing all variables at the left
%	side of the comparison operator on Expected.

result_vars(Expected, Vars) :-
	arg(1, Expected, CmpOp),
	arg(1, CmpOp, Vars).

%%	nondet_compare(+Expected, +Bindings, +Unit, +Name, +Line) is semidet.
%
%	Compare list/set results for non-deterministic predicates.
%	
%	@tbd	Properly report errors
%	@bug	Sort should deal with equivalence on the comparison
%		operator.

nondet_compare(all(Cmp), Bindings, _Unit, _Name, _Line) :-
	cmp(Cmp, _Vars, Op, Values),
	cmp_list(Values, Bindings, Op).
nondet_compare(set(Cmp), Bindings0, _Unit, _Name, _Line) :-
	cmp(Cmp, _Vars, Op, Values0),
	sort(Bindings0, Bindings),
	sort(Values0, Values),
	cmp_list(Values, Bindings, Op).

cmp_list([], [], _Op).
cmp_list([E0|ET], [V0|VT], Op) :-
	call(Op, E0, V0),
	cmp_list(ET, VT, Op).

%%	cmp(+CmpTerm, -Left, -Op, -Right) is det.

cmp(Var  == Value, Var,  ==, Value).
cmp(Var =:= Value, Var, =:=, Value).
cmp(Var =@= Value, Var, =@=, Value).
cmp(Var  =  Value, Var,  =,  Value).


%%	call_test(:Goal, -Det) is nondet.
%
%	True if Goal succeeded.  Det is unified to =true= if Goal left
%	no choicepoints and =false= otherwise.

call_test(Goal, Det) :-
	Goal,
	deterministic(Det).

%%	setup(+Module, +Options) is semidet.
%
%	Call the setup handler and  fail  if   it  cannot  run  for some
%	reason. The condition handler is  similar,   but  failing is not
%	considered an error.

setup(Module, Options) :-
	option(setup(Setup), Options, true),
	(   catch(Module:Setup, E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, E), 	% TBD
		fail
	    )
	;   print_message(error, goal_failed(Setup)),
	    fail
	).
setup(Module, Options) :-
	option(condition(Setup), Options, true),
	(   catch(Module:Setup, E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, E), 	% TBD
		fail
	    )
	;   fail
	).

%%	cleanup(+Module, +Options) is det.
%
%	Call the cleanup handler and succeed,   regardless of any errors
%	or failure without notice.

cleanup(Module, Options) :-
	option(cleanup(Cleanup), Options, true),
	ignore(catch(Module:Cleanup, _, true)).


success(Unit, Name, Line, Det, Time) :-
	assert(passed(Unit, Name, Line, Det, Time)),
	(   Det == true
	->  put(.)
	;   unit_file(Unit, File),
	    print_message(warning, plunit(nondet(File, Line, Name)))
	),
	flush_output.

failure(Unit, Name, Line, E) :-
	assert(failed(Unit, Name, Line, E)),
	report_failure(Unit, Name, Line, E).


		 /*******************************
		 *	      REPORTING		*
		 *******************************/

%%	report is semidet.
%
%	True if there are no errors.  If errors were encountered, report
%	them to current output and fail.

report :-
	report_blocked,
	report_failed.

report_blocked :-
	predicate_property(blocked(_,_,_,_), number_of_clauses(N)),
	N > 0,
	print_message(informational, plunit(blocked(N))),
	(   blocked(Unit, Name, Line, Reason),
	    unit_file(Unit, File),
	    print_message(informational,
			  plunit(blocked(File:Line, Name, Reason))),
	    fail ; true
	).
report_blocked.

report_failed :-
	predicate_property(failed(_,_,_,_), number_of_clauses(N)),
	N > 0, !,
	print_message(informational, plunit(failed(N))),
	fail.
report_failed :-
	print_message(informational, plunit(failed(0))).

report_failure(Unit, Name, Line, Error) :-
	print_message(error, failed(Unit, Name, Line, Error)).


		 /*******************************
		 *	       INFO		*
		 *******************************/

%%	current_test_set(?Unit) is nondet.
%
%	True if Unit is a currently loaded test-set.

current_test_set(Unit) :-
	current_unit(Unit, _Module, _Context, _Options).

%%	unit_file(+Unit, -File) is det.
%%	unit_file(-Unit, +File) is nondet.

unit_file(Unit, File) :-
	current_unit(Unit, Module, _Context, _Options),
	current_module(Module, File).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(context_error(plunit_close(Name, -)), _)) -->
	[ 'PL-Unit: cannot close unit ~w: no open unit'-[Name] ].
prolog:message(error(context_error(plunit_close(Name, Start)), _)) -->
	[ 'PL-Unit: cannot close unit ~w: current unit is ~w'-[Name, Start] ].
prolog:message(plunit(nondet(File, Line, Name))) -->
	[ '~w:~d: PL-Unit: Test ~w: Test succeeded with choicepoint'-
	  [File, Line, Name] ].
					% Unit start/end
prolog:message(plunit(begin(Unit))) -->
	[ 'PL-Unit: ~w '-[Unit], flush ].
prolog:message(plunit(end(_Unit))) -->
	[ at_same_line, ' done' ].
					% Blocked tests
prolog:message(plunit(blocked(N))) -->
	[ '~D tests where blocked'-[N] ].
prolog:message(plunit(blocked(Pos, Name, Reason))) -->
	[ '  ~w: test ~w: ~w'-[Pos, Name, Reason] ].
					% fail/success
prolog:message(plunit(failed(0))) --> !,
	[ 'All tests passed' ].
prolog:message(plunit(failed(N))) -->
	[ '~D tests failed'-[N] ].
prolog:message(plunit(failed(Unit, Name, Line, Error))) -->
	{ unit_file(Unit, File) },
	[ '~w:~w: test ~w: ~p~n'-[File, Line, Name, Error] ].
