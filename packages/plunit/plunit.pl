/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006-2007, University of Amsterdam

    This file is covered by the `The Artistic License', also in use by
    Perl.  See http://www.perl.com/pub/a/language/misc/Artistic.html
*/

:- module(plunit,
	  [ set_test_options/1,		% +Options
	    begin_tests/1,		% +Name
	    begin_tests/2,		% +Name, +Options
	    end_tests/1,		% +Name
	    run_tests/0,		% Run all tests
	    run_tests/1,		% Run named test-set
	    load_test_files/1		% +Options
	  ]).

/** <module> Unit Testing

Unit testing environment for SWI-Prolog and   SICStus Prolog. For usage,
please visit http://www.swi-prolog.org/packages/plunit.html.

@author		Jan Wielemaker
@license	artistic
*/

		 /*******************************
		 *    CONDITIONAL COMPILATION	*
		 *******************************/

:- discontiguous
	user:term_expansion/2.

:- dynamic
	include_code/1.

including :-
	include_code(X), !,
	X == true.
including.

if_expansion((:- if(G)), []) :-
	(   including
	->  (   catch(G, E, (print_message(error, E), fail))
	    ->  asserta(include_code(true))
	    ;   asserta(include_code(false))
	    )
	;   asserta(include_code(else_false))
	).
if_expansion((:- else), []) :-
	(   retract(include_code(X))
	->  (   X == true
	    ->  X2 = false 
	    ;   X == false
	    ->	X2 = true
	    ;	X2 = X
	    ),
	    asserta(include_code(X2))
	;   throw(error(context_error(no_if), _))
	).
if_expansion((:- endif), []) :-
	retract(include_code(_)), !.

if_expansion(_, []) :-
	\+ including.
	    
user:term_expansion(In, Out) :-
	prolog_load_context(module, plunit),
	if_expansion(In, Out).

swi     :- catch(current_prolog_flag(dialect, swi), _, fail).
sicstus :- catch(current_prolog_flag(system_type, _), _, fail).


:- if(swi).
:- set_prolog_flag(generate_debug_info, false).
:- use_module(library(option)).
:- use_module(library(quintus), [subsumes_chk/2]).

current_test_flag(Name, Value) :-
	current_prolog_flag(Name, Value).

set_test_flag(Name, Value) :-
	set_prolog_flag(Name, Value).
:- endif.

:- if(sicstus).
:- use_module(swi).			% SWI-Compatibility
:- use_module(library(terms)).
:- op(700, xfx, =@=).

'$set_source_module'(_, _).

%%	current_test_flag(?Name, ?Value) is nondet.
%
%	Query  flags  that  control  the    testing   process.  Emulates
%	SWI-Prologs flags.

:- dynamic test_flag/2.	% Name, Val

current_test_flag(optimise, Val) :-
	current_prolog_flag(compiling, Compiling),
	(   Compiling == debugcode ; true % TBD: Proper test
	->  Val = false
	;   Val = true
	).
current_test_flag(Name, Val) :-
	test_flag(Name, Val).


%%	set_test_flag(+Name, +Value) is det.

set_test_flag(Name, Val) :-
	var(Name), !,
	throw(error(instantiation_error, set_test_flag(Name,Val))).
set_test_flag( Name, Val ) :-
	retractall(test_flag(Name,_)),
	asserta(test_flag(Name, Val)).

:- endif.

		 /*******************************
		 *	      IMPORTS		*
		 *******************************/

:- use_module(library(lists)).

:- initialization
   (   current_test_flag(test_options, _)
   ->  true
   ;   set_test_flag(test_options,
		     [ run(make)	% run tests on make/0
		     ])
   ).

%%	set_test_options(+Options)
%
%	Specifies how to deal with test suites.  Defined options are:
%	
%		* load(+Load)
%		Whether or not the tests must be loaded.  Values are
%		=never=, =always=, =normal= (only if not optimised)
%		
%		* run(+When)
%		When the tests are run.  Values are =manual=, =make=
%		or make(all).
%		
%	@tbd	Verify types	

set_test_options(Options) :-
	set_test_flag(test_options, Options).

%%	loading_tests
%
%	True if tests must be loaded.

loading_tests :-
	current_test_flag(test_options, Options),
	option(load(Load), Options, normal),
	(   Load == always
	->  true
	;   Load == normal,
	    \+ current_test_flag(optimise, true)
	).

		 /*******************************
		 *	      MODULE		*
		 *******************************/

:- dynamic
	loading_unit/4,			% Unit, Module, File, OldSource
	current_unit/4,			% Unit, Module, Context, Options
	test_file_for/2.		% ?TestFile, ?PrologFile
	
%%	begin_tests(+UnitName:atom) is det.
%%	begin_tests(+UnitName:atom, Options) is det.
%
%	Start a test-unit. UnitName is the  name   of  the test set. the
%	unit is ended by :- end_tests(UnitName).

begin_tests(Unit) :-
	begin_tests(Unit, []).

begin_tests(Unit, Options) :-
	valid_options(Options, test_set_option),
	make_unit_module(Unit, Name),
	source_location(File, Line),
	begin_tests(Unit, Name, File:Line, Options).

:- if(swi).
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
	'$set_predicate_attribute'(Name:'unit test'/4, trace, 0),
	discontiguous(Name:'unit body'/2),
	asserta(loading_unit(Unit, Name, File, Old)).

set_import_modules(Module, Imports) :-
	findall(I, import_module(Module, I), IL),
	forall(member(I, IL), delete_import_module(Module, I)),
	forall(member(I, Imports), add_import_module(Module, I, end)).

:- else.

% we cannot use discontiguous as a goal in SICStus Prolog.

user:term_expansion((:- begin_tests(Set)),
		    [ (:- begin_tests(Set)),
		      (:- discontiguous('unit body'/2)),
		      (:- discontiguous('unit test'/4))
		    ]).

begin_tests(Unit, Name, File:_Line, Options) :-
	(   current_unit(Unit, Name, Supers, Options)
	->  true
	;   retractall(current_unit(Unit, Name, _, _)),
	    assert(current_unit(Unit, Name, Supers, Options))
	),
	asserta(loading_unit(Unit, Name, File, -)).

:- endif.

%%	end_tests(+Name) is det.
%
%	Close a unit-test module.
%	
%	@tbd	Run tests/clean module?
%	@tbd	End of file?

end_tests(Unit) :-
	loading_unit(StartUnit, _, _, _), !,
	(   Unit == StartUnit
	->  once(retract(loading_unit(StartUnit, _, _, Old))),
	    '$set_source_module'(_, Old)
	;   throw(error(context_error(plunit_close(Unit, StartUnit)), _))
	).
end_tests(Unit) :-
	throw(error(context_error(plunit_close(Unit, -)), _)).

%%	make_unit_module(+Name, -ModuleName) is det.
%%	unit_module(+Name, -ModuleName) is det.

:- if(swi).

unit_module(Unit, Module) :-
	atom_concat('plunit_', Unit, Module).

make_unit_module(Unit, Module) :-
	unit_module(Unit, Module),
	(   current_module(Module),
	    \+ current_unit(_, Module, _, _)
	->  throw(error(permission_error(create, plunit, Unit),
			'Existing module'))
	;  true
	).

:- else.

:- dynamic
	unit_module_store/2.

unit_module(Unit, Module) :-
	unit_module_store(Unit, Module), !.

make_unit_module(Unit, Module) :-
	prolog_load_context(module, Module),
	assert(unit_module_store(Unit, Module)).

:- endif.

		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

%%	expand_test(+Name, +Options, +Body, -Clause) is det.
%
%	Expand test(Name, Options) :-  Body  into   a  clause  for
%	'unit test'/4 and 'unit body'/2.

expand_test(Name, Options0, Body,
	    [ 'unit test'(Name, Line, Options, Module:'unit body'(Id, Vars)),
	      ('unit body'(Id, Vars) :- !, Body)
	    ]) :-
	source_location(_File, Line),
	prolog_load_context(module, Module),
	concat_atom([Name, '@line ', Line], Id),
	term_variables(Body, VarList),
	Vars =.. [vars|VarList],
	(   is_list(Options0)		% allow for single option without list
	->  Options1 = Options0
	;   Options1 = [Options0]
	),
	maplist(expand_option, Options1, Options),
	valid_options(Options, test_option).

expand_option(Var, _) :-
	var(Var), !,
	throw(error(instantiation_error)).
expand_option(A == B, true(A==B)) :- !.
expand_option(A = B, true(A=B)) :- !.
expand_option(A =@= B, true(A=@=B)) :- !.
expand_option(A =:= B, true(A=:=B)) :- !.
expand_option(O, O).


%%	expand(+Term, -Clauses) is semidet.

expand(end_of_file, _) :-
	loading_unit(Unit, _, _, _), !,
	end_tests(Unit),		% warn?
	fail.
expand(_Term, []) :-
	\+ loading_tests.
expand((test(Name) :- Body), Clauses) :- !,
	expand_test(Name, [], Body, Clauses).
expand((test(Name, Options) :- Body), Clauses) :- !,
	expand_test(Name, Options, Body, Clauses).

:- if(swi).
:- multifile
	user:term_expansion/2.
:- endif.

user:term_expansion(Term, Expanded) :-
	(   loading_unit(_, _, File, _)
	->  source_location(File, _),
	    expand(Term, Expanded)
	).


		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

:- if(swi).
:- use_module(library(error)).
:- else.
must_be(list, X) :- !,
	(   is_list(X)
	->  true
	;   is_not(list, X)
	).
must_be(Type, X) :-
	(   call(Type, X)
	->  true
	;   is_not(Type, X)
	).

is_not(Type, X) :-
	(   ground(X)
	->  throw(error(type_error(Type, X), _))
	;   throw(error(instantiation_error, _))
	).
:- endif.

%%	valid_options(+Options, :Pred) is det.
%
%	Verify Options to be a list of valid options according to
%	Pred.
%
%	@throws	=type_error= or =instantiation_error=.

valid_options(Options, Pred) :-
	must_be(list, Options),
	verify_options(Options, Pred).

verify_options([], _).
verify_options([H|T], Pred) :-
	(   call(Pred, H)
	->  verify_options(T, Pred)
	;   throw(error(domain_error(Pred, H), _))
	).


%%	test_option(+Option) is semidet.
%
%	True if Option is a valid option for test(Name, Options).

test_option(Option) :-
	test_set_option(Option), !.
test_option(true(_)).
test_option(fail).
test_option(true).
test_option(throws(_)).
test_option(error(_)).
test_option(all(_)).
test_option(set(_)).
test_option(nondet).

%%	test_option(+Option) is semidet.
%
%	True if Option is a valid option for :- begin_tests(Name,
%	Options).

test_set_option(blocked(X)) :-
	must_be(ground, X).
test_set_option(condition(X)) :-
	must_be(callable, X).
test_set_option(setup(X)) :-
	must_be(callable, X).
test_set_option(cleanup(X)) :-
	must_be(callable, X).


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
	unit_from_spec(Spec, Unit, Tests, Module, UnitOptions),
	(   option(blocked(Reason), UnitOptions)
	->  info(plunit(blocked(unit(Unit, Reason))))
	;   setup(Module, UnitOptions)
	->  info(plunit(begin(Spec))),
	    forall((Module:'unit test'(Name, Line, Options, Body),
		    matching_test(Name, Tests)),
		   run_test(Unit, Name, Line, Options, Body)),
	    info(plunit(end(Spec))),
	    (	message_level(silent)
	    ->	true
	    ;	format(user_error, '~N', [])
	    ),
	    cleanup(Module, UnitOptions)
	;   true
	).

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


%%	run_tests_in_files(+Files:list)	is det.
%
%	Run all test-units that appear in the given Files.

run_tests_in_files(Files) :-
	findall(Unit, unit_in_files(Files, Unit), Units),
	run_tests(Units).

unit_in_files(Files, Unit) :-
	is_list(Files), !,
	member(F, Files),
	absolute_file_name(F, Source,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ]),
	unit_file(Unit, Source).


		 /*******************************
		 *	   HOOKING MAKE/0	*
		 *******************************/

%%	make_run_tests(+Files)
%
%	Called indirectly from make/0 after Files have been reloaded.

make_run_tests(Files) :-
	current_test_flag(test_options, Options),
	option(run(When), Options, manual),
	(   When == make
	->  run_tests_in_files(Files)
	;   When == make(all)
	->  run_tests
	;   true
	).


		 /*******************************
		 *	   RUNNING A TEST	*
		 *******************************/

%%	run_test(+Unit, +Name, +Line, +Options, +Body) is det.

run_test(Unit, Name, Line, Options, _Body) :-
	option(blocked(Reason), Options), !,
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
	statistics(runtime, [T0,_]),
	(   catch(Module:Body, E, true)
	->  (   var(E)
	    ->	statistics(runtime, [T1,_]),
		Time is (T1 - T0)/1000.0,
		failure(Unit, Name, Line, succeeded(Time), Options),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E, Options),
		cleanup(Module, Options)
	    )
	;   statistics(runtime, [T1,_]),
	    Time is (T1 - T0)/1000.0,
	    success(Unit, Name, Line, true, Time, Options),
	    cleanup(Module, Options)
	).
run_test(Unit, Name, Line, Options, Body) :-
	option(true(Cmp), Options),
	unit_module(Unit, Module),
	setup(Module, Options), !,			% true(Binding)
	statistics(runtime, [T0,_]),
	(   catch(call_test(Module:Body, Det), E, true)
	->  (   var(E)
	    ->	statistics(runtime, [T1,_]),
		Time is (T1 - T0)/1000.0,
		(   catch(Cmp, _, fail)			% tbd: error
		->  success(Unit, Name, Line, Det, Time, Options)
		;   failure(Unit, Name, Line, wrong_answer(Cmp), Options)
		),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E, Options),
		cleanup(Module, Options)
	    )
	;   failure(Unit, Name, Line, failed, Options),
	    cleanup(Module, Options)
	).
run_test(Unit, Name, Line, Options, Body) :-
	(   option(throws(Expect), Options)
	->  true
	;   option(error(ErrorExpect), Options)
	->  Expect = error(ErrorExpect, _)
	),
	unit_module(Unit, Module),
	setup(Module, Options), !,			% true
	statistics(runtime, [T0,_]),
	(   catch(Module:Body, E, true)
	->  (   var(E)
	    ->	failure(Unit, Name, Line, no_exception, Options),
		cleanup(Module, Options)
	    ;	statistics(runtime, [T1,_]),
		Time is (T1 - T0)/1000.0,
		(   match_error(Expect, E)
		->  success(Unit, Name, Line, true, Time, Options)
		;   failure(Unit, Name, Line, wrong_error(Expect, E), Options)
		),
		cleanup(Module, Options)
	    )
	;   failure(Unit, Name, Line, failed, Options),
	    cleanup(Module, Options)
	).
run_test(Unit, Name, Line, Options, Body) :-
	unit_module(Unit, Module),
	setup(Module, Options), !,			% true
	statistics(runtime, [T0,_]),
	(   catch(call_test(Module:Body, Det), E, true)
	->  (   var(E)
	    ->	statistics(runtime, [T1,_]),
		Time is (T1 - T0)/1000.0,
		success(Unit, Name, Line, Det, Time, Options),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E, Options),
		cleanup(Module, Options)
	    )
	;   failure(Unit, Name, Line, failed, Options),
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
	statistics(runtime, [T0,_]),
	(   catch(findall(Vars, Module:Body, Bindings), E, true)
	->  (   var(E)
	    ->	statistics(runtime, [T1,_]),
		Time is (T1 - T0)/1000.0,
	        (   nondet_compare(Expected, Bindings, Unit, Name, Line)
		->  success(Unit, Name, Line, true, Time, Options)
		;   failure(Unit, Name, Line, wrong_answer, Options)
		),
		cleanup(Module, Options)
	    ;	failure(Unit, Name, Line, E, Options),
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
cmp(Var  =  Value, Var,  =,  Value).
:- if(swi).
cmp(Var =@= Value, Var, =@=, Value).
:- else.
:- if(sicstus).
cmp(Var =@= Value, Var, variant, Value). % variant/2 is the same =@=
:- endif.
:- endif.


%%	call_test(:Goal, -Det) is nondet.
%
%	True if Goal succeeded.  Det is unified to =true= if Goal left
%	no choicepoints and =false= otherwise.

:- if(swi).
call_test(Goal, Det) :-
	Goal,
	deterministic(Det).
:- else.
:- if(sicstus).
call_test(Goal, Det) :-
	statistics(choice, [Used0|_]),
	Goal,
	statistics(choice, [Used1|_]),
	(   Used1 =:= Used0
	->  Det = true
	;   Det = false
	).
:- else.
call_test(Goal, true) :-
	call(Goal).
:- endif.
:- endif.

%%	match_error(+Expected, +Received) is semidet.
%
%	True if the Received errors matches the expected error. Matching
%	is based on subsumes_chk/2.

match_error(Expect, Rec) :-
	subsumes_chk(Expect, Rec).

%%	setup(+Module, +Options) is semidet.
%
%	Call the setup handler and  fail  if   it  cannot  run  for some
%	reason. The condition handler is  similar,   but  failing is not
%	considered an error.

setup(Module, Options) :-
	option(setup(Setup), Options), !,
	(   catch(Module:Setup, E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, E), 	% TBD
		fail
	    )
	;   print_message(error, error(goal_failed(Setup), _)),
	    fail
	).
setup(Module, Options) :-
	option(condition(Setup), Options), !,
	(   catch(Module:Setup, E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, E), 	% TBD
		fail
	    )
	;   fail
	).
setup(_,_).

%%	cleanup(+Module, +Options) is det.
%
%	Call the cleanup handler and succeed,   regardless of any errors
%	or failure without notice.

cleanup(Module, Options) :-
	option(cleanup(Cleanup), Options, true),
	ignore(catch(Module:Cleanup, _, true)).


success(Unit, Name, Line, Det, Time, Options) :-
	assert(passed(Unit, Name, Line, Det, Time)),
	(   (   Det == true
	    ;	memberchk(nondet, Options)
	    )
	->  put_char(user_error, .)
	;   unit_file(Unit, File),
	    print_message(warning, plunit(nondet(File, Line, Name)))
	),
	flush_output(user_error).

failure(Unit, Name, Line, E, _Options) :-
	report_failure(Unit, Name, Line, E),
	assert_cyclic(failed(Unit, Name, Line, E)).

%%	assert_cyclic(+Term) is det.
%
%	Assert  a  possibly  cyclic  unit   clause.  Current  SWI-Prolog
%	assert/1 does not handle cyclic terms,  so we emulate this using
%	the recorded database.
%	
%	@tbd	Implement cycle-safe assert and remove this.

:- if(swi).
assert_cyclic(Term) :-
	acyclic_term(Term), !,
	assert(Term).
assert_cyclic(Term) :-
	Term =.. [Functor|Args],
	recorda(cyclic, Args, Id),
	functor(Term, _, Arity),
	length(NewArgs, Arity),
	Head =.. [Functor|NewArgs],
	assert((Head :- recorded(_, Var, Id), Var = NewArgs)).
:- else.
:- if(sicstus).
:- endif.
assert_cyclic(Term) :-
	assert(Term).
:- endif.


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
	info(plunit(blocked(N))),
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
	info(plunit(failed(N))),
	fail.
report_failed :-
	info(plunit(failed(0))).

report_failure(Unit, Name, Line, Error) :-
	print_message(error, plunit(failed(Unit, Name, Line, Error))).


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
unit_file(Unit, PlFile) :-
	nonvar(PlFile),
	test_file_for(TestFile, PlFile),
	current_module(Module, TestFile),
	current_unit(Unit, Module, _Context, _Options).


		 /*******************************
		 *	       FILES		*
		 *******************************/

%%	load_test_files(+Options) is det.
%
%	Load .plt test-files related to loaded source-files.

load_test_files(_Options) :-
	(   source_file(File),
	    file_name_extension(Base, Old, File),
	    Old \== plt,
	    file_name_extension(Base, plt, TestFile),
	    exists_file(TestFile),
	    (	test_file_for(TestFile, File)
	    ->	true
	    ;	load_files(TestFile,
			   [ if(changed),
			     imports([])
			   ]),
		asserta(test_file_for(TestFile, File))
	    ),
	    fail ; true
	).



		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

%%	info(+Term)
%
%	Runs print_message(Level, Term), where Level  is one of =silent=
%	or =informational= (default).

info(Term) :-
	message_level(Level),
	print_message(Level, Term).

message_level(Level) :-
	current_test_flag(test_options, Options),
	option(silent(Silent), Options, false),
	(   Silent == false
	->  Level = informational
	;   Level = silent
	).


message(error(context_error(plunit_close(Name, -)), _)) -->
	[ 'PL-Unit: cannot close unit ~w: no open unit'-[Name] ].
message(error(context_error(plunit_close(Name, Start)), _)) -->
	[ 'PL-Unit: cannot close unit ~w: current unit is ~w'-[Name, Start] ].
message(plunit(nondet(File, Line, Name))) -->
	[ '~w:~d: PL-Unit: Test ~w: Test succeeded with choicepoint'-
	  [File, Line, Name] ].
					% Unit start/end
:- if(swi).
message(plunit(begin(Unit))) -->
	[ 'PL-Unit: ~w '-[Unit], flush ].
message(plunit(end(_Unit))) -->
	[ at_same_line, ' done' ].
:- else.
message(plunit(begin(Unit))) -->
	[ 'PL-Unit: ~w '-[Unit]/*, flush-[]*/ ].
message(plunit(end(_Unit))) -->
	[ ' done'-[] ].
:- endif.
message(plunit(blocked(unit(Unit, Reason)))) -->
	[ 'PL-Unit: ~w blocked: ~w'-[Unit, Reason] ].

					% Blocked tests
message(plunit(blocked(N))) -->
	[ '~D tests where blocked'-[N] ].
message(plunit(blocked(Pos, Name, Reason))) -->
	[ '  ~w: test ~w: ~w'-[Pos, Name, Reason] ].

					% fail/success
message(plunit(failed(0))) --> !,
	[ 'All tests passed'-[] ].
message(plunit(failed(N))) -->
	[ '~D tests failed'-[N] ].
message(plunit(failed(Unit, Name, Line, Failure))) -->
       { unit_file(Unit, File) },
       [ '~w:~w: test ~w: '- [File, Line, Name] ],
       failure(Failure).


failure(succeeded(Time)) --> !,
	[ 'must fail but succeeded in ~2f seconds~n'-[Time] ].
failure(wrong_error(Expected, Error)) --> !,
	{ copy_term(Expected-Error, Ex-E),
	  numbervars(Ex-E, 0, _, [singletons(true)]),
	  write_options(OPS)
	},
	[ 'wrong error', nl, 
	  '    Expected: ~W'-[Ex, OPS], nl,
	  '    Got:      ~W'-[E, OPS], nl
	].
failure(wrong_answer(Cmp)) -->
	{ Cmp =.. [Op,Answer,Expected], !,
	  copy_term(Expected-Answer, Ex-A),
	  numbervars(Ex-A, 0, _, [singletons(true)]),
	  write_options(OPS)
	},
	[ 'wrong answer (compared using ~w)'-[Op], nl, 
	  '    Expected: ~W'-[Ex, OPS], nl,
	  '    Got:      ~W'-[A, OPS], nl
	].
failure(Why) -->
	[ '~p~n'-[Why] ].

write_options([ numbervars(true),
		quoted(true),
		portray(true),
		max_depth(10)
	      ]).

:- if(swi).

:- multifile
	prolog:message/3,
	user:message_hook/3.

prolog:message(Term) -->
	message(Term).

%	user:message_hook(+Term, +Kind, +Lines)

user:message_hook(make(done(Files)), _, _) :-
	make_run_tests(Files),
	fail.				% give other hooks a chance

:- endif.

:- if(sicstus).

user:generate_message_hook(Message) -->
	message(Message),
	[nl].				% SICStus requires nl at the end

%	user:message_hook(+Sevirity, +Message, +Lines) is semidet.
%
%	Redefine printing some messages. It appears   SICStus has no way
%	to get multiple messages at the same   line, so we roll our own.
%	As there is a lot pre-wired and   checked in the SICStus message
%	handling we cannot reuse the lines. Unless I miss something ...

user:message_hook(informational, plunit(begin(Unit)), _Lines) :-
	format(user_error, '% PL-Unit: ~w ', [Unit]),
	flush_output(user_error).
user:message_hook(informational, plunit(end(_Unit)), _Lines) :-
	format(user, ' done~n', []).

:- endif.
