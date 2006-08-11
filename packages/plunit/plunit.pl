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
%		When the tests are run.  Values are =never=, =always=
%		or =make=.
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
	current_unit/3.			% Unit, Module, Context
	
%%	begin_tests(+UnitName:atom) is det.
%
%	Start a test-unit. UnitName is the  name   of  the test set. the
%	unit is ended by :- end_tests(UnitName).

begin_tests(Unit) :-
	unit_module(Unit, Name),
	(   current_module(Name),
	    \+ current_unit(_, Name, _)
	->  throw(error(permission_error(create, plunit, Unit),
			'Existing module'))
	;   source_location(File, Line),
	    begin_tests(Unit, Name, File:Line)
	).

begin_tests(Unit, Name, File:Line) :-
	'$set_source_module'(Context, Context),
	Supers = [Context],
	(   current_unit(Unit, Name, Supers)
	->  true
	;   retractall(current_unit(Unit, Name, _)),
	    assert(current_unit(Unit, Name, Supers)),
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

expand_test(Name, Options, Body, 'unit test'(Name, Line, Options, Body)) :-
	source_location(_File, Line).

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

run_unit(Unit) :-
	format('Unit ~w: ', [Unit]),
	current_unit(Unit, Module, _),
	forall(Module:'unit test'(Name, Line, Options, Body),
	       run_test(Unit, Name, Line, Options, Body)),
	format(' done~n').

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
	Reason \== Default, !,		% Blocked test
	assert(blocked(Unit, Name, Line, Reason)).
run_test(Unit, Name, Line, Options, Body) :-
	unit_module(Unit, Module),
	setup(Module, Options), !,
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
	
call_test(Goal, Det) :-
	Goal,
	deterministic(Det).

%%	setup(+Module, +Options) is semidet.
%
%	Call the setup handler and  fail  if   it  cannot  run  for some
%	reason.

setup(Module, Options) :-
	option(setup(Setup), Options, true),
	(   catch(Module:Setup, E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, E), 	% TBD
		fail
	    )
	;   print_message(error, goal_failed(Setup))
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
	;   put(!)
	),
	flush_output.

failure(Unit, Name, Line, E) :-
	assert(failed(Unit, Name, Line, E)),
	report_failure(Unit, Name, Line, E).


		 /*******************************
		 *	      REPORTING		*
		 *******************************/

%%	report is semidet.
%%	report(+Out:stream) is semidet.
%
%	True if there are no errors.  If errors were encountered, report
%	them to current output and fail.

report :-
	report(current_output).

report(Out) :-
	report_blocked(Out),
	report_failed(Out).

report_blocked(Out) :-
	blocked(_, _, _, _), !,
	format(Out, '~nThe following tests are blocked:~n', []),
	(   blocked(Unit, Name, Line, Reason),
	    unit_file(Unit, File),
	    format(Out, '    ~w:~d: test ~w: ~w~n',
		   [File, Line, Name, Reason]),
	    fail ; true
	).
report_blocked(_).

report_failed(Out) :-
	predicate_property(failed(_,_,_,_), number_of_clauses(N)),
	N > 0, !,
	format(Out, '~nERROR: ~D tests FAILED~n', [N]),
	fail.
report_failed(Out) :-
	format(Out, '~nAll tests passed~n', []).

report_failure(Unit, Name, Line, Error) :-
	report_failure(current_output, Unit, Name, Line, Error).

report_failure(Out, Unit, Name, Line, Error) :-
	unit_file(Unit, File),
	format(Out, '    ~w:~w: test ~w: ~p~n',
	       [File, Line, Name, Error]).


		 /*******************************
		 *	       INFO		*
		 *******************************/

%%	current_test_set(?Unit) is nondet.
%
%	True if Unit is a currently loaded test-set.

current_test_set(Unit) :-
	current_unit(Unit, _Module, _Context).

%%	unit_file(+Unit, -File) is det.
%%	unit_file(-Unit, +File) is nondet.

unit_file(Unit, File) :-
	current_unit(Unit, Module, _Context),
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
