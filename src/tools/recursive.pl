#!/home/jan/bin/swipl -q -g true -t main -s

:- use_module(library(debug)).
:- use_module(safe).

:- op(500, xfx, @).

:- multifile
	function/7,	% Name, Type, File, StartLine, EndLine, Word, Mark
	calls/4,	% Name, Callee, File, Line
	object/2,	% Object, Source
	static/2.	% Name, File

load :-
	expand_file_name('*.functions', FFiles),
	maplist(consult, FFiles),
	expand_file_name('*.tree', TFiles),
	maplist(consult, TFiles),
	consult('buildin-predicates').

:- dynamic
	caller/4,			% Func, File, Path, shift/gc
	only/1,
	report/1.

stop('PL_error').
stop('PL_warning').
stop('fatalError').
stop('sysError').
stop('errorWarning').
stop('tracePort').
stop('PL_raise_exception').
stop('__assert_fail').
stop('PL_malloc').
stop('allocHeap__LD').
stop('garbageCollect').
stop('ensureGlobalSpace').
stop('ensureLocalSpace').
stop('makeMoreStackSpace').
stop('ensureTrailSpace').
stop('outOfStack').
stop('outOfCore').
stop('abortProlog').
stop('isSuperModule').
stop('reachableModule').
stop('scanVisibleOperators').
stop('find_modules_with_def').
stop('PL_register_blob_type').
stop('Sdprintf').
stop('Svsprintf').
stop('Output_0').
stop('trapUndefined').
stop('callEventHook').
stop('printMessage').
stop('visibleProcedure').
stop('reperror').
stop('Sputcode').
stop('handleSignals').
stop('PL_get_text__LD').
stop('frameFinished').
stop('put_byte').
stop('Sfeof').
stop('export_pi').
stop('scanPriorityOperator').
stop('format_time').
stop('free_thread_info').
stop('loadXR__LD').
stop('loadPart').
stop('autoImport').
stop('reportStreamError').
stop('PL_unify_term').
					% maintenance
stop('checkData').
					% unsolvable
stop('callProlog').
stop('PL_next_solution').
					% mild problems
stop('when_condition').
stop('compilePattern').
stop('matchPattern').
stop('collectSiblingsNode').
stop('sumProfile').
stop('freeProfileNode').
stop('getUnknownModule').
					% real problems
stop_show('unify_with_occurs_check', lao).
stop_show('var_occurs_in',	     lao).
stop_show('ph1_is_acyclic',	     lao).
stop_show('ph2_is_acyclic',	     lao).
stop_show('ph_ground',		     lao).
stop_show('eval_expression',	     no_lao).
stop_show('copy_record',	     lao).
stop_show('scanAtomsRecord',	     lao).
stop_show('analyseVariables2',	     lao).
stop_show('compileArgument',	     lao).
stop_show('compileBody',	     no_lao).
stop_show('do_copy_term',	     no_lao).
stop_show('find_if_then_end',	     no_lao).
stop_show('ar_func_n',		     no_lao).
stop_show('termHashValue',	     no_lao).
stop_show('do_number_vars',	     lao).
stop_show('free_variables_loop',     lao).
stop_show('do_unify',		     lao).
stop_show('complex_term',	     no_lao).
stop_show('writeTerm',		     no_lao).
stop_show('decompileBody',	     no_lao).
stop_show('do_load_qlf_term',	     no_lao).
stop_show('do_save_qlf_term',	     no_lao).

stop_because(Function, _Problems) :-
	stop(Function), !.
stop_because(Function, Problems) :-
	stop_show(Function, Lao), !,
	Problem =.. [Lao,Function],
	memberchk(Problem, Problems).

recursive_predicate(Name/Arity, Stack, Problem) :-
	(   function(PredAtom, predicate, File, StartLine, EndLine, _, _),
	    atomic_list_concat([Name,Arity], /, PredAtom),
	    calls(Func, Callee, LocalFile, Line),
	    sub_atom(File, _, _, 0, LocalFile),
	    between(StartLine, EndLine, Line),
	    has_loop(Callee, LocalFile, [Callee,Func], Loop, Problems)
	;   predicate(Name, Arity, Func),
	    has_loop(Func, LocalFile, [Func], Loop, Problems)
	),
	reverse(Loop, Stack),
	close_list(Problems),
	Problems = [Problem].

has_loop(Function, _, Done, Done, Problems) :-
	stop_because(Function, Problems), !.
has_loop(Function, File, Done, Loop, Problems) :-
	calls(Function, Callee, _, _),
	(   memberchk(Callee, Done)
	->  Loop = [Callee|Done]
	;   has_loop(Callee, File, [Function|Done], Loop, Problems)
	).

close_list([]) :- !.
close_list([_|T]) :-
	close_list(T).

problems :-
	setof(Pred-Problems,
	      setof(Problem,
		    Loop^recursive_predicate(Pred, Loop, Problem),
		    Problems),
	      Pairs),
	forall(member(Pred-Problems, Pairs),
	       report(Pred, Problems)).

report(Name/Arity, Problems) :-
	format('| ~w/~w | ', [Name, Arity]),
	problem_functions(Problems),
	format(' |~n').

problem_functions([]).
problem_functions([H|T]) :-
	problem_function(H),
	(   T == []
	->  true
	;   format(', '),
	    problem_functions(T)
	).

problem_function(lao(F)) :-
	format('~w', [F]).
problem_function(no_lao(F)) :-
	format('*~w*', [F]).


		 /*******************************
		 *	       MAIN		*
		 *******************************/

%%	main
%
%	Run this in the src directory as
%
%	  ==
%	  % tools/recursive.pl
%	  ==

main :-
	load,
	problems.





