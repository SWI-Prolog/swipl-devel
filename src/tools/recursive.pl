#!/home/jan/bin/swipl -q -g true -t main -s

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2013, University of Amsterdam
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
stop_show('analyseVariables2',	     lao).
stop_show('compileArgument',	     lao).
stop_show('compileBody',	     no_lao).
stop_show('find_if_then_end',	     no_lao).
stop_show('ar_func_n',		     no_lao).
stop_show('termHashValue',	     no_lao).
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





