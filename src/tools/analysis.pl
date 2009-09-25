#!/home/jan/bin/pl -q -g true -t main -s

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
	maplist(consult, TFiles).

target(growStacks, shift).
target(garbageCollect, gc).

:- dynamic
	caller/4,			% Func, File, Path, shift/gc
	only/1,
	report/1.

%%	caller_of(+Callee, +CalleeFile, -Caller, -CallerFile) is nondet.
%
%	@tbd	Deal with static functions (only called in local file)

caller_of(Callee, CalleeFile, _, _) :-
	(   volatile(Callee, CalleeFile)
	;   stop(Callee, CalleeFile)
	), !, fail.
caller_of(Callee, CalleeFile, Caller, CallerFile) :-
	static(Callee, CallerFile), !,
	calls(Caller, Callee, CallerFile, _),
	object(O, CalleeFile),
	object(O, CallerFile).
caller_of(Callee, _CalleeFile, Caller, CallerFile) :-
	calls(Caller, Callee, CallerFile, _).

r_caller_of(Callee, CalleeFile, _, What) :-
	caller(Callee, CalleeFile, _, What), !.
r_caller_of(Callee, CalleeFile, Path, What) :-
	assert(caller(Callee, CalleeFile, Path, What)),
	setof(Caller@CallerFile,
	      ( caller_of(Callee, CalleeFile, Caller, CallerFile),
		\+ caller(Caller, CallerFile, _, What)
	      ),
	      Pairs), !,
	(   debugging(called)
	->  format('~w is called by:', [Callee]),
	    forall(member(C@F, Pairs), format(' ~w', [C])),
	    nl
	;   true
	),
	forall(member(C@F, Pairs),
	       r_caller_of(C, F, [Callee|Path], What)).
r_caller_of(_,_, _, _).

target_callers(What) :-
	retractall(caller(_,_,What)),
	forall(target(Target, What),
	       r_caller_of(Target, _, [], What)).

problem(Func, Type, Problem, File, Line) :-
	function(Func, Type, File, StartLine, EndLine, Words, Marks),
	(   Words > 0,
	    calls(_, Callee, File, Line),
	    Line >= StartLine,
	    Line =< EndLine,
	    setof(What, ( caller(Callee, _CalleeFile, Path, What),
			  report(What),
			  \+ safe(Type, What, Func)), WhatList),
	    atomic_list_concat([Callee|Path], '<-', Where),
	    format(atom(Problem), 'Calls ~w ~w', [Where, WhatList])
	;   Marks > 0,
	    Line = StartLine,
	    Problem = '[mark]',
	    report(mark)
	).

problems :-
	Term = problem(_Func, _Type, _Problem, _File, _Line),
	setof(Term, Term, List), !,
	forall(member(Term, List), report(Term)),
	length(List, N),
	format('Found a total of ~D potential problems~n', [N]).
problems :-
	format('No problems found~n').

report(problem(Func, vmi, Problem, File, Line)) :-
	format('[~w] ~w:~d: ~w~n', [Func, File, Line, Problem]).
report(problem(Pred, predicate, Problem, File, Line)) :-
	format('[~w] ~w:~d: ~w~n', [Pred, File, Line, Problem]).
report(problem(Func, function, Problem, File, Line)) :-
	format('[~w()] ~w:~d: ~w~n', [Func, File, Line, Problem]).


		 /*******************************
		 *	       MAIN		*
		 *******************************/

main :-
	options,
	load,
	target_callers(_),
	problems.

options :-
	current_prolog_flag(argv, Argv),
	append(_, [--|Args], Argv), !,
	options(Args),
	(   setof(O, only(O), Types)
	->  true
	;   setof(T, F^target(F, T), Types)
	),
	forall(member(T, Types), assert(report(T))).

options([]) :- !.
options(['-t', What|T]) :- !,
	assert(only(What)),
	options(T).
options(['-d', What|T]) :- !,
	debug(What),
	options(T).
options(_) :-
	usage.

usage :-
	format(user_error, 'Usage: analysis.pl [-t type] [-d topic]~n', []),
	halt(1).
