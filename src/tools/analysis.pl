#!/home/jan/bin/pl -q -g true -t main -s

:- op(500, xfx, @).

:- multifile
	function/7,	% Name, Type, File, StartLine, EndLine, Word, Mark
	calls/4.	% Name, Callee, File, Line

load :-
	expand_file_name('*.functions', FFiles),
	maplist(consult, FFiles),
	expand_file_name('*.tree', TFiles),
	maplist(consult, TFiles).

target(growStacks).

:- dynamic
	caller/2.			% Func, File

caller_of(Callee, CalleeFile, Caller, CallerFile) :-
	(   CalleeFile = CallerFile,
	    calls(Caller, Callee, CallerFile, _)
	*-> true
	;   calls(Caller, Callee, CallerFile, _)
	).

r_caller_of(Callee, CalleeFile) :-
	caller(Callee, CalleeFile), !.
r_caller_of(Callee, CalleeFile) :-
	assert(caller(Callee, CalleeFile)),
	setof(Caller@CallerFile,
	      (	  caller_of(Callee, CalleeFile, Caller, CallerFile),
		  \+ caller(Caller, CallerFile)
	      ),
	      Pairs), !,
	forall(member(C@F, Pairs),
	       r_caller_of(C, F)).
r_caller_of(_,_).

target_callers :-
	retractall(caller(_,_)),
	forall(target(Target),
	       r_caller_of(Target, _)).

problem(Func, Type, Problem, File, Line) :-
	function(Func, Type, File, StartLine, EndLine, Words, Marks),
	(   Words > 0,
	    calls(_, Callee, File, Line),
	    Line >= StartLine,
	    Line =< EndLine,
	    caller(Callee, _CalleeFile),
	    format(atom(Problem), 'Calls ~w', [Callee])
	;   Marks > 0,
	    Line = StartLine,
	    Problem = 'Uses Mark()'
	).

problems :-
	Term = problem(_Func, _Type, _Problem, _File, _Line),
	setof(Term, Term, List),
	forall(member(Term, List), report(Term)),
	length(List, N),
	format('Found a total of ~D potential problems~n', [N]).

report(problem(Func, vmi, Problem, File, Line)) :-
	format('[~w] ~w:~d: ~w~n', [Func, File, Line, Problem]).
report(problem(Pred, predicate, Problem, File, Line)) :-
	format('[~w] ~w:~d: ~w~n', [Pred, File, Line, Problem]).
report(problem(Func, function, Problem, File, Line)) :-
	format('[~w()] ~w:~d: ~w~n', [Func, File, Line, Problem]).

main :-
	load,
	target_callers,
	problems.
