:- module(test_engines,
	  [ test_engines/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).

test_engines :-
	run_tests([ engines
		  ]).

:- begin_tests(engines).

test(alias, Alias = ename) :-
	engine_create(X, engine_self(X), E, [alias(ename)]),
	assertion(E == ename),
	engine_next(E, Alias),
	engine_destroy(E),
	assertion(\+ is_engine(E)).
test(rdef_alias, error(permission_error(create,engine,ename))) :-
	engine_create(X, engine_self(X), E, [alias(ename)]),
	call_cleanup(engine_create(x, true, _, [alias(ename)]),
		     engine_destroy(E)).
test(gsize, TheLimit =:= 1000*1024) :-
	engine_create(Limit, statistics(globallimit, Limit), E,
		      [ global(1000) ]),
	engine_next(E, TheLimit),
	engine_destroy(E).
test(lsize, TheLimit =:= 1000*1024) :-
	engine_create(Limit, statistics(locallimit, Limit), E,
		      [ local(1000) ]),
	engine_next(E, TheLimit),
	engine_destroy(E).
test(tsize, TheLimit =:= 1000*1024) :-
	engine_create(Limit, statistics(traillimit, Limit), E,
		      [ trail(1000) ]),
	engine_next(E, TheLimit),
	engine_destroy(E).
test(findall, L == [1,2,3,4,5]) :-
	e_findall(X, between(1, 5, X), L).
test(yield, L == [1,2,3,4,5]) :-
	e_yield(5, L).
test(post, Sums == [1,3,6,10,15]) :-
	numlist(1, 5, List),
	sums_list(List, Sums).
test(whisper, Final == 10) :-
	whisper(9, 1, Final).
test(count, Final =:= N*M) :-
	N = 4,					% threads
	M = 10000,				% steps
	counter(N, M, Final).
test(error, Ex == foo) :-
	engine_create(_, throw(foo), E),
	catch(engine_next(E, _), Ex, true),
	engine_destroy(E).
test(no_data, error(existence_error(term, delivery, E))) :-
	setup_call_cleanup(
	    engine_create(_, sum(0), E),
	    maplist(engine_next(E), [1]),
	    engine_destroy(E)).
test(gc, [sto(rational_trees)]) :-
	gc_engines(engine_create(_, true, _), 100).
test(gc2, [sto(rational_trees)]) :-
	gc_engines(( engine_create(X, between(1,1,X), E),
		     engine_next(E, V),
		     assertion(V == 1)
		   ), 100).
test(gc3, [sto(rational_trees)]) :-
	gc_engines(( engine_create(X, between(1,2,X), E),
		     engine_next(E, V),
		     assertion(V == 1)
		   ), 100).
test(gc4, [sto(rational_trees)]) :-
	gc_engines(( engine_create(X, between(1,2,X), E),
		     engine_next(E, V),
		     assertion(V == 1),
		     engine_destroy(E)
		   ), 100).

:- end_tests(engines).


:- meta_predicate e_findall(?, 0, -).

e_findall(Templ, Goal, List) :-
	setup_call_cleanup(
	    engine_create(Templ, Goal, E),
	    get_answers(E, List),
	    engine_destroy(E)).

e_yield(Len, List) :-
	setup_call_cleanup(
	    engine_create(_, yield_loop(1,Len), E),
	    get_answers(E, List),
	    engine_destroy(E)).

yield_loop(I, M) :-
	I =< M, !,
	engine_yield(I),
	I2 is I+1,
	yield_loop(I2, M).

get_answers(E, [H|T]) :-
	engine_next(E, H), !,
	get_answers(E, T).
get_answers(_, []).

%%	sums_list(+List, +Sums)
%
%	Demonstrate keeping state inside an engine.

sums_list(List, Sums) :-
	setup_call_cleanup(
	    engine_create(_, sum(0), E),
	    maplist(engine_post(E), List, Sums),
	    engine_destroy(E)).

sum(Sum) :-
	engine_fetch(New),
	Sum1 is New + Sum,
	engine_yield(Sum1),
	sum(Sum1).


%%	whisper(+N, +From, -Final)
%
%	Create a chain of engines,  each   of  which fetches the version
%	from its left neighbour and posts it   to its right after adding
%	one.

whisper(N, From, Final) :-
	engine_create(Final, final(Final), Last),
	whisper_list(N, Last, First),
	engine_post(First, From, Final).

whisper_list(0, First, First) :- !.
whisper_list(N, Next, First) :-
	engine_create(Final, add1_and_tell(Next, Final), Me),
	N1 is N - 1,
	whisper_list(N1, Me, First).

final(X) :-
	engine_fetch(X).

add1_and_tell(Next, Final) :-
	engine_fetch(X),
	X2 is X + 1,
	debug(whisper, 'Sending ~d to ~p', [X2, Next]),
	engine_post(Next, X2, Final).

%%	gc_engines(:Create, +N) is semidet.
%
%	Create N engines using Create and try to GC them.

:- meta_predicate
	gc_engines(0, +).

gc_engines(Create, N) :-
	garbage_collect_atoms,
	aggregate_all(count, current_engine(_), Count0),
	forall(between(1, N, _), Create),
	garbage_collect_atoms,
	(   between(1, 100, _),
	    aggregate_all(count, current_engine(_), Count1),
	    (   Count1 < Count0 + N/4
	    ->  !
	    ;   sleep(0.01),			% Reclaim is in a thread
		fail
	    )
	->  true
	;   aggregate_all(count, current_engine(_), Count2),
	    Dangling is Count2-Count0,
	    format('~NWARNING: left ~D of ~D engines dangling~n',
		   [Dangling, N])
	).

%%	counter(+N, +M, -Total)
%
%	Create a counting engine  and  creat   N  threads  that send the
%	engine M requests to count. Return the final count (which should
%	be N*M).

counter(N, M, Total) :-
	counter(E),
	length(Threads, N),
	maplist(create_counting(E, M), Threads),
	maplist(join_true, Threads),
	add(E,0,Total),
	engine_destroy(E).

counter(E) :-
	engine_create(_, add_counter(0), E).

add_counter(I) :-
	engine_fetch(Add),
	I1 is I+Add,
	engine_yield(I1),
	add_counter(I1).

create_counting(E, M, Id) :-
	thread_create(forall(between(1, M, _), add(E, 1, _)), Id, []).

join_true(Id) :-
	thread_join(Id, Result),
	assertion(Result == true).

add(E, Add, Sum) :-
	engine_post(E, Add, Sum).
