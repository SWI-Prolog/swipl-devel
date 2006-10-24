:- asserta(file_search_path(foreign, '.')).
:- use_module(rdf_db).

replay_file('rnd.reply').

%%	concur(+Threads:int, +Actions:int) is det.
%
%	Create Threads, each performing Actions.

concur(Threads, Actions) :-
	create_threads(Threads, go(Actions), Ids),
	wait(Ids).

create_threads(0, _, []) :- !.
create_threads(N, G, [Id|T]) :-
	thread_create(G, Id, []),
	N2 is N - 1,
	create_threads(N2, G, T).
	
wait([]).
wait([H|T]) :-
	thread_join(H, Result),
	(   Result == true
	->  true
	;   format('ERROR from ~w: ~w~n', [H, Result])
	),
	wait(T).

go :-
	go(20000).

g :- p, d, replay.

p :-
	protocol(x).

d :-
	rdf_debug(1),
	debug(bug(a, a, c)).
%	debug(bug(d, p3, a)).

%%	go(+N) is det.
%
%	Perform N random operations on the database.

go(N) :-
	nb_setval(rnd_file, none),
	do_random(N).

%%	record(+N)
%
%	As go/1, but record generated random numbers in the file
%	specified with replay_file/1.

record(N) :-
	replay_file(File),
	open(File, write, Out),
	nb_setval(rnd_file, out(Out)),
	do_random(N).

replay :-
	replay_file(File),
	open(File, read, In),
	nb_setval(rnd_file, in(In)),
	do_random(20000).

next(N, Max) :-
	nb_getval(rnd_file, X),
	(   X == none
	->  N is random(Max)
	;   X = in(Fd)
	->  read(Fd, N)
	;   X = out(Fd),
	    N is random(Max),
	    format(Fd, '~q.~n', [N]),
	    flush_output(Fd)
	).

do_random(N) :-
	MM is N mod 100,
	(   MM = 0
	->  rdf_statistics(triples(Triples)),
	    debug(count, 'Count ~w, Triples ~w', [N, Triples])
	;   true
	),
	next(Op, 6),
	rans(Subject),
	ranp(Predicate),
	rano(Object),
	rans(Graph),
	do(Op, Subject, Predicate, Object, Graph),
	N1 is N - 1,
	(   N > 1
	->  do_random(N1)
	;   true
	).

do(0, S, P, O, G) :-
	debug(bug(S,P,O), 'ASSERT(~q,~q,~q,~q)', [S,P,O,G]),
	rdf_assert(S,P,O,G).
do(1, S, P, O, G) :-
	debug(bug(S,P,O), 'RETRACTALL(~q,~q,~q,~q)', [S,P,O,G]),
	rdf_retractall(S,P,O,G).
do(2, S, _P, _O, _G) :- findall(x, rdf_has(S, _, _, _), _).
do(3, S, P, _O, G)   :- findall(x, rdf_has(S, P, _, G), _).
do(4, _, P, _, G) :-
	repeat,
	    ranp(P2),
	P2 \== P, !,
	rdf_assert(P, rdfs:subPropertyOf, P2, G).
do(5, _, P, _, G) :-
	repeat,
	    ranp(P2),
	P2 \== P, !,
	rdf_retractall(P, rdfs:subPropertyOf, P2, G).

rans(X) :-
	next(I, 4),
	rs(I, X).
rs(0, a).
rs(1, b).
rs(2, c).
rs(3, d).

ranp(X) :-
	next(I, 4),
	rp(I, X).
rp(0, a).
rp(1, p1).
rp(2, p2).
rp(3, p3).

rano(X) :-
	next(I, 8),
	ro(I, X).
ro(0, a).
ro(1, b).
ro(2, c).
ro(3, p1).
ro(4, literal(1)).
ro(5, literal(hello_world)).
ro(6, literal(bye)).
ro(7, d).
