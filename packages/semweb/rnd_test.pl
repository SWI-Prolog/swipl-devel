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

:- module(rdf_random_test,
	  [ concur/2,			% +Threads, +Actions
	    go/0,
	    go/1,			% +Actions
	    record/1,			% +Actions
	    replay/1			% +Actions
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
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

%%	go is det.
%%	go(+N) is det.
%
%	Perform N random operations on the database.

go :-
	go(20000).
go(N) :-
	nb_setval(rnd_file, none),
	do_random(N).

%%	record(+N)
%
%	As go/1, but  record  generated  random   numbers  in  the  file
%	specified with replay_file/1.

record(N) :-
	replay_file(File),
	open(File, write, Out),
	nb_setval(rnd_file, out(Out)),
	do_random(N).

%%	replay(+N)
%
%	Replay first N actions recorded using   record/1.  N is normally
%	the same as used for record/1.

replay(N) :-
	replay_file(File),
	open(File, read, In),
	nb_setval(rnd_file, in(In)),
	do_random(N).

%%	next(-N, +Max)
%
%	Produce a random number 0 =< N <= Max. During record/1, write to
%	file.  Using replay/1, read from file.

next(N, Max) :-
	nb_getval(rnd_file, X),
	(   X == none
	->  N is random(Max+1)
	;   X = in(Fd)
	->  read(Fd, N)
	;   X = out(Fd),
	    N is random(Max),
	    format(Fd, '~q.~n', [N]),
	    flush_output(Fd)
	).


%%	do_random(N) is det.
%
%	Take a random action on the database.

do_random(N) :-
	nb_setval(line, 1),
	random_actions(N).

random_actions(N) :-
	MM is N mod 100,
	(   MM = 0
	->  rdf_statistics(triples(Triples)),
	    debug(count, 'Count ~w, Triples ~w', [N, Triples])
	;   true
	),
	next(Op, 7),
	rans(Subject),
	ranp(Predicate),
	rano(Object),
	rang(Graph),
	do(Op, Subject, Predicate, Object, Graph),
	N1 is N - 1,
	(   N > 1
	->  random_actions(N1)
	;   true
	).

%%	do(+Operation, +Subject, +Predicate, +Object, +Graph) is det.
%
%	Execute an operation on Graph.
%	
%	@tbd	Test update

do(0, S, P, O, G) :-
	debug(bug(S,P,O), 'ASSERT(~q,~q,~q,~q)', [S,P,O,G]),
	rdf_assert(S,P,O,G).
do(1, S, P, O, G) :-
	debug(bug(S,P,O), 'RETRACTALL(~q,~q,~q,~q)', [S,P,O,G]),
	rdf_retractall(S,P,O,G).
do(2, S, _P, _O, _G) :- findall(x, rdf_has(S, _, _, _), _).
do(3, S, P, _O, _G)  :- findall(x, rdf_has(S, P, _, _), _).
do(4, S, P, _O, _G)  :- findall(x, rdf_reachable(S, P, _), _).
do(5, _S, P, O, _G)  :- findall(x, (atom(O),rdf_reachable(_, P, O)), _).
do(6, _, P, _, G) :-
	repeat,
	    ranp(P2),
	P2 \== P, !,
	rdf_assert(P, rdfs:subPropertyOf, P2, G).
do(7, _, P, _, G) :-
	repeat,
	    ranp(P2),
	P2 \== P, !,
	rdf_retractall(P, rdfs:subPropertyOf, P2, G).

%%	rans(-Subject) is det.
%
%	Generate a random subject.

rans(X) :-
	next(I, 3),
	rs(I, X).

rs(0, a).
rs(1, b).
rs(2, c).
rs(3, d).

%%	ranp(-Predicate) is det.
%
%	Generate a random predicate.

ranp(X) :-
	next(I, 3),
	rp(I, X).
rp(0, a).
rp(1, p1).
rp(2, p2).
rp(3, p3).

%%	rano(-Object) is det.
%
%	Generate a random object.

rano(X) :-
	next(I, 12),
	ro(I, X).
ro(0, a).
ro(1, b).
ro(2, c).
ro(3, p1).
ro(4, literal(1)).
ro(5, literal(hello_world)).
ro(6, literal(bye)).
ro(7, literal(lang(en, bye))).
ro(8, literal(lang(nl, bye))).
ro(9, d).
ro(10, R) :-
	next(I, 1000),
	atom_concat(r, I, R).
ro(11, literal(L)) :-
	next(I, 1000),
	atom_concat(l, I, L).
ro(12, literal(lang(Lang, L))) :-
	next(I, 1000),
	atom_concat(l, I, L),
	ranl(Lang).

ranl(Lang) :-
	next(I, 1),
	rl(I, Lang).

rl(0, en).
rl(1, nl).


%%	rang(-Graph) is det.
%
%	Generate a random graph.

rang(X:Line) :-
	next(I, 3),
	rg(I, X),
	Line = 1.
%	line(Line).

rg(0, g1).
rg(1, g2).
rg(2, g3).
rg(3, g4).

line(Line) :-
	nb_getval(line, Line),
	NL is Line+1,
	nb_setval(line, NL).

