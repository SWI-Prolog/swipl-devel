#!/bin/env swipl

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2021, University of Amsterdam
			      VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find recursive C functions.

Usage:

  - Build SWI-Prolog:

      mkdir build.analysis
      cd build.analysis
      CFLAGS="-fdump-rtl-expand" cmake -DSWIPL_PACKAGES=OFF -DCMAKE_BUILD_TYPE=Debug -G Ninja ..
      sed -i 's/DO_DEBUG -DO_DEBUG_ATOMGC//' CMakeCache.txt
      ninja
      cd src/CMakeFiles/swiplobjs.dir
      ../../../../src/tools/update-deps *.expand
      swipl ../../../../src/tools/recursive.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- initialization(main,main).

:- op(500, xfx, @).

:- dynamic
	function/7,	% Name, Type, File, StartLine, EndLine, Word, Mark
	calls/4,	% Name, Callee, File, Line
	object/2,	% Object, Source
	static/2,	% Name, File
	predicate/3.

load :-
	expand_file_name('*.functions', FFiles),
	maplist(load_file, FFiles),
	expand_file_name('*.tree', TFiles),
	maplist(load_file, TFiles),
	load_file('./built-in-predicates').

load_file(File) :-
	setup_call_cleanup(
	    open(File, read, In),
	    load_stream(In),
	    close(In)).

load_stream(In) :-
	repeat,
	read_term(In, Term, []),
	(   Term == end_of_file
	->  !
	;   call(Term)
	->  fail
	;   assertz(Term),
	    fail
	).

problems :-
	forall(calls(Func,Func),
	       recursive_function(Func)).

recursive_function(Func) :-
	distinct(Loop, recursive_function(Func, Loop)),
	maplist(write_call, Loop).

write_call((Func@_)@Site) =>
	format('    ~w at ~w~n', [Func, Site]).
write_call((Func)@Site) =>
	format('~w at ~w~n', [Func, Site]).
write_call(Func) =>
	format('~w~n', [Func]).

recursive_function(Func, [Func|Loop]) :-
	calls(Func, Func),
	between(1, 10, Depth),
	edge(Func, Callee, Site),
	path(Callee, Func, [Callee@Site], Loop0, Depth),
	reverse(Loop0, Loop).

path(Target, Target, Loop, Loop, _) :-
	!.
path(Target@_, Target, Loop, Loop, _) :-
	!.
path(Target, Target@_, Loop, Loop, _) :-
	!.
path(Here, Target, Seen, Loop, Depth) :-
	Depth > 0,
	edge(Here, There, Site),
	\+ seen(There, Seen),
	Depth1 is Depth-1,
	path(There, Target, [There@Site|Seen], Loop, Depth1).

seen(Func, Seen) :- memberchk(Func, Seen), !.
seen(Func, Seen) :- atom(Func), memberchk(Func@_, Seen), !.
seen(Func@_, Seen) :- memberchk(Func, Seen), !.


%!	calls(?Func, ?Callee) is nondet.

:- op(650, xfx, @).

calls(Func, Callee) :-
	anormal(Func, Func1),
	anormal(Callee, Callee1),
	tcalls(Func1, Callee1),
	znormal(Func, Func1),
	znormal(Callee, Callee1).

anormal(Func, _Term), var(Func) => true.
anormal(Func, Term),  atom(Func) => Term = Func@_.
anormal(Func@Loc, Term) => Term	= Func@Loc.

znormal(Func, Func) :- !.
znormal(Func, Func@_).

edge(Func, Callee, Where) :-
	anormal(Func, Func1),
	anormal(Callee, Callee1),
	dcall(Func1, Callee1, Where),
	znormal(Func, Func1),
	znormal(Callee, Callee1).

:- table tcalls/2.

tcalls(Func, Callee) :-
	tcalls(Func, Callee0),
	tcalls(Callee0, Callee).
tcalls(Func, Callee) :-
	dcall(Func, Callee, _).

:- table dcall/3.

dcall(Func@File:SLine, Callee, File:CallLine) :-
	function(Func, Type, File, SLine, ELine, _, _),
	(   Type == function
	->  calls(Func, Callee0, File, CallLine)
	;   calls(_Func, Callee0, File, CallLine) % predicate, vmi, vmh
	),
	between(SLine, ELine, CallLine),
	(   function(Callee0, function, File, SLineCallee, _, _, _)
	->  Callee = Callee0@File:SLineCallee
	;   Callee = Callee0
	).

duplicate_name(F, Files) :-
	setof(File, function(F, File), Files),
	Files = [_,_|_].

function(F, File:Line) :-
	function(F, _, File, Line, _, _, _).

cleanup_empty_functions :-
	predicate_property(function(_, _, _, _, _, _, _),
			   number_of_clauses(N0)),
	forall(empty_function(Func),
	       retract_function(Func)),
	predicate_property(function(_, _, _, _, _, _, _),
			   number_of_clauses(N1)),
	Cleaned is N0-N1,
	format('Removed ~D duplicate functions; left ~D~n',
	       [Cleaned, N1]).

retract_function(Func@File:Line) :-
	retractall(function(Func, _, File, Line, _, _, _)).

empty_function(Func@File:SL2) :-
	duplicate_name(Func, Locs),
	member(File:SL1, Locs),
	member(File:SL2, Locs),
	SL1 \== SL2,
	dcall(Func@File:SL1, _, _),
	\+ dcall(Func@File:SL2, _, _).


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
	cleanup_empty_functions,
	problems.
