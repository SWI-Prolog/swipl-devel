/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_stack,
	  [ get_prolog_backtrace/2,	% +MaxDepth, -Stack
	    get_prolog_backtrace/3,	% +Frame, +MaxDepth, -Stack
	    prolog_stack_frame_property/2, % +Frame, ?Property
	    print_prolog_backtrace/2,	% +Stream, +Stack
	    print_prolog_backtrace/3,	% +Stream, +Stack, +Options
	    backtrace/1			% +MaxDepth
	  ]).
:- use_module(library(prolog_clause)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- predicate_options(print_prolog_backtrace/3, 3,
		     [ subgoal_positions(boolean)
		     ]).

/** <module> Examine the Prolog stack

This module defines  high-level  primitives   for  examining  the Prolog
stack.  It provides the following functionality:

    * get_prolog_backtrace/2 gets a Prolog representation of the
    Prolog stack.  This can be used for printing, but also to enrich
    exceptions (see prolog_exception_hook/4).

    * print_prolog_backtrace/2 prints a backtrace as returned by
    get_prolog_backtrace/2

    * The shorthand backtrace/1 fetches and prints a backtrace.

@see	library(http/http_error) exploits these to print a backtrace
	for HTTP server handlers that throw an exception.  Use this
	as a template for your own application specific handlers.
*/

%%	get_prolog_backtrace(+MaxDepth, -Backtrace)
%
%	Return a Prolog structure representing a backtrace from the
%	current location.  The backtrace is a list of frames.  Each
%	frame is represented as one of
%
%		* frame(Level, Clause, PC)
%		* frame(Level, foreign(Name/Arity), foreign)
%
%	MaxDepth defines the maximum number of frames returned.

get_prolog_backtrace(MaxDepth, Stack) :-
	prolog_current_frame(Fr),
	prolog_frame_attribute(Fr, pc, PC),
	prolog_frame_attribute(Fr, parent, Parent),
	backtrace(MaxDepth, Parent, PC, Stack).

get_prolog_backtrace(Fr, MaxDepth, Stack) :-
	backtrace(MaxDepth, Fr, call, Stack).

backtrace(0, _, _, []) :- !.
backtrace(MaxDepth, Fr, PC, [frame(Level, Where)|Stack]) :-
	prolog_frame_attribute(Fr, level, Level),
	(   PC == foreign
	->  prolog_frame_attribute(Fr, predicate_indicator, Pred),
	    Where = foreign(Pred)
	;   PC == call
	->  prolog_frame_attribute(Fr, predicate_indicator, Pred),
	    Where = call(Pred)
	;   prolog_frame_attribute(Fr, clause, Clause)
	->  Where = clause(Clause, PC)
	;   Where = meta_call
	),
	(   prolog_frame_attribute(Fr, pc, PC2)
	->  true
	;   PC2 = foreign
	),
	(   prolog_frame_attribute(Fr, parent, Parent)
	->  D2 is MaxDepth - 1,
	    backtrace(D2, Parent, PC2, Stack)
	;   Stack = []
	).

%%	prolog_stack_frame_property(+Frame, ?Property) is nondet.
%
%	True when Property is a property of   Frame. Frame is an element
%	of a stack-trace as produced by get_prolog_backtrace/2.  Defined
%	properties are:
%
%	  * level(Level)
%	  * predicate(PI)
%	  * location(File:Line)

prolog_stack_frame_property(frame(Level,_), level(Level)).
prolog_stack_frame_property(frame(_,Where), predicate(PI)) :-
	frame_predicate(Where, PI).
prolog_stack_frame_property(frame(_,clause(Clause,PC)), location(File:Line)) :-
	subgoal_position(Clause, PC, File, CharA, _CharZ),
	File \= @(_),			% XPCE Object reference
	lineno(File, CharA, Line).

frame_predicate(foreign(PI), PI).
frame_predicate(call(PI), PI).
frame_predicate(clause(Clause, _PC), M:Name/Arity) :-
	nth_clause(Head, _, Clause), !,
	Head = M:H,
	functor(H, Name, Arity).



%%	print_prolog_backtrace(+Stream, +Backtrace)
%
%	Print a stacktrace in human readable form to Stream.

print_prolog_backtrace(Stream, Backtrace) :-
	print_prolog_backtrace(Stream, Backtrace, []).


%%	print_prolog_backtrace(+Stream, +Backtrace, +Options)
%
%	Print a stacktrace in human readable form to Stream.
%	Options is an option list that accepts:
%
%	    * subgoal_positions(+Boolean)
%	    If =true= (default), print subgoal line numbers

print_prolog_backtrace(Stream, Backtrace, Options) :-
	phrase(message(Backtrace, Options), Lines),
	print_message_lines(Stream, '', Lines).

:- public				% Called from some handlers
	message//1.

message(Backtrace) -->
	message(Backtrace, []).

message([], _) -->
	[].
message([H|T], Options) -->
	message(H, Options),
	(   {T == []}
	->  []
	;   [nl],
	    message(T, Options)
	).

message(frame(Level, Where), Options) -->
	level(Level),
	where(Where, Options).

where(foreign(PI), _) -->
	[ '~w <foreign>'-[PI] ].
where(call(PI), _) -->
	[ '~w'-[PI] ].
where(clause(Clause, PC), Options) -->
	{ option(subgoal_positions(true), Options, true),
	  subgoal_position(Clause, PC, File, CharA, _CharZ),
	  File \= @(_),			% XPCE Object reference
	  lineno(File, CharA, Line),
	  clause_predicate_name(Clause, PredName)
	}, !,
	[ '~w at ~w:~d'-[PredName, File, Line] ].
where(clause(Clause, _PC), _) -->
	{ clause_property(Clause, file(File)),
	  clause_property(Clause, line_count(Line)),
	  clause_predicate_name(Clause, PredName)
	}, !,
	[ '~w at ~w:~d'-[PredName, File, Line] ].
where(clause(Clause, _PC), _) -->
	{ clause_name(Clause, ClauseName)
	},
	[ '~w <no source>'-[ClauseName] ].
where(meta_call, _) -->
	[ '<meta call>' ].

level(Level) -->
	[ '~|~t[~D]~8+ '-[Level] ].


%%	clause_predicate_name(+ClauseRef, -Predname) is det.
%
%	Produce a name (typically  Functor/Arity)   for  a  predicate to
%	which Clause belongs.

clause_predicate_name(Clause, PredName) :-
	user:prolog_clause_name(Clause, PredName), !.
clause_predicate_name(Clause, PredName) :-
	nth_clause(Head, _N, Clause), !,
	predicate_name(user:Head, PredName).


%%	backtrace(+MaxDepth)
%
%	Get and print a stacktrace to the user_error stream.

backtrace(MaxDepth) :-
	get_prolog_backtrace(MaxDepth, Stack),
	print_prolog_backtrace(user_error, Stack).


subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
	debug(clause, 'Term-position in ~w at PC=~w:', [ClauseRef, PC]),
	clause_info(ClauseRef, File, TPos, _),
	'$clause_term_position'(ClauseRef, PC, List),
	debug(clause, '\t~w~n', [List]),
	find_subgoal(List, TPos, PosTerm),
	arg(1, PosTerm, CharA),
	arg(2, PosTerm, CharZ).

find_subgoal([], Pos, Pos).
find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
	nth1(A, PosL, Pos),
	find_subgoal(T, Pos, SPos).


%%	lineno(+File, +Char, -Line)
%
%	Translate a character location to a line-number.

lineno(File, Char, Line) :-
	setup_call_cleanup(
	    open(File, read, Fd),
	    lineno_(Fd, Char, Line),
	    close(Fd)).

lineno_(Fd, Char, L) :-
	stream_property(Fd, position(Pos)),
	stream_position_data(char_count, Pos, C),
	C > Char, !,
	stream_position_data(line_count, Pos, L0),
	L is L0-1.
lineno_(Fd, Char, L) :-
	skip(Fd, 0'\n),
	lineno_(Fd, Char, L).
