/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(prolog_break,
	  [ break_at/3
	  ]).
:- use_module(trace).			% clause_info
:- use_module(util).
:- use_module(clause).

:- dynamic
	user:prolog_event_hook/1.
:- multifile
	user:prolog_event_hook/1.

%	break_at(File, Line, Char)
%
%	Put a breakpoint at the indicated source-location.  File is a current
%	sourcefile (as reported by source_file/1).  Line is the 1-based line
%	in which Char is.  Char is the position of the break.
%
%	First, '$clause_from_source'/3 uses the SWI-Prolog clause-source
%	information to find the last clause starting before Line.  '$break_pc'
%	generated (on backtracking), a list of possible break-points.

break_at(File, Line, Char) :-
	debug('break_at(~q, ~d, ~d).~n', [File, Line, Char]),
	'$clause_from_source'(File, Line, ClauseRef),
	clause_info(ClauseRef, _File, TermPos, _NameOffset),
	'$break_pc'(ClauseRef, PC, NextPC),
	'$clause_term_position'(ClauseRef, NextPC, List),
	range(List, TermPos, A, Z),
	between(A, Z, Char),
	debug('Break at clause ~w, PC=~w~n', [ClauseRef, PC]),
	'$break_at'(ClauseRef, PC, true).

range([], Pos, A, Z) :-
	arg(1, Pos, A),
	arg(2, Pos, Z).
range([H|T], term_position(_, _, _, _, PosL), A, Z) :-
	nth1(H, PosL, Pos),
	range(T, Pos, A, Z).
	
       		 /*******************************
		 *	      FEEDBACK		*
		 *******************************/

:- pce_global(@prolog_debugger, new(object)).

user:prolog_event_hook(break(ClauseRef, PC, true)) :-
	prolog_break:break(ClauseRef, PC).
user:prolog_event_hook(break(ClauseRef, PC, false)) :-
	prolog_break:nobreak(ClauseRef, PC).

break(ClauseRef, PC) :-
	debug('Trap in Clause ~d, PC ~d~n', [ClauseRef, PC]),
	clause_info(ClauseRef, File, TermPos, _NameOffset),
	'$fetch_vm'(ClauseRef, PC, NPC, _VMI),
	'$clause_term_position'(ClauseRef, NPC, List),
	debug('ClausePos = ~w~n', [List]),
	prolog_break:range(List, TermPos, A, Z),
	debug('Range: ~d .. ~d~n', [A, Z]),
	prolog_source_view:buffer(File, Buffer),
	debug('Buffer = ~p~n', [Buffer]),
	new(F, fragment(Buffer, A, Z-A, breakpoint)),
	send(F, attribute, clause, ClauseRef),
	send(F, attribute, pc, PC),
	new(_, hyper(@prolog_debugger, F, break, debugger)).

nobreak(ClauseRef, PC) :-
	debug('Deleted break-point at clause ~d, PC ~d~n', [ClauseRef, PC]),
	get(@prolog_debugger, find_hyper, break,
	    and(@arg3?clause == ClauseRef,
		@arg3?pc == PC),
	    Hyper),
	get(Hyper, to, Fragment),
	free(Fragment).
