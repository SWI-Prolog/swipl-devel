/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_break,
	  [ break_at/3,			% +File, +Line, +CharPos
	    break_location/4		% +ClauseRef, +PC, -File, -A-Z
	  ]).
:- use_module(trace).			% clause_info
:- use_module(util).
:- use_module(clause).
:- use_module(source).

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
	debug('NextPC = ~w~n', [NextPC]),
	'$clause_term_position'(ClauseRef, NextPC, List),
	debug('Location = ~w~n', [List]),
	range(List, TermPos, A, Z),
	debug('Term from ~w-~w~n', [A, Z]),
	between(A, Z, Char), !,
	debug('Break at clause ~w, PC=~w~n', [ClauseRef, PC]),
	'$break_at'(ClauseRef, PC, true),
	debug.

range([], Pos, A, Z) :-
	arg(1, Pos, A),
	arg(2, Pos, Z).
range([H|T], term_position(_, _, _, _, PosL), A, Z) :-
	nth1(H, PosL, Pos),
	range(T, Pos, A, Z).
	
       		 /*******************************
		 *	      FEEDBACK		*
		 *******************************/

user:prolog_event_hook(break(ClauseRef, PC, Set)) :-
	break(Set, ClauseRef, PC).

break(SetClear, ClauseRef, PC) :-
	print_message(informational, break(SetClear, ClauseRef, PC)),
	(   SetClear == true
	->  debug('Trap in Clause ~d, PC ~d~n', [ClauseRef, PC]),
	    clause_property(ClauseRef, file(File)),
	    current_source_buffer(File, _Buffer),
	    mark_stop_point(ClauseRef, PC)
	;   debug('Deleted break at clause ~d, PC ~d~n', [ClauseRef, PC]),
	    unmark_stop_point(ClauseRef, PC)
	).

%	break_location(+ClauseRef, +PC, -File, -A-Z)
%
%	Determine source-code location of a break-point.

break_location(ClauseRef, PC, File, A-Z) :-
	clause_info(ClauseRef, File, TermPos, _NameOffset),
	'$fetch_vm'(ClauseRef, PC, NPC, _VMI),
	'$clause_term_position'(ClauseRef, NPC, List),
	debug('ClausePos = ~w~n', [List]),
	range(List, TermPos, A, Z),
	debug('Range: ~d .. ~d~n', [A, Z]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(break(SetClear, ClauseRef, _PC)) -->
	setclear(SetClear),
	clause_location(ClauseRef).

setclear(true) -->
	['Breakpoint at '].
setclear(false) -->
	['Cleared breakpoint from '].

clause_location(ClauseRef) -->
	{ clause_property(ClauseRef, file(File)),
	  clause_property(ClauseRef, line(Line)), !,
	  clause_name(ClauseRef, Name)
	},
	['~w at ~w:~d'-[Name, File, Line] ].
clause_location(ClauseRef) -->
	{ clause_name(ClauseRef, Name)
	},
	['~w'-[Name] ].
	
	
