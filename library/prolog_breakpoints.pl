/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/
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

:- module(prolog_breakpoints,
	  [ set_breakpoint/4,		% +File, +Line, +CharPos, -Id
	    delete_breakpoint/1,	% +Id
	    breakpoint_property/2	% ?Id, ?Property
	  ]).
:- use_module(prolog_clause).
:- use_module(library(debug)).
:- use_module(library(error)).


/** <module> Manage Prolog break-points

This module provides an  interface  for   development  tools  to set and
delete break-points, giving a location in  the source. Development tools
that want to track changes to   breakpoints must use user:message_hook/3
to intercept these message terms:

  * breakpoint(set, Id)
  * breakpoint(delete, Id)

Note that the hook must fail  after   creating  its side-effects to give
other hooks the opportunity to react.
*/

:- dynamic
	user:prolog_event_hook/1.
:- multifile
	user:prolog_event_hook/1.

%%	set_breakpoint(+File, +Line, +Char, -Id) is det.
%
%	Put a breakpoint at the  indicated   source-location.  File is a
%	current sourcefile (as reported by   source_file/1). Line is the
%	1-based line in which Char  is.  Char   is  the  position of the
%	break.
%
%	First, '$clause_from_source'/3 uses the SWI-Prolog clause-source
%	information to find  the  last   clause  starting  before  Line.
%	'$break_pc' generated (on backtracking),  a   list  of  possible
%	break-points.
%
%	Note that in addition to  setting   the  break-point, the system
%	must be in debug mode. With threading enabled, there are various
%	different ways this may  be  done.   See  debug/0,  tdebug/0 and
%	tdebug/1. Therefore, this predicate  does   *not*  enable  debug
%	mode.

set_breakpoint(File, Line, Char, Id) :-
	debug(break, 'break_at(~q, ~d, ~d).~n', [File, Line, Char]),
	'$clause_from_source'(File, Line, ClauseRef),
	clause_info(ClauseRef, InfoFile, TermPos, _NameOffset),
	(   InfoFile == File
	->  '$break_pc'(ClauseRef, PC, NextPC),
	    debug(break, 'Clause ~p, NextPC = ~w~n', [ClauseRef, NextPC]),
	    '$clause_term_position'(ClauseRef, NextPC, List),
	    debug(break, 'Location = ~w~n', [List]),
	    range(List, TermPos, A, Z),
	    debug(break, 'Term from ~w-~w~n', [A, Z]),
	    Z >= Char, !
	;   format('Failed to unify clause ~p, using first break~n',
		   [ClauseRef]),
	    '$break_pc'(ClauseRef, PC, _), !
	),
	debug(break, 'Break at clause ~w, PC=~w~n', [ClauseRef, PC]),
	with_mutex('$break', next_break_id(Id)),
	Location = file_position(File, Line, Char),
	asserta(known_breakpoint(ClauseRef, PC, Location, Id), Ref),
	catch('$break_at'(ClauseRef, PC, true), E,
	      (erase(Ref), throw(E))).


range([], Pos, A, Z) :-
	arg(1, Pos, A),
	arg(2, Pos, Z).
range([H|T], term_position(_, _, _, _, PosL), A, Z) :-
	nth1(H, PosL, Pos),
	range(T, Pos, A, Z).

:- dynamic
	known_breakpoint/4,		%
	break_id/1.

next_break_id(Id) :-
	retract(break_id(Id0)), !,
	Id is Id0+1,
	asserta(break_id(Id)).
next_break_id(1) :-
	asserta(break_id(1)).

%%	delete_breakpoint(+Id) is det.
%
%	Delete   breakpoint   with    given     Id.    If    successful,
%	print_message(breakpoint(delete, Id)) is called.   Message hooks
%	working on this message may still call breakpoint_property/2.
%
%	@error existence_error(breakpoint, Id).

delete_breakpoint(Id) :-
	integer(Id),
	known_breakpoint(ClauseRef, PC, _Location, Id), !,
	'$break_at'(ClauseRef, PC, false).
delete_breakpoint(Id) :-
	existence_error(breakpoint, Id).

%%	breakpoint_property(?Id, ?Property) is nondet.
%
%	True when Property is a property of the breakpoint Id.  Defined
%	properties are:
%
%	    * file(File)
%	    Provided if the breakpoint is in a clause associated to a
%	    file.  May not be known.
%	    * line_count(Line)
%	    Line of the breakpoint.  May not be known.
%	    * character_range(Start, Len)
%	    One-based character offset of the break-point.  May not be
%	    known.
%	    * clause(Reference)
%	    Reference of the clause in which the breakpoint resides.

breakpoint_property(Id, file(File)) :-
	known_breakpoint(ClauseRef,_,_,Id),
	clause_property(ClauseRef, file(File)).
breakpoint_property(Id, line_count(Line)) :-
	known_breakpoint(_,_,Location,Id),
	location_line(Location, Line).
breakpoint_property(Id, character_range(Start, Len)) :-
	known_breakpoint(ClauseRef,PC,_,Id),
	(   known_breakpoint(_,_,file_character_range(Start,Len),Id)
	;   break_location(ClauseRef, PC, _File, Start-End),
	    Len is End+1-Start
	).
breakpoint_property(Id, clause(Reference)) :-
	known_breakpoint(Reference,_,_,Id).

location_line(file_position(_File, Line, _Char), Line).
location_line(file_character_range(File, Start, _Len), Line) :-
	file_line(File, Start, Line).
location_line(file_line(_File, Line), Line).


%%	file_line(+File, +StartIndex, -Line) is det.
%
%	True when Line is the  1-based  line   offset  in  which we find
%	character StartIndex.

file_line(File, Start, Line) :-
	setup_call_cleanup(
	    open(File, read, In),
	    stream_line(In, Start, 1, Line),
	    close(In)).

stream_line(In, _, Line0, Line) :-
	at_end_of_stream(In), !,
	Line = Line0.
stream_line(In, Index, Line0, Line) :-
	skip(In, 0'\n),
	character_count(In, At),
	(   At > Index
	->  Line = Line0
	;   Line1 is Line0+1,
	    stream_line(In, Index, Line1, Line)
	).


		 /*******************************
		 *	      FEEDBACK		*
		 *******************************/

user:prolog_event_hook(break(ClauseRef, PC, Set)) :-
	break(Set, ClauseRef, PC).

break(true, ClauseRef, PC) :-
	known_breakpoint(ClauseRef, PC, _Location, Id), !,
	print_message(informational, breakpoint(set, Id)).
break(true, ClauseRef, PC) :- !,
	debug(break, 'Trap in Clause ~p, PC ~d~n', [ClauseRef, PC]),
	with_mutex('$break', next_break_id(Id)),
	(   break_location(ClauseRef, PC, File, A-Z)
	->  Len is Z+1-A,
	    Location = file_character_range(File, A, Len)
	;   clause_property(ClauseRef, file(File)),
	    clause_property(ClauseRef, line_count(Line))
	->  Location = file_line(File, Line)
	;   Location = unknown
	),
	asserta(known_breakpoint(ClauseRef, PC, Location, Id)),
	print_message(informational, breakpoint(set, Id)).
break(false, ClauseRef, PC) :-
	clause(known_breakpoint(ClauseRef, PC, _Location, Id), true, Ref),
	call_cleanup(print_message(informational, breakpoint(delete, Id)),
		     erase(Ref)).


%%	break_location(+ClauseRef, +PC, -File, -AZ) is det.
%
%	True when File and AZ represent the  location of the goal called
%	at PC in ClauseRef.
%
%	@param AZ is a term A-Z, where   A and Z are character positions
%	in File.

break_location(ClauseRef, PC, File, A-Z) :-
	clause_info(ClauseRef, File, TermPos, _NameOffset),
	'$fetch_vm'(ClauseRef, PC, NPC, _VMI),
	'$clause_term_position'(ClauseRef, NPC, List),
	debug(break, 'ClausePos = ~w~n', [List]),
	range(List, TermPos, A, Z),
	debug(break, 'Range: ~d .. ~d~n', [A, Z]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(breakpoint(SetClear, Id)) -->
	setclear(SetClear),
	breakpoint(Id).

setclear(set) -->
	['Breakpoint '].
setclear(delete) -->
	['Deleted breakpoint '].

breakpoint(Id) -->
	breakpoint_name(Id),
	(   { breakpoint_property(Id, file(File)),
	      file_base_name(File, Base),
	      breakpoint_property(Id, line_count(Line))
	    }
	->  [ ' at ~w:~d'-[Base, Line] ]
	;   []
	).

breakpoint_name(Id) -->
	{ breakpoint_property(Id, clause(ClauseRef)),
	  clause_name(ClauseRef, Name)
	},
	['~w in ~w'-[Id, Name]].


