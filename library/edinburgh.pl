/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/

:- module(edinburgh,
	  [ display/1,
	    display/2,
	    unknown/2,
	    debug/0,
	    nodebug/0
	  ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines  predicates  from   `traditional  Edinburgh  Prolog'
(Dec10 and C-Prolog) whose functionality  has   been  replaced  by (ISO)
Standard Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	      TERM I/O		*
		 *******************************/

%	display(+Term)
%
%	Write a term, ignoring operators.

display(Term) :-
	write_term(Term, [ignore_ops(true)]).
display(Stream, Term) :-
	write_term(Stream, Term, [ignore_ops(true)]).

		 /*******************************
		 *	      DEBUGGING		*
		 *******************************/

%	unknown(-Old, +New)
%
%	Edinburgh Prolog predicate for dealing dealing with undefined
%	procedures

:- module_transparent
	unknown/2.

unknown(Old, New) :-
	context_module(M),
	current_prolog_flag(M:unknown, O),
	map_unknown(O, Old),
	map_unknown(N, New), !,
	set_prolog_flag(M:unknown, N).

map_unknown(error,   trace).
map_unknown(warning, trace).
map_unknown(fail,    fail).

%	debug
%	nodebug
%
%	Switch on/off debug mode

debug	:- set_prolog_flag(debug, true).
nodebug :- set_prolog_flag(debug, false).
