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

:- module(edinburgh,
	  [ display/1,
	    display/2,
	    unknown/2,
	    reconsult/1,
	    debug/0,
	    nodebug/0,
	    fileerrors/2
	  ]).

:- meta_predicate
	unknown(:, :),
	reconsult(:).


/** <module> Some traditional Edinburgh predicates

This module defines  predicates  from   `traditional  Edinburgh  Prolog'
(Dec10 and C-Prolog) whose functionality  has   been  replaced  by (ISO)
Standard Prolog.
*/

		 /*******************************
		 *	      TERM I/O		*
		 *******************************/

%%	display(+Term) is det.
%%	display(+Stream, +Term) is det.
%
%	Write a term, ignoring operators.
%
%	@deprecated	New code must use write_term/3 using the option
%			ignore_ops(true).

display(Term) :-
	write_term(Term, [ignore_ops(true)]).
display(Stream, Term) :-
	write_term(Stream, Term, [ignore_ops(true)]).

%%	unknown(-Old, +New) is det.
%
%	Edinburgh Prolog predicate for dealing dealing with undefined
%	procedures

unknown(M:Old, M:New) :-
	current_prolog_flag(M:unknown, O),
	map_unknown(O, Old),
	map_unknown(N, New), !,
	set_prolog_flag(M:unknown, N).

map_unknown(error,   trace).
map_unknown(warning, trace).
map_unknown(fail,    fail).

%%	reconsult(+FileOrList) is det.
%
%	Load source file(s), wiping the  old content first. SWI-Prolog's
%	consult/1 and related predicates always do this.
%
%	@deprecated The Edinburgh Prolog consult/reconsult distinction
%	is no longer used throughout most of the Prolog world.

reconsult(File) :-
	consult(File).

%%	debug is det.
%%	nodebug is det.
%
%	Switch on/off debug mode.  Note that nodebug/0 has been defined
%	such that is is not traced itself.

debug	:- set_prolog_flag(debug, true).
nodebug :- notrace, set_prolog_flag(debug, false).

:- '$hide'(nodebug/0).

%%	fileerrors(-Old, +New) is det.
%
%	Query and change the  fileerrors  flag.   Default  it  is set to
%	=true=, causing file operations to   raise an exception. Setting
%	it to =false=  activates  the  old   Edinburgh  mode  of  silent
%	failure.
%
%	@deprecated	New code should use catch/3 to handle file errors
%			silently

fileerrors(Old, New) :-
	current_prolog_flag(fileerrors, Old),
	(   Old == New
	->  true
	;   set_prolog_flag(fileerrors, New)
	).
