/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(gui_tracer,
	  [ guitracer/0,
	    noguitracer/0,		% Switch it off
	    prolog_break_at/3		% +File, +Line, +Pos
	  ]).

guitracer :-
	current_prolog_flag(gui_tracer, true), !.
guitracer :-
	current_prolog_flag(gui_tracer, _), !,
	set_prolog_flag(gui_tracer, true),
	print_message(informational, gui_tracer(true)).
guitracer :-
	load_files([library('trace/trace')], [silent(true)]),
	print_message(informational, gui_tracer(true)).

noguitracer :-
	current_prolog_flag(gui_tracer, true), !,
	set_prolog_flag(gui_tracer, false),
	print_message(informational, gui_tracer(false)).
noguitracer.

%	prolog_break_at(+File, +Line, +Pos)
%
%	Set a Prolog break-point.  Used to set break-points from PceEmacs.

prolog_break_at(File, Line, Pos) :-
	guitracer,
	prolog_break:break_at(File, Line, Pos).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(gui_tracer(true)) -->
	['The graphical front-end will be used for subsequent tracing'].
prolog:message(gui_tracer(false)) -->
	['Subsequent tracing uses the commandline tracer'].
