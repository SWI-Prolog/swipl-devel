/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
