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
	    noguitracer/0		% Switch it off
	  ]).

guitracer :-
	current_prolog_flag(gui_tracer, true), !.
guitracer :-
	current_prolog_flag(gui_tracer, _), !,
	set_prolog_flag(gui_tracer, true).
guitracer :-
	format(user_error, 'Loading XPCE GUI tracer ...', []),
	load_files([library('trace/trace')], [silent(true)]),
	format(user_error, ' loaded!~n', []),
	format(user_error,
	       'The graphical front-end will be used for subsequent tracing.~n', []).

noguitracer :-
	current_prolog_flag(gui_tracer, _), !,
	set_prolog_flag(gui_tracer, false).
noguitracer.

