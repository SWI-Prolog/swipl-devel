/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
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
	format(user_error, ' loaded!~n', []).

noguitracer :-
	current_prolog_flag(gui_tracer, _), !,
	set_prolog_flag(gui_tracer, false).
noguitracer.

