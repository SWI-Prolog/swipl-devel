/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(pce_draw,
	  [ pcedraw/0,			% start pcedraw
	    pcedraw/1,			% and load a file into it
	    save_pcedraw/1		% save to a file
	  ]).

:- use_module(library(pce)).
:- require([ file_name_extension/3
	   , is_list/1
	   , member/2
	   ]).

:- consult(library('draw/draw')).

pcedraw :-
	draw.

pcedraw([]) :- !,
	draw.
pcedraw(Files) :-
	is_list(Files), !,
	(   member(File, Files),
	    draw(File),
	    fail
	;   true
	).
pcedraw(File) :-
	draw(File).

pce_ifhostproperty(prolog(swi),
(   save_pcedraw(File) :-
	(   feature(windows, true)
	->  file_name_extension(File, exe, Exe)
	;   Exe = File
	),
	pce_autoload_all,
	pce_autoload_all,
	qsave_program(Exe,
		      [ goal=pce_main_loop(pcedraw),
			stand_alone=true
		      ])),
(   save_pcedraw(File) :-
	format(user_error, 'SWI-Prolog only~n', []))).
