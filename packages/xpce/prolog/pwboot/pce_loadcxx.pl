/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(pce_load_cxx,
	  [ pce_load_cxx/1		% +File
	  ]).
:- use_module(library(pce)).

:- dynamic
	foreign_file/2.

pce_load_cxx(File) :-
	retractall(foreign_file(_,_)),	% play save
	asserta(foreign_file(File, [])),
	send(@pce, succeed),		% ensure XPCE is loaded
	load_foreign_executable(File),
	retractall(foreign_file(_,_)).
