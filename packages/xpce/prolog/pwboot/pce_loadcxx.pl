/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
