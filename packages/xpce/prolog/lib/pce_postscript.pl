/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_postscript,
	  [ postscript/2		% Object x file
	  ]).
:- use_module(library(pce)).


postscript(Obj, File) :-
	get(File, ensure_suffix, '.ps', FileName),
	new(F, file(FileName)),
	send(F, open, write),
	send(F, append, Obj?postscript),
	send(F, format, 'showpage\n'),
	send(F, close),
	get(F, size, Size),
	format('PostScript written to ~w, ~D bytes~n', [FileName,Size]).
