/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_postscript,
	  [ postscript/2		% Object x file
	  ]).
:- use_module(library(pce)).


postscript(Obj, File) :-
	get(File, ensure_suffix, '.eps', FileName),
	new(F, file(FileName)),
	send(F, open, write),
	send(F, append, Obj?postscript),
	send(F, format, 'showpage\n'),
	send(F, close),
	get(F, size, Size),
	format('PostScript written to ~w, ~D bytes~n', [FileName,Size]).
