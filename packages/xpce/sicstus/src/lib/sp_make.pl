/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(sicstus_make,
	  [ make/0
	  ]).

%	make/0
%
%	Recompile any modified file.  Should load into the same module the
%	file was originally loaded from!

make :-
	source_file(File),
	ensure_loaded(File),
	fail.
make.
