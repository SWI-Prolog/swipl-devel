/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- prolog_load_context(directory, Here),
   concat(Here, '../../prolog/lib', RawLib),
   absolute_file_name(RawLib, Lib),
   assert(library_directory(Lib), Ref),
   assert('$libref'(Ref)),
   '$autoload':clear_library_index.

:- consult(library(pce)).

:- retract('$libref'(Ref)),
   erase(Ref).

library_directory(Lib) :-
	get(@pce, home, Home),
	concat(Home, '/prolog/lib', RawLib),
	absolute_file_name(RawLib, Lib).
