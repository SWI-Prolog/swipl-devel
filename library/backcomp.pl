/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(backward_compatibility,
	  [ '$arch'/2,
	    '$version'/1,
	    '$home'/1
	  ]).

'$arch'(Arch, unknown) :-
	feature(arch, Arch).

'$version'(Version) :-
	feature(version, Version).

'$home'(Home) :-
	feature(home, Home).
