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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pce_load_cxx(+Spec)
    Load a Unix shared object or Windows DLL or somilar object, containing
    XPCE/C++ code.  This call deals with OS and Prolog incompatibilities. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_load_cxx(File) :-
	feature(open_shared_object, true), !,
	absolute_file_name(File,
			   [ extensions([so]),
			     access(read)
			   ],
			   Path),
	send(@pce, succeed),		% ensure XPCE is loaded
	open_shared_object(Path, _Handle).
pce_load_cxx(File) :-
	feature(dll, true), !,
	absolute_file_name(File,
			   [ extensions([dll]),
			     access(read)
			   ],
			   Path),
	send(@pce, succeed),		% ensure XPCE is loaded
	open_dll(Path, _Handle).
