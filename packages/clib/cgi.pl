/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amseterdam. All rights reserved.
*/

:- module(cgi,
	  [ cgi_get_form/1		% -ListOf Name(Value)
	  ]).
:- use_module(library(shlib)).

:- initialization
   load_foreign_library(foreign(cgi), install_cgi).
