/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(pce_help_file,
	  [ pce_help_file/2,
	    pce_help/2,
	    pce_registered_help_file/2
	  ]).
:- use_module(library(pce)).

:- pce_autoload(helper, library(pce_helper)).
:- pce_global(@helper, new(helper)).

:- dynamic
	pce_registered_help_file/2.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is closely  connected  to   library(pce_helper).   It  is  a
separate module to allow the execution of the directive pce_help_file/2,
to register a new help database without   forcing the entire help system
to be loaded.

Loading this module (normally through the   autoloader  or the require/1
directive)  will  make  the  necessary    pce_global   and  pce_autoload
declarations to load the help-system itself as soon as it is referenced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	pce_help_file(+DataBaseId, +FileName).
%
%	Declare `FileName' to hold a helper-format file holding the
%	help-database `DataBaseId'.  FileName will be converted into
%	an absolute filename.  Normally used as a directive.

pce_help_file(Id, FileName) :-
	get(file(FileName), absolute_path, Path),
	retractall(pce_registered_help_file(Id, _)),
	assert(pce_registered_help_file(Id, Path)).

%	pce_help(+DataBaseId, +Label)
%	
%	Start @helper/helper on the help module `DataBaseId', searching
%	for a fragment with label `Label'.  Normally invoked through the
%	send directly.

pce_help(DataBase, Label) :-
	send(@helper, give_help, DataBase, Label).

