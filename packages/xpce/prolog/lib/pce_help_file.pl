/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pce_help_file,
	  [ pce_help_file/2,
	    pce_help/2
	  ]).
:- use_module(library(pce)).
:- require([ concat_atom/2
	   , is_absolute_file_name/1
	   ]).

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(pce_help,	pce('appl-help')).

:- pce_autoload(helper, library(pce_helper)).
:- pce_global(@helper, new(helper)).

:- dynamic
	resource/3.

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
	(   atom(FileName),
	    \+ is_absolute_file_name(FileName)
	->  prolog_load_context(directory, Cwd),
	    concat_atom([Cwd, /, FileName], Path)
	;   Path = FileName
	),
	retractall(resource(Id, help, Path)),
	asserta(resource(Id, help, Path)).

%	pce_help(+DataBaseId, +Label)
%	
%	Start @helper/helper on the help module `DataBaseId', searching
%	for a fragment with label `Label'.  Normally invoked through the
%	send directly.

pce_help(DataBase, Label) :-
	send(@helper, give_help, DataBase, Label).

