/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(make,
	  [ make/0
	  ]).
:- use_module(library(check)).

:- system_module.


		/********************************
		*              MAKE             *
		*********************************/

%	make/0
%	
%	Reload all source files that have been changed since they were
%	loaded.

make :-
	'$update_library_index',
	findall(File, modified_file(File), Reload),
	print_message(silent, make(reload(Reload))),
	reload(Reload),
	print_message(silent, make(done)),
	list_undefined.

modified_file(File) :-
	'$time_source_file'(Source, Time),
	(   '$derived_source'(Source, File, LoadTime)
	->  true
	;   File = Source,
	    LoadTime = Time
	),
	time_file(File, Modified),
	Modified > LoadTime.

reload([]).
reload([H|T]) :-
	reload_file(H),
	reload(T).

%	reload_file(File)
%
%	Reload file into the proper module.  Note that if the file is loaded
%	into multiple modules this should be handled more carefully.

reload_file(File) :-
	findall(Context, $load_context_module(File, Context), Modules),
	(   Modules = []
	->  consult(user:File)
	;   Modules = [Module]
	->  consult(Module:File)
	;   Modules = [First|_Rest],
	    consult(First:File)
	).
	
