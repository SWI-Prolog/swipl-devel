/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

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
:- use_module(library(lists)).
:- set_prolog_flag(generate_debug_info, false).

/** <module>  Reload modified source files

This module provides the SWI-Prolog   `make'  facility that synchronises
Prolog internal database after loaded files have been edited.

@bug	Dependency tracking is incomplete.  Notably, there is no
	dependency tracking if compilation of one module depends
	on goal_expansion/2 or term_expansion/2 rules provided by
	another.
*/

%%	make
%
%	Reload all source files that have been changed since they were
%	loaded.  After loading make/0 runs list_undefined/0 to quickly
%	scan for undefined predicates.

make :-
	notrace(make_no_trace).

make_no_trace :-
	'$update_library_index',
	findall(File, modified_file(File), Reload),
	print_message(silent, make(reload(Reload))),
	reload(Reload),
	print_message(silent, make(done(Reload))),
	list_undefined([scan(local)]).

modified_file(File) :-
	source_file_property(Source, modified(Time)),
	Time > 0.0,			% See source_file/1
	(   source_file_property(Source, derived_from(File, LoadTime))
	->  true
	;   File = Source,
	    LoadTime = Time
	),
	(   catch(time_file(File, Modified), _, fail),
	    Modified > LoadTime
	->  true
	;   source_file_property(Source, includes(Included, IncLoadTime)),
	    catch(time_file(Included, Modified), _, fail),
	    Modified > IncLoadTime
	->  true
	).


reload([]).
reload([H|T]) :-
	reload_file(H),
	reload(T).

%%	reload_file(File)
%
%	Reload file into the proper module.
%
%	@bug	If the module was loaded using use_module/2, importing only
%		some of the predicates, this is not know.
%	@bug	If modules import each other, we must load them in the
%		proper order for import/export dependencies.

:- public reload_file/1.		% Used by PDT

reload_file(File) :-
	source_base_name(File, Compile),
	findall(M, source_file_property(File, load_context(M, _)), Modules),
	(   Modules = [First|Rest]
	->  load_files(First:Compile),
	    forall(member(Context, Rest),
		   load_files(Context:Compile, [if(not_loaded)]))
	;   load_files(user:Compile)
	).

source_base_name(File, Compile) :-
	file_name_extension(Compile, Ext, File),
	user:prolog_file_type(Ext, prolog), !.
source_base_name(File, File).
