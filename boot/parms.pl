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

:- module('$parms', []).

:- multifile
	user:library_directory/1.
:- dynamic
	user:library_directory/1.

:- dynamic
	library_directory_cache/2.
:- volatile
	library_directory_cache/2.

user:library_directory(Dir) :-
	cached_library_directory(local,
				 Dir0=lib,
				 Dir0),
	Dir = Dir0.
user:library_directory(Dir) :-
	cached_library_directory(user,
				 expand_file_name('~/lib/prolog', [Dir0]),
				 Dir0),
	Dir = Dir0.
user:library_directory(Dir) :-
	cached_library_directory(system,
				 absolute_file_name(swi(library), Dir0),
				 Dir0),
	Dir = Dir0.

cached_library_directory(CacheName, _, Dir) :-
	library_directory_cache(CacheName, Dir), !,
	Dir \== [].
cached_library_directory(CacheName, Goal, Dir) :-
	catch(Goal, _, fail),
	exists_directory(Dir), !,
	asserta(library_directory_cache(CacheName, Dir)).
cached_library_directory(CacheName, _, _) :-
	asserta(library_directory_cache(CacheName, [])),
	fail.
		
$default_editor(notepad) :-
	current_prolog_flag(windows, true), !.
$default_editor(vi).
