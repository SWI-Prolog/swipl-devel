/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(doc_util,
	  [ insert_alias/2,		% +Path, -Aliased
	    expand_alias/2,		% +Aliased, -Path
	    ensure_slash_end/2		% +Dir, -DirSlash
	  ]).

/** <module> PlDoc utilities

@author Jan Wielemaker
*/

		 /*******************************
		 *     PATH ALIAS PROCESSING	*
		 *******************************/

%%	insert_alias(+Path0, -Path) is det.
%
%	Translate a native path to  an   aliased  path. Path aliases are
%	defined by path_alias/2. Aliased paths   are  re-translated into
%	native form using expand_alias/2.

insert_alias(Path0, Path) :-
	path_alias(Alias, Prefix),
	atom_concat(Prefix, PostFix, Path0), !,
	atom_concat(Alias, PostFix, Path).
insert_alias(Path, Path).


%%	expand_alias(+Path0, -Path) is det.
%
%	Translate an aliased path to a native path.  

expand_alias(Path0, Path) :-
	path_alias(Alias, Prefix),
	atom_concat(Alias, Postfix, Path0), !,
	atom_concat(Prefix, Postfix, Path).
expand_alias(Path, Path).
	    

%%	path_alias(?Alias, ?Path) is nondet.
%
%	True if Alias: is an alias  for   Path.  This is used to rewrite
%	paths below the SWI-Prolog home to   give them shorter and fixed
%	names.

path_alias('/swi/', Dir) :-
	current_prolog_flag(home, Dir0),
	ensure_slash_end(Dir0, Dir).


		 /*******************************
		 *	MISC PATH OPERATIONS	*
		 *******************************/

%%	ensure_slash_end(+Dir, -DirSlash) is det.
%
%	Ensure Dir ends with a /.

ensure_slash_end(Dir, Dir) :-
	sub_atom(Dir, _, _, 0, /), !.
ensure_slash_end(Dir0, Dir) :-
	atom_concat(Dir0, /, Dir).

