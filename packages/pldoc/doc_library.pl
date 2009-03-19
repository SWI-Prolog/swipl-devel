/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(pldoc_library,
	  [ doc_load_library/0
	  ]).

%%	doc_load_library
%
%	Load the SWI-Prolog library, so we can access all comments from
%	the library.

doc_load_library :-
	set_prolog_flag(verbose_load, false),
	absolute_file_name(swi(library), Dir, [file_type(directory)]),
	load_all(Dir).


load_all([]) :- !.
load_all([H|T]) :-
	load_all(H), !,
	load_all(T).
load_all(Dir0) :-
	atom(Dir0),
	expand_file_name(Dir0, [Dir1]),
	downcase_atom(Dir1, Dir),	% Deal with Windows
	\+ ( blocked(Blocked),
	     sub_atom(Dir, _, _, 0, Blocked)
	   ),
	exists_directory(Dir), !,
	atom_concat(Dir, '/*', Pattern),
	expand_file_name(Pattern, Contents),
	load_all(Contents).
load_all(File) :-
	atom(File),
	file_name_extension(_, pl, File),
	downcase_atom(File, LwrCase),
	\+ ( blocked(Blocked),
	     sub_atom(LwrCase, _, _, 0, Blocked)
	   ), !,
	use_module(File, []).
load_all(Spec) :-
	compound(Spec), !,
	forall(absolute_file_name(Spec, Path,
				  [ access(read),
				    file_errors(fail)
				  ]),
	       load_all(Path)).
load_all(_).

%%	blocked(+Path) is semidet.
%
%	True if file or directory should not   be loaded. Note that file
%	from the directory chr are  already   loaded  by chr.pl. Similar
%	arguments apply for a few others.
%
%	@bug	We force lowercase to make it also work on Windows

blocked('/chr').
blocked('/clpq').
blocked('/clpr').
blocked('/pldoc').
blocked('/checkselect.pl').
blocked('/checklast.pl').
%blocked('/jpl.pl').			% should be added
blocked('/pldoc.pl').
blocked('/index.pl').

blocked('/ciao.pl').			% is an include-file.  We must
					% find a more general solution here
blocked('/commons.pl').
