/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Run latex2html without installing it.  Usage:

	% pl -s path/to/run.pl -g main -- file
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

setup :-
	prolog_load_context(directory, Dir),
	file_directory_name(Dir, Parent),
	asserta(user:file_search_path(ltx2html, Dir)),
	asserta(user:file_search_path(foreign, Dir)),
	asserta(user:file_search_path(library, Parent)), % find library(pldoc)
	asserta(user:file_search_path(library, Dir)).

:- setup.
:- load_files(latex2html, [silent(true)]).

main :-
	current_prolog_flag(argv, Argv),
	append(_, [--,File], Argv), !,
	(   process(File)
	->  halt
	;   halt(1)
	).
main :-
	format(user_error, 'Usage: script options -- file~n', []),
	halt(1).

process(File) :-
	exists_file(File),
	file_name_extension(Base, tex, File), !,
	latex2html(Base).
process(Base) :-
	file_name_extension(Base, tex, File),
	exists_file(File), !,
	latex2html(Base).



