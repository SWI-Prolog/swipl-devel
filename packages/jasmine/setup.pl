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

lib(jasmine).
dll(jasmine).

install :-
	install_library,
	install_dlls.

install_library :-
	forall(lib(Base), install_lib(Base)),
	lib_dest(Dest),
	make_library_index(Dest).

install_lib(Base) :-
	lib_dest(Lib),
	absolute_file_name(Base,
			   [ extensions([pl]),
			     access(read)
			   ],
			   Src),
	progress(cp(Src, Lib)).

lib_dest(Lib) :-
	current_prolog_flag(home, PlHome),
	concat_atom([PlHome, library], /, Lib).

install_dlls :-
	forall(dll(Base), install_dll(Base)).

install_dll(Base) :-
	dll_dest(Dir),
	absolute_file_name(Base,
			   [ extensions([dll]),
			     access(read)
			   ],
			   Src),
	progress(cpbin(Src, Dir)).

dll_dest(Dir) :-
	current_prolog_flag(home, PlHome),
	concat_atom([PlHome, bin], /, Dir).


%	cp(From, To)
%
%	Copy a file to a destination (file or directory).

cp(Src, Dest) :-
	cp(Src, Dest, [type(text)]).

cpbin(Src, Dest) :-
	cp(Src, Dest, [type(binary)]).

cp(Src, Dir, Options) :-
	exists_directory(Dir), !,
	file_base_name(Src, Base),
	concat_atom([Dir, Base], /, Dest),
	cp(Src, Dest, Options).
cp(Src, Dest, Options) :-
	open(Src, read, In, Options),
	open(Dest, write, Out, Options),
	copy_stream_data(In, Out),
	close(Out),
	close(In).

progress(Goal) :-
	format('~p ... ', [Goal]),
	flush_output,
	(   catch(Goal, E, (print_message(error, E), fail))
	->  format('ok~n', [])
	;   format('FAILED~n', [])
	).

		 /*******************************
		 *	     ACTIVATE		*
		 *******************************/

:- (   install
   ->  format('~N~nInstallation complete~n~n', [])
   ;   format('~N~nINSTALLATION FAILED~n~n', [])
   ),
   format('Press any key to continue ...', []), flush_output,
   get_single_char(_),
   nl,
   halt.
