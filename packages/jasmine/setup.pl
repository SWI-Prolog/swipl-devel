/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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
	progress(cpbin(Src, Lib)).

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
