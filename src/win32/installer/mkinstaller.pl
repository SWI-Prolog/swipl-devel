/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- prolog_load_context(directory, Dir),
   working_directory(_, Dir).

name :-
	version(Major, Minor, Patch),
	get_time(X), convert_time(X, Date),
	format('Name "SWI-Prolog ~w.~w.~w (~s)"~n',
	       [Major, Minor, Patch, Date]).

outfile :-
	version(Major, Minor, Patch),
	format('OutFile "w32pl~w~w~w.exe"~n',
	       [Major, Minor, Patch]).


version(Major, Minor, Patch) :-
	current_prolog_flag(version, V),
	Major is V//10000,
	Minor is V//100 mod 100,
	Patch is V mod 100.

run :-
	tell('version.nsi'),
	name,
	outfile,
	told.

		 /*******************************
		 *	       CHECK		*
		 *******************************/

:- dynamic
	install_file/1,
	install_dir/1.

parse_script(Script) :-
	retractall(install_file(_)),
	retractall(install_dir(_)),
	open(Script, read, In),
	read_line_to_codes(In, Line0),
	process_file_decls(Line0, In),
	close(In).

process_file_decls(end_of_file, _) :- !.
process_file_decls(Line, In) :-
%	format('~s~n', [Line]),
	phrase(process_file_decl, Line),
	read_line_to_codes(In, Line1),
	process_file_decls(Line1, In).

process_file_decl -->
	ws, "File", blank, ws, !,
	(   "/r", ws
	->  path(Dir),
	    { assert(install_dir(Dir))
	    }
	;   "/oname="
	->  path(_Oname),
	    blank, ws,
	    path(File)
	;   path(File),
	    { assert(install_file(File))
	    }
	),
	ws.
process_file_decl -->
	string(_),
	eos.
	
path(Path) -->
	token(Token),
	{ prolog_to_os_filename(Path, Token)
	}.
	
token(Value) -->
	(   "\""
	->  string(Codes),
	    "\""
	;   "'"
	->  string(Codes),
	    "'"
	;   string(Codes),
	    sep
	), !,
	{ name(Value, Codes)
	}.

sep -->
	peek_blank, !.
sep -->
	eos.

ws -->
	blank, !,
	ws.
ws -->
	[].

blank -->
	[C],
	{ nonvar(C),
	  code_type(C, space)
	}.

string(String, In, Rest) :-
	append(String, Rest, In).

eos([], []).

peek_blank -->
	peek(C),
	{ code_type(C, space)
	}.

peek(C, X, X) :-
	X = [C|_].

%	check_covered(+Dir)
%	
%	See whether all files in Dir are covered by some install
%	instruction.

check_covered([]) :- !.
check_covered([H|T]) :- !,
	check_covered(H),
	check_covered(T).
check_covered(Dir) :-
	exists_directory(Dir), !,
	(   install_dir(D),
	    same_file(Dir, D)
	->  already_covered(D)
	;   atom_concat(Dir, '/*', Pattern),
	    expand_file_name(Pattern, Entries),
	    check_covered(Entries)
	).
check_covered(File) :-
	install_file(F),
	same_file(F, File), !.
check_covered(Path) :-
	ignore_file(File),
	file_base_name(Path, File), !.
check_covered(File) :-
	flag(errors, E, E+1),
	print_message(error, format('File ~w is not covered by installer',
				    [File])).

already_covered(Dir) :-
	(   install_file(File),
	    atom_concat(Dir, X, File),
	    sub_atom(X, 0, _, _, /),
	    flag(errors, E, E+1),
	    print_message(error, format('File ~w already covered by ~w',
					[File, Dir])),
	    fail
	;   true
	).

check_files :-
	parse_script('pl.nsi'),
	flag(errors, Old, 0),
	check_covered(pl),
	flag(errors, New, Old),
	New == 0.

ignore_file('INDEX.pl').

		 /*******************************
		 *	       RUN IT		*
		 *******************************/

main :-
	(   run, check_files, halt
	;   halt(1)
	).

% :- main.
	
