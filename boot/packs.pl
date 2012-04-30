/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301 USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module('$pack',
	  [ attach_packs/0,
	    attach_packs/1			% +Dir
	  ]).

:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.

:- dynamic
	pack_dir/3,				% Pack, Type, Dir
	pack/2.					% Pack, BaseDir


user:file_search_path(pack, app_data(pack)).
user:file_search_path(pack, swi(pack)).

user:file_search_path(library, PackLib) :-
	pack_dir(_Name, prolog, PackLib).
user:file_search_path(foreign, PackLib) :-
	pack_dir(_Name, foreign, PackLib).

%%	attach_packs
%
%	Attach packages from all package directories.

attach_packs :-
	findall(PackDir, absolute_file_name(pack(.), PackDir,
					    [ file_type(directory),
					      access(read),
					      solutions(all)
					    ]),
		PackDirs),
	remove_dups(PackDirs, UniquePackDirs, []),
	forall('$member'(PackDir, UniquePackDirs),
	       attach_packs(PackDir)).

%%	remove_dups(+List, -Unique, +Seen) is det.
%
%	Remove duplicates from List, keeping the first solution.

remove_dups([], [], _).
remove_dups([H|T0], T, Seen) :-
	memberchk(H, Seen), !,
	remove_dups(T0, T, Seen).
remove_dups([H|T0], [H|T], Seen) :-
	remove_dups(T0, T, [H|Seen]).


%%	attach_packs(+Dir)
%
%	Attach packages from directory Dir.

attach_packs(Dir) :-
	directory_files(Dir, Entries),
	ensure_slash(Dir, SDir),
	attach_packages(Entries, SDir).

attach_packages([], _).
attach_packages([H|T], Dir) :-
	attach_package(H, Dir),
	attach_packages(T, Dir).

attach_package(Entry, Dir) :-
	\+ special(Entry),
	atomic_list_concat([Dir, Entry, '/pack.pl'], InfoFile),
	access_file(InfoFile, read),
	check_existing(Entry, Dir),
	foreign_dir(Entry, Dir, ForeignDir),
	prolog_dir(Entry, Dir, PrologDir),
	atom_concat(Dir, Entry, BaseDir),
	assertz(pack(Entry, BaseDir)),
	assertz(pack_dir(Entry, prolog, PrologDir)),
	(   ForeignDir \== (-)
	->  assertz(pack_dir(Entry, foreign, ForeignDir))
	;   true
	),
	print_message(silent, pack(attached(Entry, BaseDir))).
attach_package(_, _).

special(.).
special(..).

%%	check_existing(+Pack, +PackDir) is semidet.
%
%	Verify that we did not load this package before.

check_existing(Entry, Dir) :-
	retract(pack(Entry, Dir)), !,		% registered from same place
	retractall(pack_dir(Entry, _, _)).
check_existing(Entry, Dir) :-
	pack(Entry, OldDir), !,
	print_message(warning, pack(duplicate(Entry, OldDir, Dir))),
	fail.
check_existing(_, _).


prolog_dir(Entry, Dir, PrologDir) :-
	atomic_list_concat([Dir, Entry, '/prolog'], PrologDir),
	exists_directory(PrologDir).

foreign_dir(Entry, Dir, ForeignDir) :-
	current_prolog_flag(arch, Arch),
	atomic_list_concat([Dir, Entry, '/bin'], ForeignBaseDir),
	exists_directory(ForeignBaseDir), !,
	atomic_list_concat([Dir, Entry, '/bin/', Arch], ForeignDir),
	(   exists_directory(ForeignDir)
	->  assertz(pack_dir(Entry, foreign, ForeignDir))
	;   print_message(warning, pack(no_arch(Entry, ForeignBaseDir))),
	    fail
	).
foreign_dir(_, _, (-)).

ensure_slash(Dir, SDir) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  SDir = Dir
	;   atom_concat(Dir, /, SDir)
	).
