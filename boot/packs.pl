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
	    attach_packs/1,			% +Dir
	    '$pack_detach'/2,			% +Name, -Dir
	    '$pack_attach'/1			% +Dir
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

%%	'$pack_detach'(+Name, -Dir) is det.
%
%	Detach the given package  from  the   search  paths  and list of
%	registered packages, but does not delete the files.

'$pack_detach'(Name, Dir) :-
	(   atom(Name)
	->  true
	;   throw(error(type_error(atom, Name), _))
	),
	(   retract(pack(Name, Dir))
	->  retractall(pack_dir(Name, _, _)),
	    reload_library_index
	;   throw(error(existence_error(pack, Name), _))
	).

%%	'$pack_attach'(+Dir) is det.
%
%	Attach the given package

'$pack_attach'(Dir) :-
	attach_package(Dir), !.
'$pack_attach'(Dir) :-
	(   exists_directory(Dir)
	->  throw(error(existence_error(directory, Dir), _))
	;   throw(error(domain_error(pack, Dir), _))
	).

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
	(   PackDirs \== []
	->  remove_dups(PackDirs, UniquePackDirs, []),
	    forall('$member'(PackDir, UniquePackDirs),
		   attach_packs(PackDir))
	;   true
	).

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
	catch(directory_files(Dir, Entries), _, fail), !,
	ensure_slash(Dir, SDir),
	attach_packages(Entries, SDir).
attach_packs(_).

attach_packages([], _).
attach_packages([H|T], Dir) :-
	attach_package(H, Dir),
	attach_packages(T, Dir).

attach_package(Entry, Dir) :-
	\+ special(Entry),
	atom_concat(Dir, Entry, PackDir),
	attach_package(PackDir), !.
attach_package(_, _).

special(.).
special(..).


%%	attach_package(+PackDir) is semidet.
%
%	@tbd	Deal with autoload index.  Reload?

attach_package(PackDir) :-
	atomic_list_concat([PackDir, '/pack.pl'], InfoFile),
	access_file(InfoFile, read),
	file_base_name(PackDir, Pack),
	check_existing(Pack, PackDir),
	foreign_dir(Pack, PackDir, ForeignDir),
	prolog_dir(PackDir, PrologDir), !,
	assertz(pack(Pack, PackDir)),
	assertz(pack_dir(Pack, prolog, PrologDir)),
	update_autoload(PrologDir),
	(   ForeignDir \== (-)
	->  assertz(pack_dir(Pack, foreign, ForeignDir))
	;   true
	),
	print_message(silent, pack(attached(Pack, PackDir))).


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


prolog_dir(PackDir, PrologDir) :-
	atomic_list_concat([PackDir, '/prolog'], PrologDir),
	exists_directory(PrologDir).

update_autoload(PrologDir) :-
	atom_concat(PrologDir, '/INDEX.pl', IndexFile),
	(   exists_file(IndexFile)
	->  reload_library_index
	;   true
	).

foreign_dir(Pack, PackDir, ForeignDir) :-
	current_prolog_flag(arch, Arch),
	atomic_list_concat([PackDir, '/lib'], ForeignBaseDir),
	exists_directory(ForeignBaseDir), !,
	atomic_list_concat([PackDir, '/lib/', Arch], ForeignDir),
	(   exists_directory(ForeignDir)
	->  assertz(pack_dir(Pack, foreign, ForeignDir))
	;   print_message(warning, pack(no_arch(Pack, Arch))),
	    fail
	).
foreign_dir(_, _, (-)).

ensure_slash(Dir, SDir) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  SDir = Dir
	;   atom_concat(Dir, /, SDir)
	).
