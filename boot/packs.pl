/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2015, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module('$pack',
          [ attach_packs/0,
            attach_packs/1,                     % +Dir
            '$pack_detach'/2,                   % +Name, -Dir
            '$pack_attach'/1                    % +Dir
          ]).

:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.

:- dynamic
    pack_dir/3,                             % Pack, Type, Dir
    pack/2.                                 % Pack, BaseDir
:- volatile
    pack_dir/3,
    pack/2.


user:file_search_path(pack, app_data(pack)).
user:file_search_path(pack, swi(pack)).

user:file_search_path(library, PackLib) :-
    pack_dir(_Name, prolog, PackLib).
user:file_search_path(foreign, PackLib) :-
    pack_dir(_Name, foreign, PackLib).

%!  '$pack_detach'(+Name, -Dir) is det.
%
%   Detach the given package  from  the   search  paths  and list of
%   registered packages, but does not delete the files.

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

%!  '$pack_attach'(+Dir) is det.
%
%   Attach the given package

'$pack_attach'(Dir) :-
    attach_package(Dir),
    !.
'$pack_attach'(Dir) :-
    (   exists_directory(Dir)
    ->  throw(error(existence_error(directory, Dir), _))
    ;   throw(error(domain_error(pack, Dir), _))
    ).

%!  attach_packs
%
%   Attach packages from all package directories.

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

%!  remove_dups(+List, -Unique, +Seen) is det.
%
%   Remove duplicates from List, keeping the first solution.

remove_dups([], [], _).
remove_dups([H|T0], T, Seen) :-
    memberchk(H, Seen),
    !,
    remove_dups(T0, T, Seen).
remove_dups([H|T0], [H|T], Seen) :-
    remove_dups(T0, T, [H|Seen]).


%!  attach_packs(+Dir)
%
%   Attach packages from directory Dir.

attach_packs(Dir) :-
    catch(directory_files(Dir, Entries), _, fail),
    !,
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
    attach_package(PackDir),
    !.
attach_package(_, _).

special(.).
special(..).


%!  attach_package(+PackDir) is semidet.
%
%   @tbd    Deal with autoload index.  Reload?

attach_package(PackDir) :-
    atomic_list_concat([PackDir, '/pack.pl'], InfoFile),
    access_file(InfoFile, read),
    file_base_name(PackDir, Pack),
    check_existing(Pack, PackDir),
    foreign_dir(Pack, PackDir, ForeignDir),
    prolog_dir(PackDir, PrologDir),
    !,
    assertz(pack(Pack, PackDir)),
    assertz(pack_dir(Pack, prolog, PrologDir)),
    update_autoload(PrologDir),
    (   ForeignDir \== (-)
    ->  assertz(pack_dir(Pack, foreign, ForeignDir))
    ;   true
    ),
    print_message(silent, pack(attached(Pack, PackDir))).


%!  check_existing(+Pack, +PackDir) is semidet.
%
%   Verify that we did not load this package before.

check_existing(Entry, Dir) :-
    retract(pack(Entry, Dir)),             % registered from same place
    !,
    retractall(pack_dir(Entry, _, _)).
check_existing(Entry, Dir) :-
    pack(Entry, OldDir),
    !,
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
    exists_directory(ForeignBaseDir),
    !,
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
