/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2024, VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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
            attach_packs/2,                     % +Dir, +Options
            pack_attach/2,                      % +Dir, +Options
            '$pack_detach'/2                    % +Name, -Dir
          ]).

:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.

:- dynamic
    pack_dir/3,                             % Pack, Type, Dir
    pack/2.                                 % Pack, BaseDir

user:file_search_path(pack, app_data(pack)).

user:file_search_path(library, PackLib) :-
    pack_dir(_Name, prolog, PackLib).
user:file_search_path(foreign, PackLib) :-
    pack_dir(_Name, foreign, PackLib).
user:file_search_path(app, AppDir) :-
    pack_dir(_Name, app, AppDir).

%!  '$pack_detach'(+Name, ?Dir) is det.
%
%   Detach the given package  from  the   search  paths  and list of
%   registered packages, but does not delete the files.

'$pack_detach'(Name, Dir) :-
    (   atom(Name)
    ->  true
    ;   '$type_error'(atom, Name)
    ),
    (   retract(pack(Name, Dir))
    ->  retractall(pack_dir(Name, _, _)),
        reload_library_index
    ;   '$existence_error'(pack, Name)
    ).

%!  pack_attach(+Dir, +Options) is det.
%
%   Attach the given package.  See manual for details.

pack_attach(Dir, Options) :-
    attach_package(Dir, Options),
    !.
pack_attach(Dir, _) :-
    (   exists_directory(Dir)
    ->  '$existence_error'(directory, Dir)
    ;   '$domain_error'(pack, Dir)
    ).

%!  attach_packs
%
%   Attach  packages  from  all  package    directories.  If  there  are
%   duplicates the first package found is used.

attach_packs :-
    set_prolog_flag(packs, true),
    set_pack_search_path,
    findall(PackDir, absolute_file_name(pack(.), PackDir,
                                        [ file_type(directory),
                                          access(read),
                                          solutions(all)
                                        ]),
            PackDirs),
    (   PackDirs \== []
    ->  remove_dups(PackDirs, UniquePackDirs, []),
        forall('$member'(PackDir, UniquePackDirs),
               attach_packs(PackDir, [duplicate(keep)]))
    ;   true
    ).

set_pack_search_path :-
    getenv('SWIPL_PACK_PATH', Value),
    !,
    retractall(user:file_search_path(pack, _)),
    current_prolog_flag(path_sep, Sep),
    atomic_list_concat(Dirs, Sep, Value),
    register_pack_dirs(Dirs).
set_pack_search_path.

register_pack_dirs([]).
register_pack_dirs([H|T]) :-
    prolog_to_os_filename(Dir, H),
    assertz(user:file_search_path(pack, Dir)),
    register_pack_dirs(T).


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


%!  attach_packs(+Dir) is det.
%!  attach_packs(+Dir, +Options) is det.
%
%   Attach packages from directory Dir.  Options processed:
%
%     - duplicate(+Action)
%       What to do if the same package is already installed in a different
%       directory.  Action is one of
%         - warning
%           Warn and ignore the package
%         - keep
%           Silently ignore the package
%         - replace
%           Unregister the existing and insert the new package
%     - search(+Where)
%       Determines the order of searching package library directories.
%       Default is `last`, alternative is `first`.
%     - replace(+Boolean)
%       If `true` (default `false`), remove the default set of registered
%       packages.

attach_packs(Dir) :-
    attach_packs(Dir, []).

attach_packs(Dir, Options) :-
    (   '$option'(replace(true), Options)
    ->  forall(pack(Name, PackDir),
               '$pack_detach'(Name, PackDir)),
        retractall(user:file_search_path(pack, _))
    ;   true
    ),
    register_packs_from(Dir),
    absolute_file_name(Dir, Path,
                       [ file_type(directory),
                         file_errors(fail)
                       ]),
    catch(directory_files(Path, Entries), _, fail),
    !,
    ensure_slash(Path, SPath),
    attach_packages(Entries, SPath, Options),
    reload_library_index.
attach_packs(_, _).

register_packs_from(Dir) :-
    (   user:file_search_path(pack, Dir)
    ->  true
    ;   asserta(user:file_search_path(pack, Dir))
    ).

attach_packages([], _, _).
attach_packages([H|T], Dir, Options) :-
    attach_package(H, Dir, Options),
    attach_packages(T, Dir, Options).

attach_package(Entry, Dir, Options) :-
    \+ special(Entry),
    atom_concat(Dir, Entry, PackDir),
    attach_package(PackDir, Options),
    !.
attach_package(_, _, _).

special(.).
special(..).


%!  attach_package(+PackDir, +Options) is semidet.
%
%   @tbd    Deal with autoload index.  Reload?

attach_package(PackDir, Options) :-
    atomic_list_concat([PackDir, '/pack.pl'], InfoFile),
    access_file(InfoFile, read),
    file_base_name(PackDir, Pack),
    check_existing(Pack, PackDir, Options),
    prolog_dir(PackDir, PrologDir),
    !,
    assertz(pack(Pack, PackDir)),
    '$option'(search(Where), Options, last),
    (   Where == last
    ->  assertz(pack_dir(Pack, prolog, PrologDir))
    ;   Where == first
    ->  asserta(pack_dir(Pack, prolog, PrologDir))
    ;   '$domain_error'(option_search, Where)
    ),
    update_autoload(PrologDir),
    (   foreign_dir(Pack, PackDir, ForeignDir)
    ->  assertz(pack_dir(Pack, foreign, ForeignDir))
    ;   true
    ),
    (   app_dir(PackDir, AppDir)
    ->  assertz(pack_dir(Pack, app, AppDir))
    ;   true
    ),
    print_message(silent, pack(attached(Pack, PackDir))).


%!  check_existing(+Pack, +PackDir, +Options) is semidet.
%
%   Verify that we did not load this package before.

check_existing(Entry, Dir, _) :-
    retract(pack(Entry, Dir)),             % registered from same place
    !,
    retractall(pack_dir(Entry, _, _)).
check_existing(Entry, Dir, Options) :-
    pack(Entry, OldDir),
    !,
    '$option'(duplicate(Action), Options, warning),
    (   Action == warning
    ->  print_message(warning, pack(duplicate(Entry, OldDir, Dir))),
        fail
    ;   Action == keep
    ->  fail
    ;   Action == replace
    ->  print_message(silent, pack(replaced(Entry, OldDir, Dir))),
        '$pack_detach'(Entry, OldDir)
    ;   '$domain_error'(option_duplicate, Action)
    ).
check_existing(_, _, _).


prolog_dir(PackDir, PrologDir) :-
    atomic_list_concat([PackDir, '/prolog'], PrologDir),
    exists_directory(PrologDir).

update_autoload(PrologDir) :-
    atom_concat(PrologDir, '/INDEX.pl', IndexFile),
    (   exists_file(IndexFile)
    ->  add_autoload_directory(PrologDir)
    ;   true
    ).

add_autoload_directory(Dir) :-
    (   user:file_search_path(autoload, Dir)
    ->  true
    ;   assertz(user:file_search_path(autoload, Dir))
    ),
    reload_library_index.

foreign_dir(Pack, PackDir, ForeignDir) :-
    atomic_list_concat([PackDir, '/lib'], ForeignBaseDir),
    exists_directory(ForeignBaseDir),
    !,
    (   arch(Arch),
	atomic_list_concat([PackDir, '/lib/', Arch], ForeignDir),
        exists_directory(ForeignDir)
    ->  assertz(pack_dir(Pack, foreign, ForeignDir))
    ;   findall(Arch, arch(Arch), Archs),
	print_message(warning, pack(no_arch(Pack, Archs))),
        fail
    ).

arch(Arch) :-
    current_prolog_flag(apple_universal_binary, true),
    Arch = 'fat-darwin'.
arch(Arch) :-
    current_prolog_flag(arch, Arch).

ensure_slash(Dir, SDir) :-
    (   sub_atom(Dir, _, _, 0, /)
    ->  SDir = Dir
    ;   atom_concat(Dir, /, SDir)
    ).

app_dir(PackDir, AppDir) :-
    atomic_list_concat([PackDir, '/app'], AppDir),
    exists_directory(AppDir).
