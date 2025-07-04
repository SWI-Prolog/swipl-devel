/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2025, VU University Amsterdam
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

:- module(prolog_build_home, []).

/** <module> Setup SWI-Prolog to run from the build directory

This module is loaded if SWI-Prolog  is   started  in the build tree and
sets up paths such that all packages can be loaded and the system can be
used interactively similar to the installed  system. This serves several
purposes:

  - We can easily debug the various installations
  - We can easily develop
  - We can build the documentation without installing

This file is normally installed in `CMAKE_BINARY_DIRECTORY/home`.
*/

%!  cmake_binary_directory(-BinDir) is det.
%!  cmake_source_directory(-SrcDir) is det.
%
%   Find    the    equivalent    of      =CMAKE_BINARY_DIRECTORY=    and
%   CMAKE_SOURCE_DIRECTORY.

:- dynamic
    cmake_bindir/1.

cmake_binary_directory(BinDir) :-
    cmake_bindir(BinDir),
    !.
cmake_binary_directory(BinDir) :-
    exe_or_shared_object(Exe),
    parent_dir(Exe, BinDir),
    atom_concat(BinDir, '/home/boot.prc', BootFile),
    exists_file(BootFile),
    !,
    asserta(cmake_bindir(BinDir)).

exe_or_shared_object(File) :-	% only reliable when read-only
    '$current_prolog_flag'(libswipl, File, _Scope, read, atom),
    !.
exe_or_shared_object(File) :-
    current_prolog_flag(executable, OsExe),
    OsExe \== 'libswipl.dll',           % avoid dummy for embedded JPL test
    prolog_to_os_filename(Exe, OsExe),
    working_directory(PWD, PWD),
    exe_access(ExeAccess),
    absolute_file_name(Exe, File,
		       [ access(ExeAccess),
			 relative_to(PWD)
		       ]).

parent_dir(Dir, Dir).
parent_dir(Dir, Parent) :-
    file_directory_name(Dir, Parent0),
    Parent0 \== Dir,
    parent_dir(Parent0, Parent).

exe_access(Access) :-
    (   current_prolog_flag(unix, true)
    ->  Access = execute
    ;   Access = read
    ).

%!  swipl_package(-Pkg, -PkgBinDir) is nondet.
%
%   True when Pkg is available in the build tree at the given location.

swipl_package(Pkg, PkgBinDir) :-
    cmake_binary_directory(CMakeBinDir),
    atomic_list_concat([CMakeBinDir, packages], /, PkgRoot),
    exists_directory(PkgRoot),
    directory_files(PkgRoot, Candidates),
    '$member'(Pkg, Candidates),
    \+ special(Pkg),
    atomic_list_concat([PkgRoot, Pkg], /, PkgBinDir),
    atomic_list_concat([PkgBinDir, 'CMakeFiles'], /, CMakeDir),
    exists_directory(CMakeDir).

special(.).
special(..).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(library, swi(packages)).
user:file_search_path(foreign, AppDir) :-
    current_prolog_flag(windows, true),
    current_prolog_flag(executable, Exe),
    prolog_to_os_filename(PlExe, Exe),
    file_directory_name(PlExe, AppDir).

%!  add_package(+Package, +PkgSrcDir, +PkgBinDir) is det.
%
%   Setup the source paths and initialization for Package with the given
%   source and binary location.

add_package(xpce, PkgBinDir) :-
    !,
    add_package_path(PkgBinDir),
    (   absolute_file_name(swi('xpce/prolog/swipl-rc'),
			   RCFile,
			   [ access(read),
			     file_errors(fail)
			   ])
    ->  use_module(RCFile)
    ;   true
    ).
add_package(chr, PkgBinDir) :-
    assertz(user:file_search_path(chr, PkgBinDir)),
    assertz(user:file_search_path(chr, library(chr))),
    assertz(user:file_search_path(library, PkgBinDir)).
add_package(jpl, PkgBinDir) :-
    add_package_path(PkgBinDir),
    atomic_list_concat([PkgBinDir, 'src/main/java'], /, JarDir),
    assertz(user:file_search_path(jar, JarDir)).
add_package(http, PkgBinDir) :-
    add_package_path(PkgBinDir),
    file_directory_name(PkgBinDir, PkgDir),
    assertz(user:file_search_path(library, PkgDir)).
add_package(_Pkg, PkgBinDir) :-
    add_package_path(PkgBinDir).

%!  add_package_path(+PkgBinDir) is det.
%
%   Add the binary directories for the   package to the `foreign` search
%   path. Note that we only  need  to   add  the  binary directory if it
%   contains shared objects, but  it  is   probably  cheaper  to  add it
%   anyway. On Windows, all .dll  files  are   in  the  directory of the
%   executable.

add_package_path(_) :-
    current_prolog_flag(windows, true),
    !.
add_package_path(PkgBinDir) :-
    assertz(user:file_search_path(foreign, PkgBinDir)).

:- forall(swipl_package(Pkg, PkgBinDir),
	  add_package(Pkg, PkgBinDir)).

%!  set_version_info
%
%   Indicate we are running from the   build directory rather than using
%   an installed version.

set_version_info :-
    (   cmake_binary_directory(BinDir)
    ->  version(format('    CMake built from "~w"', [BinDir]))
    ;   current_prolog_flag(home, Home)
    ->  version(format('    CMake built with home "~w"', [Home]))
    ).

:- initialization(set_version_info).

%!  set_libswipl
%
%   Set the value for libswipl.

:- if(\+current_prolog_flag(libswipl, _)).
set_libswipl :-
    current_prolog_flag(shared_object_extension, SO),
    \+current_prolog_flag(windows, true),
    !,
    cmake_binary_directory(BinDir),
    format(atom(Value), '~w/src/libswipl.~w', [BinDir, SO]),
    set_prolog_flag(libswipl, Value).
set_libswipl.

:- initialization(set_libswipl).
:- endif.

% Avoid getting Java from the host when running under Wine.

:- if(current_prolog_flag(wine_version, _)).
delete_host_java_home :-
    (   getenv('JAVA_HOME', Dir),
	sub_atom(Dir, 0, _, _, /)
    ->  unsetenv('JAVA_HOME')
    ;   true
    ).

:- initialization(delete_host_java_home).
:- endif.


		 /*******************************
		 *        DOCUMENTATION		*
		 *******************************/

user:file_search_path(swi_man_manual, ManDir) :-
    cmake_binary_directory(BinDir),
    atomic_list_concat([BinDir, 'man/Manual'], /, ManDir).
user:file_search_path(swi_man_packages, BinDir) :-
    swipl_package(_, BinDir).


		 /*******************************
		 *        CONFIGURATION		*
		 *******************************/

:- multifile
    prolog:runtime_config/2.

prolog:runtime_config(c_libdir, LibDir) :-
    cmake_binary_directory(BinDir),
    atomic_list_concat([BinDir, src], /, LibDir).
