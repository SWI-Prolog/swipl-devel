/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cwi.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
                         CWI, Amsterdam
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

:- module(xref_packages,
          [ cmake_package_deps/0,
            list_deps/1,                        % +Package
            list_deps/2                         % +Package, ?DepPackage
          ]).
:- use_module(library(prolog_xref)).
:- use_module(library(filesex)).
:- use_module(library(solution_sequences)).

/** <module> Find dependencies between packages

This script is  work  in  progress   to  find  dependencies  between the
packages. These dependencies will be reflected   in  the cmake data such
that we can automatically pull in dependent packages.

The biggest hurdle is  finding  from  which   package  a  file  that  is
installed in the library comes.
*/

%!  cmake_package_deps
%
%   Emit  the  dependencies  between  packages  as  a  number  of  CMAKE
%   variables.

cmake_package_deps :-
    (   setof(FromPkg,
              File^PI^cross_package_link(Pkg, File, PI, FromPkg),
              Deps),
        atomic_list_concat(Deps, ' ', DepAtom),
        format('set(SWIPL_PKG_DEPS_~w ~w)~n', [Pkg, DepAtom]),
        fail
    ;   true
    ).

%!  list_deps(?Package) is det.
%!  list_deps(?Package, ?On) is det.
%
%   List relations between packages.  Intended   to  debug  and possibly
%   minimise the dependencies.

list_deps(Package) :-
    list_deps(Package, _).

list_deps(Package, FromPkg) :-
    forall(distinct(File+PI,
                    package_dependency(Package, File, PI, FromPkg)),
           (   file_name_on_path(File, Short),
               format('  ~p uses ~p from ~p~n', [Short, PI, FromPkg])
           )).


:- table
    file_package/2.

file_package(File, Package) :-
    read_link(File, _, Target),
    file_parent_directory(Target, Dir),
    file_directory_name(Dir, Parent),
    file_base_name(Parent, 'packages'),
    !,
    file_base_name(Dir, Package).

file_parent_directory(File, Parent) :-
    file_directory_name(File, Dir0),
    parent_drectory(Dir0, Parent).

parent_drectory(Dir, Dir).
parent_drectory(Dir, Parent) :-
    file_directory_name(Dir, DirectParent),
    Dir \== DirectParent,
    parent_drectory(DirectParent, Parent).


library_file(File) :-
    absolute_file_name(swi(library), LibDir,
                       [ access(read),
                         file_type(directory)
                       ]),
    directory_member(LibDir, File,
                     [ extensions([pl]),
                       recursive(true)
                     ]).

:- dynamic
    xref_library_done/0.

xref_library :-
    xref_library_done,
    !.
xref_library :-
    forall(library_file(File),
           xref_source(File)),
    assertz(xref_library_done).

cross_package_link(FilePkg, File, PI, FromPkg) :-
    distinct([FilePkg, FromPkg],
             package_dependency(FilePkg, File, PI, FromPkg)).

package_dependency(FilePkg, File, PI, FromPkg) :-
    xref_library,
    xref_defined(File, Called, imported(From)),
    atom(File),
    file_package(File, FilePkg),
    file_package(From, FromPkg),
    FilePkg \== FromPkg,
    \+ optional_install(File),
    head_pi(Called, PI).

head_pi(M:Term, M:Name/Arity) :-
    !,
    functor(Term, Name, Arity).
head_pi(Term, Name/Arity) :-
    functor(Term, Name, Arity).

optional_install(File) :-
    file_name_on_path(File, Short),
    optional_package_file(_Package, Short).

optional_package_file(plunit, library(test_wizard)).
optional_package_file(http,   library('http/xpce_httpd')).
optional_package_file('RDF',  library(rdf_diagram)).
