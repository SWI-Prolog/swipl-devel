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
          [
          ]).
:- use_module(library(lists)).
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

package(Pkg) :-
    expand_file_name('packages/*/CMakeFiles', MakeDirs),
    member(MakeDir, MakeDirs),
    file_directory_name(MakeDir, PkgBindir),
    file_base_name(PkgBindir, Pkg),
    Pkg \== xpce.

package_source_dir(Pkg, SourceDir) :-
    directory_file_path('../packages', Pkg, Dir0),
    absolute_file_name(Dir0, SourceDir).

package_file(Pkg, File, Base) :-
    package(Pkg),
    package_source_dir(Pkg, Source),
    directory_member(Source, File,
                     [ extensions([pl]),
                       recursive(true),
                       exclude('test.pl'),
                       exclude_directory('{examples,.git}')
                     ]),
    file_base_name(File, Base).
package_file(library, File, Base) :-
    absolute_file_name('../library', Source),
    directory_member(Source, File,
                     [ extensions([pl]),
                       recursive(true),
                       exclude('test.pl'),
                       exclude_directory('{examples,.git}')
                     ]),
    file_base_name(File, Base).

duplicate_source_file(Base, Files) :-
    setof(File, Pkg^package_file(Pkg, File, Base), Files),
    Files = [_,_|_].

:- dynamic
    base_package/2.

fill_base_package :-
    base_package(_,_),
    !.
fill_base_package :-
    forall(package_file(Pkg, _, Base),
           assertz(base_package(Base, Pkg))).

file_package(File, Package) :-
    fill_base_package,
    file_base_name(File, Base),
    base_package(Base, Package).

library_file(File) :-
    absolute_file_name(swi(library), LibDir,
                       [ access(read),
                         file_type(directory)
                       ]),
    directory_member(LibDir, File,
                     [ extensions([pl]),
                       recursive(true)
                     ]).

xref_library :-
    forall(library_file(File),
           xref_source(File)).

cross_package_link(FilePkg, File, Name/Arity, FromPkg, From) :-
    (   ground(Name/Arity)
    ->  functor(Called, Name, Arity),
        xref_defined(File, Called, imported(From))
    ;   distinct(Called, xref_defined(File, Called, imported(From)))
    ),
    file_package(File, FilePkg),
    file_package(From, FromPkg),
    functor(Called, Name, Arity),
    FilePkg \== FromPkg.

cross_package_link(FilePkg, File, FromPkg, From) :-
    distinct([FilePkg-File, FromPkg-From],
             (   distinct(Called, xref_defined(File, Called, imported(From))),
                 file_package(File, FilePkg),
                 file_package(From, FromPkg),
                 FilePkg \== FromPkg
             )).

cross_package_link(FilePkg, Name/Arity, FromPkg) :-
    distinct([FilePkg, FromPkg],
             (   xref_defined(File, Called, imported(From)),
                 file_package(File, FilePkg),
                 file_package(From, FromPkg),
                 FilePkg \== FromPkg,
                 functor(Called, Name, Arity)
             )).

