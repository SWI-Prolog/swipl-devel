/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2008, University of Amsterdam
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

:- module('$parms', []).

:- multifile
    user:library_directory/1.
:- dynamic
    user:library_directory/1.

:- dynamic
    library_directory_cache/2.
:- volatile
    library_directory_cache/2.

user:library_directory(Dir) :-
    cached_library_directory(local,
                             Dir0=lib,
                             Dir0),
    Dir = Dir0.
user:library_directory(Dir) :-
    cached_library_directory(user,
                             expand_file_name('~/lib/prolog', [Dir0]),
                             Dir0),
    Dir = Dir0.
user:library_directory(Dir) :-
    cached_library_directory(system,
                             absolute_file_name(swi(library), Dir0),
                             Dir0),
    Dir = Dir0.
user:library_directory(Dir) :-
    cached_library_directory(clp,
                             absolute_file_name(swi('library/clp'), Dir0),
                             Dir0),
    Dir = Dir0.

cached_library_directory(CacheName, _, Dir) :-
    library_directory_cache(CacheName, Dir),
    !,
    Dir \== [].
cached_library_directory(CacheName, Goal, Dir) :-
    catch(Goal, _, fail),
    exists_directory(Dir),
    !,
    asserta(library_directory_cache(CacheName, Dir)).
cached_library_directory(CacheName, _, _) :-
    asserta(library_directory_cache(CacheName, [])),
    fail.

'$default_editor'(notepad) :-
    current_prolog_flag(windows, true),
    !.
'$default_editor'(vi).
