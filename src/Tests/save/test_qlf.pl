/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, University of Amsterdam
                         VU University Amsterdam
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

:- module(test_qlf,
          [ test_qlf/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(debug), [assertion/1]).

test_qlf :-
    run_tests([ qlf
              ]).

find_me.

file_path(File, Path) :-
    source_file(find_me, Here),
    file_directory_name(Here, Dir),
    directory_file_path(Dir, File, Path).

:- begin_tests(qlf).

% Note that ['$qlf'(Qlf)] is an undocumented way  to save the file in an
% explicit location.

test(unicode,
     [ Found =@= Expected,
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    file_path('input/unicode', In),
    file_base_name(In, Base),
    file_name_extension(Base, qlf, QlfFile),
    current_prolog_flag(tmp_dir, Tmp),
    directory_file_path(Tmp, QlfFile, Qlf),
    catch(delete_file(Qlf), _, true),
    load_files(In, ['$qlf'(Qlf)]),
    findall(Data, data(Data), Expected),
    unload_file(In),
    assertion(\+ current_predicate(data/1)),
    consult(Qlf),
    findall(Data, data(Data), Found).

:- end_tests(qlf).
