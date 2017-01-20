/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(files,
        [ can_open_file/2,              % +Path, +Mode
          chdir/1                       % +Dir
        ]).

%!  can_open_file(+Path, +Mode)
%
%   Succeeds if the user has access to `File' in mode `Mode'.  Fails
%   silently if this is not the  case.   `Mode'  is  one  of  {read,
%   write, both}.  This used to be difficult.  Since we have
%   access_file/2 it is merely a Quintus compatibility predicate
%   and should be in quintus.pl.  We will leave it here for compatibility
%   reasons.
%
%   @deprecated Use access_file/2.

can_open_file(File, read) :-
    !,
    access_file(File, read).
can_open_file(File, write) :-
    !,
    (   exists_file(File)
    ->  access_file(File, write)
    ;   path_dir_name(File, Dir),
        access_file(Dir, write)
    ).
can_open_file(File, both) :-
    access_file(File, read),
    access_file(File, write).

path_dir_name(File, Dir) :-
    file_base_name(File, Base),
    atom_concat(RawDir, Base, File),
    (   RawDir == ''
    ->  Dir = '.'
    ;   Dir = RawDir
    ).

%!  chdir(+Dir) is det.
%
%   Change Working Directory.
%
%   @deprecated     Use using working_directory/2.

chdir(Dir) :-
    working_directory(_Old, Dir).
