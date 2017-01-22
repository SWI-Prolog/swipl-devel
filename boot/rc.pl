/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2011, University of Amsterdam
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

:- module('$rc',
          [ open_resource/3,            % +Name, ?Class, -Stream
            open_resource/4,            % +Name, ?Class, +RW, -Stream
            current_resource/3          % :Name, ?Class, ?File
          ]).

:- meta_predicate
    open_resource(:, ?, -),
    open_resource(:, ?, +, -),
    current_resource(:, ?, ?).

:- dynamic
    user:resource/3.
:- multifile
    user:resource/3.

%!  open_resource(:Name, ?Class, -Handle) is det.
%!  open_resource(:Name, ?Class, +Mode, -Handle) is det.
%
%   Open resource with given Name  and   Class,  returning  a stream
%   handle.

open_resource(Name, Class, Handle) :-
    open_resource(Name, Class, read, Handle).

open_resource(Module:RcName, Class, RW, Handle) :-
    (   default_module(Module, RModule),
        current_resource(RModule:RcName, Class, FileSpec)
    ->  absolute_file_name(FileSpec, File),
        open(File, RW, Handle, [type(binary)])
    ;   '$rc_handle'(RC),
        tag_rc_name(Module, RcName, TaggedName),
        '$rc_open'(RC, TaggedName, Class, RW, Handle)
    ).

tag_rc_name(user, RcName, RcName) :- !.
tag_rc_name(Module, RcName, TaggedName) :-
    atomic_list_concat([Module, ':', RcName], TaggedName).
tag_rc_name(_, RcName, RcName).

%!  current_resource(:Name, ?Class, ?File) is nondet.
%
%   List all currently declared resources.   Should  eventually deal
%   with resources that are already part of the state.

current_resource(M:Name, Class, File) :-
    current_module(M),
    current_predicate(M:resource/3),
    M:resource(Name, Class, File).
