/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(zip,
          [ zip_open/4,                         % +File, +Mode, -Zipper, +Options
            zip_members/2,                      % +Zipper, -Entries
            zip_file_info/3                     % +Zipper, -Name, -Attrs
          ]).
:- use_module(library(error)).

%!  zip_open(+File, +Mode, -Zipper, +Options) is det.

zip_open(File, Mode, Zipper, Options) :-
    must_be(oneof([read,write]), Mode),
    open(File, Mode, Stream, [type(binary)]),
    zip_open_stream(Stream, Zipper, Options).

%!  zip_members(+Zipper, -Members:list(atom)) is det.
%
%   True when Members is the list of file names in the Zipper.

zip_members(Zipper, Members) :-
    setup_call_cleanup(
        zip_lock(Zipper),
        ( zip_goto(Zipper, first),
          zip_members_(Zipper, Members)
        ),
        zip_unlock(Zipper)).

zip_members_(Zipper, [Name|T]) :-
    zip_file_info(Zipper, Name, _Attrs),
    (   zip_goto(Zipper, next)
    ->  zip_members_(Zipper, T)
    ;   T = []
    ).

%!  zip_file_info(+Zipper, -Name, -Attrs) is det.
%
%   Obtain information about the current zip entry.

zip_file_info(Zipper, Name, Attrs) :-
    zip_file_info_(Zipper, Name,
                   info(CompressedSize, UnCompressedSize, Extra, Comment)),
    Attrs0 = zip{compressed_size:CompressedSize,
                 uncompressed_size:UnCompressedSize},
    zip_attr(Extra, extra, Attrs0, Attrs1),
    zip_attr(Comment, comment, Attrs1, Attrs).

zip_attr("", _, Attrs, Attrs) :- !.
zip_attr('', _, Attrs, Attrs) :- !.
zip_attr(Value, Name, Attrs0, Attrs) :-
    put_dict(Name, Attrs0, Value, Attrs).
