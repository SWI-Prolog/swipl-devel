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
          [ zip_open/4,                    % +File, +Mode, -Zipper, +Options
            zip_close/1,                   % +Zipper
            zip_close/2,                   % +Zipper, +Comment
                                           % Entry predicates
            with_zipper/2,                 % +Zipper, :Goal
            zipper_open_new_file_in_zip/4, % +Zipper, +File, -Stream, +Options
            zipper_goto/2,                 % +Zipper, +Where
            zipper_open_current/3,         % +Zipper, -Stream, +Options
            zipper_members/2,              % +Zipper, -Entries
            zipper_file_info/3             % +Zipper, -Name, -Attrs
          ]).
:- use_module(library(error)).
:- use_module(library(option)).

:- meta_predicate
    with_zipper(+, 0).

/** <module> Access resource ZIP archives

This library provides access to ZIP files.   ZIP files are used to store
SWI-Prolog _resources_. Ths library provides more  high level access and
documentation in addition to the low level   access provided as built in
as it is needed to bootstrap SWI-Prolog.

Access to a zip file is provided by  means of a _zipper_ object. This is
a _blob_ that is subject to atom garbage collection. Collecting a zipper
closes the underlying OS access.

A zipper is a  stateful  object.   We  recognise  the  following states:
_idle_, _scan_, _read_entry_, _write_entry_ and   _close_. The interface
raise  a  _permission_error_  when  trying  to  make  an  illegal  state
transition.

Being stateful, a zipper cannot  be   used  simultaneously from multiple
threads. The zipper becomes _owned_ by a   thread  when moving to _scan_
using zipper_goto/2. It is released after zipper_open_current/3 followed
by closing the stream.
*/

%!  zip_open(+File, +Mode, -Zipper, +Options) is det.
%
%   Create a Zipper, providing access to File.  Mode is one of `read` or
%   `write`. The Options list is currently ignored.

zip_open(File, Mode, Zipper, _Options) :-
    must_be(oneof([read,write]), Mode),
    open(File, Mode, Stream, [type(binary)]),
    zip_open_stream(Stream, Zipper, [close_parent(true)]).

%!  zip_close(+Zipper) is det.
%!  zip_close(+Zipper, +Options) is det.
%
%   Close a zipper. Options processed:
%
%     - comment(+Comment)
%     If the zipper is open for writing, set the global comment
%     for the zip file.

zip_close(Zipper) :-
    zip_close_(Zipper, _).
zip_close(Zipper, Options) :-
    option(comment(Comment), Options, _),
    zip_close_(Zipper, Comment).

%!  zipper_goto(+Zipper, +Where) is semidet.
%
%   Seek Zipper to a specified entry.  Where is one of
%
%     - first
%     Go to the first entry.  Fails if the zip is empty.
%     - next
%     Go to the next entry.  Fails if there is no next entry.
%     - file(Name)
%     Go to the entry with the specified name.
%

%!  zipper_open_current(+Zipper, -Stream, +Options) is det.
%
%   Open the current entry as an  input   stream.  Before  this call the
%   caller must use zipper_goto/2 to position to archive.  Options:
%
%     - type(+Type)
%     - encoding(+Encoding)
%     - bom(+Boolean)
%     Determine type and encoding of the stream.  The semantics
%     is the same as for open/4.
%     - release(+Boolean)
%     If `true` (default), release te archive for access by other
%     threads after the entry is closed.
%
%   It is allowed to call zip_close/1   immediately  after this call, in
%   which case the archive is closed when the entry is closed.

%!  with_zipper(+Zipper, :Goal)
%
%   Run Goal while holding ownership over Zipper.

with_zipper(Zipper, Goal) :-
    setup_call_cleanup(
        zip_lock(Zipper),
        Goal,
        zip_unlock(Zipper)).

%!  zip_members(+Zipper, -Members:list(atom)) is det.
%
%   True when Members is the list of file names in the Zipper.

zipper_members(Zipper, Members) :-
    with_zipper(Zipper,
                ( zipper_goto(Zipper, first),
                  zip_members_(Zipper, Members)
                )).

zip_members_(Zipper, [Name|T]) :-
    zip_file_info_(Zipper, Name, _Attrs),
    (   zipper_goto(Zipper, next)
    ->  zip_members_(Zipper, T)
    ;   T = []
    ).

%!  zipper_file_info(+Zipper, -Name, -Attrs) is det.
%
%   Obtain information about the current  zip   entry.  Name  is an atom
%   representing the name of the entry. Attrs is a dict holding:
%
%     - compressed_size:Bytes
%     Size in the archive
%     - uncompressed_size:Bytes
%     Bytes after decompression
%     - time:Stamp
%     Numeric time stamp in Prolog native format (float
%     expressing seconds since Jan 1, 1970).  Note that
%     the resolution of time in zip archives is one
%     second.
%     - extra:Extra
%     - comment:Extra
%     Optional additional fields.
%     - offset:Offset
%     Direct pointer to this entry.  May be used with zip_goto/2.

zipper_file_info(Zipper, Name, Attrs) :-
    zip_file_info_(Zipper, Name,
                   info(CompressedSize, UnCompressedSize,
                        Extra, Comment,
                        Time, Offset)),
    Attrs0 = zip{compressed_size:CompressedSize,
                 uncompressed_size:UnCompressedSize,
                 offset:Offset
                },
    zip_attr(Extra,   extra,   Attrs0, Attrs1),
    zip_attr(Comment, comment, Attrs1, Attrs2),
    zip_attr(Time,    time,    Attrs2, Attrs).

zip_attr("", _, Attrs, Attrs) :- !.
zip_attr('', _, Attrs, Attrs) :- !.
zip_attr(Value, Name, Attrs0, Attrs) :-
    put_dict(Name, Attrs0, Value, Attrs).
