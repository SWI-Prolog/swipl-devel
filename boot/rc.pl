/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2018, University of Amsterdam
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

:- module('$rc',
          [ open_resource/2,            % +Name, -Stream
            open_resource/3,            % +Name, +RW, -Stream
            current_resource/2          % :Name, ?File
          ]).

:- meta_predicate
    open_resource(:, -),
    open_resource(:, -, +),
    current_resource(:, ?).

:- dynamic
    user:resource/2,
    user:resource/3.
:- multifile
    user:resource/2,
    user:resource/3.

%!  open_resource(:Name, -Stream) is det.
%!  open_resource(:Name, -Stream, +Options) is det.
%!  open_resource(:Name, ?Class, -Stream) is det.
%
%   Open resource with given Name, returning a Stream.

open_resource(Name, Handle) :-
    open_resource(Name, Handle, []).

open_resource(Module:RcName, Stream, Options) :-
    is_list(Options),
    !,
    (   default_module(Module, RModule),
        current_resource(RModule:RcName, FileSpec)
    ->  absolute_file_name(FileSpec, File),
        open(File, read, Stream, Options)
    ;   '$rc_handle'(Zipper),
        tag_rc_name(Module, RcName, TaggedName),
        zipper_goto(Zipper, file(TaggedName))
    ->  zipper_open_current(Zipper, Stream,
                            [ release(true)
                            | Options
                            ])
    ).
open_resource(Name, _Class, Stream) :-
    open_resource(Name, Stream).

tag_rc_name(user, RcName, RcName) :- !.
tag_rc_name(Module, RcName, TaggedName) :-
    atomic_list_concat([Module, ':', RcName], TaggedName).
tag_rc_name(_, RcName, RcName).

%!  current_resource(:Name, ?File) is nondet.
%
%   List all currently declared resources.   Should  eventually deal
%   with resources that are already part of the state.

current_resource(M:Name, File) :-
    current_module(M),
    (   current_predicate(M:resource/2),
        M:resource(Name, File)
    ;   current_predicate(M:resource/3),
        M:resource(Name, _Class, File)
    ).

%!  c_open_resource(:Name, +Mode, -Stream)
%
%   Callback for PL_open_resource().

:- public c_open_resource/3.
:- meta_predicate c_open_resource(:, +, -).

c_open_resource(Name, Mode, Stream) :-
    atom_chars(Mode, Chars),
    (   Chars = [r|MChars]
    ->  mode_options(MChars, Options),
        open_resource(Name, Stream, Options)
    ;   '$domain_error'(open_resource_mode, Mode)
    ).

mode_options([], []).
mode_options([t|Chars], [type(text)|T]) :-
    !,
    mode_options(Chars, T).
mode_options([b|Chars], [type(binary)|T]) :-
    !,
    mode_options(Chars, T).
mode_options([_|Chars], T) :-
    mode_options(Chars, T).


		 /*******************************
		 *      RESOURCES AS FILES	*
		 *******************************/

:- register_iri_scheme(res, res_iri_hook, []).
:- meta_predicate with_zipper(+, 0).

res_iri_hook(open(Mode,Options), IRI, Stream) :-
    (   Mode == read
    ->  resource_and_entry(IRI, Zipper, Entry),
        zipper_goto(Zipper, file(Entry)),
        zipper_open_current(Zipper, Stream, Options)
    ;   '$permission_error'(open, source_sink, IRI)
    ).
res_iri_hook(access(Mode), IRI, True) :-
    resource_and_entry(IRI, Zipper, Entry),
    (   read_mode(Mode),
        catch(with_zipper(Zipper,
                          zipper_goto(Zipper, file(Entry))),
              error(existence_error(_, _), _),
              fail)
    ->  True = true
    ;   True = false
    ).
res_iri_hook(time, IRI, Time) :-
    resource_and_entry(IRI, Zipper, Entry),
    catch(( zipper_goto(Zipper, file(Entry)),
            zipper_file_property(Zipper, time, Time)
          ),
          error(existence_error(_, _), Context),
          throw(error(existence_error(source_sink, IRI), Context))).
res_iri_hook(size, IRI, Size) :-
    resource_and_entry(IRI, Zipper, Entry),
    catch(( zipper_goto(Zipper, file(Entry)),
            zipper_file_property(Zipper, size, Size)
          ),
          error(existence_error(_, _), Context),
          throw(error(existence_error(source_sink, IRI), Context))).

read_mode(read).
read_mode(exists).

resource_and_entry(IRI, Clone, Entry) :-
    string_concat("res://", Entry, IRI),
    '$rc_handle'(Zipper),
    zip_clone(Zipper, Clone).

%!  with_zipper(+Zipper, :Goal)

with_zipper(Zipper, Goal) :-
    setup_call_cleanup(
        zip_lock(Zipper),
        Goal,
        zip_unlock(Zipper)).

%!  zipper_file_property(+Zipper, +Prop, -Value)

zipper_file_property(Zipper, Prop, Value) :-
    zip_file_info_(Zipper, _Name, Info),
    zip_prop_arg(Prop, Arg),
    arg(Arg, Info, Value).

zip_prop_arg(size, 2).
zip_prop_arg(time, 5).
