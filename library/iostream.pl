/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
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

:- module(iostream,
          [ open_any/5,         % +Spec, +Mode, -Stream, -Close, +Options
            close_any/1         % +Close
          ]).
:- use_module(library(option)).
:- use_module(library(error)).
:- if(exists_source(library(uri))).
:- use_module(library(uri)).
:- endif.

/** <module> Utilities to deal with streams

This  library  contains  utilities  that   deal  with  streams,  notably
originating from non-built-in sources such   as URLs, archives, windows,
processes, etc.

The predicate open_any/5 acts as a   _broker_  between applications that
can process data from a stream  and   libraries  that can create streams
from diverse sources. Without this predicate, processing data inevitally
follows  the  pattern  below.  As    _call_some_open_variation_  can  be
anything, this blocks us from writing  predicates such as load_xml(From,
DOM) that can operate on arbitrary input sources.

  ==
  setup_call_cleanup(
      call_some_open_variation(Spec, In),
      process(In),
      close(In)).
  ==

Libraries   that   can   open    streams     can    install   the   hook
iostream:open_hook/6  to  make  their  functionality  available  through
open_any/5.

@see    library(archive), library(process), library(zlib),
        library(http/http_stream)
*/

:- multifile
    open_hook/6.    % +Spec, +Mode, -Stream, -Close, +Options0, -Options

%!  open_any(+Specification, +Mode, -Stream, -Close, +Options)
%
%   Establish a stream from  Specification   that  should  be closed
%   using  Close,  which  can  either  be    called   or  passed  to
%   close_any/1. Options processed:
%
%     - encoding(Enc)
%     Set stream to encoding Enc.
%
%   Without loaded plugins, the open_any/5   processes the following
%   values  for  Specification.  If  no   rule  matches,  open_any/5
%   processes Specification as file(Specification).
%
%     - Stream
%     A plain stream handle. Possisible post-processing options such
%     as encoding are applied. Close does _not_ close the stream,
%     but resets other side-effects such as the encoding.
%     - stream(Stream)
%     Same as a plain Stream.
%     - FileURL
%     If Specification is of the form =file://...=, the pointed
%     to file is opened using open/4.  Requires library(uri) to
%     be installed.
%     - file(Path)
%     Explicitly open the file Path.  Path can be an Path(File)
%     term as accepted by absolute_file_name/3.
%     - string(String)
%     Open a Prolog string, atom, list of characters or codes
%     as an _input_ stream.
%
%   The typical usage scenario is  given   in  the code below, where
%   <process> processes the input.
%
%     ==
%     setup_call_cleanup(
%         open_any(Spec, read, In, Close, Options),
%         <process>(In),
%         Close).
%     ==
%
%   Currently, the following libraries extend this predicate:
%
%      - library(http/http_open)
%      Adds support for URLs using the `http` and `https` schemes.

open_any(Spec, Mode, Stream, Close, Options) :-
    \+ ( ground(Spec),                      % argument sanity check
         var(Stream),
         var(Close),
         is_list(Options) ),
    !,
    open_error(Spec, Mode, Stream, Close, Options).
open_any(Spec, _Mode, Stream, Close, Options) :-
    is_stream(Spec),
    !,
    Stream = Spec,
    input_options(Spec, Stream, true, Close, Options).
:- if(current_predicate(uri_file_name/2)).
open_any(Spec, Mode, Stream, Close, Options0) :-
    atomic(Spec),
    uri_file_name(Spec, File),
    !,
    open_any_builtin(file(File), Mode, Stream0, Close0, Options0, Options),
    input_options(Stream0, Stream, Close0, Close, Options).
:- endif.
open_any(Spec, Mode, Stream, Close, Options0) :-
    open_any_builtin(Spec, Mode, Stream0, Close0, Options0, Options),
    !,
    input_options(Stream0, Stream, Close0, Close, Options).
open_any(Spec, Mode, Stream, Close, Options0) :-
    open_hook(Spec, Mode, Stream0, Close0, Options0, Options),
    !,
    input_options(Stream0, Stream, Close0, Close, Options).
open_any(Spec, Mode, Stream, Close, Options0) :-
    open_any_builtin(file(Spec), Mode, Stream0, Close0, Options0, Options),
    input_options(Stream0, Stream, Close0, Close, Options).

open_error(Spec, _Mode, _Stream, _Close, _Options) :-
    var(Spec), !, instantiation_error(Spec).
open_error(_Spec, _Mode, Stream, _Close, _Options) :-
    nonvar(Stream),
    !,
    uninstantiation_error(Stream).
open_error(_Spec, _Mode, _Stream, Close, _Options) :-
    nonvar(Close),
    !,
    uninstantiation_error(Close).
open_error(_Spec, _Mode, _Stream, _Close, Options) :-
    \+ is_list(Options),
    !,
    must_be(list, Options).

%!  input_options(+Stream0, -Stream, +Close0, -Close, +Options) is det.
%
%   Establish the final stream.

input_options(Spec, Stream, Close0, Close, Options) :-
    option(encoding(Enc), Options),
    !,
    Stream = Spec,
    stream_property(Stream, encoding(Enc0)),
    set_stream(Stream, encoding(Enc)),
    mkconj(set_stream(Stream, encoding(Enc0)), Close0, Close).
input_options(Stream, Stream, Close, Close, _).

mkconj(set_stream(In,encoding(_)), close(In), close(In)) :- !.
mkconj(true,                       X,         X) :- !.
mkconj(X,                          true,      X) :- !.
mkconj(X,                          Y,         (X,Y)) :- !.

%!  open_any_builtin(+Spec, +Mode, -Stream, -Close,
%!                   +Options0, -Options) is semidet.
%
%   Built-in open-any operations

open_any_builtin(stream(Stream), _Mode, Stream, true, Options, Options) :-
    must_be(stream, Stream).
open_any_builtin(file(Spec), Mode, Stream, close(Stream), Options0, Options) :-
    (   compound(Spec)
    ->  absolute_file_name(Spec, Path, [access(Mode)|Options0])
    ;   Path = Spec
    ),
    partition(open_option, Options0, OpenOptions, Options),
    open(Path, Mode, Stream, OpenOptions).
open_any_builtin(string(S), read, Stream, close(Stream), Options, Options) :-
    open_string(S, Stream).

open_option(encoding(_)).
open_option(type(_)).

%!  close_any(+Goal)
%
%   Execute the `Close` closure returned  by open_any/5. The closure
%   can also be called directly. Using close_any/1 can be considered
%   better style and enhances tractability of the source code.

close_any(Var) :-
    var(Var), !, instantiation_error(Var).
close_any((A,B)) :- close_any(A), close_any(B).
close_any(true).
close_any(close(Stream)) :- close(Stream).
close_any(set_stream(S, encoding(Enc))) :- set_stream(S, encoding(Enc)).


                 /*******************************
                 *             HOOKS            *
                 *******************************/

%!  open_hook(+Spec, +Mode, -Stream, -Close, +Options0, -Options) is semidet.
%
%   Open Spec in Mode, producing Stream.
%
%   @arg Close is unified to a goal that must be called to undo the
%   side-effects of the action, e.g., typically the term close(Stream)
%   @arg Options0 are the options passed to open_any/5
%   @arg Options are passed to the post processing filters that
%   may be installed by open_any/5.
