/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(streams,
	  [ with_output_to/3		% ?Outtput, :Goal, +Options
	  ]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(option), [option/2, option/3]).
:- autoload(library(apply), [maplist/2, maplist/3]).

:- meta_predicate with_output_to(?, 0, +).

/** <module> Manage Prolog streams

This library provides high level primitives for stream operations.  It
is related  to the  charsio.pl and codesio.pl  libraries.  Considering
these are de-facto standard Prolog  libraries we prefer to leave these
untouched.

*/

%!  with_output_to(?Output, :Goal, +Options) is det.
%
%   Run  Goal and  once/1 while  capturing all  output to  all streams
%   (`current_output`, `user_output`  and `user_error`) in  the string
%   Output.  Options processed:
%
%     - capture(ListOfStreams)
%       List of streams to capture.  Default is `[]`, causing the
%       predicate to call with_output_to/2.  The only admissible
%       list elements are the alias names for the Prolog standard
%       streams.  As `current_output` is always captured, the only
%       two values are `user_output` and `user_error`
%     - color(Boolean)
%       When `true`, pretend the output is a terminal, causing messages
%       to use ANSI term escape sequences for color.
%
%   For example, the  following captures an error  message.  Note that
%   we must catch and print the message  inside Goal.  If we do not do
%   so the exception of Goal is simply propagated into the environment
%   without binding Output.
%
%   ```
%   ?- with_output_to(string(Out),
%                     catch(A is log(-1), E, print_message(error, E)),
%                     [capture([user_error]), color(true)]).
%   Out = "\u001B[1;31mERROR: is/2: Arithmetic: \c
%          evaluation error: `undefined'\n\u001B[0m",
%   E = error(evaluation_error(undefined), context(system:(is)/2, _)).
%   ```

with_output_to(Output, Goal, []) =>
    with_output_to(Output, Goal).
with_output_to(Output, Goal, Options) =>
    option(capture(Streams), Options, []),
    must_be(list(oneof([user_output,user_error])), Streams),
    setup_call_cleanup(
	output_state(State, Streams),
	with_output_to(
	    Output,
	    capture(Goal, Streams, Options)),
	restore_output(State, Streams)).

capture(Goal, Streams, Options) :-
    current_output(S),
    (   option(color(true), Options)
    ->  set_stream(S, tty(true))
    ;   true
    ),
    maplist(capture_output(S), Streams),
    once(Goal),
    maplist(flush_output, [current_output|Streams]).

output_state(State, Streams) :-
    maplist(stream_id, Streams, State).

stream_id(Alias, Stream) :-
    stream_property(Stream, alias(Alias)).

restore_output(State, Streams) :-
    maplist(restore_stream, Streams, State).

restore_stream(Alias, Stream) :-
    set_stream(Stream, alias(Alias)).

capture_output(S, Alias) :-
    set_stream(S, alias(Alias)).
