/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (C): 2008-2026, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(pure_input,
          [ phrase_from_file/2,         % :Grammar, +File
            phrase_from_file/3,         % :Grammar, +File, +Options
            phrase_from_stream/2,       % :Grammar, +Stream
            phrase_from_stream/3,       % :Grammar, +Stream, +Options
            stream_to_lazy_codes/2,     % :Stream -Codes
            stream_to_lazy_chars/2,     % :Stream -Chars
            stream_to_lazy_list/2,      % :Stream -Codes

            syntax_error//1,            % +ErrorTerm
                                        % Low level interface
            lazy_list_location//1,      % -Location
            lazy_list_character_count//1 % -CharacterCount
          ]).
:- autoload(library(error), [type_error/2, permission_error/3, must_be/2]).
:- autoload(library(option), [option/3]).

:- set_prolog_flag(generate_debug_info, false).

/** <module> Pure Input from files and streams

This module is part of pio.pl,   dealing with _pure_ _input_: processing
input streams from the outside  world   using  pure  predicates, notably
grammar rules (DCG).  Using  pure   predicates  makes  non-deterministic
processing of input much simpler.

Pure input uses attributed variables  to   read  input from the external
source into a list _|on demand|_. The   overhead of lazy reading is more
than compensated for by using block reads based on read_pending_codes/3.

Ulrich Neumerkel came up with the idea to use coroutining for creating a
_lazy list_. His implementation  repositioned  the   file  to  deal with
re-reading  that  can  be  necessary    on   backtracking.  The  current
implementation uses destructive assignment together  with more low-level
attribute handling to realise pure input on any (buffered) stream.

@tbd    Provide support for alternative input readers, e.g. reading
        terms, tokens, etc.
*/

:- predicate_options(phrase_from_stream/3, 3,
                     [ as(oneof([codes,chars]))
                     ]).
:- predicate_options(phrase_from_file/3, 3,
                     [ pass_to(system:open/4, 4),
                       pass_to(phrase_from_stream/3, 3)
                     ]).

%!  phrase_from_file(:Grammar, +File) is nondet.
%!  phrase_from_file(:Grammar, +File, +Options) is nondet.
%
%   Process the content of File using the   DCG  rule Grammar. The space
%   usage of this mechanism depends on the   length of the not committed
%   part of Grammar. Committed parts of the temporary list are reclaimed
%   by the garbage collector, while the list   is extended on demand due
%   to unification of the attributed tail variable.
%
%   Options are passed to open/4   and  phrase_from_stream/3. The latter
%   notable allows for as(chars) to call a DCG compiled for dealing with
%   character lists rather than codes.
%
%   Below is an example that counts the number of times a string appears
%   in a file. The library  dcg/basics   provides  string//1 matching an
%   arbitrary string and remainder//1 which matches the remainder of the
%   input without parsing.
%
%   ```
%   :- use_module(library(dcg/basics)).
%
%   file_contains(File, Pattern) :-
%           phrase_from_file(match(Pattern), File).
%
%   match(Pattern) -->
%           string(_),
%           string(Pattern),
%           remainder(_).
%
%   match_count(File, Pattern, Count) :-
%           aggregate_all(count, file_contains(File, Pattern), Count).
%   ```
%
%   This can be called as (note that   the  pattern must be a string
%   (code list)):
%
%   ```
%   ?- match_count('pure_input.pl', `file`, Count).
%   ```

:- meta_predicate
    phrase_from_file(//, +),
    phrase_from_file(//, +, +),
    phrase_from_stream(//, +).

phrase_from_file(Grammar, File) :-
    phrase_from_file(Grammar, File, []).

phrase_from_file(Grammar, File, Options) :-
    setup_call_cleanup(
        open(File, read, In, Options),
        phrase_from_stream(Grammar, In, Options),
        close(In)).

%!  phrase_from_stream(:Grammar, +Stream).
%!  phrase_from_stream(:Grammar, +Stream, +Options).
%
%   Run Grammer against the character codes   on Stream. Stream must
%   be buffered.  Options:
%
%     - as(Type)
%       Where Type is one of `codes` (default) or `chars`.  See also
%       stream_to_lazy_codes/2 and stream_to_lazy_chars/2.

phrase_from_stream(Grammar, In) :-
    phrase_from_stream(Grammar, In, []).

phrase_from_stream(Grammar, In, Options) :-
    option(as(Type), Options, codes),
    must_be(oneof([codes,chars]), Type),
    stream_to_lazy_list(In, List, Type),
    phrase(Grammar, List).

%!  syntax_error(+Error)//
%
%   Throw the syntax error Error  at   the  current  location of the
%   input. This predicate is designed to  be called from the handler
%   of phrase_from_file/3.
%
%   @throws error(syntax_error(Error), Location)

syntax_error(Error) -->
    lazy_list_location(Location),
    { throw(error(syntax_error(Error), Location))
    }.

%!  lazy_list_location(-Location)// is det.
%
%   Determine current (error) location in  a   lazy  list. True when
%   Location is an (error) location term that represents the current
%   location in the DCG list.
%
%   @arg    Location is a term file(Name, Line, LinePos, CharNo) or
%           stream(Stream, Line, LinePos, CharNo) if no file is
%           associated to the stream RestLazyList.  Finally, if the
%           Lazy list is fully materialized (ends in =|[]|=), Location
%           is unified with `end_of_file-CharCount`.
%   @see    lazy_list_character_count//1 only provides the character
%           count.

lazy_list_location(Location, Here, Here) :-
    lazy_list_location(Here, Location).

lazy_list_location(Here, Location) :-
    '$skip_list'(Skipped, Here, Tail),
    (   attvar(Tail)
    ->  get_attr(Tail, pure_input, State),
        State = lazy_input(Stream, PrevPos, Pos, _Type, _),
        Details = [Line, LinePos, CharNo],
        (   stream_property(Stream, file_name(File))
        ->  PosParts = [file, File|Details]
        ;   PosParts = [stream, Stream|Details]
        ),
        Location =.. PosParts,
        (   PrevPos == (-)                  % nothing is read.
        ->  Line = 1, LinePos = 0, CharNo = 0
        ;   stream_position_data(char_count, Pos, EndRecordCharNo),
            CharNo is EndRecordCharNo - Skipped,
            set_stream_position(Stream, PrevPos),
            stream_position_data(char_count, PrevPos, StartRecordCharNo),
            Skip is CharNo-StartRecordCharNo,
            forall(between(1, Skip, _), get_code(Stream, _)),
            stream_property(Stream, position(ErrorPos)),
            stream_position_data(line_count, ErrorPos, Line),
            stream_position_data(line_position, ErrorPos, LinePos)
        )
    ;   Tail == []
    ->  Location = end_of_file-Skipped
    ;   type_error(lazy_list, Here)
    ).


%!  lazy_list_character_count(-CharCount)//
%
%   True when CharCount is the current   character count in the Lazy
%   list. The character count is computed by finding the distance to
%   the next frozen tail of the lazy list. CharCount is one of:
%
%     - An integer
%     - A term end_of_file-Count
%
%   @see    lazy_list_location//1 provides full details of the location
%           for error reporting.

lazy_list_character_count(Location, Here, Here) :-
    lazy_list_character_count(Here, Location).

lazy_list_character_count(Here, CharNo) :-
    '$skip_list'(Skipped, Here, Tail),
    (   attvar(Tail)
    ->  get_attr(Tail, pure_input, State),
        arg(3, State, Pos),
        stream_position_data(char_count, Pos, EndRecordCharNo),
        CharNo is EndRecordCharNo - Skipped
    ;   Tail == []
    ->  CharNo = end_of_file-Skipped
    ;   type_error(lazy_list, Here)
    ).


%!  stream_to_lazy_codes(+Stream, -List) is det.
%!  stream_to_lazy_chars(+Stream, -List) is det.
%!  stream_to_lazy_list(+Stream, -List) is det.
%
%   Create a lazy list  representing  the   character  codes  (chars) in
%   Stream. List is a partial  list   ending  in an attributed variable.
%   Unifying this variable reads the next block   of  data. The block is
%   stored with the attribute value  such  that   there  is  no  need to
%   re-read it.
%
%   @deprecated stream_to_lazy_list/2 is a backward compatibility alias
%   for stream_to_lazy_codes/2.
%   @compat Unlike the previous version of this predicate this
%           version does not require a repositionable stream.  It
%           does require a buffer size of at least the maximum
%           number of bytes of a multi-byte sequence (6).

stream_to_lazy_codes(Stream, List) :-
    stream_to_lazy_list(Stream, List, codes).
stream_to_lazy_chars(Stream, List) :-
    stream_to_lazy_list(Stream, List, chars).
stream_to_lazy_list(Stream, List) :-
    stream_to_lazy_list(Stream, List, codes).

stream_to_lazy_list(Stream, List, Type) :-
    (   stream_property(Stream, buffer(false))
    ->  permission_error(create, lazy_list, Stream)
    ;   true
    ),
    stream_to_lazy_list(Stream, -, Type, List).

stream_to_lazy_list(Stream, PrevPos, Type, List) :-
    stream_property(Stream, position(Pos)),
    put_attr(List, pure_input, lazy_input(Stream, PrevPos, Pos, Type, _)).

attr_unify_hook(State, Value) :-
    '$notrace'(attr_unify_hook_ndebug(State, Value)).

attr_unify_hook_ndebug(State, Value) :-
    State = lazy_input(Stream, _PrevPos, Pos, Type, Read),
    (   var(Read)
    ->  fill_buffer(Stream),
        (   Type == codes
        ->  read_pending_codes(Stream, NewList, Tail)
        ;   read_pending_chars(Stream, NewList, Tail)
        ),
        (   Tail == []
        ->  nb_setarg(5, State, []),
            Value = []
        ;   stream_to_lazy_list(Stream, Pos, Type, Tail),
            nb_linkarg(5, State, NewList),
            Value = NewList
        )
    ;   Value = Read
    ).
