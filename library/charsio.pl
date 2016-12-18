/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2015, University of Amsterdam
                              VU University Amsterdam
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

:- module(charsio,
          [ format_to_chars/3,          % +Format, +Args, -Codes
            format_to_chars/4,          % +Format, +Args, -Codes, ?Tail
            write_to_chars/2,           % +Term, -Codes
            write_to_chars/3,           % +Term, -Codes, ?Tail
            atom_to_chars/2,            % +Atom, -Codes
            atom_to_chars/3,            % +Atom, -Codes, ?Tail
            number_to_chars/2,          % +Number, -Codes
            number_to_chars/3,          % +Number, -Codes, ?Tail
                                        % read predicates
            read_from_chars/2,          % +Codes, -Term
            read_term_from_chars/3,     % +Codes, -Term, +Options
            open_chars_stream/2,        % +Codes, -Stream
            with_output_to_chars/2,     % :Goal, -Codes
            with_output_to_chars/3,     % :Goal, -Codes, ?Tail
            with_output_to_chars/4      % :Goal, -Stream, -Codes, ?Tail
          ]).
:- use_module(library(error)).

:- meta_predicate
    with_output_to_chars(0, -),
    with_output_to_chars(0, -, ?),
    with_output_to_chars(0, -, -, ?).

:- predicate_options(read_term_from_chars/3, 3,
                     [pass_to(system:read_term/3, 3)]).

/** <module> I/O on Lists of Character Codes

This module emulates the Quintus/SICStus  library charsio.pl for reading
and writing from/to lists of character   codes. Most of these predicates
are straight calls into similar SWI-Prolog primitives.  Some can even be
replaced by ISO standard predicates.

@compat The naming of this library is not in line with the ISO standard.
We believe that the SWI-Prolog  native   predicates  form a more elegant
alternative for this library.
*/

%!  format_to_chars(+Format, +Args, -Codes) is det.
%
%   Use format/2 to write to a list of character codes.

format_to_chars(Format, Args, Codes) :-
    format(codes(Codes), Format, Args).

%!  format_to_chars(+Format, +Args, -Codes, ?Tail) is det.
%
%   Use format/2 to write to a difference list of character codes.

format_to_chars(Format, Args, Codes, Tail) :-
    format(codes(Codes, Tail), Format, Args).

%!  write_to_chars(+Term, -Codes)
%
%   Write a term to a code  list.  True   when  Codes  is  a list of
%   character codes written by write/1 on Term.

write_to_chars(Term, Codes) :-
    format(codes(Codes), '~w', [Term]).

%!  write_to_chars(+Term, -Codes, ?Tail)
%
%   Write a term to a code list.  Codes\Tail is a difference list of
%   character codes produced by write/1 on Term.

write_to_chars(Term, Codes, Tail) :-
    format(codes(Codes, Tail), '~w', [Term]).

%!  atom_to_chars(+Atom, -Codes) is det.
%
%   Convert Atom into a list of character codes.
%
%   @deprecated     Use ISO atom_codes/2.

atom_to_chars(Atom, Codes) :-
    atom_codes(Atom, Codes).

%!  atom_to_chars(+Atom, -Codes, ?Tail) is det.
%
%   Convert Atom into a difference list of character codes.

atom_to_chars(Atom, Codes, Tail) :-
    format(codes(Codes, Tail), '~a', [Atom]).

%!  number_to_chars(+Number, -Codes) is det.
%
%   Convert Atom into a list of character codes.
%
%   @deprecated     Use ISO number_codes/2.

number_to_chars(Number, Codes) :-
    number_codes(Number, Codes).

%!  number_to_chars(+Number, -Codes, ?Tail) is det.
%
%   Convert Number into a difference list of character codes.

number_to_chars(Number, Codes, Tail) :-
    must_be(number, Number),
    format(codes(Codes, Tail), '~w', [Number]).

%!  read_from_chars(+Codes, -Term) is det.
%
%   Read Codes into Term.
%
%   @compat The SWI-Prolog version does not require Codes to end
%           in a full-stop.

read_from_chars([], end_of_file) :- !.
read_from_chars(List, Term) :-
    atom_to_term(List, Term, _).

%!  read_term_from_chars(+Codes, -Term, +Options) is det.
%
%   Read Codes into Term.  Options are processed by read_term/3.
%
%   @compat sicstus

read_term_from_chars(Codes, Term, Options) :-
    read_term_from_atom(Codes, Term, Options).

%!  open_chars_stream(+Codes, -Stream) is det.
%
%   Open Codes as an input stream.
%
%   @see open_string/2.

open_chars_stream(Codes, Stream) :-
    open_string(Codes, Stream).

%!  with_output_to_chars(:Goal, -Codes) is det.
%
%   Run Goal as with once/1.  Output written to =current_output=
%   is collected in Codes.

with_output_to_chars(Goal, Codes) :-
    with_output_to(codes(Codes), Goal).

%!  with_output_to_chars(:Goal, -Codes, ?Tail) is det.
%
%   Run Goal as with once/1.  Output written to =current_output=
%   is collected in Codes\Tail.

with_output_to_chars(Goal, Codes, Tail) :-
    with_output_to(codes(Codes, Tail), Goal).

%!  with_output_to_chars(:Goal, -Stream, -Codes, ?Tail) is det.
%
%   Same as with_output_to_chars/3 using  an   explicit  stream. The
%   difference list Codes\Tail contains  the   character  codes that
%   Goal has written to Stream.

with_output_to_chars(Goal, Stream, Codes, Tail) :-
    with_output_to(codes(Codes, Tail), with_stream(Stream, Goal)).

with_stream(Stream, Goal) :-
    current_output(Stream),
    call(Goal).
