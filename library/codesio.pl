/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2015, University of Amsterdam
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

:- module(codesio,
          [ format_to_codes/3,          % +Format, +Args, -Codes
            format_to_codes/4,          % +Format, +Args, -Codes, ?Tail
            write_to_codes/2,           % +Term, -Codes
            write_to_codes/3,           % +Term, -Codes, ?Tail
            write_term_to_codes/3,      % +Term, -Codes, +Options
            write_term_to_codes/4,      % +Term, -Codes, ?Tail, +Options
                                        % read predicates
            read_from_codes/2,          % +Codes, -Term
            read_term_from_codes/3,     % +Codes, -Term, +Options
            open_codes_stream/2,        % +Codes, -Stream
            with_output_to_codes/2,     % :Goal, -Codes
            with_output_to_codes/3,     % :Goal, -Codes, ?Tail
            with_output_to_codes/4      % :Goal, -Stream, -Codes, ?Tail
          ]).

:- meta_predicate
    with_output_to_codes(0, -),
    with_output_to_codes(0, -, ?),
    with_output_to_codes(0, -, -, ?).

:- predicate_options(read_term_from_codes/3, 3,
                     [pass_to(system:read_term/3, 3)]).
:- predicate_options(write_term_to_codes/3, 3,
                     [pass_to(system:write_term/3, 3)]).
:- predicate_options(write_term_to_codes/4, 4,
                     [pass_to(system:write_term/3, 3)]).

/** <module> I/O on Lists of Character Codes

This module emulates the SICStus  library   codesio.pl  for  reading and
writing from/to lists of character codes.   Most of these predicates are
straight calls into similar SWI-Prolog primitives.

This library is based on library(charsio)   that originates from Quintus
Prolog. The naming is updated to reflect  the ISO naming conventions and
the ISO predicats atom_codes/2, etc  are   obviously  removed  from this
library.

@compat SICStus 4
*/

%!  format_to_codes(+Format, +Args, -Codes) is det.
%
%   Use format/2 to write to a list of character codes.

format_to_codes(Format, Args, Codes) :-
    format(codes(Codes), Format, Args).

%!  format_to_codes(+Format, +Args, -Codes, ?Tail) is det.
%
%   Use format/2 to write to a difference list of character codes.

format_to_codes(Format, Args, Codes, Tail) :-
    format(codes(Codes, Tail), Format, Args).

%!  write_to_codes(+Term, -Codes)
%
%   Codes is a list of character codes produced by write/1 on Term.

write_to_codes(Term, Codes) :-
    format(codes(Codes), '~w', [Term]).

%!  write_to_codes(+Term, -Codes, ?Tail)
%
%   Codes is a difference-list of character codes produced by write/1 on Term.

write_to_codes(Term, Codes, Tail) :-
    format(codes(Codes, Tail), '~w', [Term]).

%!  write_term_to_codes(+Term, -Codes, +Options) is det.
%
%   True  when  Codes  is  a  string  that  matches  the  output  of
%   write_term/3 using Options.

write_term_to_codes(Term, Codes, Options) :-
    format(codes(Codes), '~W', [Term, Options]).

%!  write_term_to_codes(+Term, -Codes, ?Tail, +Options) is det.
%
%   True  when  Codes\Tail  is  a  difference  list  containing  the
%   character codes that matches the   output  of write_term/3 using
%   Options.

write_term_to_codes(Term, Codes, Tail, Options) :-
    format(codes(Codes, Tail), '~W', [Term, Options]).

%!  read_from_codes(+Codes, -Term) is det.
%
%   Read Codes into Term.
%
%   @compat The SWI-Prolog version does not require Codes to end
%           in a full-stop.

read_from_codes([], Term) :-
    !,
    Term = end_of_file.
read_from_codes(List, Term) :-
    atom_to_term(List, Term, _).

%!  read_term_from_codes(+Codes, -Term, +Options) is det.
%
%   Read Codes into Term.  Options are processed by read_term/3.
%
%   @compat sicstus

read_term_from_codes(Codes, Term, Options) :-
    read_term_from_atom(Codes, Term, Options).

%!  open_codes_stream(+Codes, -Stream) is det.
%
%   Open Codes as an input stream.
%
%   @see open_string/2.

open_codes_stream(Codes, Stream) :-
    open_string(Codes, Stream).

%!  with_output_to_codes(:Goal, Codes) is det.
%
%   Run Goal with as once/1.  Output written to =current_output=
%   is collected in Codes.

with_output_to_codes(Goal, Codes) :-
    with_output_to(codes(Codes), Goal).

%!  with_output_to_codes(:Goal, -Codes, ?Tail) is det.
%
%   Run Goal with as once/1.  Output written to =current_output=
%   is collected in Codes\Tail.

with_output_to_codes(Goal, Codes, Tail) :-
    with_output_to(codes(Codes, Tail), Goal).

%!  with_output_to_codes(:Goal, -Stream, -Codes, ?Tail) is det.
%
%   As  with_output_to_codes/3,  but  Stream  is  unified  with  the
%   temporary  stream.  This  predicate   exists  for  compatibility
%   reasons. In SWI-Prolog, the temporary   stream is also available
%   as `current_output`.

with_output_to_codes(Goal, Stream, Codes, Tail) :-
    with_output_to(codes(Codes, Tail), with_stream(Stream, Goal)).

with_stream(Stream, Goal) :-
    current_output(Stream),
    call(Goal).
