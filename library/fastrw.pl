/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2016, VU University Amsterdam
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

:- module(fastrw,
          [ fast_read/1,                % -Term
            fast_write/1,               % +Term
            fast_read/2,                % +Stream, -Term
            fast_write/2,               % +Stream, +Term
            fast_write_to_string/3      % +Term, -String, ?Tail
          ]).
:- use_module(library(lists)).

/** <module> Fast reading and writing of terms

This library provides the SICStus   and  Ciao library(fastrw) interface.
The idea behind this library  is  to   design  a  fast serialization for
Prolog  terms.  Ideally,  this  should    be   portable  between  Prolog
implementation. Unfortunately there is no   portably  binary term format
defined.

The current implementation  is  based   on  PL_record_external(),  which
provides a binary representation of terms  that is processed efficiently
and can handle cycles as well as attributes.   We try to keep the format
compatible between versions, but this is   not guaranteed. Conversion is
always possible by reading a database  using   the  old version, dump it
using write_canonical/1 and read it into the new version.

This library is built upon the following built in predicates:

  - fast_term_serialized/2 translates between a term and its
  serialization as a byte string.
  - fast_read/2 and fast_write/2 read/write binary serializations.

@tbd    Establish a portable binary format.
@compat The format is not compatible to SICStus/Ciao (which are not
        compatible either).  Funture versions of this library might
        implement a different encoding.
@bug    The current implementation of fast_read/1 *is not safe*.
        It is guaranteed to safely read terms written using
        fast_write/1, but may crash on arbitrary input.  The
        implementation does perform some basic sanity checks,
        including validation of the magic start byte.
*/

%!  fast_read(-Term)
%
%   The next term is read from current standard input and is unified
%   with Term. The syntax of the term   must  agree with fast_read /
%   fast_write format. If the end  of   the  input has been reached,
%   Term is unified with the term =end_of_file=.

fast_read(Term) :-
    fast_read(current_input, Term).

%!  fast_write(+Term)
%
%   Output Term in a way that   fast_read/1  and fast_read/2 will be
%   able to read it back.

fast_write(Term) :-
    fast_write(current_output, Term).

%!  fast_write_to_string(+Term, -String, ?Tail)
%
%   Perform a fast-write to the difference-slist String\Tail.

fast_write_to_string(T, S, R) :-
    fast_term_serialized(T, String),
    string_codes(String, Codes),
    append(Codes, R, S).
