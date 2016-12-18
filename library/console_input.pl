/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013, VU University Amsterdam
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

:- module(prolog_console_input,
          [
          ]).
:- use_module(library(lists)).

:- multifile
    prolog:complete_input/4.

%!  prolog:complete_input(+BeforeCursor, +AfterCursor,
%!                        -Delete, -Completions) is det.
%
%   Compute    auto    completions    for      the     input    line
%   BeforeCursor+AfterCursor.
%
%   @arg    Delete is an atom or string representing the text that is
%           replaced by the completion
%   @arg    Completions is a list of elements of this shape:
%
%             - Atom
%             Used for a plain completion without comment
%             - Atom-Comment
%             Used for a completion with comment.  This will be
%             used for predicates.

prolog:complete_input(Before, _After, Delete, Completions) :-
    string_codes(Before, Chars),
    reverse(Chars, BeforeRev),
    complete(BeforeRev, Delete, Completions).

complete(BeforeRev, Prefix, Files) :-   % complete files
    phrase(file_prefix(Prefix), BeforeRev, _),
    !,
    atom_concat(Prefix, '*', Pattern),
    expand_file_name(Pattern, Files).
complete(BeforeRev, Prefix, Atoms) :-   % complete atoms
    phrase(atom_prefix(Prefix), BeforeRev, _),
    !,
    '$atom_completions'(Prefix, Atoms).

%!  atom_prefix(-Prefix) is det.

atom_prefix(Prefix) -->
    atom_chars(RevString),
    { reverse(RevString, String),
      string_codes(Prefix, String) % do not create an atom
    }.

atom_chars([H|T]) --> atom_char(H), !, atom_chars(T).
atom_chars([]) --> [].

atom_char(C) --> [C], { atom_char(C) }.

atom_char(C) :- code_type(C, csym).

%!  file_prefix(-Prefix)// is semidet.
%
%   True when the part before the cursor looks like a file name.

file_prefix(Prefix) -->
    file_chars(RevString), "'",
    { reverse(RevString, String),
      atom_codes(Prefix, String)
    }.

file_chars([H|T]) --> file_char(H), !, file_chars(T).
file_chars([]) --> [].

file_char(C) --> [C], { file_char(C) }.

file_char(C) :- code_type(C, csym).
file_char(0'/).
file_char(0'.).
file_char(0'-).
file_char(0'~).
:- if(current_prolog_flag(windows,true)).
file_char(0':).
file_char(0'\s).
:- endif.
