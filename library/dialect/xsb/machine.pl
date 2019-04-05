/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(machine,
          [ gc_heap/0,
            trimcore/0,

            abolish_table_info/0,

            str_cat/3,

            term_type/2,

            xsb_expand_file_name/2,       % +File, -Expanded
            expand_filename_no_prepend/2, % FileName, -ExpandedName
            parse_filename/4,             % +FileName, -Dir, -Base, -Extension

            xwam_state/2                  % +Id, -Value
          ]).
:- use_module(library(debug)).
:- use_module(library(error)).

%!  gc_heap
%
%   Explicitly invoke heap garbage collection.

gc_heap :-
    garbage_collect.

%!  trimcore
%
%   Trim the stacks.

trimcore :-
    trim_stacks.

%!  abolish_table_info
%
%   Undocumented in the XSB manual.

abolish_table_info.

%!  str_cat(+Atom1, +Atom2, -Atom3)

str_cat(A, B, AB) :-
    must_be(atom, A),
    must_be(atom, B),
    atom_concat(A, B, AB).

%!  term_type(+Term, -Type:integer)
%
%   Emulation of internal XSB predicate

term_type(Term, Type) :-
    (   atom(Term)
    ->  Type = 5
    ;   compound(Term)
    ->  (   Term = [_|_]
        ->  Type = 3
        ;   Type = 1
        )
    ;   integer(Term)
    ->  Type = 2
    ;   float(Term)
    ->  Type = 6
    ;   var(Term)
    ->  Type = 0
    ;   assertion(fail)
    ).

		 /*******************************
		 *              FILES		*
		 *******************************/

%!  xsb_expand_file_name(+File, -Expanded)
%
%

xsb_expand_file_name(File, Expanded) :-
    absolute_file_name(File, Expanded, [expand(true)]).

%!  expand_filename_no_prepend(+FileName, -ExpandedName)
%
%

expand_filename_no_prepend(File, Expanded) :-
    expand_file_name(File, Absolute),
    working_directory(Dir0, Dir0),
    ensure_slash(Dir0, Dir),
    (   atom_concat(Dir, Ex0, Absolute)
    ->  Expanded = Ex0
    ;   Expanded = Absolute
    ).

%!  parse_filename(+FileName, -Dir, -Base, -Extension)
%
%

parse_filename(FileName, Dir, Base, Extension) :-
    sub_atom(FileName, 0, _, _, '~'),
    !,
    expand_file_name(FileName, Absolute),
    parse_filename_2(Absolute, Dir, Base, Extension).
parse_filename(FileName, Dir, Base, Extension) :-
    parse_filename_2(FileName, Dir, Base, Extension).

parse_filename_2(FileName, Dir, Base, Extension) :-
    file_directory_name(FileName, Dir0),
    (   Dir0 == '.'
    ->  Dir = ''
    ;   ensure_slash(Dir0, Dir)
    ),
    file_base_name(FileName, File),
    file_name_extension(Base, Extension, File).

ensure_slash(Dir, DirS) :-
    sub_atom(Dir, _, _, 0, '/'),
    !,
    DirS = Dir.
ensure_slash(Dir, DirS) :-
    atom_concat(Dir, '/', DirS).


%!  xwam_state(+Id, -Value)
%
%   Low-level query.  Used by the XSB test suite.

xwam_state(2, DelayReg) :-
    !,
    (   nb_current('$delay_list', [_|_])
    ->  DelayReg = 1
    ;   DelayReg = 0
    ).
xwam_state(Id, _Value) :-
    domain_error(xwam_state, Id).
