/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2011, University of Amsterdam
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

:- module(unicode_data,
          [ unicode_property/2          % ?Code, ?Property
          ]).
:- use_module(library(table)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides access to the   UNICODE datafile distributed by the
unicode organisation (http://www.unicode.org). This  file describes many
aspects for all defined UNICODE  code   positions,  such  as their name,
type, etc.  The meaning of the fields is defined here:

        http://www.unicode.org/Public/UNIDATA/UCD.html#UCD_File_Format

This library uses the table package for accessing structured files. This
maps the file in memory and performs  binary search. This is not blindly
fast and this library should therefore   not be used for computationally
intensive tasks. In such cases it  can   be  used  to generate tables in
Prolog or even to create a dedicated C datastructure.

The file UnicodeData.txt itself is not part   of the library and must be
obtained and installed seperately. This is because of its size (close to
1MB). Increasing the footprint of the environment with 1MB is too much.

The UCD file must be  named  UnicodeData.txt   and  placed  in  the same
directory  as  this  file  or  in    the   search  path  'unicode'  (see
file_search_path/2).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%       unicode_property(?Code, ?Property)
%
%       Logical predicate relating code  points   to  properties.  It is
%       optimised for asking a single  property   of  a  known code, but
%       works with any instantiation.

unicode_property(Code, Property) :-
    table(Handle),
    property(Property),
    in_table(Handle, [code(Code), Property], _),
    \+ arg(1, Property, '').

property(name(_)).
property(general_category(_)).
property(canonical_combining_class(_)).
property(bidi_class(_)).
property(decomposition_type(_)).
property(numeric_type_1(_)).
property(numeric_type_2(_)).
property(numeric_type_3(_)).
property(bidi_mirrored(_)).
property(unicode_1_name(_)).
property(iso_comment(_)).
property(simple_uppercase_mapping(_)).
property(simple_lowercase_mapping(_)).
property(simple_titlecase_mapping(_)).

:- dynamic
    handle/1.
:- volatile
    handle/1.

:- multifile
    user:file_search_path/2.
:- dynamic
    user:file_search_path/2.

:- (   user:file_search_path(unicode, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       assert(user:file_search_path(unicode, Dir))
   ).

table(Handle) :-
    handle(Handle),
    !.
table(Handle) :-
    absolute_file_name(unicode('UnicodeData.txt'),
                       Path,
                       [ access(read)
                       ]),
    new_table(Path,
              [ code(hexadecimal, [sorted, unique]),
                name(atom, [downcase]),                             % 1
                general_category(atom),                             % 2
                canonical_combining_class(integer),                 % 3
                bidi_class(atom, [downcase]),                       % 4
                decomposition_type(atom),                           % 5
                numeric_type_1(integer, [syntax]),                  % 6
                numeric_type_2(integer, [syntax]),                  % 7
                numeric_type_3(integer, [syntax]),                  % 8
                bidi_mirrored(atom, [downcase]),                    % 9
                unicode_1_name(atom, [downcase]),                   % 10
                iso_comment(atom),                                  % 11
                simple_uppercase_mapping(hexadecimal, [syntax]),    % 12
                simple_lowercase_mapping(hexadecimal, [syntax]),    % 13
                simple_titlecase_mapping(hexadecimal, [syntax])     % 14
              ],
              [ field_separator(0';)
              ],
              Handle),
    assert(handle(Handle)).

