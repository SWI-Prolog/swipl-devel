/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2006-2026, University of Amsterdam
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

:- module(unicode_derived_core_properties,
          [ unicode_derived_core_property/2,    % ?Code, ?Prop
            unicode_property/3,                 % +File, ?Code, ?Prop
            id_superscript/1,
            id_subscript/1,
            white_space/1                       % ?Code
          ]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [member/2, numlist/3]).
:- use_module(library(readutil), [read_line_to_codes/2]).

:- dynamic
    derived_property/3,
    loaded/1.

user:file_search_path(unicode, Dir) :-
    source_file(unicode_derived_core_property(_,_), File),
    file_directory_name(File, Dir).

unicode_derived_core_property(Code, Prop) :-
    absolute_file_name(unicode('DerivedCoreProperties.txt'),
                       File,
                       [ access(read)
                       ]),
    unicode_property(File, Code, Prop).

%!  unicode_derived_core_property(+File, ?Code, ?Prop)
%
%   True when Code has Prop according to file.

unicode_property(File, Code, Prop) :-
    loaded(File),
    !,
    derived_property(Code, Prop, File).
unicode_property(File, Code, Prop) :-
    retractall(derived_property(_, _,File)),
    process_file(File),
    assert(loaded(File)),
    unicode_property(File, Code, Prop).


process_file(File) :-
    open(File, read, In),
    call_cleanup(process_stream(In, File), close(In)).

process_stream(In, File) :-
    read_line_to_codes(In, Line),
    (   Line == end_of_file
    ->  true
    ;   process_line(Line, File),
        process_stream(In, File)
    ).

process_line(Line, File) :-
    debug(unicode_data, 'Line "~s"', [Line]),
    (   phrase(line(Codes, Class), Line)
    ->  forall(member(C, Codes),
               assert(derived_property(C, Class, File)))
    ;   format('ERROR: Could not parse "~s"~n', [Line]),
        abort
    ).



line([], -) -->
    ws, "#", skip_rest,
    !.
line([], -) -->
    ws.
line(Codes, Class) -->
    ucc(First),
    (   ".."
    ->  ucc(Last),
        { numlist(First, Last, Codes) }
    ;   { Codes = [First] }
    ),
    ws, ";", ws,
    class(Class0),
    (   ws, ";", ws
    ->  class(Prop),
        {Class =.. [Class0,Prop]}
    ;   {Class = Class0}
    ),
    ws,
    "#",
    skip_rest.

class(Class) -->
    identifier(Id),
    { downcase_atom(Id, Class) }.

identifier(Word) -->
    [C0], { code_type(C0, csymf) },
    csyms(Cs),
    { atom_codes(Word, [C0|Cs]) }.

csyms([H|T]) -->
    [H], { code_type(H, csym) },
    !,
    csyms(T).
csyms([]) -->
    [].

ucc(Val) -->
    hex_digit(D0),
    hex_digit(D1),
    hex_digit(D2),
    hex_digit(D3),
    { Val0 is D0<<12 + D1<<8 + D2<<4 + D3 },
    xucc(Val0, Val).

xucc(Val0, Val) -->
    hex_digit(D),
    !,
    { Val1 is Val0<<4 + D },
    xucc(Val1, Val).
xucc(Val, Val) -->
    [].

hex_digit(D) -->
    [C],
    { code_type(C, xdigit(D)) }.

w -->
    [C],
    { code_type(C, white) }.

ws -->
    w,
    !,
    ws.
ws -->
    [].

skip_rest(_, []).


                /*******************************
                *         WHITE SPACE          *
                *******************************/

%!  white_space(?Code)
%
%   Official Unicode table for white space for programming languages.

white_space(0x0009).
white_space(0x000A).
white_space(0x000B).
white_space(0x000C).
white_space(0x000D).
white_space(0x0020).
white_space(0x0085).
white_space(0x200E).
white_space(0x200F).
white_space(0x2028).
white_space(0x2029).


                /*******************************
                *      DIGIT SUPERSCRIPT       *
                *******************************/

id_superscript(0x2070).
id_superscript(0x00B9).
id_superscript(0x00B2).
id_superscript(0x00B3).
id_superscript(0x2074).
id_superscript(0x2075).
id_superscript(0x2076).
id_superscript(0x2077).
id_superscript(0x2078).
id_superscript(0x2079).

id_subscript(0x2080).
id_subscript(0x2081).
id_subscript(0x2082).
id_subscript(0x2083).
id_subscript(0x2084).
id_subscript(0x2085).
id_subscript(0x2086).
id_subscript(0x2087).
id_subscript(0x2088).
id_subscript(0x2089).

