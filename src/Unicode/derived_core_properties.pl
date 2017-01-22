/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2011, University of Amsterdam
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
	  [ unicode_derived_core_property/2,	% ?Code, ?Prop
	    unicode_property/3			% +File, ?Code, ?Prop
	  ]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [member/2, numlist/3]).
:- use_module(library(readutil), [read_line_to_codes/2]).

:- dynamic
	derived_property/3,
	loaded/1.

%	unicode_derived_core_property(+File, ?Code, ?Prop)
%
%

unicode_derived_core_property(Code, Prop) :-
	unicode_property('DerivedCoreProperties.txt', Code, Prop).

unicode_property(File, Code, Prop) :-
	loaded(File), !,
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
	ws, "#", skip_rest, !.
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
	class(Class),
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
	[H], { code_type(H, csym) }, !,
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
	hex_digit(D), !,
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
	w, !,
	ws.
ws -->
	[].

skip_rest(_, []).
