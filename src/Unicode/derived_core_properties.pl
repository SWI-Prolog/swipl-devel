/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
	derived_property(File, Code, Prop).
unicode_property(File, Code, Prop) :-
	retractall(derived_property(File, _,_)),
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
	phrase(line(Codes, Class), Line),
	forall(member(C, Codes),
	       assert(derived_property(File, C, Class))).
	


line([], -) -->
	ws, "#", skip_rest, !.
line([], -) -->
	ws, !.
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
