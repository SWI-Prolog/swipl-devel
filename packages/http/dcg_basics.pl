/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module(dcg_basics,
	  [ blank/2,			% <blank>
	    blanks/2,			% <blank>*
	    nonblank/3,			% <nonblank>
	    nonblanks/3,		% <nonblank>* --> chars		(long)
	    blanks_to_nl/2,		% [space,tab,ret]*nl
	    string/3,			% <any>* -->chars 		(short)
	    string_without/4,		% Exclude, -->chars 		(long)
					% Characters
	    alpha_to_lower/3,		% Get lower|upper, return lower
					% Decimal numbers
	    digits/3,			% [0-9]* -->chars
	    digit/3,			% [0-9] --> char
	    integer/3,			% [+-][0-9]+ --> integer
	    float/3,	% [+-]?[0-9]+(.[0-9]*)?(e[+-]?[0-9]+)? --> float
	    number/3,			% integer | float
					% Hexadecimal numbers
	    xdigits/3,			% [0-9a-f]* --> 0-15*
	    xdigit/3,			% [0-9a-f] --> 0-15
	    xinteger/3,			% [0-9a-f]+ --> integer
					% Misc
	    eos/2,			% demand end-of-string
					% generation (TBD)
	    atom/3			% generate atom
	  ]).


%	string_without(+End, -Codes)
%	
%	Take as tokens from the input until the next token appears in
%	End.  End itself is left on the input.  Typical use is to read
%	upto a defined delimiter such as a newline or other reserved


%	character.

string_without(Not, [C|T]) -->
	[C],
	{ \+ memberchk(C, Not)
	}, !,
	string_without(Not, T).
string_without(_, []) -->
	[].

%	string(-Codes)
%	
%	Take as few as possible tokens from the input, taking one more
%	each time on backtracking. This code is normally followed by a
%	test for a delimiter.

string(String, In, Rest) :-
	append(String, Rest, In).

%	blanks//0
%	
%	Skip 0 or more white-space characters.

blanks -->
	blank, !,
	blanks.
blanks -->
	[].

blank -->
	[C],
	{ nonvar(C),
	  code_type(C, space)
	}.

nonblanks([H|T]) -->
	[H],
	{ code_type(H, graph)
	}, !,
	nonblanks(T).
nonblanks([]) -->
	[].

nonblank(H) -->
	[H],
	{ code_type(H, graph)
	}.

blanks_to_nl -->
	"\n", !.
blanks_to_nl -->
	blank, !,
	blanks_to_nl.
blanks_to_nl -->
	eos.

		 /*******************************
		 *	 CHARACTER STUFF	*
		 *******************************/

%	alpha_to_lower(-C)
%
%	Read a letter and return it as a lowercase letter.  In output
%	mode this simply emits the character.

alpha_to_lower(L) -->
	{ integer(L) }, !,
	[L].
alpha_to_lower(L) -->
	[C],
	{ code_type(C, alpha),
	  code_type(C, to_upper(L))
	}.


		 /*******************************
		 *	      NUMBERS		*
		 *******************************/

%	digits(?Chars)
%	digit(?Char)
%	integer(?Integer)

digits([H|T]) -->
	digit(H), !,
	digits(T).
digits([]) -->
	[].

digit(C) -->
	[C],
	{ code_type(C, digit)
	}.

integer(I) -->
	{ integer(I), !,
	  number_codes(I, Chars)
	},
	string(Chars).
integer(I) -->
	int_codes(Codes),
	{ number_codes(I, Codes)
	}.

int_codes([C,D0|D]) -->
	sign(C), !,
	digit(D0),
	digits(D).
int_codes([D0|D]) -->
	digit(D0),
	digits(D).


float(F) -->
	{ float(F), !,
	  number_codes(F, Chars)
	},
	string(Chars).
float(F) -->
	int_codes(I),
	(   dot,
	    digit(DF0),
	    digits(DF)
	->  {Fr = [0'., DF0|DF]}
	;   {Fr = ""}
	),
	(   exp
	->  int_codes(DI),
	    {E=[0'e|DI]}
	;   {E = ""}
	),
	{ (E \== "" ; Fr \== ""),
	  flatten([I, Fr, E], Codes),
	  number_codes(F, Codes)
	}.

sign(0'-) --> "-".
sign(0'+) --> "+".
	
dot --> ".".

exp --> "e".
exp --> "E".

number(N) -->
	{ nonvar(N), !,
	  number(N),
	  number_codes(N, Chars)
	},
	string(Chars).
number(N) -->
	int_codes(I),
	(   dot,
	    digit(DF0),
	    digits(DF)
	->  {F = [0'., DF0|DF]}
	;   {F = ""}
	),
	(   exp
	->  int_codes(DI),
	    {E=[0'e|DI]}
	;   {E = ""}
	),
	{ flatten([I, F, E], Codes),
	  number_codes(N, Codes)
	}.


		 /*******************************
		 *	    HEX NUMBERS		*
		 *******************************/

xinteger(Val) -->
	{ integer(Val),
	  sformat(S, '~16r', [Val]),
	  string_to_list(S, Codes)
	},
	string(Codes).
xinteger(Val) -->
	xdigit(D0),
	xdigits(D),
	{ mkval([D0|D], 16, Val)
	}.

xdigit(D) -->
	[C],
	{ code_type(C, xdigit(D))
	}.

xdigits([D0|D]) -->
	xdigit(D0), !,
	xdigits(D).
xdigits([]) -->
	[].

mkval([W0|Weights], Base, Val) :-
	mkval(Weights, Base, W0, Val).

mkval([], _, W, W).
mkval([H|T], Base, W0, W) :-
	W1 is W0*Base+H,
	mkval(T, Base, W1, W).


		 /*******************************
		 *	   END-OF-STRING	*
		 *******************************/


eos([], []).

		 /*******************************
		 *	     GENERATION		*
		 *******************************/

atom(Atom) -->
	{ atomic(Atom),
	  atom_codes(Atom, Codes)
	},
	string(Codes).





