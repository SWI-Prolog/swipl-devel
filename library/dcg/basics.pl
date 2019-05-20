/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2016, University of Amsterdam
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

:- module(dcg_basics,
	  [ white//0,			% <white inside line>
	    whites//0,			% <white inside line>*
	    blank//0,			% <blank>
	    blanks//0,			% <blank>*
	    nonblank//1,		% <nonblank>
	    nonblanks//1,		% <nonblank>* --> chars		(long)
	    blanks_to_nl//0,		% [space,tab,ret]*nl
	    string//1,			% <any>* -->chars		(short)
	    string_without//2,		% Exclude, -->chars		(long)
					% Characters
	    alpha_to_lower//1,		% Get lower|upper, return lower
					% Decimal numbers
	    digits//1,			% [0-9]* -->chars
	    digit//1,			% [0-9] --> char
	    integer//1,			% [+-][0-9]+ --> integer
	    float//1,			% [+-]?[0-9]+(.[0-9]*)?(e[+-]?[0-9]+)? --> float
	    number//1,			% integer | float
					% Hexadecimal numbers
	    xdigits//1,			% [0-9A-Fa-f]* --> 0-15*
	    xdigit//1,			% [0-9A-Fa-f] --> 0-15
	    xinteger//1,		% [0-9A-Fa-f]+ --> integer

	    prolog_var_name//1,		% Read a Prolog variable name

	    eos//0,			% Test end of input.
	    remainder//1,		% -List

					% generation (TBD)
	    atom//1			% generate atom
	  ]).
:- use_module(library(lists)).
:- use_module(library(error)).


/** <module> Various general DCG utilities

This library provides various commonly  used   DCG  primitives acting on
list  of  character  *codes*.  Character   classification  is  based  on
code_type/2.

This module started its life as  library(http/dcg_basics) to support the
HTTP protocol. Since then, it was increasingly  used in code that has no
relation to HTTP and therefore  this  library   was  moved  to  the core
library.

@tbd	This is just a starting point. We need a comprehensive set of
	generally useful DCG primitives.
*/

%%	string_without(+EndCodes, -Codes)// is det.
%
%	Take as many codes from the input  until the next character code
%	appears in the list EndCodes.  The   terminating  code itself is
%	left on the input.  Typical  use  is   to  read  upto  a defined
%	delimiter such as a newline  or   other  reserved character. For
%	example:
%
%	    ==
%	        ...,
%	        string_without("\n", RestOfLine)
%	    ==
%
%	@arg EndCodes is a list of character codes.
%	@see string//1.

string_without(End, Codes) -->
	{ string(End), !,
	  string_codes(End, EndCodes)
	},
	list_string_without(EndCodes, Codes).
string_without(End, Codes) -->
	list_string_without(End, Codes).

list_string_without(Not, [C|T]) -->
	[C],
	{ \+ memberchk(C, Not)
	}, !,
	list_string_without(Not, T).
list_string_without(_, []) -->
	[].

%%	string(-Codes)// is nondet.
%
%	Take as few as possible tokens from the input, taking one more
%	each time on backtracking. This code is normally followed by a
%	test for a delimiter.  For example:
%
%	==
%	upto_colon(Atom) -->
%		string(Codes), ":", !,
%		{ atom_codes(Atom, Codes) }.
%	==
%
%	@see string_without//2.

string([]) -->
	[].
string([H|T]) -->
	[H],
	string(T).

%%	blanks// is det.
%
%	Skip zero or more white-space characters.

blanks -->
	blank, !,
	blanks.
blanks -->
	[].

%%	blank// is semidet.
%
%	Take next =space= character from input. Space characters include
%	newline.
%
%	@see white//0

blank -->
	[C],
	{ nonvar(C),
	  code_type(C, space)
	}.

%%	nonblanks(-Codes)// is det.
%
%	Take all =graph= characters

nonblanks([H|T]) -->
	[H],
	{ code_type(H, graph)
	}, !,
	nonblanks(T).
nonblanks([]) -->
	[].

%%	nonblank(-Code)// is semidet.
%
%	Code is the next non-blank (=graph=) character.

nonblank(H) -->
	[H],
	{ code_type(H, graph)
	}.

%%	blanks_to_nl// is semidet.
%
%	Take a sequence of blank//0 codes if blanks are followed by a
%	newline or end of the input.

blanks_to_nl -->
	"\n", !.
blanks_to_nl -->
	blank, !,
	blanks_to_nl.
blanks_to_nl -->
	eos.

%%	whites// is det.
%
%	Skip white space _inside_ a line.
%
%	@see blanks//0 also skips newlines.

whites -->
	white, !,
	whites.
whites -->
	[].

%%	white// is semidet.
%
%	Take next =white= character from input. White characters do
%	_not_ include newline.

white -->
	[C],
	{ nonvar(C),
	  code_type(C, white)
	}.


		 /*******************************
		 *	 CHARACTER STUFF	*
		 *******************************/

%%	alpha_to_lower(?C)// is semidet.
%
%	Read a letter (class  =alpha=)  and   return  it  as a lowercase
%	letter. If C is instantiated and the  DCG list is already bound,
%	C must be =lower= and matches both a lower and uppercase letter.
%	If the output list is unbound, its first element is bound to C.
%	For example:
%
%	  ==
%	  ?- alpha_to_lower(0'a, `AB`, R).
%	  R = [66].
%	  ?- alpha_to_lower(C, `AB`, R).
%	  C = 97, R = [66].
%	  ?- alpha_to_lower(0'a, L, R).
%	  L = [97|R].
%	  ==

alpha_to_lower(L) -->
	[C],
	{   nonvar(C)
	->  code_type(C, alpha),
	    code_type(C, to_upper(L))
	;   L = C
	}.


		 /*******************************
		 *	      NUMBERS		*
		 *******************************/

%%	digits(?Chars)// is det.
%%	digit(?Char)// is det.
%%	integer(?Integer)// is det.
%
%	Number processing. The predicate  digits//1   matches  a posibly
%	empty set of digits,  digit//1  processes   a  single  digit and
%	integer processes an  optional  sign   followed  by  a non-empty
%	sequence of digits into an integer.

digits([H|T]) -->
	digit(H), !,
	digits(T).
digits([]) -->
	[].

digit(C) -->
	[C],
	{ code_type(C, digit)
	}.

integer(I, Head, Tail) :-
	nonvar(I), !,
	format(codes(Head, Tail), '~d', [I]).
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


%%	float(?Float)// is det.
%
%	Process a floating  point  number.   The  actual  conversion  is
%	controlled by number_codes/2.

float(F, Head, Tail) :-
	float(F), !,
	with_output_to(codes(Head, Tail), write(F)).
float(F) -->
	number(F),
	{ float(F) }.

%%	number(+Number)// is det.
%%	number(-Number)// is semidet.
%
%	Generate extract a number. Handles   both  integers and floating
%	point numbers.

number(N, Head, Tail) :-
	number(N), !,
	format(codes(Head, Tail), '~w', N).
number(N) -->
	{ var(N)
	},
	!,
	int_codes(I),
	(   dot,
	    digit(DF0),
	    digits(DF)
	->  {F = [0'., DF0|DF]}
	;   {F = []}
	),
	(   exp
	->  int_codes(DI),
	    {E=[0'e|DI]}
	;   {E = []}
	),
	{ append([I, F, E], Codes),
	  number_codes(N, Codes)
	}.
number(N) -->
	{ type_error(number, N) }.

sign(0'-) --> "-".
sign(0'+) --> "+".

dot --> ".".

exp --> "e".
exp --> "E".

		 /*******************************
		 *	    HEX NUMBERS		*
		 *******************************/

%%	xinteger(+Integer)// is det.
%%	xinteger(-Integer)// is semidet.
%
%	Generate or extract an integer from   a  sequence of hexadecimal
%	digits. Hexadecimal characters include both  uppercase (A-F) and
%	lowercase (a-f) letters. The value may   be  preceeded by a sign
%	(+/-)

xinteger(Val, Head, Tail) :-
	integer(Val), !,
	format(codes(Head, Tail), '~16r', [Val]).
xinteger(Val) -->
	sign(C), !,
	xdigit(D0),
	xdigits(D),
	{ mkval([D0|D], 16, Val0),
	  (   C == 0'-
	  ->  Val is -Val0
	  ;   Val = Val0
	  )
	}.
xinteger(Val) -->
	xdigit(D0),
	xdigits(D),
	{ mkval([D0|D], 16, Val)
	}.

%%	xdigit(-Weight)// is semidet.
%
%	True if the next code is a  hexdecimal digit with Weight. Weight
%	is  between  0  and  15.  Hexadecimal  characters  include  both
%	uppercase (A-F) and lowercase (a-f) letters.

xdigit(D) -->
	[C],
	{ code_type(C, xdigit(D))
	}.

%%	xdigits(-WeightList)// is det.
%
%	List of weights of a sequence   of hexadecimal codes. WeightList
%	may be empty. Hexadecimal  characters   include  both  uppercase
%	(A-F) and lowercase (a-f) letters.

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

%%	eos//
%
%	Matches  end-of-input.  The  implementation    behaves   as  the
%	following portable implementation:
%
%	  ==
%	  eos --> call(eos_).
%	  eos_([], []).
%	  ==
%
%	@tbd	This is a difficult concept and violates the _context free_
%		property of DCGs.  Explain the exact problems.

eos([], []).

%%	remainder(-List)//
%
%	Unify List with the remainder of the input.

remainder(List, List, []).


		 /*******************************
		 *	   PROLOG SYNTAX		*
		 *******************************/

%%	prolog_var_name(-Name:atom)// is semidet.
%
%	Matches a Prolog variable name. Primarily  intended to deal with
%	quasi quotations that embed Prolog variables.

prolog_var_name(Name) -->
	[C0], { code_type(C0, prolog_var_start) }, !,
	prolog_id_cont(CL),
	{ atom_codes(Name, [C0|CL]) }.

prolog_id_cont([H|T]) -->
	[H], { code_type(H, prolog_identifier_continue) }, !,
	prolog_id_cont(T).
prolog_id_cont([]) --> "".


		 /*******************************
		 *	     GENERATION		*
		 *******************************/

%%	atom(++Atom)// is det.
%
%	Generate codes of Atom.  Current implementation uses write/1,
%	dealing with any Prolog term.  Atom must be ground though.

atom(Atom, Head, Tail) :-
	must_be(ground, Atom),
	format(codes(Head, Tail), '~w', [Atom]).
