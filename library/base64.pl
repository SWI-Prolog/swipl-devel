/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(base64,
	  [ base64/2,			% ?PlainText, ?Encoded
	    base64//1			% ?PlainText
	  ]).

/** <module> Base64 encoding and decoding

Prolog-based base64 encoding using  DCG   rules.  Encoding  according to
rfc2045. For example:

==
1 ?- base64('Hello World', X).

X = 'SGVsbG8gV29ybGQ='

Yes
2 ?- base64(H, 'SGVsbG8gV29ybGQ=').

H = 'Hello World'
==

@tbd	Stream I/O
@tbd	White-space introduction and parsing
@author	Jan Wielemaker
*/

%%	base64(+Plain, -Encoded) is det.
%%	base64(-Plain, +Encoded) is det.
%
%	Translates between plaintext and base64  encoded atom or string.
%	See also base64//1.

base64(Plain, Encoded) :-
	nonvar(Plain), !,
	atom_codes(Plain, PlainCodes),
	phrase(base64(PlainCodes), EncCodes),
	atom_codes(Encoded, EncCodes).
base64(Plain, Encoded) :-
	nonvar(Encoded), !,
	atom_codes(Encoded, EncCodes),
	phrase(base64(PlainCodes), EncCodes),
	atom_codes(Plain, PlainCodes).
base64(_, _) :-
	throw(error(instantiation_error, _)).


%%	base64(+PlainText)// is det.
%%	base64(-PlainText)// is det.
%
%	Encode/decode list of character codes using _base64_.  See also
%	base64/2.

base64(Input) -->
	{ nonvar(Input) }, !,
	encode(Input).
base64(Output) -->
	decode(Output).


		 /*******************************
		 *	      ENCODING		*
		 *******************************/

encode([I0, I1, I2|Rest]) --> !,
	[O0, O1, O2, O3],
	{ A is (I0<<16)+(I1<<8)+I2,
	  O00 is (A>>18) /\ 0x3f,
	  O01 is (A>>12) /\ 0x3f,
	  O02 is  (A>>6) /\ 0x3f,
	  O03 is       A /\ 0x3f,
	  base64_char(O00, O0),
	  base64_char(O01, O1),
	  base64_char(O02, O2),
	  base64_char(O03, O3)
	},
	encode(Rest).
encode([I0, I1]) --> !,
	[O0, O1, O2, 0'=],
	{ A is (I0<<16)+(I1<<8),
	  O00 is (A>>18) /\ 0x3f,
	  O01 is (A>>12) /\ 0x3f,
	  O02 is  (A>>6) /\ 0x3f,
	  base64_char(O00, O0),
	  base64_char(O01, O1),
	  base64_char(O02, O2)
	}.
encode([I0]) --> !,
	[O0, O1, 0'=, 0'=],
	{ A is (I0<<16),
	  O00 is (A>>18) /\ 0x3f,
	  O01 is (A>>12) /\ 0x3f,
	  base64_char(O00, O0),
	  base64_char(O01, O1)
	}.
encode([]) -->
	[].


		 /*******************************
		 *	      DECODE		*
		 *******************************/

decode(Text) -->
	[C0, C1, C2, C3], !,
	{ base64_char(B0, C0),
	  base64_char(B1, C1)
	}, !,
	{   C3 == 0'=
	->  (   C2 == 0'=
	    ->  A is (B0<<18) + (B1<<12),
	        I0 is (A>>16) /\ 0xff,
	        Text = [I0|Rest]
	    ;   base64_char(B2, C2)
	    ->  A is (B0<<18) + (B1<<12) + (B2<<6),
	        I0 is (A>>16) /\ 0xff,
	        I1 is  (A>>8) /\ 0xff,
	        Text = [I0,I1|Rest]
	    )
	;   base64_char(B2, C2),
	    base64_char(B3, C3)
	->  A is (B0<<18) + (B1<<12) + (B2<<6) + B3,
	    I0 is (A>>16) /\ 0xff,
	    I1 is  (A>>8) /\ 0xff,
	    I2 is      A  /\ 0xff,
	    Text = [I0,I1,I2|Rest]
	},
	decode(Rest).
decode([]) -->
	[].


		 /*******************************
		 *   BASIC CHARACTER ENCODING	*
		 *******************************/

base64_char(00, 0'A).
base64_char(01, 0'B).
base64_char(02, 0'C).
base64_char(03, 0'D).
base64_char(04, 0'E).
base64_char(05, 0'F).
base64_char(06, 0'G).
base64_char(07, 0'H).
base64_char(08, 0'I).
base64_char(09, 0'J).
base64_char(10, 0'K).
base64_char(11, 0'L).
base64_char(12, 0'M).
base64_char(13, 0'N).
base64_char(14, 0'O).
base64_char(15, 0'P).
base64_char(16, 0'Q).
base64_char(17, 0'R).
base64_char(18, 0'S).
base64_char(19, 0'T).
base64_char(20, 0'U).
base64_char(21, 0'V).
base64_char(22, 0'W).
base64_char(23, 0'X).
base64_char(24, 0'Y).
base64_char(25, 0'Z).
base64_char(26, 0'a).
base64_char(27, 0'b).
base64_char(28, 0'c).
base64_char(29, 0'd).
base64_char(30, 0'e).
base64_char(31, 0'f).
base64_char(32, 0'g).
base64_char(33, 0'h).
base64_char(34, 0'i).
base64_char(35, 0'j).
base64_char(36, 0'k).
base64_char(37, 0'l).
base64_char(38, 0'm).
base64_char(39, 0'n).
base64_char(40, 0'o).
base64_char(41, 0'p).
base64_char(42, 0'q).
base64_char(43, 0'r).
base64_char(44, 0's).
base64_char(45, 0't).
base64_char(46, 0'u).
base64_char(47, 0'v).
base64_char(48, 0'w).
base64_char(49, 0'x).
base64_char(50, 0'y).
base64_char(51, 0'z).
base64_char(52, 0'0).
base64_char(53, 0'1).
base64_char(54, 0'2).
base64_char(55, 0'3).
base64_char(56, 0'4).
base64_char(57, 0'5).
base64_char(58, 0'6).
base64_char(59, 0'7).
base64_char(60, 0'8).
base64_char(61, 0'9).
base64_char(62, 0'+).
base64_char(63, 0'/).
