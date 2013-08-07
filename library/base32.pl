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

:- module(base32,
	  [ base32/2,			% ?PlainText, ?Encoded
	    base32//1			% ?PlainText
	  ]).

/** <module> Base32 encoding and decoding

Prolog-based base32 encoding using  DCG   rules.  Encoding  according to
rfc4648.

For example:

==
1 ?- base32('Hello World', X).

X = 'JBSWY3DPEBLW64TMMQ======'

Yes
2 ?- base32(H, 'JBSWY3DPEBLW64TMMQ======').

H = 'Hello World'
==

@see	http://en.wikipedia.org/wiki/Base32
@author	Jan Wielemaker
*/

%%	base32(+Plain, -Encoded) is det.
%%	base32(-Plain, +Encoded) is det.
%
%	Translates between plaintext and base32  encoded atom or string.
%	See also base32//1.

base32(Plain, Encoded) :-
	nonvar(Plain), !,
	atom_codes(Plain, PlainCodes),
	phrase(base32(PlainCodes), EncCodes),
	atom_codes(Encoded, EncCodes).
base32(Plain, Encoded) :-
	nonvar(Encoded), !,
	atom_codes(Encoded, EncCodes),
	phrase(base32(PlainCodes), EncCodes),
	atom_codes(Plain, PlainCodes).
base32(_, _) :-
	throw(error(instantiation_error, _)).


%%	base32(+PlainText)// is det.
%%	base32(-PlainText)// is det.
%
%	Encode/decode list of character codes using _base32_.  See also
%	base32/2.

base32(Input) -->
	{ nonvar(Input) }, !,
	encode(Input).
base32(Output) -->
	decode(Output).


		 /*******************************
		 *	      ENCODING		*
		 *******************************/

encode([I0, I1, I2, I3, I4|Rest]) --> !,
	[O0, O1, O2, O3, O4, O5, O6, O7],
	{ A is (I0<<32)+(I1<<24)+(I2<<16)+(I3<<8)+I4,
	  O00 is (A>>35) /\ 0x1f,
	  O01 is (A>>30) /\ 0x1f,
	  O02 is (A>>25) /\ 0x1f,
	  O03 is (A>>20) /\ 0x1f,
	  O04 is (A>>15) /\ 0x1f,
	  O05 is (A>>10) /\ 0x1f,
	  O06 is  (A>>5) /\ 0x1f,
	  O07 is       A /\ 0x1f,
	  base32_char(O00, O0),
	  base32_char(O01, O1),
	  base32_char(O02, O2),
	  base32_char(O03, O3),
	  base32_char(O04, O4),
	  base32_char(O05, O5),
	  base32_char(O06, O6),
	  base32_char(O07, O7)
	},
	encode(Rest).
encode([I0, I1, I2, I3]) --> !,
	[O0, O1, O2, O3, O4, O5, O6, 0'=],
	{ A is (I0<<32)+(I1<<24)+(I2<<16)+(I3<<8),
	  O00 is (A>>35) /\ 0x1f,
	  O01 is (A>>30) /\ 0x1f,
	  O02 is (A>>25) /\ 0x1f,
	  O03 is (A>>20) /\ 0x1f,
	  O04 is (A>>15) /\ 0x1f,
	  O05 is (A>>10) /\ 0x1f,
	  O06 is  (A>>5) /\ 0x1f,
	  base32_char(O00, O0),
	  base32_char(O01, O1),
	  base32_char(O02, O2),
	  base32_char(O03, O3),
	  base32_char(O04, O4),
	  base32_char(O05, O5),
	  base32_char(O06, O6)
	}.
encode([I0, I1, I2]) --> !,
	[O0, O1, O2, O3, O4, 0'=, 0'=, 0'=],
	{ A is (I0<<32)+(I1<<24)+(I2<<16),
	  O00 is (A>>35) /\ 0x1f,
	  O01 is (A>>30) /\ 0x1f,
	  O02 is (A>>25) /\ 0x1f,
	  O03 is (A>>20) /\ 0x1f,
	  O04 is (A>>15) /\ 0x1f,
	  base32_char(O00, O0),
	  base32_char(O01, O1),
	  base32_char(O02, O2),
	  base32_char(O03, O3),
	  base32_char(O04, O4)
	}.
encode([I0, I1]) --> !,
	[O0, O1, O2, O3, 0'=, 0'=, 0'=, 0'=],
	{ A is (I0<<32)+(I1<<24),
	  O00 is (A>>35) /\ 0x1f,
	  O01 is (A>>30) /\ 0x1f,
	  O02 is (A>>25) /\ 0x1f,
	  O03 is (A>>20) /\ 0x1f,
	  base32_char(O00, O0),
	  base32_char(O01, O1),
	  base32_char(O02, O2),
	  base32_char(O03, O3)
	}.
encode([I0]) --> !,
	[O0, O1, 0'=, 0'=, 0'=, 0'=, 0'=, 0'=],
	{ A is (I0<<32),
	  O00 is (A>>35) /\ 0x1f,
	  O01 is (A>>30) /\ 0x1f,
	  base32_char(O00, O0),
	  base32_char(O01, O1)
	}.
encode([]) -->
	[].


		 /*******************************
		 *	      DECODE		*
		 *******************************/

decode(Text) -->
	[C0, C1, C2, C3, C4, C5, C6, C7], !,
	{ base32_char(B0, C0),
	  base32_char(B1, C1)
	}, !,
	{   C7 == 0'=
	->  (   C6 == 0'=, C5 == 0'=
	    ->  (   C4 == 0'=
	        ->  (   C3 = 0'=, C2 = 0'=
		    ->	A is (B0<<35) + (B1<<30),
			I0 is (A>>32) /\ 0xff,
			Text = [I0|Rest]
		    ;   base32_char(B2, C2),
			base32_char(B3, C3),
			base32_char(B4, C4),
			A is (B0<<35) + (B1<<30) + (B2<<25) + (B3<<20) + (B4<<15),
			I0 is (A>>32) /\ 0xff,
			I1 is (A>>24) /\ 0xff,
			Text = [I0,I1|Rest]
		    )
		;   base32_char(B2, C2),
	            base32_char(B3, C3),
	            base32_char(B4, C4),
	            base32_char(B5, C5),
	            A is (B0<<35) + (B1<<30) + (B2<<25) + (B3<<20) +
			 (B4<<15) + (B5<<10),
		    I0 is (A>>32) /\ 0xff,
	            I1 is (A>>24) /\ 0xff,
	            I2 is (A>>16) /\ 0xff,
	            Text = [I0,I1,I2|Rest]
	        )
	    ;   base32_char(B2, C2),
	        base32_char(B3, C3),
	        base32_char(B4, C4),
	        base32_char(B5, C5),
	        base32_char(B6, C6)
	    ->  A is (B0<<35) + (B1<<30) + (B2<<25) + (B3<<20) +
	             (B4<<15) + (B5<<10) + (B6<<5),
	        I0 is (A>>32) /\ 0xff,
	        I1 is (A>>24) /\ 0xff,
	        I2 is (A>>16) /\ 0xff,
	        I3 is  (A>>8) /\ 0xff,
	        Text = [I0,I1,I2,I3|Rest]
	    )
	;   base32_char(B2, C2),
	    base32_char(B3, C3),
	    base32_char(B4, C4),
	    base32_char(B5, C5),
	    base32_char(B6, C6),
	    base32_char(B7, C7)
	->  A is (B0<<35) + (B1<<30) + (B2<<25) + (B3<<20) +
		 (B4<<15) + (B5<<10) + (B6<<5) + B7,
	    I0 is (A>>32) /\ 0xff,
	    I1 is (A>>24) /\ 0xff,
	    I2 is (A>>16) /\ 0xff,
	    I3 is  (A>>8) /\ 0xff,
	    I4 is      A  /\ 0xff,
	    Text = [I0,I1,I2,I3,I4|Rest]
	},
	decode(Rest).
decode([]) -->
	[].


		 /*******************************
		 *   BASIC CHARACTER ENCODING	*
		 *******************************/

base32_char(00, 0'A).
base32_char(01, 0'B).
base32_char(02, 0'C).
base32_char(03, 0'D).
base32_char(04, 0'E).
base32_char(05, 0'F).
base32_char(06, 0'G).
base32_char(07, 0'H).
base32_char(08, 0'I).
base32_char(09, 0'J).
base32_char(10, 0'K).
base32_char(11, 0'L).
base32_char(12, 0'M).
base32_char(13, 0'N).
base32_char(14, 0'O).
base32_char(15, 0'P).
base32_char(16, 0'Q).
base32_char(17, 0'R).
base32_char(18, 0'S).
base32_char(19, 0'T).
base32_char(20, 0'U).
base32_char(21, 0'V).
base32_char(22, 0'W).
base32_char(23, 0'X).
base32_char(24, 0'Y).
base32_char(25, 0'Z).
base32_char(26, 0'2).
base32_char(27, 0'3).
base32_char(28, 0'4).
base32_char(29, 0'5).
base32_char(30, 0'6).
base32_char(31, 0'7).
