/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(utf8,
	  [ utf8_codes//1		% ?String
	  ]).

/** <module> UTF-8 encoding/decoding on lists of character codes.
*/

%%	utf8_codes(?Codes)// is det.
%
%	DCG translating between  a  Unicode   code-list  and  its  UTF-8
%	encoded  byte-string.  The  DCG  works   two  ways.  Encoding  a
%	code-list to a UTF-8 byte string is achieved using
%
%		phrase(utf8_codes(Codes), UTF8)
%
%	The  algorithm  is  a  close  copy    of  the  C-algorithm  used
%	internally and defined in src/pl-utf8.c
%
%	NOTE: in many  cases  you  can   avoid  this  library  and leave
%	encoding and decoding to I/O streams. If   only part of the data
%	is to be encoded the  encoding  of   a  stream  can  be switched
%	temporary using set_stream(Stream, encoding(utf8))
%
%	@see set_stream/2.

utf8_codes([H|T]) -->
	utf8_code(H), !,
	utf8_codes(T).
utf8_codes([]) -->
	[].

utf8_code(C) -->
	[C0],
	{ nonvar(C0) }, !,		% decoding version
	(   {C0 < 0x80}
	->  {C = C0}
	;   {C0/\0xe0 =:= 0xc0}
	->  utf8_cont(C1, 0),
	    {C is (C0/\0x1f)<<6\/C1}
	;   {C0/\0xf0 =:= 0xe0}
	->  utf8_cont(C1, 6),
	    utf8_cont(C2, 0),
	    {C is ((C0/\0xf)<<12)\/C1\/C2}
	;   {C0/\0xf8 =:= 0xf0}
	->  utf8_cont(C1, 12),
	    utf8_cont(C2, 6),
	    utf8_cont(C3, 0),
	    {C is ((C0/\0x7)<<18)\/C1\/C2\/C3}
	;   {C0/\0xfc =:= 0xf8}
	->  utf8_cont(C1, 18),
	    utf8_cont(C2, 12),
	    utf8_cont(C3, 6),
	    utf8_cont(C4, 0),
	    {C is ((C0/\0x3)<<24)\/C1\/C2\/C3\/C4}
	;   {C0/\0xfe =:= 0xfc}
	->  utf8_cont(C1, 24),
	    utf8_cont(C2, 18),
	    utf8_cont(C3, 12),
	    utf8_cont(C4, 6),
	    utf8_cont(C5, 0),
	    {C is ((C0/\0x1)<<30)\/C1\/C2\/C3\/C4\/C5}
	).
utf8_code(C) -->
	{ nonvar(C) }, !,		% encoding version
	(   { C < 0x80 }
	->  [C]
	;   { C < 0x800 }
	->  { C0 is 0xc0\/((C>>6)/\0x1f),
	      C1 is 0x80\/(C/\0x3f)
	    },
	    [C0,C1]
	;   { C < 0x10000 }
	->  { C0 is 0xe0\/((C>>12)/\0x0f),
	      C1 is 0x80\/((C>>6)/\0x3f),
	      C2 is 0x80\/(C/\0x3f)
	    },
	    [C0,C1,C2]
	;   { C < 0x200000 }
	->  { C0 is 0xf0\/((C>>18)/\0x07),
	      C1 is 0x80\/((C>>12)/\0x3f),
	      C2 is 0x80\/((C>>6)/\0x3f),
	      C3 is 0x80\/(C/\0x3f)
	    },
	    [C0,C1,C2,C3]
	;   { C < 0x4000000 }
	->  { C0 is 0xf8\/((C>>24)/\0x03),
	      C1 is 0x80\/((C>>18)/\0x3f),
	      C2 is 0x80\/((C>>12)/\0x3f),
	      C3 is 0x80\/((C>>6)/\0x3f),
	      C4 is 0x80\/(C/\0x3f)
	    },
	    [C0,C1,C2,C3,C4]
	;   { C < 0x80000000 }
	->  { C0 is 0xfc\/((C>>30)/\0x01),
	      C1 is 0x80\/((C>>24)/\0x3f),
	      C2 is 0x80\/((C>>18)/\0x3f),
	      C3 is 0x80\/((C>>12)/\0x3f),
	      C4 is 0x80\/((C>>6)/\0x3f),
	      C5 is 0x80\/(C/\0x3f)
	    },
	    [C0,C1,C2,C3,C4,C5]
	).

utf8_cont(Val, Shift) -->
	[C],
	{ C/\0xc0 =:= 0x80,
	  Val is (C/\0x3f)<<Shift
	}.
