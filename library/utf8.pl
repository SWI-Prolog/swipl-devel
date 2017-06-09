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

:- module(utf8,
          [ utf8_codes//1               % ?String
          ]).

/** <module> UTF-8 encoding/decoding on lists of character codes.
*/

%!  utf8_codes(?Codes)// is det.
%
%   DCG translating between  a  Unicode   code-list  and  its  UTF-8
%   encoded  byte-string.  The  DCG  works   two  ways.  Encoding  a
%   code-list to a UTF-8 byte string is achieved using
%
%           phrase(utf8_codes(Codes), UTF8)
%
%   The  algorithm  is  a  close  copy    of  the  C-algorithm  used
%   internally and defined in src/pl-utf8.c
%
%   NOTE: in many  cases  you  can   avoid  this  library  and leave
%   encoding and decoding to I/O streams. If   only part of the data
%   is to be encoded the  encoding  of   a  stream  can  be switched
%   temporary using set_stream(Stream, encoding(utf8))
%
%   @see set_stream/2.

utf8_codes([H|T]) -->
    utf8_code(H),
    !,
    utf8_codes(T).
utf8_codes([]) -->
    [].

utf8_code(C) -->
    [C0],
    { nonvar(C0) },                % decoding version
    !,
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
    { nonvar(C) },                 % encoding version
    !,
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
