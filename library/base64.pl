/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2017, University of Amsterdam
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

:- module(base64,
          [ base64_encoded/3,		% ?Plain, ?Encoded, +Options
            base64_encoded//2,          % ?Plain, +Options

            base64/2,                   % ?PlainText, ?Encoded
            base64//1,                  % ?PlainText

            base64url/2,                % ?PlainText, ?Encoded
            base64url//1                % ?PlainText
          ]).
:- use_module(library(error)).
:- use_module(library(option)).

/** <module> Base64 encoding and decoding

Prolog-based base64 encoding using  DCG   rules.  Encoding  according to
rfc2045. For example:

==
1 ?- base64('Hello World', X).
X = 'SGVsbG8gV29ybGQ='.

2 ?- base64(H, 'SGVsbG8gV29ybGQ=').
H = 'Hello World'.
==

The Base64URL encoding provides a URL and file name friendly alternative
to base64. Base64URL encoded strings do not contain white space.

@tbd    Stream I/O
@tbd    White-space introduction and parsing
@author Jan Wielemaker
*/

%!  base64_encoded(+Plain, -Encoded, +Options) is det.
%!  base64_encoded(-Plain, +Encoded, +Options) is det.
%
%   General the base64 encoding and   decoding.  This predicate subsumes
%   base64/2  and  base64url/2,  providing  control  over  padding,  the
%   characters used for encoding and the output type. Options:
%
%     - charset(+Charset)
%     Define the encoding character set to use.  The (default) `classic`
%     uses the classical rfc2045 characters.  The value `url` uses URL
%     and file name friendly characters.  See base64url/2.
%     - padding(+Boolean)
%     If `true` (default), the output is padded with `=` characters.
%     - as(+Type)
%     Defines the type of the output.  One of `string` (default) or
%     `atom`.
%
%   @arg Plain is an atom or string containing the unencoded (plain)
%   text.
%   @arg Encoded is an atom or string containing the base64 encoded
%   version of Plain.

base64_encoded(Plain, Encoded, Options) :-
    option(charset(CharSet), Options, classic),
    option(padding(Padding), Options, true),
    option(as(As), Options, string),
    (   nonvar(Plain)
    ->  atom_codes(Plain, PlainCodes),
        phrase(base64(Padding, PlainCodes, CharSet), EncCodes),
        as(As, Encoded, EncCodes)
    ;   nonvar(Encoded)
    ->  atom_codes(Encoded, EncCodes),
        phrase(base64(Padding, PlainCodes, CharSet), EncCodes),
        as(As, Plain, PlainCodes)
    ;   instantiation_error(base64(Plain, Encoded))
    ).

as(atom, Atom, Codes) :-
    !,
    atom_codes(Atom, Codes).
as(string, String, Codes) :-
    !,
    string_codes(String, Codes).
as(As, _, _) :-
    must_be(oneof([atom,string]), As).

%!  base64(+Plain, -Encoded) is det.
%!  base64(-Plain, +Encoded) is det.
%
%   Translates between plaintext and base64  encoded atom or string.
%   See also base64//1.

base64(Plain, Encoded) :-
    nonvar(Plain),
    !,
    atom_codes(Plain, PlainCodes),
    phrase(base64(true, PlainCodes, classic), EncCodes),
    atom_codes(Encoded, EncCodes).
base64(Plain, Encoded) :-
    nonvar(Encoded),
    !,
    atom_codes(Encoded, EncCodes),
    phrase(base64(true, PlainCodes, classic), EncCodes),
    atom_codes(Plain, PlainCodes).
base64(Plain, Encoded) :-
    instantiation_error(base64(Plain, Encoded)).

%!  base64url(+Plain, -Encoded) is det.
%!  base64url(-Plain, +Encoded) is det.
%
%   Translates between plaintext  and  base64url   encoded  atom  or
%   string. Base64URL encoded values can safely  be used as URLs and
%   file names. The use "-" instead of   "+", "_" instead of "/" and
%   do not use padding. This implies   that the encoded value cannot
%   be embedded inside a longer string.

base64url(Plain, Encoded) :-
    nonvar(Plain),
    !,
    atom_codes(Plain, PlainCodes),
    phrase(encode(false, PlainCodes, url), EncCodes),
    atom_codes(Encoded, EncCodes).
base64url(Plain, Encoded) :-
    nonvar(Encoded),
    !,
    atom_codes(Encoded, EncCodes),
    phrase(decode(false, PlainCodes, url), EncCodes),
    atom_codes(Plain, PlainCodes).
base64url(_, _) :-
    throw(error(instantiation_error, _)).

%!  base64_encoded(+PlainText, +Options)// is det.
%!  base64_encoded(-PlainText, +Options)// is det.

base64_encoded(PlainText, Options) -->
    { option(charset(CharSet), Options, classic),
      option(padding(Padding), Options, true)
    },
    base64(Padding, PlainText, CharSet).


%!  base64(+PlainText)// is det.
%!  base64(-PlainText)// is det.
%
%   Encode/decode list of character codes using _base64_.  See also
%   base64/2.

base64(PlainText) -->
    base64(true, PlainText, classic).

%!  base64url(+PlainText)// is det.
%!  base64url(-PlainText)// is det.
%
%   Encode/decode list of character codes  using Base64URL. See also
%   base64url/2.

base64url(PlainText) -->
    base64(false, PlainText, url).

base64(Padded, Input, Charset) -->
    { nonvar(Input) },
    !,
    encode(Padded, Input, Charset).
base64(Padded, Output, Charset) -->
    decode(Padded, Output, Charset).

                 /*******************************
                 *            ENCODING          *
                 *******************************/

%!  encode(+Padded, +PlainText, +Charset)//

encode(Padded, [I0, I1, I2|Rest], Charset) -->
    !,
    [O0, O1, O2, O3],
    { A is (I0<<16)+(I1<<8)+I2,
      O00 is (A>>18) /\ 0x3f,
      O01 is (A>>12) /\ 0x3f,
      O02 is  (A>>6) /\ 0x3f,
      O03 is       A /\ 0x3f,
      base64_char(Charset, O00, O0),
      base64_char(Charset, O01, O1),
      base64_char(Charset, O02, O2),
      base64_char(Charset, O03, O3)
    },
    encode(Padded, Rest, Charset).
encode(true, [I0, I1], Charset) -->
    !,
    [O0, O1, O2, 0'=],
    { A is (I0<<16)+(I1<<8),
      O00 is (A>>18) /\ 0x3f,
      O01 is (A>>12) /\ 0x3f,
      O02 is  (A>>6) /\ 0x3f,
      base64_char(Charset, O00, O0),
      base64_char(Charset, O01, O1),
      base64_char(Charset, O02, O2)
    }.
encode(true, [I0], Charset) -->
    !,
    [O0, O1, 0'=, 0'=],
    { A is (I0<<16),
      O00 is (A>>18) /\ 0x3f,
      O01 is (A>>12) /\ 0x3f,
      base64_char(Charset, O00, O0),
      base64_char(Charset, O01, O1)
    }.
encode(false, [I0, I1], Charset) -->
    !,
    [O0, O1, O2],
    { A is (I0<<16)+(I1<<8),
      O00 is (A>>18) /\ 0x3f,
      O01 is (A>>12) /\ 0x3f,
      O02 is  (A>>6) /\ 0x3f,
      base64_char(Charset, O00, O0),
      base64_char(Charset, O01, O1),
      base64_char(Charset, O02, O2)
    }.
encode(false, [I0], Charset) -->
    !,
    [O0, O1],
    { A is (I0<<16),
      O00 is (A>>18) /\ 0x3f,
      O01 is (A>>12) /\ 0x3f,
      base64_char(Charset, O00, O0),
      base64_char(Charset, O01, O1)
    }.
encode(_, [], _) -->
    [].


                 /*******************************
                 *            DECODE            *
                 *******************************/

%!  decode(+Padded, -PlainText, +Charset)//

decode(true, Text, Charset) -->
    [C0, C1, C2, C3],
    !,
    { base64_char(Charset, B0, C0),
      base64_char(Charset, B1, C1)
    },
    !,
    {   C3 == 0'=
    ->  (   C2 == 0'=
        ->  A is (B0<<18) + (B1<<12),
            I0 is (A>>16) /\ 0xff,
            Text = [I0|Rest]
        ;   base64_char(Charset, B2, C2)
        ->  A is (B0<<18) + (B1<<12) + (B2<<6),
            I0 is (A>>16) /\ 0xff,
            I1 is  (A>>8) /\ 0xff,
            Text = [I0,I1|Rest]
        )
    ;   base64_char(Charset, B2, C2),
        base64_char(Charset, B3, C3)
    ->  A is (B0<<18) + (B1<<12) + (B2<<6) + B3,
        I0 is (A>>16) /\ 0xff,
        I1 is  (A>>8) /\ 0xff,
        I2 is      A  /\ 0xff,
        Text = [I0,I1,I2|Rest]
    },
    decode(true, Rest, Charset).
decode(false, Text, Charset) -->
    [C0, C1, C2, C3],
    !,
    { base64_char(Charset, B0, C0),
      base64_char(Charset, B1, C1),
      base64_char(Charset, B2, C2),
      base64_char(Charset, B3, C3),
      A is (B0<<18) + (B1<<12) + (B2<<6) + B3,
      I0 is (A>>16) /\ 0xff,
      I1 is  (A>>8) /\ 0xff,
      I2 is      A  /\ 0xff,
      Text = [I0,I1,I2|Rest]
    },
    decode(false, Rest, Charset).
decode(false, Text, Charset) -->
    [C0, C1, C2],
    !,
    { base64_char(Charset, B0, C0),
      base64_char(Charset, B1, C1),
      base64_char(Charset, B2, C2),
      A is (B0<<18) + (B1<<12) + (B2<<6),
      I0 is (A>>16) /\ 0xff,
      I1 is  (A>>8) /\ 0xff,
      Text = [I0,I1]
    }.
decode(false, Text, Charset) -->
    [C0, C1],
    !,
    { base64_char(Charset, B0, C0),
      base64_char(Charset, B1, C1),
      A is (B0<<18) + (B1<<12),
      I0 is (A>>16) /\ 0xff,
      Text = [I0]
    }.
decode(_, [], _) -->
    [].



                 /*******************************
                 *   BASIC CHARACTER ENCODING   *
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

base64url_char_x(62, 0'-).
base64url_char_x(63, 0'_).

base64_char(classic, Value, Char) :-
    (   base64_char(Value, Char)
    ->  true
    ;   syntax_error(base64_char(Value, Char))
    ).
base64_char(url, Value, Char) :-
    (   base64url_char_x(Value, Char)
    ->  true
    ;   base64_char(Value, Char)
    ->  true
    ;   syntax_error(base64_char(Value, Char))
    ).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(syntax_error(base64_char(_D,E))) -->
    { nonvar(E) },
    !,
    [ 'Illegal Base64 character: "~c"'-[E] ].
