/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2013, University of Amsterdam
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

:- module(writef,
          [ writef/1,                   % +Format
            writef/2,                   % +Format, +Args
            swritef/2,                  % -String, +Format
            swritef/3                   % -String, +Format, +Args
          ]).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Old-style formatted write

This library provides writef/1 and   friends. These predicates originate
from Edinburgh C-Prolog and and provided for compatibility purposes. New
code should use format/1, format/2  and   friends,  which  are currently
supported by more Prolog implementations.

The   writef-family   of   predicates   conflicts    with   the   modern
_|character-esacapes|_ flag about  the   interpretation  of \-sequences.
This can be avoided by

  1. Disable character escapes (not recommended unless one wants to
  run really outdated code unmodified).
  2. Double the \ for conflicting interpretations
  3. Use ISO compliant alternatives for conflicting interpretations

@copyright      Copied from Edinburgh C-Prolog. Original version by Byrd,
                changed many times since.
*/

%!  writef(+Format) is det.
%!  writef(+Format, +Arguments) is det.
%
%   Formatted write to the  =current_output=.   Format  is  a format
%   specifier. Some escape sequences require  arguments that must be
%   provided in the list Arguments. There   are  two types of escape
%   sequences: special characters  start  with   =|\|=  and  include
%   arguments start with =|%|=. The special character sequences are:
%
%       | =|\n|= | Output a newline character |
%       | =|\l|= | Output a line separator (same as =|\n|=) |
%       | =|\r|= | Output a carriage-return character (ASCII 13) |
%       | =|\r|= | Output a TAB character (ASCII 9) |
%       | =|\\|= | Output =|\|= |
%       | =|\%|= | Output =|%|= |
%       | =|\nnn|= | Output character <nnn>. <nnn> is a 1-3 decimal number |
%
%   Escape sequences to include arguments  from Arguments. Each time
%   a %-escape sequence is found in   Format  the next argument from
%   Arguments is formatted according to the specification.
%
%       | =|%t|= | print/1 the next item (mnemonic: term) |
%       | =|%w|= | write/1 the next item |
%       | =|%q|= | writeq/1 the next item  |
%       | =|%d|= | display/1 the next item |
%       | =|%n|= | Put the next item as a character |
%       | =|%r|= | Write the next item N times where N is the second item (an integer) |
%       | =|%s|= | Write the next item as a String (so it must be a list of characters) |
%       | =|%f|= |Perform a ttyflush/0 (no items used) |
%       | =|%Nc|= | Write the next item Centered in N columns. |
%       | =|%Nl|= | Write the next item Left justified in N columns. |
%       | =|%Nr|= | Write the next item Right justified in N columns. |
%
%   @deprecated New code should use format/1, format/2, etc.

writef(Format) :-
    writef(Format, []).

writef([F|String], List) :-
    '$writefs'([F|String], List),
    fail.                           % clean up global stack
writef(String, List) :-
    string(String),
    string_codes(String, Fstring),
    '$writefs'(Fstring, List),
    fail.                           % clean up global stack
writef(Format, List) :-
    atom(Format),
    name(Format, Fstring),
    '$writefs'(Fstring, List),
    fail.                           % clean up global stack
writef(_, _).

%!  swritef(-String, +Format) is det.
%!  swritef(-String, +Format, +Arguments) is det.
%
%   Use writef/1 or writef/2 and  write   the  result to a _string_.
%   Note that this is a  string   in  the sense of string_codes/2,
%   _not_ a list of character(-code)s.
%
%   @deprecated.  See format/2,3 and/or with_output_to/2.

swritef(String, Format, Arguments) :-
    with_output_to(string(String), writef(Format, Arguments)).
swritef(String, Format) :-
    with_output_to(string(String), writef(Format)).

                        % Formatted write for a string (i.e. a list of
                        % character codes).

'$writefs'([], _).
'$writefs'([0'%, A|Rest], List) :-      %   %<$action'>
    '$action'(A, List, More),
    !,
    '$writefs'(Rest, More).
'$writefs'([0'%, D|Rest], [Head|Tail]) :-       %   %<columns><just>
    between(0'0, 0'9, D),
    '$getpad'(Size, Just, [D|Rest], More),
    !,
    '$padout'(Head, Size, Just),
    '$writefs'(More, Tail).
'$writefs'([0'\\, C|Rest], List) :-     %   \<special>
    '$special'(C, Char),
    !,
    put(Char),
    '$writefs'(Rest, List).
'$writefs'([0'\\|Rest], List) :-        %   \<character code in decimal>
    '$getcode'(Char, Rest, More),
    !,
    put(Char),
    '$writefs'(More, List).
'$writefs'([Char|Rest], List) :-        %   <ordinary character>
    put(Char),
    '$writefs'(Rest, List).


'$action'(0't, [Head|Tail], Tail) :-    %   Term
    print(Head).
'$action'(0'd, [Head|Tail], Tail) :-    %   Display
    write_canonical(Head).
'$action'(0'w, [Head|Tail], Tail) :-    %   Write
    write(Head).
'$action'(0'q, [Head|Tail], Tail) :-    %   Quoted
    writeq(Head).
'$action'(0'p,  [Head|Tail], Tail) :-   %   Print
    print(Head).
'$action'(0'f, List, List) :-           %   Flush
    ttyflush.
'$action'(0'n, [Char|Tail], Tail) :-    %   iNteger (character)
    put(Char).
'$action'(0'r, [Thing, Times|Tail], Tail) :-    %   Repeatedly
    '$writelots'(Times, Thing).
'$action'(0's, [Head|Tail], Tail) :-    %   String
    '$padout'(Head).

'$special'(0'n, 10).            /*  n  */
'$special'(0'l, 10).            /*  l  */
'$special'(0'r, 10).            /*  r  */
'$special'(0't,  9).            /*  t  */
'$special'(0'\\, 0'\\).         /*  \  */
'$special'(0'%, 0'%).           /*  %  */

'$getcode'(Char, In, Out) :-
    '$getdigits'(3, Digits, In, Out),
    Digits = [_|_],
    name(Char, Digits),
    Char < 128.

'$getdigits'(Limit, [Digit|Digits], [Digit|Out0], Out) :-
    Limit > 0,
    between(0'0, 0'9, Digit),
    Fewer is Limit - 1,
    !,
    '$getdigits'(Fewer, Digits, Out0, Out).
'$getdigits'(_, [], Out, Out).

'$writelots'(N, T) :-
    N > 0,
    !,
    write(T),
    M is N - 1,
    '$writelots'(M, T).
'$writelots'(_, _).

/*  The new formats are %nC, %nL, and %nR for centered, left, and right
    justified output of atoms, integers, and strings.  This is meant to
    simplify the production of tabular output when it is appropriate.
    At least one space will always precede/follow the item written.
*/

'$getpad'(Size, Just, In, Out) :-
    '$getdigits'(3, Digits, In, [Out1|Out]),
    name(Size, Digits),
    '$getpad'(Out1, Just).

'$getpad'(0'r, r).              %  right justified
'$getpad'(0'l, l).              %  left justified
'$getpad'(0'c, c).              %  centered
'$getpad'(0'R, r).              %  right justified
'$getpad'(0'L, l).              %  left justified
'$getpad'(0'C, c).              %  centered


                                %   '$padout'(A, S, J) writes the item A in a
                                %   field of S or more characters, Justified.

'$padout'(String, Size, Just) :-
    '$string'(String),
    !,
    name(Atom, String),
    '$padout'(Atom, Size, Just).
'$padout'(Term, Size, Just) :-
    format(string(Atom), Term, Atom),
    atom_length(Atom, Length),
    '$padout'(Just, Size, Length, Left, Right),
    tab(Left),
    write(Atom),
    tab(Right).

'$string'(0) :- !, fail.
'$string'([]) :- !.
'$string'([H|T]) :-
    '$print'(H),
    !,
    '$string'(T).

'$print'(10).                   % newline
'$print'(9).                    % tab
'$print'(X) :-
    integer(X),
    between(32, 0'~, X).


                                %   '$padout'(Just, Size, Length, Left, Right)
                                %   calculates the number of spaces to put
                                %   on the Left and Right of an item needing
                                %   Length characters in a field of Size.

'$padout'(l, Size, Length, 0, Right) :-
    !,
    Right is max(1, Size-Length).
'$padout'(r, Size, Length, Left, 0) :-
    !,
    Left is max(1, Size-Length).
'$padout'(c, Size, Length, Left, Right) :-
    Left is max(1, round((Size - Length)/2)),
    Right is max(1, Size - Length - Left).

                                %   '$padout'(Str) writes a string.

'$padout'([Head|Tail]) :-
    !,
    put(Head),
    '$padout'(Tail).
'$padout'([]).
