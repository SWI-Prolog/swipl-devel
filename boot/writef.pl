/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: writef/1, writef/2, write_ln/1
*/

:- module($writef,
	[ write_ln/1
	, writef/1
	, writef/2
	, swritef/2
	, swritef/3
	]).

%   Copied from Edinburgh C-Prolog. Original version by Byrd, changed
%   many times since then.
%
%   Changes for version 1.3.0:
%	
%	- Format can be of type string.
%	- Adjustment now allowed for arbitrary types.
%	- Uses more appropriate SWI-Prolog built-in predicates.
%	- Uses 0'<letter> to specify ascii values (more readable).

write_ln(X) :-
	write(X), nl.

writef(Format) :-
	writef(Format, []).

writef([F|String], List) :-
	$writefs([F|String], List),
	fail.				% clean up global stack
writef(String, List) :-
	string(String),
	string_to_list(String, Fstring),
	$writefs(Fstring, List),
	fail.				% clean up global stack
writef(Format, List) :-
	atom(Format),
	name(Format, Fstring),
	$writefs(Fstring, List),
	fail.				% clean up global stack
writef(_, _).

swritef(Atom, Format, Arguments) :-
	$write_on_string(writef(Format, Arguments), Atom).
swritef(Atom, Format) :-
	$write_on_string(writef(Format), Atom).

			% Formatted write for a string (i.e. a list of
			% character codes).

$writefs([], _).
$writefs([0'%, A|Rest], List) :-	%   %<$action'>
	$action(A, List, More), !,
	$writefs(Rest, More).
$writefs([0'%, D|Rest], [Head|Tail]) :-	%   %<columns><just>
	between(0'0, 0'9, D),
	$getpad(Size, Just, [D|Rest], More),  !,
	$padout(Head, Size, Just),
	$writefs(More, Tail).
$writefs([0'\, C|Rest], List) :-	%   \<special>
	$special(C, Char), !,
	put(Char),
	$writefs(Rest, List).
$writefs([0'\|Rest], List) :-		%   \<character code in decimal>
	$getcode(Char, Rest, More), !,
	put(Char),
	$writefs(More, List).
$writefs([Char|Rest], List) :-		%   <ordinary character>
	put(Char),
	$writefs(Rest, List).


$action(0't, [Head|Tail], Tail) :-	%   Term
	print(Head).
$action(0'd, [Head|Tail], Tail) :-	%   Display
	display(Head).
$action(0'w, [Head|Tail], Tail) :-	%   Write
	write(Head).
$action(0'q, [Head|Tail], Tail) :-	%   Quoted
	writeq(Head).
$action(0'p,  [Head|Tail], Tail) :-	%   Print
	print(Head).
$action(0'f, List, List) :-		%   Flush
	ttyflush.
$action(0'n, [Char|Tail], Tail) :-	%   iNteger (character)
	put(Char).
$action(0'r, [Thing, Times|Tail], Tail) :-	%   Repeatedly
	$writelots(Times, Thing).
$action(0's, [Head|Tail], Tail) :-	%   String
	$padout(Head).

$special(0'n, 10).		/*  n  */
$special(0'l, 10).		/*  l  */
$special(0'r, 10).		/*  r  */
$special(0't,  9).		/*  t  */
$special(0'\, 0'\).		/*  \  */
$special(0'%, 0'%).		/*  %  */

$getcode(Char, In, Out) :-
	$getdigits(3, Digits, In, Out),
	name(Char, Digits),
	Char < 128.

$getdigits(Limit, [Digit|Digits], In, Out) :-
	Limit > 0,
	$char(In, Digit, Out0),
	between(0'0, 0'9, Digit),
	Fewer is Limit - 1, !,
	$getdigits(Fewer, Digits, Out0, Out).
$getdigits(_, [], Out, Out).

$writelots(N, T) :-
	N > 0, !,
	write(T),
	M is N - 1,
	$writelots(M, T).
$writelots(_, _).

/*  The new formats are %nC, %nL, and %nR for centered, left, and right
    justified output of atoms, integers, and strings.  This is meant to
    simplify the production of tabular output when it is appropriate.
    At least one space will always precede/follow the item written.
*/

$getpad(Size, Just, In, Out) :-
	$getdigits(3, Digits, In, Out0),
	name(Size, Digits),
	$char(Out0, Out1, Out),
	$getpad(Out1, Just).

$getpad(0'r, r).		%  right justified
$getpad(0'l, l).		%  left justified
$getpad(0'c, c).		%  centered
$getpad(0'R, r).		%  right justified
$getpad(0'L, l).		%  left justified
$getpad(0'C, c).		%  centered


				%   $padout(A, S, J) writes the item A in a
				%   field of S or more characters, Justified.

$padout(String, Size, Just) :-
	$string(String), !,
	name(Atom, String),
	$padout(Atom, Size, Just).
$padout(Term, Size, Just) :-
	term_to_atom(Term, Atom),
	atom_length(Atom, Length),
	$padout(Just, Size, Length, Left, Right),
	tab(Left),
	write(Atom),
	tab(Right).

$string(0) :- !, fail.
$string([]) :- !.
$string([H|T]) :-
	$print(H), !,
	$string(T).

$print(10).			% newline
$print(9).			% tab
$print(X) :-
	integer(X),
	between(32, 0'~, X).


				%   $padout(Just, Size, Length, Left, Right)
				%   calculates the number of spaces to put
				%   on the Left and Right of an item needing
				%   Length characters in a field of Size.

$padout(l, Size, Length, 0, Right) :- !,
	Right is max(1, Size-Length).
$padout(r, Size, Length, Left, 0) :- !,
	Left is max(1, Size-Length).
$padout(c, Size, Length, Left, Right) :-
	Left is max(1, (Size - Length)/2),
	Right is max(1, Size - Length - Left).

				%   $padout(Str) writes a string.

$padout([Head|Tail]) :- !,
	put(Head),
	$padout(Tail).
$padout([]).
