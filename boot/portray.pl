/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: portray/1 connection to print/1
*/

:- module($portray, [$portray/1]).

:- user:dynamic(portray/1).
:- user:multifile(portray/1).

%   $portray is called from C through print/1.

$portray($$VAR(N)) :- !, 
	$varname(N, Name), 
	format('~s', [Name]).
$portray(Term) :-
	user:portray(Term), !.

$varname(N, [C]) :-
	N < 26, !, 
	C is N + 0'A.
$varname(N, [C1, C2]) :-
	C1 is N // 26 + 0'A, 
	C2 is N mod 26 + 0'A.
