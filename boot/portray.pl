/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: portray/1 connection to print/1
*/

:- module($portray, [$portray/1, $portray_variable/1]).

:- user:dynamic(portray/1).
:- user:multifile(portray/1).

%   $portray is called from C through print/1.

$portray(Var) :-
	$portray_variable(Var), !.
$portray(Term) :-
	user:portray(Term), !.


$portray_variable($$VAR(Name)) :-
	atom(Name), !,
	format('~w', [Name]).
$portray_variable($$VAR(N)) :-
	$varname(N, Name), 
	format('~s', [Name]).
$portray_variable($VAR(Name)) :-	% QP compatibility
	atom(Name), !,
	format('~w', [Name]).
$portray_variable($VAR(N)) :-
	$varname(N, Name), 
	format('~s', [Name]).

$varname(N, [C]) :-
	N < 26, !, 
	C is N + 0'A.
$varname(N, L) :-
	LD is N mod 26 + 0'A,
	NN is N // 26,
	$varname(NN, L0),
	append(L0, [LD], L).
	
