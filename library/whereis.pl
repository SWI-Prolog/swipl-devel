/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Find predicates
*/

:- module(whereis,
	[ whereis/1
	]).

:- module_transparent
	whereis/1.

%	whereis(+Spec)
%	Find predicate definition.

whereis(Spec) :-
	'$find_predicate'(Spec, List),
	member(Head, List),
	'$predicate_name'(Head, PredName),
	(   '$defined_predicate'(Head)
	->  predicate_property(Head, file(File)),
	    predicate_property(Head, line_count(Line)),
	    format('~t~8|~w is in ~w:~d~n', [PredName, File, Line])
	;   '$strip_module'(Head, Module, H),
	    functor(H, Name, Arity),
	    '$find_library'(Module, Name, Arity, _, Lib),
	    libname(Lib, LibName),
	    format('~t~8|~w is in ~w~n', [PredName, LibName])
	),
	fail ; true.

libname(File, library(LibName)) :-
	findall(LN, ( library_directory(X),
		      concat(X, LN, File)),
		[LN0]),
	(concat('/', LN1, LN0) -> true ; LN1 = LN0),
	(concat(LibName, '.pl', LN1) -> true ; LibName = LN1).
	
