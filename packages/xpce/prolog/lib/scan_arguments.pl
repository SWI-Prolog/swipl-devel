% File:		scan_arguments.pl
% Module:	scan_arguments
% Part of:	SWI-Prolog Utility Library
% Author:	Anjo Anjewierden, Jan Wielemaker (UvA SWI)
% Purpose:	Scanning of argument/value pairs
% Works with:	SWI-Prolog 1.4
% Notice:	Copyright (c) 1990 University of Amsterdam
% History:	18/01/89 (Created)
%		10/04/90 (Last Modified)


%-----------------------------------------------
%   Module Definitions
%-----------------------------------------------

:- module(scan_arguments,
	[ scan_arguments/2	% Arguments x ValueList
	, scan_arguments/3	% Arguments x ValueList -> Rest
	]).

:- use_module(library(pce)).
:- require([ select/3
	   ]).


%-----------------------------------------------
%   Scan Arguments
%-----------------------------------------------

%   scan_arguments(+Args, [+Name=-Value/+Default])
%   scan_arguments(+Args, +Spec, -Rest)
%
%   scan_arguments/2 is used to scan a list of arguments and assign
%   values to variables.  Args is a list of instantiated arguments
%   provided by the caller, the second argument contains name/value
%   pairs where the value is unified with the value given in Args.
%   The version Name=Value/Default allows Name to be omitted in Args
%   and still bind Default to Value.
%
%   scan_arguments/3 is equivalent to scan_arguments/2, but unprocessed
%   arguments are bound to `Rest' instead of printing an error message.
%
%   Error messages are printed on missing arguments.
%
%	?- scan_arguments([hello=world], [hello=X])
%
%	X = world
%
%	?- scan_arguments([name=anjo], [name=N, city=C/amsterdam]).
%
%	N = anjo
%	C = amsterdam

scan_arguments(Args, List, Rest) :-
	get_arguments(List, Args, Rest).

scan_arguments(Args, List) :-
	get_arguments(List, Args, Rest),
	(   Rest == []
	->  true
	;   format(user_error,
		   'scan_arguments:Arguments not required: ~w~n', Rest)
	).

get_arguments([], Args, Args) :- !.
get_arguments([Name = Value|T], Args, RestArgs) :-
	non_default_argument(Value), !,
	(   select(Args, Name=Value, Rest)
	->  get_arguments(T, Rest, RestArgs)
	;   format(user_error,
		   'Argument ~w not present and no default defined', [Name])
	).
get_arguments([Name = Value / Default|T], Args, RestArgs) :-
	(   select(Args, Name=Value, Rest)
	->  get_arguments(T, Rest, RestArgs)
	;   Value = Default,
	    get_arguments(T, Args, RestArgs)
	).


non_default_argument(Value) :- var(Value), !.
non_default_argument(_/_) :- !, fail.
non_default_argument(_).
