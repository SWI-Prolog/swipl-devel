/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

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
	(   select(Name=Value, Args, Rest)
	->  get_arguments(T, Rest, RestArgs)
	;   format(user_error,
		   'Argument ~w not present and no default defined', [Name])
	).
get_arguments([Name = Value / Default|T], Args, RestArgs) :-
	(   select(Name=Value, Args, Rest)
	->  get_arguments(T, Rest, RestArgs)
	;   Value = Default,
	    get_arguments(T, Args, RestArgs)
	).


non_default_argument(Value) :- var(Value), !.
non_default_argument(_/_) :- !, fail.
non_default_argument(_).
