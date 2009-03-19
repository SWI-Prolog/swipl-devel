/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2006, University of Amsterdam

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


:- module(http_parameters,
	  [ http_parameters/2,		% +Request, -Params
	    http_parameters/3		% +Request, -Params, +TypeG
	  ]).
:- use_module(http_client).
:- use_module(http_mime_plugin).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(error)).

:- module_transparent
	http_parameters/3.

%%	http_parameters(+Request, ?Parms) is det.
%%	http_parameters(+Request, ?Parms, +Options) is det.
%
%	Get HTTP GET  or  POST   form-data,  applying  type  validation,
%	default values, etc.  Provided options are:
%
%		* attribute_declarations(:Goal)
%		Causes the declarations for an attributed named A to be
%		fetched using call(Goal, A, Declarations).
%
%		* form_data(-Data)
%		Return the data read from the GET por POST request as a
%		list Name = Value.  All data, including name/value pairs
%		used for Parms, is unified with Data.

http_parameters(Request, Params) :-
	http_parameters(Request, Params, []).

http_parameters(Request, Params, Options) :-
	option(attribute_declarations(G), Options, -),
	(   G == (-)
	->  DeclGoal = G
	;   strip_module(G, M, G2),
	    DeclGoal = M:G2
	),
	http_parms(Request, Params, DeclGoal, Form),
	ignore(memberchk(form_data(Form), Options)).


http_parms(Request, Params, DeclGoal, Search) :-
	memberchk(method(get), Request), !,
	(   memberchk(search(Search), Request)
	->  true
	;   Search = []
	),
	fill_parameters(Params, Search, DeclGoal).
http_parms(Request, Params, DeclGoal, Data) :-
	member(method(post), Request), !,
	debug(post_request, 'POST Request: ~p', [Request]),
        http_read_data(Request, Data, []),
	debug(post, 'POST Data: ~p', [Data]),
	fill_parameters(Params, Data, DeclGoal).

%%	fill_parameters(+ParamDecls, +FormData, +DeclGoal)
%
%	Fill values from the parameter list

fill_parameters([], _, _).
fill_parameters([H|T], FormData, DeclGoal) :-
	fill_parameter(H, FormData, DeclGoal),
	fill_parameters(T, FormData, DeclGoal).

fill_parameter(H, FormData, _) :-
	H =.. [Name,Value,Options], !,
	fill_param(Name, Value, Options, FormData).
fill_parameter(H, FormData, DeclGoal) :-
	H =.. [Name,Value],
	(   call(DeclGoal, Name, Options)
	->  true
	;   throw(error(existence_error(attribute_declaration, Name), _))
	),
	fill_param(Name, Value, Options, FormData).

fill_param(Name, Values, Options, FormData) :-
	memberchk(zero_or_more, Options), !,
	fill_param_list(FormData, Name, Values, Options).
fill_param(Name, Value, Options, FormData) :-
	(   memberchk(Name=Value0, FormData),
	    Value0 \== ''		% Not sure
	->  check_type(Options, Name, Value0, Value)
	;   memberchk(default(Value), Options)
	->  true
	;   memberchk(optional(true), Options)
	->  true
	;   throw(error(existence_error(form_data, Name), _))
	).


fill_param_list([], _, [], _).
fill_param_list([Name=Value0|Form], Name, [Value|VT], Options) :- !,
	check_type(Options, Name, Value0, Value),
	fill_param_list(Form, Name, VT, Options).
fill_param_list([_|Form], Name, VT, Options) :-
	fill_param_list(Form, Name, VT, Options).


%%	check_type(+Options, +FieldName, +ValueIn, -ValueOut) is det.
%
%	Validate an HTTP form value.
%
%	@param Option		list as provided with the parameter
%	@param FieldName	Name of the HTTP field (for better message)
%	@param ValueIn		Atom value as received from HTTP layer
%	@param ValueOut		Possibly converted final value
%	@error type_error(Type, Value)

check_type([], _, Value, Value).
check_type([H|T], Field, Value0, Value) :-
	(   check_type3(H, Value0, Value1)
	->  check_type(T, Field, Value1, Value)
	;   check_type2(H, Value0)
	->  check_type(T, Field, Value0, Value)
	;   format(string(Msg), 'HTTP parameter ~w', [Field]),
	    throw(error(type_error(H, Value0),
			context(_, Msg)))
	).

%%	check_type3(+Type, +ValueIn, -ValueOut) is semidet.
%
%	HTTP parameter type-check for types that need converting.
%
%	@error	syntax_error

check_type3(number, Atom, Number) :-
	atom_number(Atom, Number).
check_type3(integer, Atom, Integer) :-
	atom_number(Atom, Integer),
	integer(Integer).
check_type3(float, Atom, Float) :-
	atom_number(Atom, Number),
	Float is float(Number).
check_type3(between(Low, High), Atom, Value) :-
	atom_number(Atom, Number),
	(   (float(Low) ; float(High))
	->  Value is float(Number)
	;   Value = Number
	),
	must_be(between(Low, High), Value).

%%	check_type2(+Type, +ValueIn) is semidet.
%
%	HTTP parameter type-check for types that need no conversion.

check_type2(oneof(Set), Value) :- !,
	memberchk(Value, Set).
check_type2(length > N, Value) :- !,
	atom_length(Value, Len),
	Len > N.
check_type2(length >= N, Value) :- !,
	atom_length(Value, Len),
	Len >= N.
check_type2(length < N, Value) :- !,
	atom_length(Value, Len),
	Len < N.
check_type2(length =< N, Value) :- !,
	atom_length(Value, Len),
	Len =< N.
check_type2(_, _).


		 /*******************************
		 *	   XREF SUPPORT		*
		 *******************************/

:- multifile
	prolog:called_by/2,
	emacs_prolog_colours:goal_colours/2.

prolog:called_by(http_parameters(_,_,Options), [G+2]) :-
	option(attribute_declarations(G), Options, _),
	callable(G), !.

emacs_prolog_colours:goal_colours(http_parameters(_,_,Options),
				  built_in-[classify, classify, Colours]) :-
	option_list_colours(Options, Colours).

option_list_colours(Var, error) :-
	var(Var), !.
option_list_colours([], classify) :- !.
option_list_colours(Term, list-Elements) :-
	Term = [_|_], !,
	option_list_colours_2(Term, Elements).
option_list_colours(_, error).

option_list_colours_2(Var, classify) :-
	var(Var).
option_list_colours_2([], []).
option_list_colours_2([H0|T0], [H|T]) :-
	option_colours(H0, H),
	option_list_colours_2(T0, T).

option_colours(Var,  classify) :-
	var(Var), !.
option_colours(_=_,  built_in-[classify,classify]) :- !.
option_colours(attribute_declarations(_), 		% DCG = is a hack!
	       option(attribute_declarations)-[dcg]) :- !.
option_colours(Term, option(Name)-[classify]) :-
	compound(Term),
	Term =.. [Name,_Value], !.
option_colours(_, error).
