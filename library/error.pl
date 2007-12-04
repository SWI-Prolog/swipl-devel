/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(error,
	  [ type_error/2,		% +Type, +Term
	    domain_error/2,		% +Domain, +Term
	    existence_error/2,		% +Type, +Term
	    permission_error/3,		% +Action, +Type, +Term
	    instantiation_error/1,	% +Term

	    must_be/2,			% +Type, +Term
	    is_of_type/2		% +Type, +Term
	  ]).

/** <module> Error generating support

This  module  provides  predicates  to  simplify  error  generation  and
checking. It's implementation is based on a discussion on the SWI-Prolog
mailinglist on best practices in error   handling. The utility predicate
must_be/2  provides  simple  run-time  type    validation.  The  *_error
predicates are simple wrappers around throw/1   to simplify throwing the
most common ISO error terms.

@author Jan Wielemaker
@author Richard O'Keefe
@see	library(debug) and library(prolog_stack).
*/

:- multifile
	has_type/2.

%%	type_error(+Type, +Term).
%%	domain_error(+Type, +Term).
%%	existence_error(+Type, +Term).
%%	permission_error(+Action, +Type, +Term).
%%	instantiation_error(+Term).
%
%	Throw ISO compliant error messages.

type_error(Type, Term) :-
	throw(error(type_error(Type, Term), _)).
domain_error(Type, Term) :-
	throw(error(domain_error(Type, Term), _)).
existence_error(Type, Term) :-
	throw(error(existence_error(Type, Term), _)).
permission_error(Action, Type, Term) :-
	throw(error(permission_error(Action, Type, Term), _)).
instantiation_error(_Term) :-
	throw(error(instantiation_error, _)).

%%	must_be(+Type, @Term) is det.
%
%	True if Term satisfies the type   constraints  for Type. Defined
%	types are =atom=, =atomic=,   =between=,  =boolean=, =callable=,
%	=chars=,  =codes=,  =text=,  =compound=,   =constant=,  =float=,
%	=integer=,  =nonneg=,  =positive_integer=,   =negative_integer=,
%	=nonvar=, =number=, =oneof=,  =list=,   =symbol=,  =var=,
%	=rational= and =string=.
%	
%	Most of these types are defined by an arity-1 built-in predicate
%	of the same name. Below  is  a   brief  definition  of the other
%	types.
%	
%	| boolean | one of =true= or =false= |
%	| chars | Proper list of 1-character atoms |
%	| codes | Proper list of Unicode character codes |
%	| text | One of =atom=, =string=, =chars= or =codes= |
%	| between(L,U) | Number between L and U (including L and U) |
%	| nonneg | Integer >= 0 |
%	| positive_integer | Integer > 0 |
%	| negative_integer | Integer < 0 |
%	| oneof(L) | Ground term that is member of L |
%	| list(Type) | Proper list with elements of Type |
%
%	@throws instantiation_error if Term is insufficiently
%	instantiated and type_error(Type, Term) if Term is not of Type.

must_be(Type, X) :-
	(   has_type(Type, X)
	->  true
	;   is_not(Type, X)
	).

%%	is_not(+Type, @Term)
%
%	Throws appropriate error. It is _known_ that Term is not of type
%	Type.
%
%	@throws type_error(Type, Term)
%	@throws instantiation_error

is_not(list, X) :- !,
	not_a_list(list, X).
is_not(chars, X) :- !,
	not_a_list(chars, X).
is_not(codes, X) :- !,
	not_a_list(codes, X).
is_not(Type, X) :-
	(   ground(X)
	->  type_error(Type, X)
	;   instantiation_error(X)
	).

not_a_list(Type, X) :-
	'$skip_list'(_, X, Rest),
	(   var(Rest)
	->  instantiation_error(X)
	;   type_error(Type, X)
	).

%%	is_of_type(+Type, @Term) is semidet.
%
%	True if Term satisfies Type.

is_of_type(Type, Term) :-
	has_type(Type, Term).


%%	has_type(+Type, @Term) is semidet.
%
%	True if Term satisfies Type.

has_type(any, _).
has_type(atom, X)	  :- atom(X).
has_type(atomic, X)	  :- atomic(X).
has_type(between(L,U), X) :- (   integer(L) 
			     ->  integer(X), between(L,U,X)
			     ;   number(X), X >= L, X =< U
			     ).
has_type(boolean, X) 	  :- (X==true;X==false), !.
has_type(callable, X)	  :- callable(X).
has_type(chars,	X)	  :- chars(X).
has_type(codes,	X)	  :- codes(X).
has_type(text, X)	  :- text(X).
has_type(compound, X)	  :- compound(X).
has_type(constant, X)	  :- atomic(X).
has_type(float, X)	  :- float(X).
has_type(ground, X)	  :- ground(X).
has_type(integer, X)	  :- integer(X).
has_type(nonneg, X)	  :- integer(X), X >= 0.
has_type(positive_integer, X)	  :- integer(X), X > 0.
has_type(negative_integer, X)	  :- integer(X), X < 0.
has_type(nonvar, X)	  :- nonvar(X).
has_type(number, X)	  :- number(X).
has_type(oneof(L), X)	  :- ground(X), memberchk(X, L).
has_type(proper_list, X)  :- is_list(X).
has_type(list, X)  	  :- is_list(X).
has_type(symbol, X)	  :- atom(X).
has_type(var, X)	  :- var(X).
has_type(rational, X)	  :- rational(X).
has_type(string, X)	  :- string(X).
has_type(stream, X)	  :- is_stream(X).
has_type(list(Type), X)	  :- element_types(X, Type).

chars(0) :- !, fail.
chars([]).
chars([H|T]) :-
	atom(H), atom_length(H, 1),
	chars(T).

codes(x) :- !, fail.
codes([]).
codes([H|T]) :-
	integer(H), between(1, 0x10ffff, H),
	codes(T).

text(X) :-
	(   atom(X)
	;   string(X)
	;   chars(X)
	;   codes(X)
	), !.

element_types(X, _) :-
	var(X),
	instantiation_error(X).
element_types([], _).
element_types([H|T], Type) :-
	must_be(Type, H),
	element_types(T, Type).
