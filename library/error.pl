/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

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
	    representation_error/1,	% +Reason
	    syntax_error/1,		% +Culprit

	    must_be/2,			% +Type, +Term
	    is_of_type/2		% +Type, +Term
	  ]).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Error generating support

This  module  provides  predicates  to  simplify  error  generation  and
checking. It's implementation is based on a discussion on the SWI-Prolog
mailinglist on best practices in error   handling. The utility predicate
must_be/2  provides  simple  run-time  type    validation.  The  *_error
predicates are simple wrappers around throw/1   to simplify throwing the
most common ISO error terms.

@author Jan Wielemaker
@author Richard O'Keefe
@author Ulrich Neumerkel
@see	library(debug) and library(prolog_stack).
@see	print_message/2 is used to print (uncaught) error terms.
*/

:- multifile
	has_type/2.

		 /*******************************
		 *	     ISO ERRORS		*
		 *******************************/

%%	type_error(+Type, +Term).
%
%	Tell the user that Term is not  of the expected Type. This error
%	is closely related to domain_error/2 because the notion of types
%	is  not  really  set  in  stone  in  Prolog.  We  introduce  the
%	difference using a simple example.
%
%	Suppose an argument must  be  a   non-negative  integer.  If the
%	actual argument is not an integer, this is a _type_error_. If it
%	is a negative integer, it is a _domain_error_.
%
%	Typical borderline cases are  predicates   accepting  a compound
%	term, e.g., point(X,Y). One could argument   that the basic type
%	is a compound-term and  any  other   compound  term  is a domain
%	error. Most Prolog programmers consider each  compound as a type
%	and  would  consider  a  compoint  that   is  not  point(_,_)  a
%	_type_error_.

type_error(Type, Term) :-
	throw(error(type_error(Type, Term), _)).

%%	domain_error(+Type, +Term).
%
%	The argument is of the proper  type,   but  has  a value that is
%	outside the supported  values.  See   type_error/2  for  a  more
%	elaborate  discussion  of  the  distinction  between  type-  and
%	domain-errors.

domain_error(Type, Term) :-
	throw(error(domain_error(Type, Term), _)).

%%	existence_error(+Type, +Term).
%
%	Term is of the correct type and  correct domain, but there is no
%	existing (external) resource that is represented by it.

existence_error(Type, Term) :-
	throw(error(existence_error(Type, Term), _)).

%%	permission_error(+Action, +Type, +Term).
%
%	It is not allowed to perform Action   on the object Term that is
%	of the given Type.

permission_error(Action, Type, Term) :-
	throw(error(permission_error(Action, Type, Term), _)).

%%	instantiation_error(+Term).
%
%	An argument is under-instantiated. I.e. it  is not acceptable as
%	it is, but if some variables are  bound to appropriate values it
%	would be acceptable.
%
%	@param	Term is the term that needs (further) instantiation.
%		Unfortunately, the ISO error does not allow for passing
%		this term along with the error, but we pass it to this
%		predicate for documentation purposes and to allow for
%		future enhancement.

instantiation_error(_Term) :-
	throw(error(instantiation_error, _)).

%%	representation_error(+Reason).
%
%	A  representation  error  indicates   a    limitation   of   the
%	implementation. SWI-Prolog has  no  such   limits  that  are not
%	covered by other errors, but  an   example  of  a representation
%	error in another Prolog implementation could   be  an attempt to
%	create a term with an arity higher than supported by the system.

representation_error(Reason) :-
	throw(error(representation_error(Reason), _)).

%%	syntax_error(+Culprit)
%
%	A text has invalid syntax.  The error is described by Culprit.
%
%	@tbd	Deal with proper description of the location of the
%		error.  For short texts, we allow for Type(Text), meaning
%		Text is not a valid Type.  E.g. syntax_error(number('1a'))
%		means that =1a= is not a valid number.

syntax_error(Culprit) :-
	throw(error(syntax_error(Culprit), _)).


		 /*******************************
		 *	      MUST-BE		*
		 *******************************/

%%	must_be(+Type, @Term) is det.
%
%	True if Term satisfies the type constraints for Type. Defined
%	types are =atom=, =atomic=, =between=, =boolean=, =callable=,
%	=chars=, =codes=, =text=, =compound=, =constant=, =float=,
%	=integer=, =nonneg=, =positive_integer=, =negative_integer=,
%	=nonvar=, =number=, =oneof=, =list=, =list_or_partial_list=,
%	=symbol=, =var=, =rational=, =encoding= and =string=.
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
%	| encoding | Valid name for a character encoding |
%	| cyclic | Cyclic term (rational tree) |
%	| acyclic | Acyclic term (tree) |
%	| list(Type) | Proper list with elements of Type |
%	| list_or_partial_list | A list or an open list (ending in a variable |
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
is_not(list(Of), X) :- !,
	not_a_list(list(Of), X).
is_not(list_or_partial_list, X) :- !,
	type_error(list, X).
is_not(chars, X) :- !,
	not_a_list(chars, X).
is_not(codes, X) :- !,
	not_a_list(codes, X).
is_not(var,X) :- !,
	throw(error(uninstantiation_error(X), _)).
is_not(rational, X) :- !,
	not_a_rational(X).
is_not(cyclic, X) :-
	domain_error(acyclic_term, X).
is_not(acyclic, X) :-
	domain_error(cyclic_term, X).
is_not(Type, X) :-
	(   var(X)
	->  instantiation_error(X)
	;   ground_type(Type), \+ ground(X)
	->  instantiation_error(X)
	;   type_error(Type, X)
	).

ground_type(ground).
ground_type(oneof(_)).
ground_type(stream).
ground_type(text).
ground_type(string).

not_a_list(Type, X) :-
	'$skip_list'(_, X, Rest),
	(   var(Rest)
	->  instantiation_error(X)
	;   Rest == []
	->  Type = list(Of),
	    element_is_not(X, Of)
	;   functor(Type, Name, _),
	    type_error(Name, X)
	).


element_is_not([H|T], Of) :-
	has_type(Of, H), !,
	element_is_not(T, Of).
element_is_not([H|_], Of) :- !,
	is_not(Of, H).
element_is_not(_List, _Of) :-
	assertion(fail).

not_a_rational(X) :-
	(   var(X)
	->  instantiation_error(X)
	;   X = rdiv(N,D)
	->  must_be(integer, N), must_be(integer, D),
	    type_error(rational,X)
	;   type_error(rational,X)
	).

%%	is_of_type(+Type, @Term) is semidet.
%
%	True if Term satisfies Type.

is_of_type(Type, Term) :-
	has_type(Type, Term).


%%	has_type(+Type, @Term) is semidet.
%
%	True if Term satisfies Type.

has_type(impossible, _) :-	instantiation_error(_).
has_type(any, _).
has_type(atom, X)	  :- atom(X).
has_type(atomic, X)	  :- atomic(X).
has_type(between(L,U), X) :- (   integer(L)
			     ->  integer(X), between(L,U,X)
			     ;   number(X), X >= L, X =< U
			     ).
has_type(boolean, X)	  :- (X==true;X==false), !.
has_type(callable, X)	  :- callable(X).
has_type(chars,	X)	  :- chars(X).
has_type(codes,	X)	  :- codes(X).
has_type(text, X)	  :- text(X).
has_type(compound, X)	  :- compound(X).
has_type(constant, X)	  :- atomic(X).
has_type(float, X)	  :- float(X).
has_type(ground, X)	  :- ground(X).
has_type(cyclic, X)	  :- cyclic_term(X).
has_type(acyclic, X)	  :- acyclic_term(X).
has_type(integer, X)	  :- integer(X).
has_type(nonneg, X)	  :- integer(X), X >= 0.
has_type(positive_integer, X)	  :- integer(X), X > 0.
has_type(negative_integer, X)	  :- integer(X), X < 0.
has_type(nonvar, X)	  :- nonvar(X).
has_type(number, X)	  :- number(X).
has_type(oneof(L), X)	  :- ground(X), \+ \+ memberchk(X, L).
has_type(proper_list, X)  :- is_list(X).
has_type(list, X)	  :- is_list(X).
has_type(list_or_partial_list, X)  :- is_list_or_partial_list(X).
has_type(symbol, X)	  :- atom(X).
has_type(var, X)	  :- var(X).
has_type(rational, X)	  :- rational(X).
has_type(string, X)	  :- string(X).
has_type(stream, X)	  :- is_stream(X).
has_type(encoding, X)	  :- current_encoding(X).
has_type(list(Type), X)	  :- is_list(X), element_types(X, Type).

chars(Chs) :-
	is_list(Chs),
	chars_i(Chs).

chars_i([]).
chars_i([H|T]) :-
	atom(H), atom_length(H, 1),
	chars_i(T).

codes(Cds) :-
	is_list(Cds),
	codes_i(Cds).

codes_i([]).
codes_i([H|T]) :-
	integer(H), between(1, 0x10ffff, H),
	codes_i(T).

text(X) :-
	(   atom(X)
	;   string(X)
	;   chars(X)
	;   codes(X)
	), !.

element_types([], _).
element_types([H|T], Type) :-
	has_type(Type, H),
	element_types(T, Type).

is_list_or_partial_list(L0) :-
	'$skip_list'(_, L0,L),
	( var(L) -> true ; L == [] ).

%%	current_encoding(?Name) is nondet.
%
%	True if Name is the name of   a supported encoding. See encoding
%	option of e.g., open/4.

current_encoding(octet).
current_encoding(ascii).
current_encoding(iso_latin_1).
current_encoding(text).
current_encoding(utf8).
current_encoding(unicode_be).
current_encoding(unicode_le).
current_encoding(wchar_t).
