/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2018, University of Amsterdam
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

:- module(error,
          [ type_error/2,               % +Type, +Term
            domain_error/2,             % +Domain, +Term
            existence_error/2,          % +Type, +Term
            existence_error/3,          % +Type, +Term, +Set
            permission_error/3,         % +Action, +Type, +Term
            instantiation_error/1,      % +Term
            uninstantiation_error/1,    % +Term
            representation_error/1,     % +Reason
            syntax_error/1,             % +Culprit
            resource_error/1,           % +Culprit

            must_be/2,                  % +Type, +Term
            is_of_type/2,               % +Type, +Term
            current_type/3              % ?Type, @Var, -Body
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
@see    library(debug) and library(prolog_stack).
@see    print_message/2 is used to print (uncaught) error terms.
*/

:- multifile
    has_type/2.

                 /*******************************
                 *           ISO ERRORS         *
                 *******************************/

%!  type_error(+Type, +Term).
%
%   Tell the user that Term is not  of the expected Type. This error
%   is closely related to domain_error/2 because the notion of types
%   is  not  really  set  in  stone  in  Prolog.  We  introduce  the
%   difference using a simple example.
%
%   Suppose an argument must  be  a   non-negative  integer.  If the
%   actual argument is not an integer, this is a _type_error_. If it
%   is a negative integer, it is a _domain_error_.
%
%   Typical borderline cases are  predicates   accepting  a compound
%   term, e.g., point(X,Y). One could argue that the basic type is a
%   compound-term and any other compound  term   is  a domain error.
%   Most Prolog programmers consider each  compound   as  a type and
%   would consider a compound that is not point(_,_) a _type_error_.

type_error(Type, Term) :-
    throw(error(type_error(Type, Term), _)).

%!  domain_error(+Type, +Term).
%
%   The argument is of the proper  type,   but  has  a value that is
%   outside the supported  values.  See   type_error/2  for  a  more
%   elaborate  discussion  of  the  distinction  between  type-  and
%   domain-errors.

domain_error(Type, Term) :-
    throw(error(domain_error(Type, Term), _)).

%!  existence_error(+Type, +Term).
%
%   Term is of the correct type and  correct domain, but there is no
%   existing (external) resource that is represented by it.

existence_error(Type, Term) :-
    throw(error(existence_error(Type, Term), _)).

%!  existence_error(+Type, +Term, +Set).
%
%   Term is of the correct type  and   correct  domain,  but there is no
%   existing (external) resource that  is  represented   by  it  in  the
%   provided set.
%
%   @compat This error is not in ISO.

existence_error(Type, Term, Set) :-
    throw(error(existence_error(Type, Term, Set), _)).

%!  permission_error(+Action, +Type, +Term).
%
%   It is not allowed to perform Action   on the object Term that is
%   of the given Type.

permission_error(Action, Type, Term) :-
    throw(error(permission_error(Action, Type, Term), _)).

%!  instantiation_error(+Term).
%
%   An argument is under-instantiated. I.e. it  is not acceptable as
%   it is, but if some variables are  bound to appropriate values it
%   would be acceptable.
%
%   @param  Term is the term that needs (further) instantiation.
%           Unfortunately, the ISO error does not allow for passing
%           this term along with the error, but we pass it to this
%           predicate for documentation purposes and to allow for
%           future enhancement.

instantiation_error(_Term) :-
    throw(error(instantiation_error, _)).

%!  uninstantiation_error(+Term)
%
%   An argument is over-instantiated. This error  is used for output
%   arguments whose value cannot be known  upfront. For example, the
%   goal open(File, read, input) cannot   succeed because the system
%   will allocate a new unique stream   handle that will never unify
%   with `input`.

uninstantiation_error(Term) :-
    throw(error(uninstantiation_error(Term), _)).

%!  representation_error(+Reason).
%
%   A  representation  error  indicates   a    limitation   of   the
%   implementation. SWI-Prolog has  no  such   limits  that  are not
%   covered by other errors, but  an   example  of  a representation
%   error in another Prolog implementation could   be  an attempt to
%   create a term with an arity higher than supported by the system.

representation_error(Reason) :-
    throw(error(representation_error(Reason), _)).

%!  syntax_error(+Culprit)
%
%   A text has invalid syntax.  The error is described by Culprit.
%
%   @tbd    Deal with proper description of the location of the
%           error.  For short texts, we allow for Type(Text), meaning
%           Text is not a valid Type.  E.g. syntax_error(number('1a'))
%           means that =1a= is not a valid number.

syntax_error(Culprit) :-
    throw(error(syntax_error(Culprit), _)).

%!  resource_error(+Culprit)
%
%   A goal cannot be completed due to lack of resources.

resource_error(Culprit) :-
    throw(error(resource_error(Culprit), _)).


                 /*******************************
                 *            MUST-BE           *
                 *******************************/

%!  must_be(+Type, @Term) is det.
%
%   True if Term satisfies the type constraints for Type. Defined
%   types are =atom=, =atomic=, =between=, =boolean=, =callable=,
%   =chars=, =codes=, =text=, =compound=, =constant=, =float=,
%   =integer=, =nonneg=, =positive_integer=, =negative_integer=,
%   =nonvar=, =number=, =oneof=, =list=, =list_or_partial_list=,
%   =symbol=, =var=, =rational=, =encoding=, =dict= and =string=.
%
%   Most of these types are defined by an arity-1 built-in predicate
%   of the same name. Below  is  a   brief  definition  of the other
%   types.
%
%   | acyclic | Acyclic term (tree); see acyclic_term/1 |
%   | any | |
%   | between(FloatL,FloatU) | Number [FloatL..FloatU] |
%   | between(IntL,IntU) | Integer [IntL..IntU] |
%   | boolean | One of =true= or =false= |
%   | char | Atom of length 1 |
%   | chars | Proper list of 1-character atoms |
%   | code | Representation Unicode code point |
%   | codes | Proper list of Unicode character codes |
%   | constant | Same as `atomic` |
%   | cyclic | Cyclic term (rational tree); see cyclic_term/1 |
%   | dict | A dictionary term; see is_dict/1 |
%   | encoding | Valid name for a character encoding; see current_encoding/1 |
%   | list | A (non-open) list; see is_list/1 |
%   | negative_integer | Integer < 0 |
%   | nonneg | Integer >= 0 |
%   | oneof(L) | Ground term that is member of L |
%   | positive_integer | Integer > 0 |
%   | proper_list | Same as list |
%   | list(Type) | Proper list with elements of Type |
%   | list_or_partial_list | A list or an open list (ending in a variable); see is_list_or_partial_list/1 |
%   | stream | A stream name or valid stream handle; see is_stream/1 |
%   | symbol | Same as `atom` |
%   | text | One of =atom=, =string=, =chars= or =codes= |
%   | type | Term is a valid type specification |
%
%   Note: The Windows version can only represent Unicode code points
%   up to 2^16-1. Higher values cause a representation error on most
%   text handling predicates.
%
%   @throws instantiation_error if Term is insufficiently
%   instantiated and type_error(Type, Term) if Term is not of Type.

must_be(Type, X) :-
    (   nonvar(Type),
        has_type(Type, X)
    ->  true
    ;   nonvar(Type)
    ->  is_not(Type, X)
    ;   instantiation_error(Type)
    ).

%!  is_not(+Type, @Term)
%
%   Throws appropriate error. It is _known_ that Term is not of type
%   Type.
%
%   @throws type_error(Type, Term)
%   @throws instantiation_error

is_not(list, X) :-
    !,
    not_a_list(list, X).
is_not(list(Of), X) :-
    !,
    not_a_list(list(Of), X).
is_not(list_or_partial_list, X) :-
    !,
    type_error(list, X).
is_not(chars, X) :-
    !,
    not_a_list(list(char), X).
is_not(codes, X) :-
    !,
    not_a_list(list(code), X).
is_not(var,X) :-
    !,
    uninstantiation_error(X).
is_not(cyclic, X) :-
    domain_error(cyclic_term, X).
is_not(acyclic, X) :-
    domain_error(acyclic_term, X).
is_not(Type, X) :-
    (   var(X)
    ->  instantiation_error(X)
    ;   ground_type(Type), \+ ground(X)
    ->  instantiation_error(X)
    ;   current_type(Type, _Var, _Body)
    ->  type_error(Type, X)
    ;   existence_error(type, Type)
    ).

ground_type(ground).
ground_type(oneof(_)).
ground_type(stream).
ground_type(text).
ground_type(string).
ground_type(rational).

not_a_list(Type, X) :-
    '$skip_list'(_, X, Rest),
    (   var(Rest)
    ->  instantiation_error(X)
    ;   Rest == []
    ->  Type = list(Of),
        (   nonvar(Of)
        ->  element_is_not(X, Of)
        ;   instantiation_error(Of)
        )
    ;   type_error(Type, X)
    ).


element_is_not([H|T], Of) :-
    has_type(Of, H),
    !,
    element_is_not(T, Of).
element_is_not([H|_], Of) :-
    !,
    is_not(Of, H).
element_is_not(_List, _Of) :-
    assertion(fail).

%!  is_of_type(+Type, @Term) is semidet.
%
%   True if Term satisfies Type.

is_of_type(Type, Term) :-
    nonvar(Type),
    !,
    has_type(Type, Term),
    !.
is_of_type(Type, _) :-
    instantiation_error(Type).

%!  has_type(+Type, @Term) is semidet.
%
%   True if Term satisfies Type.

:- '$clausable'(has_type/2).            % always allow clause/2

has_type(any, _).
has_type(atom, X)         :- atom(X).
has_type(atomic, X)       :- atomic(X).
has_type(between(L,U), X) :- (   integer(L)
    ->  integer(X), between(L,U,X)
    ;   number(X), X >= L, X =< U
    ).
has_type(boolean, X)      :- (X==true;X==false), !.
has_type(callable, X)     :- callable(X).
has_type(char,  X)        :- '$is_char'(X).
has_type(code,  X)        :- '$is_char_code'(X).
has_type(chars, X)        :- '$is_char_list'(X, _Len).
has_type(codes, X)        :- '$is_code_list'(X, _Len).
has_type(text, X)         :- text(X).
has_type(compound, X)     :- compound(X).
has_type(constant, X)     :- atomic(X).
has_type(float, X)        :- float(X).
has_type(ground, X)       :- ground(X).
has_type(cyclic, X)       :- cyclic_term(X).
has_type(acyclic, X)      :- acyclic_term(X).
has_type(integer, X)      :- integer(X).
has_type(nonneg, X)       :- integer(X), X >= 0.
has_type(positive_integer, X)     :- integer(X), X > 0.
has_type(negative_integer, X)     :- integer(X), X < 0.
has_type(nonvar, X)       :- nonvar(X).
has_type(number, X)       :- number(X).
has_type(oneof(L), X)     :- ground(X), \+ \+ memberchk(X, L).
has_type(proper_list, X)  :- is_list(X).
has_type(list, X)         :- is_list(X).
has_type(list_or_partial_list, X)  :- is_list_or_partial_list(X).
has_type(symbol, X)       :- atom(X).
has_type(var, X)          :- var(X).
has_type(rational, X)     :- rational(X).
has_type(string, X)       :- string(X).
has_type(stream, X)       :- is_stream(X).
has_type(encoding, X)     :- current_encoding(X).
has_type(dict, X)         :- is_dict(X).
has_type(list(Type), X)   :- is_list(X), element_types(X, Type).
has_type(type, Type)      :- ground(Type), current_type(Type,_,_).

text(X) :-
    (   atom(X)
    ;   string(X)
    ;   '$is_char_list'(X, _)
    ;   '$is_code_list'(X, _)
    ),
    !.

element_types(List, Type) :-
    nonvar(Type),
    !,
    element_types_(List, Type).
element_types(_List, Type) :-
    instantiation_error(Type).

element_types_([], _).
element_types_([H|T], Type) :-
    has_type(Type, H),
    !,
    element_types_(T, Type).

is_list_or_partial_list(L0) :-
    '$skip_list'(_, L0,L),
    ( var(L) -> true ; L == [] ).

%!  current_encoding(?Name) is nondet.
%
%   True if Name is the name of   a supported encoding. See encoding
%   option of e.g., open/4.

current_encoding(octet).
current_encoding(ascii).
current_encoding(iso_latin_1).
current_encoding(text).
current_encoding(utf8).
current_encoding(unicode_be).
current_encoding(unicode_le).
current_encoding(wchar_t).


%!  current_type(?Type, @Var, -Body) is nondet.
%
%   True when Type is a currently defined type and Var satisfies Type of
%   the body term Body succeeds.

current_type(Type, Var, Body) :-
    clause(has_type(Type, Var), Body0),
    qualify(Body0, Body).

qualify(Var, VarQ) :-
    var(Var),
    !,
    VarQ = Var.
qualify((A0,B0), (A,B)) :-
    qualify(A0, A),
    qualify(B0, B).
qualify(G0, G) :-
    predicate_property(system:G0, built_in),
    !,
    G = G0.
qualify(G, error:G).


		 /*******************************
		 *           SANDBOX		*
		 *******************************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(error:current_type(_,_,_)).
