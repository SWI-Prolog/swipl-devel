/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2024, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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
          [ instantiation_error/1,      % +FormalSubTerm
            uninstantiation_error/1,    % +Culprit
            type_error/2,               % +ValidType, +Culprit
            domain_error/2,             % +ValidDomain, +Culprit
            existence_error/2,          % +ObjectType, +Culprit
            existence_error/3,          % +ObjectType, +Culprit, +Set
            permission_error/3,         % +Operation, +PermissionType, +Culprit
            representation_error/1,     % +Flag
            resource_error/1,           % +Resource
            syntax_error/1,             % +ImplDepAtom

            must_be/2,                  % +Type, +Term
            is_of_type/2,               % +Type, +Term
            current_type/3              % ?Type, @Var, -Body
          ]).
:- set_prolog_flag(generate_debug_info, false).
:- use_module(library(debug), [assertion/1]).

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

%!  type_error(+ValidType, +Culprit).
%
%   Tell the user that Culprit is not   of  the expected ValidType. This
%   error is closely related to  domain_error/2   because  the notion of
%   types is not really  set  in  stone   in  Prolog.  We  introduce the
%   difference using a simple example.
%
%   Suppose an argument must be a   non-negative  integer. If the actual
%   argument is not an integer,  this  is   a  _type_error_.  If it is a
%   negative integer, it is a _domain_error_.
%
%   Typical borderline cases are predicates   accepting a compound term,
%   e.g.,  point(X,Y).  One  could  argue  that  the  basic  type  is  a
%   compound-term and any other compound term   is  a domain error. Most
%   Prolog programmers consider each  compound  as   a  type  and  would
%   consider a compound that is not point(_,_) a _type_error_.

type_error(ValidType, Culprit) :-
    throw(error(type_error(ValidType, Culprit), _)).

%!  domain_error(+ValidDomain, +Culprit).
%
%   The argument is of the proper type, but  has a value that is outside
%   the  supported  values.  See  type_error/2   for  a  more  elaborate
%   discussion of the distinction between type- and domain-errors.

domain_error(ValidDomain, Culprit) :-
    throw(error(domain_error(ValidDomain, Culprit), _)).

%!  existence_error(+ObjectType, +Culprit).
%
%   Culprit is of the correct type and   correct domain, but there is no
%   existing (external) resource of type  ObjectType that is represented
%   by it.

existence_error(ObjectType, Culprit) :-
    throw(error(existence_error(ObjectType, Culprit), _)).

%!  existence_error(+ObjectType, +Culprit, +Set).
%
%   Culprit is of the correct type and   correct domain, but there is no
%   existing (external) resource of type  ObjectType that is represented
%   by it in the provided  set.  The   thrown  exception  term carries a
%   formal  term  structured  as   follows:  existence_error(ObjectType,
%   Culprit, Set)
%
%   @compat This error is outside the ISO Standard.

existence_error(ObjectType, Culprit, Set) :-
    throw(error(existence_error(ObjectType, Culprit, Set), _)).

%!  permission_error(+Operation, +PermissionType, +Culprit).
%
%   It is not allowed to perform   Operation on (whatever is represented
%   by) Culprit that is of the given   PermissionType  (in fact, the ISO
%   Standard is confusing and vague about these terms' meaning).

permission_error(Operation, PermissionType, Culprit) :-
    throw(error(permission_error(Operation, PermissionType, Culprit), _)).

%!  instantiation_error(+FormalSubTerm).
%
%   An argument is under-instantiated. I.e. it   is not acceptable as it
%   is, but if some variables are bound   to appropriate values it would
%   be acceptable.
%
%   @arg  FormalSubTerm is the term that needs (further)
%         instantiation. Unfortunately, the ISO error does not allow
%         for passing this term along with the error, but we pass it
%         to this predicate for documentation purposes and to allow
%         for future enhancement.

instantiation_error(_FormalSubTerm) :-
    throw(error(instantiation_error, _)).

%!  uninstantiation_error(+Culprit)
%
%   An argument is over-instantiated. This  error   is  used  for output
%   arguments whose value cannot be known upfront. For example, the goal
%   open(File, read, input)  cannot  succeed   because  the  system will
%   allocate a new unique  stream  handle   that  will  never unify with
%   `input`.

uninstantiation_error(Culprit) :-
    throw(error(uninstantiation_error(Culprit), _)).

%!  representation_error(+Flag).
%
%   A representation error indicates a limitation of the implementation.
%   SWI-Prolog has no such limits that are  not covered by other errors,
%   but  an  example  of  a  representation   error  in  another  Prolog
%   implementation could be an attempt to create   a  term with an arity
%   higher than supported by the system.

representation_error(Flag) :-
    throw(error(representation_error(Flag), _)).

%!  syntax_error(+Culprit)
%
%   A text has invalid  syntax.  The   error  is  described  by Culprit.
%   According   to   the   ISO   Standard,    Culprit   should   be   an
%   implementation-dependent atom.
%
%   @tbd    Deal with proper description of the location of the
%           error.  For short texts, we allow for Type(Text), meaning
%           Text is not a valid Type.  E.g. syntax_error(number('1a'))
%           means that =1a= is not a valid number.

syntax_error(Culprit) :-
    throw(error(syntax_error(Culprit), _)).

%!  resource_error(+Resource)
%
%   A goal cannot be completed due to   lack  of resources. According to
%   the ISO Standard, Resource  should   be  an implementation-dependent
%   atom.

resource_error(Resource) :-
    throw(error(resource_error(Resource), _)).


                 /*******************************
                 *            MUST-BE           *
                 *******************************/

%!  must_be(+Type, @Term) is det.
%
%   True if Term satisfies the type constraints for Type. Defined
%   types are `atom`, `atomic`, `between`, `boolean`, `callable`,
%   `chars`, `codes`, `text`, `compound`, `constant`, `float`,
%   `integer`, `nonneg`, `positive_integer`, `negative_integer`,
%   `nonvar`, `number`, `oneof`, `list`, `list_or_partial_list`,
%   `symbol`, `var`, `rational`, `encoding`, `dict` and `string`.
%
%   Most of these types are defined by an arity-1 built-in predicate
%   of the same name. Below  is  a   brief  definition  of the other
%   types.
%
%   | acyclic | Acyclic term (tree); see acyclic_term/1 |
%   | any | any term |
%   | between(FloatL,FloatU) | Number [FloatL..FloatU] |
%   | between(IntL,IntU) | Integer [IntL..IntU] |
%   | boolean | One of `true` or `false` |
%   | callable | Atom or compound term |
%   | char | Atom of length 1 |
%   | chars | Proper list of 1-character atoms |
%   | code | Representation Unicode code point (0..0x10ffff) |
%   | codes | Proper list of Unicode character codes |
%   | compound | compound term |
%   | compound(Term) | Compound with same name/arity as term; checks arguments |
%   | constant | Same as `atomic` |
%   | cyclic | Cyclic term (rational tree); see cyclic_term/1 |
%   | dict | A dictionary term; see is_dict/1 |
%   | encoding | Valid name for a character encoding; see current_encoding/1 |
%   | list | A (non-open) list; see is_list/1 |
%   | list(Type) | Proper list with elements of Type |
%   | list_or_partial_list | A list or an open list (ending in a variable); see is_list_or_partial_list/1 |
%   | negative_integer | Integer < 0 |
%   | nonneg | Integer >= 0 |
%   | oneof(L) | Ground term that is member of L |
%   | pair | Key-Value pair.  Same as compound(any-any) |
%   | positive_integer | Integer > 0 |
%   | proper_list | Same as list |
%   | stream | A stream name or valid stream handle; see is_stream/1 |
%   | symbol | Same as `atom` |
%   | text | One of `atom`, `string`, `chars` or `codes` |
%   | type | Term is a valid type specification |
%
%   In  addition,  types   may   be    composed   using   `TypeA,TypeB`,
%   `TypeA;TypeB` and negated using `\Type`.
%
%   @error instantiation_error if Term is insufficiently
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
    current_type(Type, _Var, _Body),
    !,
    (   var(X)
    ->  instantiation_error(X)
    ;   ground_type(Type), \+ ground(X)
    ->  instantiation_error(X)
    ;   type_error(Type, X)
    ).
is_not(Type, _) :-
    existence_error(type, Type).

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
:- public                               % May be called through current_type/3
    is_list_or_partial_list/1,
    current_encoding/1,
    element_types/2.

has_type(any, _).
has_type(atom, X)         :- atom(X).
has_type(atomic, X)       :- atomic(X).
has_type(between(L,U), X) :-
    (   integer(L)
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
has_type(compound(Term),X):- compound(X), is_term_of_type(Term,X).
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
has_type(pair, X)         :- nonvar(X), X = _-_.
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
has_type(list_or_partial_list(Type), X)   :- is_list_or_partial_list(X), element_types(X, Type).
has_type(type, Type)      :- ground(Type), current_type(Type,_,_).
has_type((A,B), X)	  :- (is_of_type(A,X)->is_of_type(B,X)).
has_type((A;B), X)	  :- (is_of_type(A,X)->true;is_of_type(B,X)).
has_type(\A, X)	          :- \+ is_of_type(A,X).

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

element_types_(Var, _) :-
    var(Var),
    !.
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

%!  is_term_of_type(Term, X)
%
%   Supports types as e.g. compound(oneof(list(atom))).

is_term_of_type(Term, X) :-
    compound_name_arity(Term, N, A),
    compound_name_arity(X, N, A),
    term_arg_types(1, A, Term, X).

term_arg_types(I, A, Type, X) :-
    I =< A,
    !,
    arg(I, Type, AType),
    arg(I, X, XArg),
    has_type(AType, XArg),
    I2 is I+1,
    term_arg_types(I2, A, Type, X).
term_arg_types(_, _, _, _).


		 /*******************************
		 *           SANDBOX		*
		 *******************************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(error:current_type(_,_,_)).
