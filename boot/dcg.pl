/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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

:- module('$dcg',
	  [ dcg_translate_rule/2,
	    phrase/2,
	    phrase/3
	  ]).

		/********************************
		*        GRAMMAR RULES          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The DCG compiler. The original code was copied from C-Prolog and written
by Fernando Pereira, EDCAAD, Edinburgh,  1984.   Since  then many people
have modified and extended this code. It's a nice mess now and it should
be redone from scratch. I won't be doing   this  before I get a complete
spec explaining all an implementor needs to   know  about DCG. I'm a too
basic user of this facility myself (though   I  learned some tricks from
people reporting bugs :-)

The original version contained '$t_tidy'/2  to   convert  ((a,b),  c) to
(a,(b,c)), but as the  SWI-Prolog  compiler   doesn't  really  care (the
resulting code is simply the same), I've removed that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

dcg_translate_rule(((LP,MNT)-->RP),(H:-B)) :- !,
	'$set_source_module'(M, M),
	'$extend'(LP, S0, SR, H),
	'$t_body'(RP, M:M, S0, S1, B0),
	'$t_body'(MNT, M:M, SR, S1, B1),
	'$body_optimized'((B0,B1),B2,S0),
	'$body_optimized'(B2,B,SR).
dcg_translate_rule((LP-->RP), (H:-B)) :-
	'$extend'(LP, S0, S, H),
	'$set_source_module'(M, M),
	'$t_body'(RP, M:M, S0, S, B0),
	'$body_optimized'(B0,B,S0).

'$body_optimized'(B0,B,S0) :-
	(   B0 = (S00=X, B),		% map a(H,T) :- H = [a,b|T], b(T)
	    S00 == S0
	->  S0 = X			% into a([a,b|T0]) :- b(T0, T).
	;   B0 = (S00=X),		% map a(H,T) :- H = [a,b|T]
	    S00 == S0
	->  S0 = X,			% into a([a,b|T], T)
	    B = true
	;   B0 = B
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On the DCG Translation of {}

	a --> x, {y}.

There are two options.  In traditional systems we see:

	a(A, B) :- x(A, B), y.

And in modern system we see:

	a(A, B) :- x(A, C), y, B=C.


Martin Sondergaard's grammar was breaking down on
=================================================

s --> v, star0, {write('You can not do that')}.
star0 --> [].
star0 --> [_], star0.

meaning to write a  message  for  any   sentence  starting  with  a `v',
skipping the remainder. With delayed binding  this causes a large number
of messages as star0 only eats one token on backtracing.

You can fix this using remaining as below rather then star0.

remaining --> [_], !, remaining.
remaining --> [].


Without delayed unification of the tail we get the following trouble
====================================================================
(comment from Richard O'Keefe)

Suppose I have

    p --> [a], !, {fail}. p --> [].

That is, p//0 is suppose to match the empty  string as long as it is not
followed by a. Now consider

    p([a], [a])

If the first clause is translated as

    p(S0, S) :- S0 = [a|S1], !, fail, S = S1.

then it will work *correctly*, and the call  p([a], [a]) will fail as it
is supposed to. If the first clause is translated as

    p(S0, S) :- S0 = [a|S], !, fail.

then the call p([a], [a]) will succeed, which is quite definitely wrong.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'$t_body'(Var, Q, S, SR, phrase(QVar, S, SR)) :-
	var(Var), !,
	qualify(Q, Var, QVar).
'$t_body'(M:X, _:C, S, SR, Ct) :- !,
	'$t_body'(X, M:C, S, SR, Ct).

'$t_body'([], _, S, SR, S=SR) :- !.		% inline lists
'$t_body'(List, _, S, SR, C) :-
	(   List = [_|_]
	->  !,
	    (   is_list(List)
	    ->  '$append'(List, SR, OL),
		C = (S = OL)
	    ;   C = '$append'(List, SR, S)	% Deals with [H|T] in body
	    )
	;   string(List)			% double_quotes = string
	->  !,
	    string_to_list(List, Codes),
	    '$append'(Codes, SR, OL),
	    C = (S = OL)
	).
'$t_body'(!, _, S, SR, ( !, SR = S) ) :- !.
'$t_body'({}, _, S, S, true) :- !.
'$t_body'({T}, Q, S, SR, (QT, SR = S)) :- !,	% (*)
	qualify(Q, T, QT).
%'$t_body'({T}, S, S, T) :- !.			% (*)
'$t_body'((T, R), Q, S, SR, (Tt, Rt)) :- !,
	'$t_body'(T, Q, S, SR1, Tt),
	'$t_body'(R, Q, SR1, SR, Rt).
'$t_body'((T;R), Q, S, SR, (Tt;Rt)) :- !,
	'$t_body'(T, Q, S, S1, T1), '$t_fill'(S, SR, S1, T1, Tt),
	'$t_body'(R, Q, S, S2, R1), '$t_fill'(S, SR, S2, R1, Rt).
'$t_body'((T|R), Q, S, SR, (Tt;Rt)) :- !,
	'$t_body'(T, Q, S, S1, T1), '$t_fill'(S, SR, S1, T1, Tt),
	'$t_body'(R, Q, S, S2, R1), '$t_fill'(S, SR, S2, R1, Rt).
'$t_body'((C->T), Q, S, SR, (Ct->Tt)) :- !,
	'$t_body'(C, Q, S, SR1, Ct),
	'$t_body'(T, Q, SR1, SR, Tt).
'$t_body'((C*->T), Q, S, SR, (Ct*->Tt)) :- !,
	'$t_body'(C, Q, S, SR1, Ct),
	'$t_body'(T, Q, SR1, SR, Tt).
'$t_body'((\+ C), Q, S, SR, (\+ Ct, SR = S)) :- !,
	'$t_body'(C, Q, S, _, Ct).
'$t_body'(T, Q, S, SR, QTt) :-
	'$extend'(T, S, SR, Tt),
	qualify(Q, Tt, QTt).

'$t_fill'(S, SR, S1, T, (T, SR=S)) :-
	S1 == S, !.
'$t_fill'(_S, SR, SR, T, T).

qualify(M:C, X0, X) :-
	M == C, !,
	X = X0.
qualify(M:_, X, M:X).

%	'$extend'(+Head, +Extra1, +Extra2, -NewHead)
%
%	Extend Head with two more arguments (on behalf DCG compilation).
%	The solution below is one option. Using   =..  and append is the
%	alternative. In the current version (5.3.2), the =.. is actually
%	slightly faster, but it creates less garbage.

:- dynamic  '$extend_cache'/4.
:- volatile '$extend_cache'/4.

'$dcg_reserved'([]).
'$dcg_reserved'([_|_]).
'$dcg_reserved'({_}).
'$dcg_reserved'({}).
'$dcg_reserved'(!).
'$dcg_reserved'((\+_)).
'$dcg_reserved'((_,_)).
'$dcg_reserved'((_;_)).
'$dcg_reserved'((_|_)).
'$dcg_reserved'((_->_)).
'$dcg_reserved'((_*->_)).
'$dcg_reserved'((_-->_)).

'$extend'(V, _, _, _) :-
	var(V), !,
	throw(error(instantiation_error,_)).
'$extend'(M:OldT, A1, A2, M:NewT) :- !,
	'$extend'(OldT, A1, A2, NewT).
'$extend'(OldT, A1, A2, NewT) :-
	'$extend_cache'(OldT, A1, A2, NewT), !.
'$extend'(OldT, A1, A2, NewT) :-
	( callable(OldT) -> true ; throw(error(type_error(callable,OldT),_)) ),
	( '$dcg_reserved'(OldT) -> throw(error(permission_error(define,dcg_nonterminal,OldT),_)) ; true ),
	functor(OldT, Name, Arity),
	functor(CopT, Name, Arity),
	NewArity is Arity+2,
	functor(NewT, Name, NewArity),
	'$copy_args'(1, Arity, CopT, NewT),
	A1Pos is Arity+1,
	A2Pos is Arity+2,
	arg(A1Pos, NewT, A1C),
	arg(A2Pos, NewT, A2C),
	assert('$extend_cache'(CopT, A1C, A2C, NewT)),
	OldT = CopT,
	A1C = A1,
	A2C = A2.

'$copy_args'(I, Arity, Old, New) :-
	I =< Arity, !,
	arg(I, Old, A),
	arg(I, New, A),
	I2 is I + 1,
	'$copy_args'(I2, Arity, Old, New).
'$copy_args'(_, _, _, _).

%%	phrase(:RuleSet, ?List).
%%	phrase(:RuleSet, ?List, ?Rest).
%
%	Interface to DCGs

:- meta_predicate
	phrase(//, ?),
	phrase(//, ?, ?).
:- noprofile((phrase/2,
	      phrase/3)).

phrase(RuleSet, Input) :-
	phrase(RuleSet, Input, []).
phrase(RuleSet, Input, Rest) :-
	(   strip_module(RuleSet, M, Plain),
	    nonvar(Plain),
	    dcg_special(Plain)
	->  '$t_body'(Plain, M:M, S0, S, Body),
	    Input = S0, Rest = S,
	    call(M:Body)
	;   call(RuleSet, Input, Rest)
	).

dcg_special((_,_)).
dcg_special((_;_)).
dcg_special((_|_)).
dcg_special((_->_)).
dcg_special(!).
dcg_special({_}).
dcg_special([]).
dcg_special([_|_]).
dcg_special(\+_).
