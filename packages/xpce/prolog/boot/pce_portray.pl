/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_portray, []).
:- use_module(pce_boot(pce_principal)).

:- multifile
	user:portray/1,
	user:prolog_list_goal/1,
	user:prolog_predicate_name/2,
	user:prolog_clause_name/2.
:- dynamic
	user:portray/1.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE portray rules. These rules print object references indicating their
class-name and the goal to the implementation of methods more readable
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

user:portray(Obj) :-
	object(Obj),
	Obj = @Ref, !,
	(   send(Obj, '_instance_of', var)
	->  get(Obj, '_value', Value),
	    format('@~w(= ~p)', [Ref, Value])
	;   get(Obj, '_class_name', CN),
	    format('@~w/~w', [Ref, CN])
	).
user:portray(pce_principal:send_implementation(_Id, Args, Receiver)) :-
	object(Receiver), !,
	format('~p->~p', [Receiver, Args]).
user:portray(pce_principal:get_implementation(_Id, Args, Receiver, RVal)) :-
	object(Receiver), !,
	format('~p<-~p --> ~p', [Receiver, Args, RVal]).

%	user:prolog_list_goal(:Goal)
%
%	Called from the SWI-Prolog debugger 'L' command to show the
%	relevant sourcecode given the current Goal.

user:prolog_list_goal(pce_principal:send_implementation(Id, _Args, _Ref)) :- !,
	(   Head = pce_principal:send_implementation(Id, Args, Ref),
	    method_from_id(Id, ->(Class, Sel)),
	    clause(Head, Body)
	->  format('~N~n% XPCE Method ~w->~w:~n~n', [Class, Sel]),
	    portray_clause(((Ref->Args) :- Body)),
	    nl
	;   format('No XPCE method implementation for id=~p~n', [Id])
	).
user:prolog_list_goal(pce_principal:get_implementation(Id, _Args, _Ref, _Rval)) :- !,
	(   Head = pce_principal:get_implementation(Id, Args, Ref, Rval),
	    method_from_id(Id, <-(Class, Sel)),
	    clause(Head, Body)
	->  format('~N~n% XPCE Method ~w<-~w:~n~n', [Class, Sel]),
	    portray_clause(((Rval = <-(Ref,Args)) :- Body)),
	    nl
	;   format('No XPCE method implementation for id=~p~n', [Id])
	).

%	user:prolog_predicate_name(:Goal, -Name)
%
%	Hook used by the Prolog graphical tracer to display the frames
%	in the stack.

user:prolog_predicate_name(pce_principal:send_implementation(Id0, _, _),
			   Id) :-
	method_from_id(Id0, SG),
	atom_from_method(SG, Id).
user:prolog_predicate_name(pce_principal:get_implementation(Id0, _, _, _),
			   Id) :-
	method_from_id(Id0, SG),
	atom_from_method(SG, Id).

clean_id(Id, Id) :-
	atomic(Id).
clean_id(Term, Id) :-
	compound(Term),
	arg(1, Term, Id0),
	clean_id(Id0, Id).

%	user:prolog_clause_name(+ClauseRef, -Name)
%
%	Translate the reference to a method-clause into the corresponding
%	method.

user:prolog_clause_name(Ref, Name) :-
	clause(Head, _, Ref),
	prolog_predicate_name(Head, Name).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%	Get the type, class and selector from a method identifier.
%	Should we cache this info for proper performance on the tracer?

method_from_id(Id, Method) :-
	compound(Id), !,
	arg(1, Id, Id2),
	method_from_id(Id2, Method).
method_from_id(Id, ->(Class, Selector)) :-
	pce_principal:pce_lazy_send_method(Selector, Class, Binder),
	arg(1, Binder, Id), !.
method_from_id(Id, <-(Class, Selector)) :-
	pce_principal:pce_lazy_get_method(Selector, Class, Binder),
	arg(1, Binder, Id).

atom_from_method(->(Class, Selector), Atom) :-
	concat_atom([Class, (->), Selector], Atom).
atom_from_method(<-(Class, Selector), Atom) :-
	concat_atom([Class, (<-), Selector], Atom).
