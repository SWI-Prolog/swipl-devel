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
	Obj = @Ref,
	object(Obj), !,
	(   send(Obj, '_instance_of', var)
	->  get(Obj, '_value', Value),
	    format('@~w(= ~p)', [Ref, Value])
	;   get(Obj, '_class_name', CN),
	    format('@~w/~w', [Ref, CN])
	).
user:portray(pce_principal:send_implementation(Id, Args, Receiver)) :-
	object(Receiver), !,
	method_from_id(Id, (Class->_Method)),
	format('Send-method on ~p: ~w->~p', [Receiver, Class, Args]).
user:portray(pce_principal:get_implementation(Id, Args, Receiver, RVal)) :-
	object(Receiver), !,
	method_from_id(Id, <-(Class, _Method)),
	format('Get-method on ~p: ~w<-~p --> ~p',
	       [Receiver, Class, Args, RVal]).

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
