/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(pce_portray, []).

:- multifile
	user:portray/1,
	user:prolog_list_goal/1,
	user:prolog_predicate_name/2.
:- dynamic
	user:portray/1,
	user:prolog_list_goal/1,
	user:prolog_predicate_name/2.

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
	    clause(Head, Body)
	->  format('~N~n% XPCE Method ~w:~n~n', Id),
	    portray_clause(((Ref->Args) :- Body)),
	    nl
	;   format('No XPCE method implementation for id=~p~n', [Id])
	).
user:prolog_list_goal(pce_principal:get_implementation(Id, _Args, _Ref, _Rval)) :- !,
	(   Head = pce_principal:get_implementation(Id, Args, Ref, Rval),
	    clause(Head, Body)
	->  format('~N~n% XPCE Method ~w:~n~n', Id),
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
	clean_id(Id0, Id).
user:prolog_predicate_name(pce_principal:get_implementation(Id0, _, _, _),
			   Id) :-
	clean_id(Id0, Id).

clean_id(Id, Id) :-
	atomic(Id).
clean_id(Term, Id) :-
	compound(Term),
	arg(1, Term, Id0),
	clean_id(Id0, Id).
