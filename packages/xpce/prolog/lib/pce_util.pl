/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_util,
	  [ get_object/3, get_object/4, get_object/5
	  , get_object/6, get_object/7, get_object/8
	  , get_object/9, get_object/10, get_object/11
	  , get_object/12, get_object/13

	  , send_list/2, send_list/3
	  , get_chain/3

	  , chain_list/2

	  , default/3
	  ]).


:- meta_predicate
	get_object(+, :, -),
	get_object(+, :, +, -),
	get_object(+, :, +, +, -),
	get_object(+, :, +, +, +, -),
	get_object(+, :, +, +, +, +, -),
	get_object(+, :, +, +, +, +, +, -),
	get_object(+, :, +, +, +, +, +, +, -),
	get_object(+, :, +, +, +, +, +, +, +, -),
	get_object(+, :, +, +, +, +, +, +, +, +, -),
	get_object(+, :, +, +, +, +, +, +, +, +, +, -),
	get_object(+, :, +, +, +, +, +, +, +, +, +, +, -),
	pce_get_object(+, :, +, -),

	send_list(:, +),
	send_list(:, +, +),
	send_list1(:, +),
	send_list1(:, +, +),

	get_chain(+, :, -).


:- use_module(library(pce)).
:- require([ pce_error/1
	   ]).

pce_ifhostproperty(prolog(quintus),
		   (   pce_get_object(Obj, Sel, Args, Rval) :-
		       pce_principal:'$pce_get_object'(Obj, Sel, Args, Rval)),
		   [

(:- use_module(library('../boot/pce_principal'),
	       [ '$pce_get_object'/4
	       ])),

(pce_get_object(Obj, Sel, Args, Rval) :-
	'$pce_get_object'(Obj, Sel, Args, Rval))]).


%   get_object(+@Object, +Selector, ...+Argument, ..., -Output)
%
%   Succeeds once if Output is the value returned by invoking get method
%   called Selector on Object.  Output is an object description, except for the
%   special objects @nil, @default, @on and @off all of which are both
%   object descriptions and object names.

get_object(Obj, Sel, Out) :-
	pce_get_object(Obj, Sel,
		       arguments, Out).
get_object(Obj, Sel, A1, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1), Out).
get_object(Obj, Sel, A1, A2, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2), Out).
get_object(Obj, Sel, A1, A2, A3, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3), Out).
get_object(Obj, Sel, A1, A2, A3, A4, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3, A4), Out).
get_object(Obj, Sel, A1, A2, A3, A4, A5, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3, A4, A5), Out).
get_object(Obj, Sel, A1, A2, A3, A4, A5, A6, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3, A4, A5, A6), Out).
get_object(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3, A4, A5, A6, A7), Out).
get_object(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3, A4, A5, A6, A7, A8), Out).
get_object(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, A9, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3, A4, A5, A6, A7, A8, A9), Out).
get_object(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Out) :-
	pce_get_object(Obj, Sel,
		       arguments(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10),
		       Out).


%	send_list(+ListOfObjs, +ListOfSels)
%
%	Send a messages to the carthesian product of ListOfObjs and
%	ListOfSels.

pce_ifhostproperty(prolog(quintus), [],
(   send_list([], _) :- !)).
send_list(_, []) :- !.
pce_ifhostproperty(prolog(quintus), [],
(   send_list([Object|Objects], Selectors) :- !, 
        send_list(Object, Selectors), 
        send_list(Objects, Selectors))).
send_list(Object, [Selector|Selectors]) :- !, 
        send_list(Object, Selector), 
        send_list(Object, Selectors).
send_list(Object, Selector) :-
        send_list1(Object, Selector).

send_list1(Module:Obj, Selector) :-
        atom(Module), !,
        send_list_module(Obj, Selector, Module).
send_list1(Object, Selector) :-
        send(Object, Selector).

send_list_module([], _, _) :- !.
send_list_module(_, [], _) :- !.
send_list_module([Object|Objects], Selectors, Module) :- !, 
        send_list_module(Object, Selectors, Module), 
        send_list_module(Objects, Selectors, Module).
send_list_module(Object, [Selector|Selectors], Module) :- !, 
        send_list_module(Object, Selector, Module), 
        send_list_module(Object, Selectors, Module).
send_list_module(Object, Selector, Module) :-
        send(Object, Module:Selector).


%       send_list(+ListOfObjs, +ListOfSels, +ListOfArgs)
%
%       Send a messages to the carthesian product of ListOfObjs and
%       ListOfSels.

pce_ifhostproperty(prolog(quintus), [],
(   send_list([], _,  _) :- !)).
send_list(_, [], _) :- !.
send_list(_, _, []) :- !.
pce_ifhostproperty(prolog(quintus), [],
(   send_list([Object|Objects], Selectors, Arguments) :- !, 
        send_list(Object, Selectors, Arguments), 
        send_list(Objects, Selectors, Arguments))).
send_list(Objects, [Selector|Selectors], Arguments) :- !, 
        send_list(Objects, Selector, Arguments), 
        send_list(Objects, Selectors, Arguments).
send_list(Object, Selector, [Argument|Arguments]) :- !, 
        send_list(Object, Selector, Argument), 
        send_list(Object, Selector, Arguments).
send_list(Object, Selector, Argument) :-
        send_list1(Object, Selector, Argument).

send_list1(Module:Obj, Selector, Arg) :-
        atom(Module), !,
        send_list_module(Obj, Selector, Arg, Module).
send_list1(Obj, Selector, Arg) :-
        send(Obj, Selector, Arg).

send_list_module([], _, _, _) :- !.
send_list_module(_, [], _, _) :- !.
send_list_module(_, _, [], _) :- !.
send_list_module([Object|Objects], Selectors, Arguments, Module) :- !, 
	send_list_module(Object, Selectors, Arguments, Module), 
	send_list_module(Objects, Selectors, Arguments, Module).
send_list_module(Objects, [Selector|Selectors], Arguments, Module) :- !, 
	send_list_module(Objects, Selector, Arguments, Module), 
	send_list_module(Objects, Selectors, Arguments, Module).
send_list_module(Object, Selector, [Argument|Arguments], Module) :- !, 
	send_list_module(Object, Selector, Argument, Module), 
	send_list_module(Object, Selector, Arguments, Module).
send_list_module(Object, Selector, Argument, Module) :-
	send(Object, Module:Selector, Argument).


%   get_chain(+@Object, +Selector, -List)
%
%   List is a Prolog list constructed from the PCE chain returned by <-Selector
%   on Object.  get_chain/3 returns a list of object names, 

get_chain(Object, Selector, List) :-
	get(Object, Selector, Chain), 
	chain_list(Chain, List).


%   chain_list(@+Chain, -List)
%   chain_list_object(@+Chain, -List)
%
%   List is a Prolog list of all objects in Chain.  chain_list/2 returns object
%   names, chain_list_object/2 object descriptions.

chain_list(Chain, List) :-
	nonvar(Chain), !,
	(   Chain == @nil
	->  List = []
	;   to_object(Chain, ChainObject),
	    send(ChainObject, instance_of, chain),
	    (   send(ChainObject, current_no, 1)
	    ->  'chain list 2'(ChainObject, List)
	    ;   List = []
	    )
	).
chain_list(Chain, List) :-
	new(Chain, chain),
	send_list(Chain, append, List).

'chain list 2'(Chain, [El|Rest]) :-
	get(Chain, next, El), !, 
	'chain list 2'(Chain, Rest).
'chain list 2'(Chain, []) :-
	\+ get(Chain, current, _).

to_object(@Ref, @Ref) :- !.
to_object(Term, Obj) :-
	new(Obj, Term).


		/********************************
		*             DEFAULTS		*
		********************************/

%	default(+Argument, +Default, -Value)
%	default(+Argument, resource(+Object, +Name), -Value)
%
%	Get the default value for an argument.

default(@default, resource(Obj, Name), Value) :- !, 
	(   get(Obj, resource_value, Name, Value)
	->  true
	;   pce_error(get_resource_failed(Name, Obj)),
	    fail
	).
default(@default, Default, Default) :- !.
default(Value,    _Default, Value).
