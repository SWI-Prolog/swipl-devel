/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(pce_goal_expansion, []).
:- use_module(pce_realise).
:- use_module(pce_boot(pce_expansion), [pce_compiling/1]).
:- use_module(pce_boot(pce_principal), [get/3]).
:- require([ pce_error/1
	   , append/3
	   ]).

expandable(send, A)       :- A >= 2.
expandable(get, A)        :- A >= 3.
expandable(send_super, A) :- A >= 2.
expandable(get_super, A)  :- A >= 3.

%	Convert old send/get into new format.

expand(OldSend, Send) :-
	compound(OldSend),
	OldSend =.. [ send, R, Sel | Args ],
	atom(Sel),
	Args \== [], !,
	Msg =.. [Sel|Args],
	Send = send(R, Msg).
expand(OldGet, Get) :-
	compound(OldGet),
	OldGet =.. [ get, R, Sel | AllArgs ],
	atom(Sel),
	append(Args, [Result], AllArgs),
	Args \== [], !,
	Msg =.. [Sel|Args],
	Get = get(R, Msg, Result).

%	Deal with send-super

expand(send(R, Msg), send_class(R, Super, SuperMsg)) :-
	compound(Msg),
	Msg =.. [send_super, Selector | Args], !,
	atom(Selector),
	current_super_class(send_super, Super),
	SuperMsg =.. [Selector|Args].
expand(get(R, Msg, Answer), get_class(R, Super, SuperMsg, Answer)) :-
	compound(Msg),
	Msg =.. [get_super, Selector | Args], !,
	atom(Selector),
	current_super_class(get_super, Super),
	SuperMsg =.. [Selector|Args].
expand(send_super(R, Msg), send_class(R, Super, Msg)) :-
	current_super_class(send_super, Super).
expand(get_super(R, Msg, V), get_class(R, Super, Msg, V)) :-
	current_super_class(get_super, Super).
expand(SendSuperN, send_class(R, Super, Msg)) :-
	compound(SendSuperN),
	SendSuperN =.. [send_super, R, Sel | Args],
	atom(Sel),
	Msg =.. [Sel|Args],
	current_super_class(send_super, Super).
expand(GetSuperN, get_class(R, Super, Msg, Answer)) :-
	compound(GetSuperN),
	GetSuperN =.. [get_super, R, Sel | AllArgs],
	atom(Sel),
	append(Args, [Answer], AllArgs),
	Msg =.. [Sel|Args],
	current_super_class(get_super, Super).

%	current_super_class(-Super)
%
%	Return the name of the super-class of the currently compiling
%	class.

current_super_class(_, Super) :-
	pce_compiling(Class),
	super_class(Class, Super), !.
current_super_class(Op, _) :-
	pce_error(context_error(Op, nosuper, goal)),
	fail.

%	super_class(+ClassName, -SuperClassName)
%
%	Deternine the super-class of a class.  Try to avoid realising
%	the class to improve compilation and save-state performance.
%	First clause deals with the most common case, where we want to
%	have the super-class of the currently compiling class.

super_class(Class, Super) :-
	pce_expansion:attribute(Class, super, Super), !.
super_class(Class, Super) :-
	pce_prolog_class(Class, Super), !.
super_class(Class, Super) :-
	get(@classes, member, Class, ClassObject),
	get(ClassObject, super_class_name, Super).


		 /*******************************
		 *	  REGISTER HOOK		*
		 *******************************/

pce_ifhostproperty(prolog(sicstus),
(   user:goal_expansion(Goal, _Context, ExpandedGoal) :-
	functor(Goal, Name, Arity),
	expandable(Name, Arity),
	expand(Goal, ExpandedGoal)
),
(   user:goal_expansion(Goal, ExpandedGoal) :-
	functor(Goal, Name, Arity),
	expandable(Name, Arity),
	expand(Goal, ExpandedGoal)
)).

