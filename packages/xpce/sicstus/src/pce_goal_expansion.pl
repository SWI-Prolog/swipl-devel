/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(pce_goal_expansion, []).
:- use_module(pce_boot(pce_realise)).
:- use_module(pce_boot(pce_expansion), [pce_compiling/1]).
:- use_module(pce_boot(pce_principal), [get/3, get/4]).
:- require([ pce_error/1
	   , append/3
	   ]).

expandable(send, A)       :- A >= 2.
expandable(get, A)        :- A >= 3.
expandable(send_super, A) :- A >= 2.
expandable(get_super, A)  :- A >= 3.

%	requalify(:QIn, -In, +Context, +Out, -QOut)
%
%	In is optionally qualified with a module.  The qualifier is
%	transferred to Out.  If In is not qualified, Context is used
%	to qualify Out.

requalify(V,   V, C, R, C:R) :-
	var(V), !.
requalify(M:I, I, _, R, M:R) :- !.
requalify(I,   I, C, R, C:R).

qualify(T, _, T) :-
	compound(T),
	functor(T, :, 2), !.
qualify(T, C, C:T).

%	Convert old send/get into new format.

expand(OldSend, C, Send) :-
	OldSend =.. [ send, R, Sel0 | Args ],
	Args \== [], !,
	requalify(Sel0, Sel, C, Msg0, Msg),
	atom(Sel),
	Msg0 =.. [Sel|Args],
	Send = send(R, Msg).
expand(OldGet, C, Get) :-
	OldGet =.. [ get, R, Sel0 | AllArgs ],
	append(Args, [Result], AllArgs),
	Args \== [], !,
	requalify(Sel0, Sel, C, Msg0, Msg),
	atom(Sel),
	Msg0 =.. [Sel|Args],
	Get = get(R, Msg, Result).

%	Deal with send-super

expand(send(R, Msg0), C, send_class(R, Super, SuperMsg)) :-
	requalify(Msg0, Msg, C, SuperMsg0, SuperMsg),
	compound(Msg),
	Msg =.. [send_super, Selector | Args], !,
	current_super_class(send_super, Super),
	SuperMsg0 =.. [Selector|Args].
expand(get(R, Msg0, Answer), C, get_class(R, Super, SuperMsg, Answer)) :-
	requalify(Msg0, Msg, C, SuperMsg0, SuperMsg),
	compound(Msg),
	Msg =.. [get_super, Selector | Args], !,
	current_super_class(get_super, Super),
	SuperMsg0 =.. [Selector|Args].
expand(send_super(R, Msg0), C, send_class(R, Super, Msg)) :-
	qualify(Msg0, C, Msg),
	current_super_class(send_super, Super).
expand(get_super(R, Msg0, V), C, get_class(R, Super, Msg, V)) :-
	qualify(Msg0, C, Msg),
	current_super_class(get_super, Super).
expand(SendSuperN, C, send_class(R, Super, Msg)) :-
	compound(SendSuperN),
	SendSuperN =.. [send_super, R, Sel0 | Args],
	requalify(Sel0, Sel, C, Msg0, Msg),
	Msg0 =.. [Sel|Args],
	current_super_class(send_super, Super).
expand(GetSuperN, C, get_class(R, Super, Msg, Answer)) :-
	compound(GetSuperN),
	GetSuperN =.. [get_super, R, Sel0 | AllArgs],
	requalify(Sel0, Sel, C, Msg0, Msg),
	append(Args, [Answer], AllArgs),
	Msg0 =.. [Sel|Args],
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
%	Determine the super-class of a class.  Try to avoid realising
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

:- multifile
	user:goal_expansion/3.

%user:goal_expansion(Goal, Context, ExpandedGoal) :-
%	functor(Goal, Name, Arity),
%	expandable(Name, Arity),
%	format('Expanding ~w (Context = ~w)~n', [Goal, Context]),
%	expand(Goal, Context, ExpandedGoal),
%	format('-->        ~w~n', [ExpandedGoal]).
user:goal_expansion(Goal, Context, ExpandedGoal) :-
	functor(Goal, Name, Arity),
	expandable(Name, Arity),
	expand(Goal, Context, ExpandedGoal).
