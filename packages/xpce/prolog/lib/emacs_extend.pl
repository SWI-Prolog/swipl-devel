/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(emacs_extend,
	  [ declare_emacs_mode/2,
	    declare_emacs_mode/3,
	    emacs_begin_mode/5,
	    emacs_extend_mode/2,
	    emacs_end_mode/0
	  ]).
:- use_module(library(pce)).
:- require([ forall/2
	   , member/2
	   ]).

:- meta_predicate
	emacs_begin_mode(:, +, +, +, +).

%	declare_emacs_mode(+ModeName, +FileSpec).
%
%	Specifies that PceEmacs mode `ModeName' may be defined by
%	(auto)loading `FileSpec'.

declare_emacs_mode(Mode, File) :-
	get(string('emacs_%s_mode', Mode), value, EmacsModeClass),
	pce_autoload(EmacsModeClass, File),
	(   \+ special_mode(Mode)
	->  get(@mode_name_type, context, Ctx),
	    send(Ctx, add, Mode),
	    send(Ctx, sort)
	;   true
	).

special_mode(shell).
special_mode(gdb).
special_mode(annotate).

%	declare_emacs_mode(+ModeName, +FileSpec, +ListOfPatterns)
%	
%	Sames as declare_emacs_mode/2.  `ListOfPatterns' is a list of
%	regular expressions that will automatically start this mode.

declare_emacs_mode(Mode, File, Extensions) :-
	declare_emacs_mode(Mode, File),
	forall(member(Ext, Extensions),
	       send(@emacs_mode_list, append,
		    attribute(regex(Ext), Mode))).

%	:- emacs_begin_mode(+Mode, +Super, +Summary, +Bindings, +Syntax).
%
%	Binding:
%
%		Selector = [key(Key)] [+ button(Button)]
%
%	Syntax:
%
%		Char [=+] Category(Args)

emacs_begin_mode(Mode0, Super, Summary, Bindings, Syntax) :-
	strip_module(Mode0, Module, Mode),
	get(string('emacs_%s_mode', Mode), value, PceClass),
	get(string('emacs_%s_mode', Super), value, PceSuperClass),
	pce_begin_class(Module:PceClass, PceSuperClass, Summary),
	new(KB, emacs_key_binding(Mode, Super)),
	new(MM, emacs_mode_menu(Mode, Super)),
	new(ST, syntax_table(Mode, Super)),
	make_bindings(Bindings, KB, MM),
	make_syntax(Syntax, ST).

make_bindings(Bindings, KB, MM) :-
	forall(member((Selector = Term), Bindings),
	       bind(Term, Selector, KB, MM)).

make_syntax(Syntax, ST) :-
	forall(member(S, Syntax), syntax(S, ST)).


bind(key(Key), Selector, KB, _) :-
	send(KB, function, Key, Selector).
bind(button(Button), Selector, _, MM) :-
	send(MM, append, Button, Selector).
bind(A+B, Selector, KB, MM) :-
	bind(A, Selector, KB, MM),
	bind(B, Selector, KB, MM).
	
syntax(Char = Term, ST) :-
	Term =.. [Class|Args],
	Send =.. [send, ST, syntax, Char, Class | Args],
	Send.
syntax(Char + Term, ST) :-
	Term =.. [Class|Args],
	Send =.. [send, ST, add_syntax, Char, Class | Args],
	Send.

%	emacs_extend_mode(Mode, Bindings).
%
%	Extend an existing emacs mode

emacs_extend_mode(Mode, Bindings) :-
	get(string('emacs_%s_mode', Mode), value, PceClass),
	get(@pce, convert, PceClass, class, _), % force	loading
	pce_extend_class(PceClass),
	get(@pce, convert, Mode, emacs_key_binding, KB),
	get(@pce, convert, Mode, emacs_mode_menu, MM),
	forall(member((Selector = Term), Bindings),
	       bind(Term, Selector, KB, MM)).


%	emacs_end_mode/0
%
%	Just for symetry right now.

emacs_end_mode :-
	pce_end_class.
