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
	    emacs_end_mode/0,
	    emacs_mode_bindings/3
	  ]).
:- meta_predicate
	emacs_begin_mode(:, +, +, +, +),
	emacs_extend_mode(:, +),
	emacs_mode_bindings(:, +, +).

:- use_module(library(pce)).
:- require([ concat/3
	   , forall/2
	   , member/2
	   , strip_module/3
	   ]).


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
%		Selector = [key(Key)]
%			   [+ button(Button)]
%			   [+ button(Button, Function)]		(pullright)
%
%	Syntax:
%
%		Char [=+] Category(Args)

emacs_begin_mode(Mode0, Super, Summary, Bindings, Syntax) :-
	strip_module(Mode0, Module, Mode),
	get(string('emacs_%s_mode', Mode), value, PceClass),
	get(string('emacs_%s_mode', Super), value, PceSuperClass),
	pce_begin_class(Module:PceClass, PceSuperClass, Summary),
	emacs_mode_bindings(Mode0, Bindings, Syntax).

emacs_mode_bindings(Mode0, Bindings, Syntax) :-
	strip_module(Mode0, Module, Mode),
	get(string('emacs_%s_mode', Mode), value, PceClass),
	get(@pce, convert, PceClass, class, ClassObject),
	get(ClassObject, super_class, SuperClass),
	get(SuperClass, name, SuperName),
	concat(emacs_, M0, SuperName),
	concat(SuperMode, '_mode', M0),
	new(KB, emacs_key_binding(Mode, SuperMode)),
	new(MM, emacs_mode_menu(Mode, SuperMode)),
	new(ST, syntax_table(Mode, SuperMode)),
	make_bindings(Bindings, Module, KB, MM),
	make_syntax(Syntax, ST).

make_bindings(Bindings, Module, KB, MM) :-
	forall(member((Selector = Term), Bindings),
	       bind(Term, Selector, Module, KB, MM)).

make_syntax(Syntax, ST) :-
	forall(member(S, Syntax), syntax(S, ST)).


bind(key(Key), Selector, _, KB, _) :-
	send(KB, function, Key, Selector).
bind(button(Button), Selector, _, _, MM) :-
	send(MM, append, Button, Selector).
bind(button(Button, Func), Selector, Module, _, MM) :-
	Module:send(MM, append, Button, emacs_argument_item(Selector, Func)).
bind(A+B, Selector, Module, KB, MM) :-
	bind(A, Selector, Module, KB, MM),
	bind(B, Selector, Module, KB, MM).
	
syntax(Char = Term, ST) :-
	Term =.. [Class|Args],
	Send =.. [send, ST, syntax, Char, Class | Args],
	Send.
syntax(Char + Term, ST) :-
	Term =.. [Class|Args],
	Send =.. [send, ST, add_syntax, Char, Class | Args],
	Send.
syntax(paragraph_end(Regex), ST) :-
	send(ST, paragraph_end, Regex).
syntax(sentence_end(Regex), ST) :-
	send(ST, sentence_end, Regex).

%	emacs_extend_mode(Mode, Bindings).
%
%	Extend an existing emacs mode

emacs_extend_mode(Mode0, Bindings) :-
	strip_module(Mode0, Module, Mode),
	get(string('emacs_%s_mode', Mode), value, PceClass),
	get(@pce, convert, PceClass, class, _), % force	loading
	pce_extend_class(PceClass),
	get(@pce, convert, Mode, emacs_key_binding, KB),
	get(@pce, convert, Mode, emacs_mode_menu, MM),
	forall(member((Selector = Term), Bindings),
	       bind(Term, Selector, Module, KB, MM)).


%	emacs_end_mode/0
%
%	Just for symetry right now.

emacs_end_mode :-
	pce_end_class.
