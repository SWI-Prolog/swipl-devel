/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(emacs_extend,
	  [ declare_emacs_mode/2,
	    declare_emacs_mode/3
	  ]).
:- use_module(library(pce)).
:- require([ concat_atom/2
	   , concat/3
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
	declare_file_patterns(Extensions, Mode, @emacs_mode_list).

declare_file_patterns([], _, _).
declare_file_patterns([Ext|Rest], Mode, Sheet) :-
	send(Sheet, value, regex(Ext), Mode),
	declare_file_patterns(Rest, Mode, Sheet).

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

emacs_expansion((:- emacs_begin_mode(Mode, Super, Summary, Bindings, Syntax)),
		[(:- pce_begin_class(PceMode, PceSuper, Summary)),
		 (:- pce_class_directive(emacs_extend:emacs_mode_bindings(Mode,
							     Module,
							     Bindings,
							     Syntax)))
		]) :-
	emacs_mode_class(Mode, PceMode),
	emacs_mode_class(Super, PceSuper),
	prolog_load_context(module, Module).
emacs_expansion((:- emacs_extend_mode(Mode, Bindings)),
		[(:- pce_extend_class(PceMode)),
		 (:- pce_class_directive(emacs_extend:emacs_mode_bindings(Mode,
							     Module,
							     Bindings,
							     [])))
		]) :-
	emacs_mode_class(Mode, PceMode),
	prolog_load_context(module, Module).
emacs_expansion((:- emacs_end_mode), (:- pce_end_class)).

%	emacs_mode_bindings(+Mode, +Module, +Bindings, +Syntax)

emacs_mode_bindings(Mode, Module, Bindings, Syntax) :-
	emacs_mode_class(Mode, PceClass),
	get(@pce, convert, PceClass, class, ClassObject),
	get(ClassObject, super_class, SuperClass),
	get(SuperClass, name, SuperName),
	emacs_mode_class(SuperMode, SuperName),
	new(KB, emacs_key_binding(Mode, SuperMode)),
	new(MM, emacs_mode_menu(Mode, SuperMode)),
	(   get(@syntax_tables, member, Mode, ST)
	->  true
	;   new(ST, syntax_table(Mode, SuperMode))
	),
	make_bindings(Bindings, Module, KB, MM),
	make_syntax(Syntax, ST).

make_bindings([], _, _, _).
make_bindings([Selector = Term|Rest], Module, KB, MM) :-
	bind(Term, Selector, Module, KB, MM),
	make_bindings(Rest, Module, KB, MM).

make_syntax([], _).
make_syntax([S|Rest], ST) :-
	syntax(S, ST),
	make_syntax(Rest, ST).

bind(key(Key), Selector, _, KB, _) :-
	send(KB, function, Key, Selector).
bind(button(Button), Selector, _, _, MM) :-
	send(MM, append, Button, Selector).
bind(button(Button, Func), Selector, Module, _, MM) :-
	send(MM, Module:append(Button, emacs_argument_item(Selector, Func))).
bind(A+B, Selector, Module, KB, MM) :-
	bind(A, Selector, Module, KB, MM),
	bind(B, Selector, Module, KB, MM).
	
syntax(Char = Term, ST) :-
	Term =.. TermArgs,
	Msg =.. [syntax, Char | TermArgs],
	send(ST, Msg).
syntax(Char + Term, ST) :-
	Term =.. TermArgs,
	Msg =.. [add_syntax, Char | TermArgs],
	send(ST, Msg).
syntax(paragraph_end(Regex), ST) :-
	send(ST, paragraph_end, Regex).
syntax(sentence_end(Regex), ST) :-
	send(ST, sentence_end, Regex).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%	emacs_mode_class(?ModeName, ?ClassName)
%
%	Convert between plain PceEmacs modename and the mode class.

emacs_mode_class(ModeName, ClassName) :-
	atom(ModeName), !,
	(   ModeName == []
	->  ClassName = emacs_mode
	;   concat_atom([emacs_, ModeName, '_mode'], ClassName)
	).
emacs_mode_class(ModeName, ClassName) :-
	concat(emacs_, M0, ClassName),
	concat(ModeName, '_mode', M0), !.
emacs_mode_class(@default, emacs_mode).


		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

:- multifile
	user:pce_pre_expansion_hook/2.
:- dynamic
	user:pce_pre_expansion_hook/2.

user:pce_pre_expansion_hook(In, Out) :-
	emacs_expansion(In, Out).
