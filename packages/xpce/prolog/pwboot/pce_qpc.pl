/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- use_module(library(strings), [concat_atom/2]).

:- dynamic   user:term_expansion/2.
:- multifile user:term_expansion/2.

initialization_goal(pce_global, 2, _).
initialization_goal(pce_autoload, 2, _).
initialization_goal(new, 2, _).
initialization_goal(send, X, Send) :- X >= 2, \+ arg(1, Send, @(class)).
initialization_goal(get, X, _) :- X >= 3.
initialization_goal(free, 1, _).
initialization_goal(object, 1, _).
initialization_goal(object, 2, _).
initialization_goal(pce_help_file, 2, _).
initialization_goal(pce_image_directory, 1, _).
initialization_goal(declare_emacs_mode, 2, _).


		 /*******************************
		 *	       PCEDRAW		*
		 *******************************/

draw_expansion((:- draw_begin_shape(Name, Super, Summary, Recognisers)),
	       [(:- pce_begin_class(draw_shape_class:Name, Super, Summary)),
		(:- pce_class_directive(draw_shapes:associate_recognisers(Recognisers)))
	       ]).
draw_expansion((:- draw_end_shape), (:- pce_end_class)).

		 /*******************************
		 *	     PCE EMACS		*
		 *******************************/

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
emacs_mode_class(@(default), emacs_mode).

:- multifile pce_pre_expansion_hook/2.
:- dynamic   pce_pre_expansion_hook/2.

pce_pre_expansion_hook(In, Out) :-
	draw_expansion(In, Out), !.
pce_pre_expansion_hook(In, Out) :-
	emacs_expansion(In, Out).

		 /*******************************
		 *      INITIALIZATION TAGS	*
		 *******************************/

user:term_expansion((:- Directive), D) :-
	functor(Directive, Name, Arity),
	initialization_goal(Name, Arity, Directive), !,
	D = (:- initialization(Directive)).


		 /*******************************
		 *	       REQUIRE		*
		 *******************************/

user:term_expansion(:-(require(Preds)), List) :- !,
	require:te_require_list(Preds, List).



		 /*******************************
		 *	   HOST PROPERTIES	*
		 *******************************/

user:term_expansion(pce_ifhostproperty(Prop, Clause), TheClause) :-
        (   property(Prop)
        ->  TheClause = Clause
        ;   TheClause = []
        ).
user:term_expansion(pce_ifhostproperty(Prop, Then, Else), TheClause) :-
        (   property(Prop)
        ->  TheClause = Then
        ;   TheClause = Else
        ).


property(prolog(quintus)).
property(file_extensions([qof, pl])).
property(repeat_meta_declaraction).
property(need_extern_declaration).
property(use_predicate_references).
property(qpc).


		 /*******************************
		 *	     LIBRARIES		*
		 *******************************/

:- use_module(library(fromonto)).
:- use_module(library(charsio)).
:- use_module(require).
:- use_module(pce_utils).
:- use_module(pce_expansion).
:- use_module(language(pce_messages)).
:- use_module(library(expandmath)).

		 /*******************************
		 *	 WHAT WE RECORD		*
		 *******************************/

/*
:- initialization pce_begin_recording(-documentation).
:- initialization pce_begin_recording(-source).
*/
