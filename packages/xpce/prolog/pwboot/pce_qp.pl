/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(pce_host, []).
:- use_module(language(pce_messages)).
:- use_module(library(strings), [concat_atom/2]).

pwversion('3.1').

:- multifile 'QU_messages':generate_message/3.
:- multifile 'QU_messages':context_type/3.

'QU_messages':generate_message(Spec, Out, Tail) :-
	pce_message(Spec, Out, Tail).
'QU_messages':context_type(Spec, Out, Tail) :-
	pce_message_context(Spec, Out, Tail).

:- initialization
	user:asserta((term_expansion(:-(require(Preds)),
				     :-(require:require(Module:Preds))):-
		     prolog_load_context(module, Module), !)).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(pce, quintus(Dir)) :-
	pwversion(PwVersion),
	concat_atom([prowindows, PwVersion], Dir).
user:file_search_path(contrib, 	pce(contrib)).

		 /*******************************
		 *	    C-EXCEPTIONS	*
		 *******************************/

pce_error(E) :-
	print_message(error, E).

:- extern(pce_error(+term)).

		 /*******************************
		 *	      HELP HOOK		*
		 *******************************/

:- multifile user:user_help/0.
:- dynamic user:user_help/0.

user:user_help :-
	(   pce_qux:callable_predicate(manpce)
	->  true
	;   pce_qux:pce_info(loading(manpce))
	),
	pce_qux:auto_call(manpce),
	(   predicate_property(qui:init_qui, _) % dubious
	->  pce_qux:xpce_loop
	;   true
	).

		 /*******************************
		 *	       ABOUT		*
		 *******************************/

about('ProWindows version 3.2', boldhuge).
about('Based on', italic).
about('XPCE version %s'+[@pce?version], huge).
about('Copyright 1992-1996, AIIL / University of Amsterdam', normal).
about('Contact:', italic).
about('AI International ltd.', huge).
about('Castle Chambers\nHigh Street, Berkhamsted\nHerts, HP4 2DF', normal).
about('Tel: 01442 873873, Fax: 01442 860200', normal).
about('E-mail:', italic).
about('support@aiil.co.uk\ninfo@aiil.co.uk', normal).

		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

property(prolog(quintus)).
property(file_extensions([qof, pl])).
property(repeat_meta_declaraction).
property(need_extern_declaration).
property(use_predicate_references).
property(register_source_locations).

		/********************************
		*       REINITIALISATION	*
		********************************/

pce_load_rcfile :-
	absolute_file_name('~/.pwrc', [access(read), file_errors(fail)], F),
	user:ensure_loaded(F), !.
pce_load_rcfile.

:- initialization
	pce_load_rcfile.
