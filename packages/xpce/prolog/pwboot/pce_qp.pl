/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_host, []).
:- use_module(language(pce_messages)).
:- use_module(library(strings), [concat_atom/2]).

pwversion('3.2').

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

about('ProWindows version %s'+[Version], boldhuge) :-
	pwversion(Version).
about('Based on', italic).
about('XPCE version %s'+[@pce?version], huge).
about('Copyright 1992-1998, SICS / University of Amsterdam', normal).
about('Contact:', italic).
about('Swedish Institute of Computer Science', huge).
about('P.O. Box 1263\nSE-164 29 Kista\nSweden', normal).
about('Tel: +46-8-752-1500, Fax: +46-8-751-7230', normal).
about('Support: qpsupport@sics.se', normal).
about('Sales: qpsales@sics.se', normal).
about('WWW:', italic).
about('http://www.sics.se/quintus/', normal).

		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

property(prolog(quintus)).
property(file_extensions([qof, pl])).
property(repeat_meta_declaraction).
property(need_extern_declaration).
property(use_predicate_references).
property(register_source_locations).
property(system_source_prefix(Package)) :-
	pwversion(Version),
	concat_atom(['/prowindows', Version, '/'], Package).


		/********************************
		*       REINITIALISATION	*
		********************************/

pce_load_rcfile :-
	absolute_file_name('~/.pwrc', [access(read), file_errors(fail)], F),
	user:ensure_loaded(F), !.
pce_load_rcfile.

:- initialization
	pce_load_rcfile.
