/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
