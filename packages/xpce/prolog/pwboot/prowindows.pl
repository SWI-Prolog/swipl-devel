/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(pce_host,
	  [ xpce_loop/0
	  ]).

:- meta_predicate ignore(:).
:- meta_predicate pce_predicate_reference(:, ?).

:- use_module(language(pce_messages)).

:- multifile 'QU_messages':generate_message/3.
:- multifile 'QU_messages':context_type/3.

'QU_messages':generate_message(Spec, Out, Tail) :-
	pce_message(Spec, Out, Tail).
'QU_messages':context_type(Spec, Out, Tail) :-
	pce_message_context(Spec, Out, Tail).

:- initialization
	user:asserta((term_expansion(:-(require(Preds)),
				:-(require:require(Module:Preds))):-
			prolog_load_context(module, Module),
			!)).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(package,	quintus('prowindows3.0')).
user:file_search_path(pce,	quintus('prowindows3.0')).
user:file_search_path(contrib, 	pce(contrib)).
%user:file_search_path(demo, 	pce(demo)).


%  this initialization should be done before any :-(send) directives.
%  thus this file should be the first loaded.

:- initialization
	(   (prolog_flag(system_type, development), fail)
	->  '$Quintus: check_license'(prowindows, '3.0')
	;   true  % no checking in runtimes
	).

:- multifile user:user_help/0.
:- dynamic user:user_help/0.

user:user_help :-
	(   pce_qux:defined_predicate(manpce)
	->  true
	;   pce_qux:pce_info(loading(manpce))
	),
	pce_qux:auto_call(manpce),
	(   predicate_property(qui:init_qui, _) % dubious
	->  xpce_loop
	;   true
	).

% if the callback results in an exception,
% print_exception/1 is called with the error term

:- extern(print_exception(+term)).

print_exception(Excp) :-
       print_message(error, pce_host_send_or_get),
       print_message(error, Excp).

/*
:- initialization
	(    predicate_property(qui:init_qui,_)
	->   print_message(informational, help_goal(qui_user_help))
	;    prolog_flag(system_type, development)
	->   print_message(informational, help_goal(user_help))
	;    true
	).
*/


property(prolog(quintus)).
property(file_extensions([qof, pl])).
property(repeat_meta_declaraction).
property(need_extern_declaration).
property(use_predicate_references).
property(register_source_locations).

file_extensions([qof, pl]).


callable_predicate(_Module:Head) :-
	predicate_property(Head, built_in),
	!.
callable_predicate(Module:Head) :-
        default_module(Module, DefModule),
        current_predicate(_, DefModule:Head), !.

default_module(M, M).
default_module(M, user) :- M \== user.


modified_since_last_loaded(_):-
	fail.

xpce_loop:-
	repeat,
	    pce_principal:send(@display, dispatch),
	fail.

'pceloadc++'(File) :-
        'pceloadc++'(File, []).

'pceloadc++'(File, Libs) :-
	 % Dont forget libg++.a !
         Fact1 = foreign('__pl_start', c, xpce_pl_start),
         Fact2 = foreign_file(File, ['__pl_start']),
         asserta(Fact1),
         asserta(Fact2),
         load_foreign_files([File], [system('pl-crt0.o'),
                                     '-lgcc' | Libs]),
         retract(Fact1),
         retract(Fact2).


		/********************************
		*       REINITIALISATION	*
		********************************/

pce_load_rcfile :-
	absolute_file_name('~/.pwrc', [access(read), file_errors(fail)], F),
	user:ensure_loaded(F), !.
pce_load_rcfile.

:- initialization
	pce_load_rcfile.
