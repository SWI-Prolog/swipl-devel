:- module(pce_host,
	  [ user_help/0,
	    qui_user_help/0,
	    xpce_loop/0,
	    pce_reset/0
	  ]).

:- meta_predicate ignore(:).
:- meta_predicate pce_predicate_reference(:, ?).

:- use_module(library(strings), [concat_atom/2]).

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

%  this initialization should be done before any :-(send) directives.
%  thus this file should be the first loaded.

:- initialization
	( (prolog_flag(system_type, development), fail) ->
	  '$Quintus: check_license'(prowindows, '3.0')
	; true  % no checking in runtimes
	),
	pce_init,
	prowindows_version,
        asserta(user:file_search_path(package, quintus('prowindows3.0'))),
	asserta(user:file_search_path(contrib, package(contrib))),
        absolute_file_name(quintus('prowindows3.0'), Home),
	pl_send(@host, name_reference, prolog, 1),
        pl_send(@pce, home, Home, 1),
	add_input_callback.


:- dynamic pw_version_done/0.

prowindows_version :-
	( pw_version_done ->
	  true
	; otherwise ->
	  List = [ 'Quintus ProWindows 3.0 (XPCE ',
		   PceVersion,
		   ') Interface Copyright (C) 1995, Quintus Corporation / University of Amsterdam.
All Rights Reserved.'],
	  ubpl_get(@pce, version, PceVersion, 1),
	  concat_atom(List, VersionString),
	  version(VersionString),
	  assert(pw_version_done)
	).

pce_pre_init :-
	( predicate_property(qui:init_qui, _) ->
	  xt_create_app_context(App),
	  pce_appcontext(App, App)
	; true
	).

% Setting up an input callback - we need to open the display
% to get its connection file descriptor and then pass that
% to setup_input that registers pceDispatch() with QP_add_input().
%
% NOTE: pceDispatch will process prowindows events until it detects
% input on an fd that is given by the second argument to setup_input/3.
% We assume that the prolog read loop is listening on FD 0.
%
% For QUI we don't call this because:
% a. We do not know what FD the QUI event loop is reading from 
% b. QP_select always thinks there is input on the QUI fd (even though
%    subsequent XtAppPending returns 0) and this gets processed before
%    any input callbacks.
% 

add_input_callback :-
	pl_send(@display, open, 1),
	ubpl_get(@display, connection_fd, FD, 1),
	( predicate_property(qui:init_qui, _) ->
	  true
	; setup_input(FD, 0, 0)
	).


foreign(setup_input, c, setup_input(+integer, +integer, [-integer])).
foreign(prolog_pce_init, c, pce_init).
foreign(pceDispatch, c, pce_dispatch(+integer, +integer)).
foreign(pceXtAppContext, c, pce_appcontext(+integer, [-integer])).
foreign(pl_send0, c, pl_send(+term, +term, [-integer])).
foreign(pl_send1, c, pl_send(+term, +term, +term, [-integer])).
foreign(pl_send2, c, pl_send(+term, +term, +term, +term, [-integer])).
foreign(pl_send3, c, pl_send(+term, +term, +term, +term, +term, [-integer])).
foreign(pl_sendN, c, '$pl_pce_send'(+term, +term, +term, [-integer])).
foreign(pl_get0, c, pl_get(+term, +term, +term, [-integer])).
foreign(pl_get1, c, pl_get(+term, +term, +term, +term, [-integer])).
foreign(pl_get2, c, pl_get(+term, +term, +term, +term, +term, [-integer])).
foreign(pl_get3, c, pl_get(+term, +term, +term, +term, +term, +term, 
			[-integer])).
foreign(pl_getN, c, '$pl_pce_get'(+term, +term, +term, +term, [-integer])).
foreign(pl_get_objectN, c, '$pl_pce_get_object'(+term, +term, +term, +term,
			 [-integer])).
foreign(ubpl_get0, c, ubpl_get(+term, +term, -term, [-integer])).
foreign(ubpl_get1, c, ubpl_get(+term, +term, +term, -term, [-integer])).
foreign(ubpl_get2, c, ubpl_get(+term, +term, +term, +term, -term, [-integer])).
foreign(ubpl_get3, c, ubpl_get(+term, +term, +term, +term, +term, -term, 
			[-integer])).
foreign(ubpl_getN, c, '$ubpl_pce_get'(+term, +term, +term, -term, [-integer])).
foreign(ubpl_get_objectN, c, '$ubpl_pce_get_object'(+term, +term, +term, -term,
			 [-integer])).
foreign(pl_object1, c, pl_object(+term, [-integer])).
foreign(pl_object2, c, pl_object(+term, +term, [-integer])).
foreign(ubpl_object2, c, ubpl_object(+term, -term, [-integer])).
foreign(pl_new, c, pl_new(+term, +term, [-integer])).
foreign(ubpl_new, c, ubpl_new(-term, +term, [-integer])).
foreign(pce_q_reset, c, pce_reset).
foreign(open, c, unix_open(+string, +integer, [-integer])).
foreign('XtCreateApplicationContext', c, xt_create_app_context([-integer])).
foreign(qp_pce_predicate_reference, c, qp_pce_predicate_reference(+term, +term, [-integer])).


foreign_file(system(libpce), 
	     [ prolog_pce_init, pl_send0, pl_send1, pl_send2,
	       pl_send3, pl_sendN, pl_get0, pl_get1, 
	       pl_get2, pl_get3, pl_getN, pl_get_objectN,
	       ubpl_get0, ubpl_get1,
	       ubpl_get2, ubpl_get3, ubpl_getN, ubpl_get_objectN, 
	       pl_object1, pl_object2, pl_new, ubpl_new, pce_q_reset,
	       qp_pce_predicate_reference,
	       ubpl_object2,
	       setup_input,
	       pceDispatch,
	       pceXtAppContext,
	       'XtCreateApplicationContext',
	       open
	     ]).

:- load_foreign_executable(system(libpce)).

user_help :-
	use_module(library(pce_manual)),
	call(manpce).

pce_principal:pce_predicate_reference(Spec, PceRef) :-
	strip_module(Spec, Module, Head),
	(   var(Head)
	->  qp_pce_predicate_reference(Head, PceRef, 1)
	;   qp_pce_predicate_reference(Module:Head, PceRef, 1)
	).

strip_module(RT, M, T) :-
	strip_module(RT, T, M, user).

strip_module(Module:RT2, T, M, _) :-
	atom(Module), !,
	strip_module(RT2, T, M, Module).
strip_module(T, T, M, M).

qui_user_help :-
	user:user_help,
	xpce_loop.

:- multifile user:message_hook/3.

user:message_hook(execution_aborted,error,[['Execution aborted'-[]]]):-
     write('Aborting ...'),
     nl,
     pce_reset,
     fail.


% callbacks from XPCE call user:call/1 with the goal term to call
 
user:(:- extern(call(+term))).

% if the callback results in an exception,
% print_exception/1 is called with the error term

:- extern(print_exception(+term)).

print_exception(Excp) :-
       print_message(error, pce_host_send_or_get),
       print_message(error, Excp).



pce_principal:send(A,B):-
        pl_send(A,B,1).
pce_principal:send(A,B,C):-
        pl_send(A,B,C,1).
pce_principal:send(A,B,C,D):-
        pl_send(A,B,C,D,1).
pce_principal:send(A,B,C,D,E):-
        pl_send(A,B,C,D,E,1).
pce_principal:'$pce_send'(A,B,C):-
        '$pl_pce_send'(A,B,C,1).
pce_principal:get(A,B,C):-
        (var(C) ->
		ubpl_get(A,B,C,1)
	;	pl_get(A,B,C,1)
	).
pce_principal:get(A,B,C,D):-
        (var(D) ->
		ubpl_get(A,B,C,D,1)
	;	pl_get(A,B,C,D,1)
	).
pce_principal:get(A,B,C,D,E):-
        (var(E) ->
		ubpl_get(A,B,C,D,E,1)
	;	pl_get(A,B,C,D,E,1)
	).
pce_principal:get(A,B,C,D,E,F):-
        (var(F) ->
		ubpl_get(A,B,C,D,E,F,1)
	;
		pl_get(A,B,C,D,E,F,1)
	).
pce_principal:'$pce_get'(A,B,C,D):-
        (var(D) ->
		'$ubpl_pce_get'(A,B,C,D,1)
	;
		'$pl_pce_get'(A,B,C,D,1)
	).
pce_principal:'$pce_get_object'(A,B,C,D):-
        (var(D) ->
		'$ubpl_pce_get_object'(A,B,C,D,1)
	;
		'$pl_pce_get_object'(A,B,C,D,1)
	).
pce_principal:object(A):-
        pl_object(A,1).
pce_principal:object(A,B):-
        (var(B) ->
		ubpl_object(A,B,1)
	;
		pl_object(A,B,1)
	).
pce_principal:new(A,B):-
        ( var(A) ->
		ubpl_new(A,B,1)
	; (A == @AA, var(AA)) ->
		ubpl_new(AAA, B, 1),
		A = AAA
	;
		pl_new(A,B,1)
	).



:- initialization
	( predicate_property(qui:init_qui,_) ->
	  print_message(informational, help_goal(qui_user_help))
	; prolog_flag(system_type, development) ->
	  print_message(informational, help_goal(user_help))
	; true
	).



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
	  pl_send(@display,dispatch,1),
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
