/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(pce_principal,
	  [ new/2, free/1,

	    send/2, send/3, send/4, send/5, send/6, send/7,
	    send/8, send/9, send/10, send/11, send/12, 

	    get/3, get/4, get/5, get/6, get/7, get/8,
	    get/9, get/10, get/11, get/12, get/13,

	    object/1, object/2,
	    
	    pce_open/3,

	    pce_predicate_reference/2
	  ]).

:- meta_predicate
	new(+, :),
	send(+, :, +),
	send(+, :, +, +),
	send(+, :, +, +, +),
	send(+, :, +, +, +, +),
	send(+, :, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +, +),
	get(+, :, ?),
	get(+, :, +, ?),
	get(+, :, +, +, ?),
	get(+, :, +, +, +, ?),
	get(+, :, +, +, +, +, ?),
	get(+, :, +, +, +, +, +, ?),
	get(+, :, +, +, +, +, +, +, ?),
	get(+, :, +, +, +, +, +, +, +, ?),
	get(+, :, +, +, +, +, +, +, +, +, ?),
	get(+, :, +, +, +, +, +, +, +, +, +, ?),
	get(+, :, +, +, +, +, +, +, +, +, +, +, ?),
	pce_predicate_reference(:, ?).

:- use_module(library(strings), [concat_chars/2]).

:- initialization(op(100, fx,  @)).
:- initialization(op(150, yfx, ?)).
:- initialization(op(990, xfx, :=)).

:- extern(call(+term)).

foreign(pl_new,	     qp_new(+term, +term, [-integer])).
foreign(pl_send0,    qp_send(+term, +term, [-integer])).
foreign(pl_send1,    qp_send(+term, +term, +term, [-integer])).
foreign(pl_send2,    qp_send(+term, +term, +term, +term, [-integer])).
foreign(pl_send3,    qp_send(+term, +term, +term, +term, +term, [-integer])).
foreign(pl_sendn,    qp_sendn(+term, +term, +term, [-integer])).
foreign(pl_get0,     qp_get(+term, +term, +term, [-integer])).
foreign(pl_get1,     qp_get(+term, +term, +term, +term, [-integer])).
foreign(pl_get2,     qp_get(+term, +term, +term, +term, +term, [-integer])).
foreign(pl_get3,     qp_get(+term, +term, +term, +term, +term, +term,
			    [-integer])).
foreign(pl_getn,     qp_getn(+term, +term, +term, +term, [-integer])).
foreign(pl_object1,  qp_object(+term, [-integer])).
foreign(pl_object2,  qp_object(+term, +term, [-integer])).
foreign(pl_pce_init, pce_init(+term)).

foreign(pl_pce_predicate_reference, c,
	qp_predicate_reference(+term, +term, [-integer])).
foreign(xt_create_app_context, c,
	xt_create_app_context([-integer])).
foreign(pce_xt_appcontext, c,
	pce_appcontext(+integer, [-integer])).
foreign(setup_input, c,
	setup_input(+integer, +integer, +integer, [-integer])).
foreign(qp_pce_reset, c, 
	pce_reset).
foreign(qp_pce_redraw, c, 
	pce_redraw).
foreign(qp_pce_exit, c,
	pce_exit).
foreign(qp_pce_open, c,
	qp_pce_open(+term, +term, -integer, [-address])).

foreign_file(system(libpce),
	     [ pl_new,
	       pl_send0, pl_send1, pl_send2, pl_send3, pl_sendn,
	       pl_get0, pl_get1, pl_get2, pl_get3, pl_getn,
	       pl_object1, pl_object2,
	       pl_pce_predicate_reference,
	       pl_pce_init,
	       setup_input,
	       qp_pce_open,
	       qp_pce_reset,
	       qp_pce_redraw,
	       qp_pce_exit,
	       pce_xt_appcontext,
	       xt_create_app_context
	     ]).
:- load_foreign_executable(system(libpce)).

		 /*******************************
		 *	  EXIT/HALT HOOK	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Undocumented: Hook into halt/0 to call the XPCE exit handling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- initialization
   si:'$Quintus: hook'(assert, exit, permanent, pce_principal:pce_exit).


		 /*******************************
		 *	     NEW/FREE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Quintus 3.2 has a bug if a variable   is passed over an argument of type
+term and a local-shift appears during the execution of the function. To
fix this, such argument  are  packed  in   a  compound  term.  The macro
QP_varbug(t) in interface.c extracts the real term   from it. If this is
fixed, remove the QP_varbug() here and remove   all  t(x) from the new/2
and get[3-13] predicates in pce_principal.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

new(Ref, Term) :-
	qp_new(t(Ref), Term, 1).

free(Ref) :-
	(   object(Ref)
	->  send(Ref, free)
	;   true
	).


		 /*******************************
		 *	       SEND		*
		 *******************************/

send(Ref, Selector) :-
	qp_send(Ref, Selector, 1).
send(Ref, Selector, A0) :-
	qp_send(Ref, Selector, A0, 1).
send(Ref, Selector, A0, A1) :-
	qp_send(Ref, Selector, A0, A1, 1).
send(Ref, Selector, A0, A1, A2) :-
	qp_send(Ref, Selector, A0, A1, A2, 1).
send(Ref, Selector, A0, A1, A2, A3) :-
	qp_sendn(Ref, Selector, a(A0, A1, A2, A3), 1).
send(Ref, Selector, A0, A1, A2, A3, A4) :-
	qp_sendn(Ref, Selector, a(A0, A1, A2, A3, A4), 1).
send(Ref, Selector, A0, A1, A2, A3, A4, A5) :-
	qp_sendn(Ref, Selector, a(A0, A1, A2, A3, A4, A5), 1).
send(Ref, Selector, A0, A1, A2, A3, A4, A5, A6) :-
	qp_sendn(Ref, Selector, a(A0, A1, A2, A3, A4, A5, A6), 1).
send(Ref, Selector, A0, A1, A2, A3, A4, A5, A6, A7) :-
	qp_sendn(Ref, Selector, a(A0, A1, A2, A3, A4, A5, A6, A7), 1).
send(Ref, Selector, A0, A1, A2, A3, A4, A5, A6, A7, A8) :-
	qp_sendn(Ref, Selector, a(A0, A1, A2, A3, A4, A5, A6, A7, A8), 1).
send(Ref, Selector, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) :-
	qp_sendn(Ref, Selector, a(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9), 1).


		 /*******************************
		 *		GET		*
		 *******************************/

%	For the t(R), see comments with new/2 in this file!

get(Ref, Sel, R) :-
	qp_get(Ref, Sel, t(R), 1).
get(Ref, Sel, A0, R) :-
	qp_get(Ref, Sel, A0, t(R), 1).
get(Ref, Sel, A0, A1, R) :-
	qp_get(Ref, Sel, A0, A1, t(R), 1).
get(Ref, Sel, A0, A1, A2, R) :-
	qp_get(Ref, Sel, A0, A1, A2, t(R), 1).
get(Ref, Sel, A0, A1, A2, A3, R) :-
	qp_getn(Ref, Sel, a(A0, A1, A2, A3), t(R), 1).
get(Ref, Sel, A0, A1, A2, A3, A4, R) :-
	qp_getn(Ref, Sel, a(A0, A1, A2, A3, A4), t(R), 1).
get(Ref, Sel, A0, A1, A2, A3, A4, A5, R) :-
	qp_getn(Ref, Sel, a(A0, A1, A2, A3, A4, A5), t(R), 1).
get(Ref, Sel, A0, A1, A2, A3, A4, A5, A6, R) :-
	qp_getn(Ref, Sel, a(A0, A1, A2, A3, A4, A5, A6), t(R), 1).
get(Ref, Sel, A0, A1, A2, A3, A4, A5, A6, A7, R) :-
	qp_getn(Ref, Sel, a(A0, A1, A2, A3, A4, A5, A6, A7), t(R), 1).
get(Ref, Sel, A0, A1, A2, A3, A4, A5, A6, A7, A8, R) :-
	qp_getn(Ref, Sel, a(A0, A1, A2, A3, A4, A5, A6, A7, A8), t(R), 1).
get(Ref, Sel, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, R) :-
	qp_getn(Ref, Sel, a(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9), t(R), 1).


		 /*******************************
		 *	   OBJECT/[1,2]		*
		 *******************************/

object(Ref) :-
	qp_object(Ref, 1).

object(Ref, Term) :-
	qp_object(Ref, Term, 1).


		 /*******************************
		 *	   PREDICATE REF	*
		 *******************************/

pce_predicate_reference(Spec, PceRef) :-
	strip_module(Spec, Module, Head),
	(   var(Head)
	->  qp_predicate_reference(Head, PceRef, 1)
	;   qp_predicate_reference(Module:Head, PceRef, 1)
	).

strip_module(RT, M, T) :-
	strip_module(RT, T, M, user).

strip_module(Module:RT2, T, M, _) :-
	atom(Module), !,
	strip_module(RT2, T, M, Module).
strip_module(T, T, M, M).


		 /*******************************
		 *	       OPEN		*
		 *******************************/

pce_open(Object, Mode, Stream) :-
	qp_pce_open(Object, Mode, ErrNum, CStream),
	(   CStream =:= 0
	->  raise_exception(existence_error(pce_open(Object, Mode, Stream),
					    1, object, Object, errno(ErrNum)))
	;   stream_code(Stream, CStream)
	).


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

:- multifile
	user:message_hook/3,
	user:query_hook/6.
%:- dynamic   user:message_hook/3.

user:message_hook(execution_aborted,error,[['Execution aborted'-[]]]):-
	write('Aborting ...'),
	nl,
	pce_reset,
	fail.
user:message_hook(term_reading, silent, []) :-
	pce_redraw,
	fail.

%	This is called by the toplevel when prompting for more answers (;)

user:query_hook(toplevel, _, _, _, _, _) :-
	pce_redraw,
	fail.

% callbacks from XPCE call user:call/1 with the goal term to call
 
user:(:- extern(call(+term))).

		 /*******************************
		 *        INITIALISATION	*
		 *******************************/

:- initialization
	(   (prolog_flag(system_type, development))
	->  pce_host:pwversion(PwVersion),
	    '$Quintus: check_license'(prowindows, PwVersion)
	;   true  % no checking in runtimes
	).


add_input_callback :-
	get(@pce, window_system, windows), !.
add_input_callback :-
	predicate_property(qui:'QP_GetQuiAppContext'(_), _), !,
	call(qui:'QP_GetQuiAppContext'(XtAppContext)),
        pce_appcontext(XtAppContext, XtAppContext),
	send(@display, open),
	get(@display, connection_fd, FD),
	setup_input(FD, 0, 1, 0).
add_input_callback :-
	send(@display, open),
	get(@display, connection_fd, FD),
	setup_input(FD, 0, 0, 0).

:- dynamic pw_version_done/0.

prowindows_version :-
	(   pw_version_done
	->  true
	;   atom_chars(NL, [10]),
	    pce_host:pwversion(PwVersion),
	    List = [ 'ProWindows ', PwVersion,
		     ' (XPCE ', PceVersion, ') Interface',
		     NL,
		     'Copyright (C) 1998, ',
		     'SICS/University of Amsterdam. All Rights Reserved.'],
	    get(@pce, version, PceVersion),
	    concat_chars(List, VersionChars),
	    atom_chars(VersionAtom, VersionChars),
	    version(VersionAtom),
	    assert(pw_version_done)
	).

pce_initialise :-
%	format('Initializing ProWindows ...~n', []),
	(   absolute_file_name(pce(.),
			       [ file_type(directory),
				 access(read)
			       ],
			       PceHome)
	->  pce_init(PceHome),
	    initialise_objects,
	    prowindows_version,
	    add_input_callback
	;   fail
	).

:- extern(pce_initialise).

initialise_objects :-
	new(@class, var(class, class, @nil)).
