/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(pce_host,
	  [ 
	  ]).
:- require([ absolute_file_name/3
	   ]).

		 /*******************************
		 *     COMPATIBILITY STUFF	*
		 *******************************/

:- multifile
	user:term_expansion/2.
:- dynamic
	user:term_expansion/2.

:- op(1150, fx, initialization).
%user:term_expansion((:- initialization(G)), (:- G)).

%	property(+Property)
%
%	Used by pce_ifhostproperty/[2,3] to determine properties of
%	the host Prolog system.

property(prolog(sicstus)).

		 /*******************************
		 *	       HOME		*
		 *******************************/

:- dynamic
	pce_home_/1.

pce_home(PceHome) :-
	pce_home_(PceHome), !.
pce_home(PceHome) :-
	find_pce_home(PceHome),
	asserta(pce_home_(PceHome)).

find_pce_home(PceHome) :-
	absolute_file_name(pce('.'),
			   [ file_type(directory),
			     file_errors(fail),
			     access(exists)
			   ], PceHome), !.
find_pce_home(PceHome) :-
	absolute_file_name('$XPCEHOME',
			   [ file_type(directory),
			     access(exists)
			   ], PceHome), !.		   


		 /*******************************
		 *	  FOREIGN STUFF		*
		 *******************************/

foreign(pl_send,       send(+term, +term, [-integer])).
foreign(pl_get,	       get(+term, +term, +term, [-integer])).
foreign(pl_send_class, send_class(+term, +term, +term, [-integer])).
foreign(pl_get_class,  get_class(+term, +term, +term, +term, [-integer])).
foreign(pl_new,	       new(+term, +term, [-integer])).
foreign(pl_object1,    object(+term, [-integer])).
foreign(pl_object2,    object(+term, +term, [-integer])).
foreign(pl_pce_method_implementation,
		       pce_method_implementation(+term, +term, [-integer])).
foreign(pl_pce_open,   pce_open(+term, +term, +term, [-integer])).
foreign(pl_pce_init,   pce_init(+term, [-integer])).
foreign('PL_call_abort_handlers', run_foreign_abort_hooks).

foreign_resource(pce,
		 [ pl_send,
		   pl_get,
		   pl_send_class,
		   pl_get_class,
		   pl_object1,
		   pl_object2,
		   pl_new,
		   pl_pce_method_implementation,
		   pl_pce_open,
		   'PL_call_abort_handlers',
		   init(init_pce),
		   deinit(deinit_pce)
		 ]).
:- load_foreign_resource(pce).

		 /*******************************
		 *	      HOOKS		*
		 *******************************/

:- multifile
	user:portray_message/2.

user:portray_message(informational, abort(_)) :-
	run_foreign_abort_hooks,
	fail.				% give others the opportunity

