/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_host, [user_help/0]).

		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

property(prolog(sicstus)).		% this is SICStus
property(file_extensions([pl,ql])).	% Loadable file extensions

:- op(1199, fx, initialization).

		/********************************
		*        LOADING C-PART		*
		********************************/

foreign(prolog_pce_init, pce_init).
foreign_file('../../sicstus/sparc-sunos-4.1/interface.o', [prolog_pce_init]).
foreign_file('../../sparc-sunos-4.1/XPCE.a', []).

'$load_pce' :-
	load_foreign_files([ '../../sicstus/sparc-sunos-4.1/interface.o'
			   , '../../sparc-sunos-4.1/XPCE.a'
			   ],
			   [ '-lXt'
			   , '-lX11'
			   , '-lm'
			   ]),
	pce_init,
	prolog:'$set_meta_declaration'(send(+, :), pce_principal),
	prolog:'$set_meta_declaration'(send(+, :, +), pce_principal),
	prolog:'$set_meta_declaration'(send(+, :, +, +), pce_principal),
	prolog:'$set_meta_declaration'(send(+, :, +, +, +), pce_principal),
	prolog:'$set_meta_declaration'(get(+, :, -), pce_principal),
	prolog:'$set_meta_declaration'(get(+, :, ?, -), pce_principal),
	prolog:'$set_meta_declaration'(get(+, :, ?, ?, -), pce_principal),
	prolog:'$set_meta_declaration'(get(+, :, ?, ?, ?, -), pce_principal).

user_help :-
	use_module(library(pce_manual)),
	manpce.

:- initialization(pce_reinitialise).

pce_reinitialise :-
	pce_boot:pce_reinitialise,
	format('~nFor HELP, please invoke the predicate `manpce''.~n~n', []).
	
 
print_exception(Excp) :-
       format("Exception caught by hostSend/Get:~n", []),
       prolog:puncaught(Excp).

:- user:assert((term_expansion((:- initialization(Goal)), (:- (Goal))))).
