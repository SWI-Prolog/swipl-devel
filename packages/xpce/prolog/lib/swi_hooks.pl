/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_swi_hooks, []).

:- multifile
	prolog:debug_control_hook/1,
	prolog:help_hook/1.

       		 /*******************************
		 *	    DEBUG HOOKS		*
		 *******************************/

prolog:debug_control_hook(spy(Method)) :-
	call(spypce(Method)).
prolog:debug_control_hook(nospy(Method)) :-
	call(nospypce(Method)).


		 /*******************************
		 *	     HELP HOOK		*
		 *******************************/

prolog:help_hook(help) :- !,
	call(prolog_help).
prolog:help_hook(apropos(What)) :- !,
	call(prolog_apropos(What)).
prolog:help_hook(help(What)) :- !,
	call((   pce_to_method(What, Method)
	     ->  manpce(Method)
	     ;   prolog_help(What)
	     )).
