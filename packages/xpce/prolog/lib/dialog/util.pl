/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(dia_util, []).
:- use_module(library(pce)).

:- pce_begin_class(dia_transient_hyper, hyper,
		   "Hyper-link to a transient window").

unlink_from(H) :->
	get(H, to, Transient),
	send(H, send_super, unlink_from),
	send(Transient, destroy).

:- pce_end_class.
