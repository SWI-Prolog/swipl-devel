/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
