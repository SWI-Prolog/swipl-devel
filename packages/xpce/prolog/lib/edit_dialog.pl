/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(dia_main,
	  [ dialog/0			% start dialog editor
	  ]).
:- use_module(library(pce)).

:- pce_autoload(dia_editor, library('dialog/dialog')).

dialog :-
	send(new(dia_editor), open).
	
