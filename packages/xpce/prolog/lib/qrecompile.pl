/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_qrecompile,
	  [ qcompile_pce/0
	  ]).

qmodule(library(pce)).
qmodule(library(pce_manual)).
qmodule(library(pcedraw)).
qmodule(library('emacs/emacs')).
qmodule(library('dialog/dialog')).

qcompile_pce :-
	format('Checking library-index~n'),
	make,
	format('Recompiling modules~n'),
	qmodule(Module),
	format('~*c~n', [64, 0'*]),
	format('* Qcompile module ~w~n', [Module]),
	format('~*c~n', [64, 0'*]),
	once(qcompile(Module)),
	fail.
qcompile_pce.

