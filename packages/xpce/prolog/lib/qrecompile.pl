/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
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

