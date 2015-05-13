/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_install,
	  [ qcompile_libraries/0
	  ]).
:- use_module(library(make)).

/** <module> Installation support predicates

This module provides helper predicates for  the (Windows) installer. The
entry point is called from src/win32/installer.pl.nsi.
*/

%%	qcompile_libraries
%
%	Quick-load compilation of the Prolog libraries.

qcompile_libraries :-
	make,				% update library index
	qcompile_xpce.

qcompile_xpce :-			% no XPCE around
	\+ absolute_file_name(swi(xpce),
			      [ access(exist),
				file_type(directory),
				file_errors(fail)
			      ], _), !,
	print_message(informational, qcompile(no(xpce))).
qcompile_xpce :-
	qcompile_libs.


		 /*******************************
		 *	 PRECOMPILED PARTS	*
		 *******************************/

qmodule(pce, library(pce)).
qmodule(lib, library(pce_manual)).
qmodule(lib, library(pcedraw)).
qmodule(lib, library('emacs/emacs')).
qmodule(lib, library('dialog/dialog')).
qmodule(lib, library('trace/trace')).
qmodule(lib, library('cql/cql')).

qcompile_libs :-
	forall(qmodule(_Type, Module),
	       (   exists_source(Module)
	       ->  print_message(informational, qcompile(Module)),
		   qcompile(Module)
	       ;   print_message(informational, qcompile(no(Module)))
	       )).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(qcompile(no(What))) -->
	[ 'Cannot find ~w'-[What] ].
prolog:message(qcompile(library(Lib))) -->
	[ nl, '~*c'-[64, 0'*], nl ],
	[ 'Qcompile library ~q'-[Lib], nl ],
	[ '~*c'-[64, 0'*] ].
