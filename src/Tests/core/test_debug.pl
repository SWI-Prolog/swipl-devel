/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(test_debug, [test_debug/0]).
:- use_module(library(plunit)).
:- use_module(library(edinburgh)).	% debug/0 and friends.

/** <module> Test debugging predicates

This module is a Unit test for Prolog debugging features.

@author	Jan Wielemaker
*/

test_debug :-
	run_tests([ prolog_frame_attribute
		  ]).

:- begin_tests(prolog_frame_attribute).

test(none, fail) :-
	prolog_frame_attribute(none, predicate_indicator, _PI).
test(foreign_choice, true) :-
	debug, nodebug, !.		% check discardFrame()

:- end_tests(prolog_frame_attribute).
