/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

:- module(test_unify, [test_unify/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog unifying

This module is a Unit test for Prolog unification oddities.  If basic
unification is wrong you won't get as far as running this test :-)

@author	Jan Wielemaker
*/

test_unify :-
	run_tests([ unify,
		    can_compare
		  ]).

:- begin_tests(unify).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
blam([]).
blam([L|L]) :- blam(L).

test(blam, X == [[[]], []]) :-
	blam(X),
	length(X, 2), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p(X,Y):-				% Bug#436
  U=U,
  V=V,
  findall(_C,
          q(X,Y,U,V),
          _Cs).

q(_,_,_,_).

test(unify_self, true) :-
	p(_,_).

unify_fv(X) :-
	(   X == a
	->  Y = _			% mapped to true, but must init Y
	;   Y = x
	),
	garbage_collect,		% verify consistency
	copy_term(Y,_).			% use and verify Y

test(unify_fv, true) :-
	unify_fv(a).

:- end_tests(unify).

:- begin_tests(can_compare).

v(_).

test(ground, true) :-
	?=(a,b).
test(ground, true) :-
	?=(a,a).
test(ground, fail) :-
	v(X),
	?=(a,X).

:- end_tests(can_compare).
