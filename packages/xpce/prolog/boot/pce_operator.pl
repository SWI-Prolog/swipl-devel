/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(pce_operator,
	[ push_operators/1
	, pop_operators/0
	]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Often, one  wants to define  operators to improve  the  readibility of
some very  specific code.  Operators in  Prolog are global objects and
changing operators changes syntax  and  possible semantics of existing
sources.   For this   reason   it is   desirable to  reset    operator
declarations after   the code that  needs  them   has been read.  This
module defines a  rather cruel  method to do  this.  In  the future it
might be better to  limit operator definitions  to  the file they  are
declated (like style_check/1).

Usage:

:- push_operators(
	[ op(900, fx, hello_world)
	, op(600, xf, *)
	]).
	
hello_world World :-
	....

:- pop_operators.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	operator_stack/1.

push_operators(New) :-
	undo_operators(New, Undo),
	set_operators(New),
	asserta(operator_stack(Undo)).

pop_operators :-
	retract(operator_stack(Undo)), !,
	set_operators(Undo).

set_operators([]).
set_operators([op(P,T,A)|R]) :-
	op(P, T, A),
	set_operators(R).

undo_operators([], []).
undo_operators([O0|T0], [U0|T]) :-
	undo_operator(O0, U0),
	undo_operators(T0, T).

undo_operator(op(_P, T, N), op(OP, OT, N)) :-
	current_op(OP, OT, N),
	same_op_type(T, OT), !.
undo_operator(op(P, T, [H|R]), [OH|OT]) :- !,
	undo_operator(op(P, T, H), OH),
	undo_operator(op(P, T, R), OT).
undo_operator(op(_, _, []), []) :- !.
undo_operator(op(_P, T, N), op(0, T, N)).
	
same_op_type(T, OT) :-
	op_type(T, Type),
	op_type(OT, Type).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(yfy, infix).
op_type(xf,  postfix).
op_type(yf,  postfix).
