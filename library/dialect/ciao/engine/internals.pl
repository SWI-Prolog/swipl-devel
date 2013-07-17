/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam

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

:- module(ciao_internals,
	  [ module_concat/3,	% +Module, :Goal, -NewGoal
	    '$setarg'/4		% +Arg, +Term, +Value, +Mode
	  ]).

:- meta_predicate
	module_concat(+, :, -).


%%	module_concat(+Module, +Goal0, -Goal)
%
%	Not clear what this should do.   This  makes library(tcltk) work
%	...

module_concat(Module, Goal, Module:Plain) :-
	strip_module(Goal, _, Plain).

'$setarg'(Arg, Term, Value, true) :- nb_setarg(Arg, Term, Value).
'$setarg'(Arg, Term, Value, on)   :- setarg(Arg, Term, Value).
