/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(gensym,
	[ reset_gensym/0,
	  reset_gensym/1,
	  gensym/2
	]).

:- style_check(+dollar).		% lock these predicates

%	gensym(+Base, -Unique)
%
%	Generate <Base>0, <Base>1, etc atoms on each subsequent call.

gensym(Base, Atom) :-
	atom_concat($gs_, Base, Key), 
	flag(Key, Old, Old), 
	record_gensym(Key, Old),
	succ(Old, New), 
	flag(Key, _, New), 
	atom_concat(Base, New, Atom).

record_gensym(Key, 0) :- !,
	recordz($gensym, Key).
record_gensym(_, _).

%	reset_gensym/0
%
%	Reset all gensym counters.  Please beware this is dangerous: gensym
%	may be in use by other modules that do not expect their counter to
%	be reset!

reset_gensym :-
	recorded($gensym, Key, Ref),
	    erase(Ref),
	    flag(Key, _, 0),
	fail.
reset_gensym.

%	reset_gensym(+Base)
%
%	Reset a specific gensym counter.  Please beware this still is
%	dangerous as other code may use gensym with the same atom!

reset_gensym(Base) :-
	atom_concat($gs_, Base, Key), 
	flag(Key, _, 0).
