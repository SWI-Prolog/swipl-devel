/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(gensym,
	[ reset_gensym/0,
	  reset_gensym/1,		% +Base
	  gensym/2			% +Base, -Symbol
	]).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Generate unique symbols

The predicate gensym/2 is a  traditional   predicate  to generate unique
symbols.  It should be used with care.
*/

%%	gensym(+Base, -Unique)
%
%	Generate <Base>1, <Base>2, etc atoms   on  each subsequent call.
%	Note that there is nothing  that   prevents  other  parts of the
%	application to `invent'  the  same   identifier.  The  predicate
%	gensym/2 is thread-safe in the sense that two threads generating
%	identifiers from the same Base  will   never  generate  the same
%	identifier.
%
%	@see	uuid/1, term_hash/2, variant_sha1/2 may be used to
%		generate various unique or content-based identifiers
%		safely.

gensym(_Base, Atom) :-
	nonvar(Atom), !,
	throw(error(uninstantiation_error(Atom),
		    context(gensym/2, '2nd argument'))).
gensym(Base, Atom) :-
	atom_concat('$gs_', Base, Key),
	with_mutex('$gensym', increment_key(Key, N)),
	atom_concat(Base, N, Atom).

increment_key(Key, New) :-
	flag(Key, Old, Old),
	record_gensym(Key, Old),
	succ(Old, New),
	flag(Key, _, New).

record_gensym(Key, 0) :- !,
	recordz('$gensym', Key).
record_gensym(_, _).

%%	reset_gensym
%
%	Reset all gensym counters.  Please beware this is dangerous: gensym
%	may be in use by other modules that do not expect their counter to
%	be reset!

reset_gensym :-
	with_mutex('$gensym', do_reset_gensym).

do_reset_gensym :-
	(   recorded('$gensym', Key, Ref),
	    erase(Ref),
	    flag(Key, _, 0),
	    fail
	;   true
	).

%%	reset_gensym(+Base)
%
%	Reset a specific gensym counter.  Please beware this still is
%	dangerous as other code may use gensym with the same atom!

reset_gensym(Base) :-
	atom_concat('$gs_', Base, Key),
	with_mutex('$gensym', flag(Key, _, 0)).
