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

:- module(pce_mkcommon,
	  [ mkcommon/1
	  ]).
:- require([ forall/2
	   , member/2
	   , memberchk/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Used to make common.pl; the file holding all predicates available in all
supported Prolog platforms.  To generate this   file, start each of your
Prolog environments and do:

	?- tell('myprolog.pl'),
	   predicate_property(X, built_in),
	   write_canonical(X), put(0'.), nl,
	   fail ; told.

Next, load this package into Prolog and type:

	?- tell('common.pl'), mkcommon(['quintus.pl', 'sicstus.pl'], told.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	common_term/1.

mkcommon([First|Files] :-
	retractall(common_term(_)),
	read_terms(Files, QpTerms),
	forall(member(T, QpTerms), assert(common_term(T))),
	common(Files),
	setof(T, common_term(T), Terms),
	forall(member(T, Terms),
	       (write_canonical(built_in(T)), put(.), nl)).

common([]).
common([F|T]) :-
	read_terms(F, Terms),
	findall(T2, (common_term(T2), \+ memberchk(T2, Terms)), Del),
	forall(member(D, Del), retract(common_term(D))),
	common(T).

read_terms(F, Terms) :-
	seeing(Old), see(F),
	read_terms(Terms),
	seen, see(Old).

read_terms([H|T]) :-
	read(Raw),
	Raw \== end_of_file, !,
	(   Raw = built_in(H)
	->  true
	;   H = Raw
	),
	read_terms(T).
read_terms([]).
	
	    
	
