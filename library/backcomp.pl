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

:- module(backward_compatibility,
	  [ '$arch'/2,
	    '$version'/1,
	    '$home'/1,
	    '$argv'/1,
	    displayq/1,
	    displayq/2,
	    concat/3,
	    read_variables/2,
	    read_variables/3,
	    feature/2,
	    set_feature/2,
	    substring/4,
	    flush/0,
	    write_ln/1
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library defines predicates that used to   exist in older version of
SWI-Prolog, but are considered obsolete as there functionality is neatly
covered by new features. Most often, these constructs are superceeded by
ISO-standard compliant predicates.

Please   also   note   the    existence     of    library(quintus)   and
library(edinburgh) for more compatibility predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'$arch'(Arch, unknown) :-
	current_prolog_flag(arch, Arch).
'$version'(Version) :-
	current_prolog_flag(version, Version).
'$home'(Home) :-
	current_prolog_flag(home, Home).
'$argv'(Argv) :-
	current_prolog_flag(argv, Argv).

%	or write_canonical/[1,2]

displayq(Term) :-
	write_term(Term, [ignore_ops(true),quoted(true)]).
displayq(Stream, Term) :-
	write_term(Stream, Term, [ignore_ops(true),quoted(true)]).

%	concat/3 is superseeded by ISO atom_concat/3

concat(A, B, C) :-
	atom_concat(A, B, C).

%	Replaced by ISO read_term/[2,3].

read_variables(Term, Vars) :-
	read_term(Term, [variable_names(Vars)]).

read_variables(Stream, Term, Vars) :-
	read_term(Stream, Term, [variable_names(Vars)]).

%	feature(?Key, ?Value)
%	set_feature(+Key, @Term)
%
%	Replaced by ISO current_prolog_flag/2 and set_prolog_flag/2.

feature(Key, Value) :-
	current_prolog_flag(Key, Value).

set_feature(Key, Value) :-
	set_prolog_flag(Key, Value).

%	substring(+String, +Offset, +Length, -Sub)

substring(String, Offset, Length, Sub) :-
	Offset0 is Offset - 1,
	sub_string(String, Offset0, Length, _After, Sub).

%	flush/0

flush :-
	flush_output.

%	write_ln(X) was renamed to writeln(X) for better compatibility

write_ln(X) :-
	write(X), nl.
