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

:- module(write,
	  [ write_term/3,
	    write_term/2,
%	    write_option/1,		(type)
	    write/2, write/1,
	    writeq/2, writeq/1,
	    write_canonical/2, write_canonical/1,
	    print/2, print/1, write_list1/1,
	    portray_clause/2, portray_clause/1,
	    numbervars/3, prettyvars/1,
	    printable_char/1
        ]).

:- use_module('../../listing').
:- use_module('../../error').
:- use_module('../../apply').

%%	write_list(+List) is det.
%
%	Writes a list to current output one element in each line.

write_list1(List) :-
	must_be(list, List),
	maplist(writeln, List).

%%	prettyvars(?Term)
%
%	Similar to numbervars(Term,0,_), except that singleton variables
%	in Term are unified with '$VAR'('_'), so that when the resulting
%	term is output with a  write   option  numbervars(true),  in the
%	place of singleton variables _  is   written.  This predicate is
%	used by portray_clause/2.

prettyvars(Term) :-
	numbervars(Term, 0, _, [singleton(true)]).

%%	printable_char(+Char) is semidet.
%
%	Char is the code of a character which can be printed. Not really
%	sure what this means. Mapped to code_type/2 using type =graph=.

printable_char(Char) :-
	code_type(Char, graph).

