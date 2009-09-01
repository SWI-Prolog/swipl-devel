/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(turtle_base,
	  [				% Tests
	    turtle_name_start_char/1,	% +Integer
	    turtle_name/1,		% +Atom
					% Input
	    turtle_read_name/4,		% +C0, +Stream, -C, -Atom
	    turtle_read_string/4,	% +C0, +Stream, -C, -Atom
	    turtle_read_relative_uri/4,	% +C0, +Stream, -C, -Atom
					% Output
	    turtle_write_quoted_string/2, % +Stream, +Atom
	    turtle_write_uri/2		% +Stream, +Atom
	  ]).

/** <module> Basic RDF/Turtle helper predicates

This module provides a couple  of   time-critical  primitives to speedup
reading a writing the  RDF/Turtle   serialization.  The  definitions are
based on:

	http://www.w3.org/TeamSubmission/2008/SUBM-turtle-20080114/
*/

:- use_foreign_library(foreign(turtle)).

%%	turtle_name_start_char(+Code) is semidet.
%
%	True if Code is a valid character code to start a Turtle name.

%%	turtle_name(+Atom:atom) is semidet.
%
%	True if Atom is a valid Turtle name.
%
%	@see xml_name/2.

%%	turtle_read_name(+C0, +Stream, -C, -Atom) is semidet.
%
%	Read a Turtle name starting with C0 from Stream. If successfull,
%	C is unified with the first character after the name and Atom is
%	an atom representing the name read.

%%	turtle_read_string(+C0, +Stream, -C, -Atom) is semidet.
%
%	Read a Turtle quotes string  starting   with  C0 from Stream. If
%	successfull, C is unified with  the   first  character after the
%	string and Atom is an atom representing the text of the string.
%
%	@error syntax_error(Culprit)

%%	turtle_read_relative_uri(+C0, +Stream, -C, -Atom) is semidet.
%
%	Read a Turtle relative URI (<...>) into an atom and unify C with
%	the next character.
%
%	@error syntax_error(Culprit)


		 /*******************************
		 *	      WRITING		*
		 *******************************/

%%	turtle_write_quoted_string(+Out, +Value) is det.
%
%	Write a string as =|"..."|=


%%	turtle_write_uri(+Out, +Value) is det.
%
%	Write a URI as =|<...>|=


