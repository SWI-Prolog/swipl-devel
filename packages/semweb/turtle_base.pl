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
	  [ turtle_name/1
	  ]).

/** <module> Basic RDF/Turtle helper predicates

This module provides a couple  of   time-critical  primitives to speedup
reading a writing the  RDF/Turtle   serialization.  The  definitions are
based on:

	http://www.w3.org/TeamSubmission/2008/SUBM-turtle-20080114/
*/

%%	turtle_name(+Atom:atom) is semidet.
%
%	True if Atom is a valid Turtle name.
%
%	@see xml_name/2.
