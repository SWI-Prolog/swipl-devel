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

:- module(mime,
	  [ mime_parse/2		% +Data, -Mime
	  ]).

:- load_foreign_library(foreign(mime), install_mime).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines an interface to   the rfc2045 (MIME) parsing library
by Double Precision, Inc, part of the   maildrop system. This library is
distributed under the GPL and  therefore   all  code  using this library
should comply to the GPL.

Parsing MIME messages is accomplished  using   a  single predicate. This
predicate parses the input  and  returns   a  complex  term  holding the
various MIME message  parts.  The  mime   message  is  encoded  into the
following structure: 

	mime(Attributes, Data, SubMimeList)

Where Data is the (decoded) field data   returned as an atom, Attributes
is a property-list and SubMimeList is a  list of mime/3 terms reflecting
the sub-parts. Attributes contains the following members:

	# id(Atom)
	# description(Atom)
	# language(Atom)
	# md5(Atom)
	# type(Atom)
	# character_set(Atom)
	# transfer_encoding(Atom)
	# disposition(Atom)
	# filename(Atom)
	# name(Atom)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

