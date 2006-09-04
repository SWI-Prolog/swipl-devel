/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Machine independent saved-state format and object  format (should be the
same).

<XR>  ::= <atom>
	| <string>
	| <functor>
	| <procedure>
	| <integer>
	| <real>

File format:

    MAGIC
    VERSION INFORMATION
    BYTE ORDER
    OFFSET TABLE
	<atom-table offset>
	<functor-table offset>
	<source-file-table-offset>
    ATOM TABLE
	<size>
	<array of indices, <size> long>
	<text-strings, 0-terminated>
    FUNCTOR TABLE
	<size>
	<array of (atom,arity) tuples, <size> long>
    SOURCE IOSTREAM TABLE
        <size>
	{ <atom>			% path-name
	  <time>			% last-modification-stamp>
	}
    MODULE ID TABLE
	<size>
	{ <atom>			% module-name
	}
    MODULES
        <size>
        <module>
    RECORDS
    FLAGS
    FEATURES
    INITIALISATION GOALS

Lazy definition of predicates?

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

