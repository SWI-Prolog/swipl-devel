/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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

