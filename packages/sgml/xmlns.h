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

#ifndef XMLNS_H_INCLUDED
#define XMLNS_H_INCLUDED

typedef struct _xmlns
{ dtd_symbol *name;			/* Prefix of the NS */
  dtd_symbol *url;			/* pointed-to URL */
  struct _xmlns *next;			/* next name */
} xmlns;

void		xmlns_free(sgml_environment *env);
xmlns*		xmlns_find(sgml_environment *env, dtd_symbol *ns);
void		update_xmlns(dtd_parser *p, dtd_element *e,
			     int natts, sgml_attribute *atts);
int		xmlns_resolve_attribute(dtd_parser *p, dtd_symbol *id,
					const ichar **local, const ichar **url);
int		xmlns_resolve_element(dtd_parser *p,
				      const ichar **local, const ichar **url);

#endif /*XMLNS_H_INCLUDED*/
