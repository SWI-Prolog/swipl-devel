/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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
