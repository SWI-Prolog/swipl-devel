/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#ifndef _ORDER_H_INCLUDED
#define _ORDER_H_INCLUDED

#define ORD_MAGIC	 372132363	/* just a number */

#define ORD_IGNORE	 3		/* ignore these */
#define ORD_BREAK	 2		/* break a word */
#define ORD_TAG	 	 1		/* foo (a bar) */
#define ORD_END		 0		/* end of word */

#define ORD(ot, c)	(ot->ords[(int)(c)&0xff])

typedef struct
{ int		magic;			/* ORD_MAGIC */
  atom_t	name;			/* name of the table */
  unsigned char ords[256];		/* mapping of the table */
} ordtable, *OrdTable;

OrdTable	findOrdTable(atom_t name);
int             compare_strings(const char *s1, const char *s2,
				int n, OrdTable ot);
install_t       install_order(void);

#endif /*_ORDER_H_INCLUDED*/
