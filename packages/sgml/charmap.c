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

#include <stdio.h>
#include <stdlib.h>
#include "dtd.h"

static void
char_range(dtd_charclass *map, int from, int to, int msk)
{ unsigned char *ca;
  int i;

  for(i=from, ca=map->class+i; i++<=to; )
    *ca++ |= msk;
}


dtd_charclass *
new_charclass()
{ dtd_charclass *map = sgml_calloc(1, sizeof(*map));
  unsigned char *ca = map->class;

  char_range(map, 'a', 'z', CH_LCLETTER);
  char_range(map, 'A', 'Z', CH_LCLETTER);
  char_range(map, '0', '9', CH_DIGIT);
  
  ca['.'] |= CH_CNM;
  ca['-'] |= CH_CNM;
  ca[183] |= CH_CNM;			/* XML */
  ca[':'] |= CH_CNMSTRT;		/* HTML and XML */
  ca['_'] |= CH_CNMSTRT;		/* HTML and XML */

  char_range(map, 192, 214, CH_CNMSTRT); /* XML ISO-LATIN-1 accented chars */
  char_range(map, 216, 246, CH_CNMSTRT);
  char_range(map, 248, 255, CH_CNMSTRT);

  ca['\t'] |= CH_WHITE;
  ca[' ']  |= CH_WHITE;
  ca['\r'] |= CH_RE;
  ca['\n'] |= CH_RS;

  return map;
}


dtd_charfunc *
new_charfunc()
{ dtd_charfunc *f = sgml_calloc(1, sizeof(*f));
  ichar *cf = f->func;

  cf[CF_STAGO]	= '<';
  cf[CF_STAGC]	= '>';
  cf[CF_ETAGO1]	= '<';
  cf[CF_ETAGO2]	= '/';
  cf[CF_VI]	= '=';
  cf[CF_NS]     = ':';
  cf[CF_LIT]	= '"';
  cf[CF_LITA]	= '\'';
  cf[CF_PERO]	= '%';
  cf[CF_ERO]	= '&';
  cf[CF_ERC]	= ';';
  cf[CF_MDO1]	= '<';
  cf[CF_MDO2]	= '!';
  cf[CF_MDC]	= '>';
  cf[CF_PRO1]	= '<';
  cf[CF_PRO2]	= '?';
  cf[CF_PRC]	= '>';
  cf[CF_GRPO]	= '(';
  cf[CF_GRPC]	= ')';
  cf[CF_SEQ]	= ',';
  cf[CF_AND]	= '&';
  cf[CF_OR]	= '|';
  cf[CF_OPT]	= '?';
  cf[CF_PLUS]	= '+';
  cf[CF_DSO]	= '[';
  cf[CF_DSC]	= ']';
  cf[CF_REP]	= '*';
  cf[CF_RS]	= '\n';
  cf[CF_RE]	= '\r';
  cf[CF_CMT]	= '-';

  return f;
}



dtd_charmap *
new_charmap()
{ dtd_charmap *map = sgml_malloc(sizeof(*map));
  int i;

  for(i=0 ; i<INPUT_CHARSET_SIZE; i++)
    map->map[i] = i;

  return map;
}
