/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "dtd.h"
#include <stdlib.h>

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
