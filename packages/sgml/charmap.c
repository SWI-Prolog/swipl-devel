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
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

dtd_charclass *
new_charclass()
{ dtd_charclass *map = calloc(1, sizeof(*map));
  unsigned char *ca = map->class;
  int i;

  for(i='a' ; i<='z'; i++)
    ca[i] |= CH_LCLETTER;
  for(i='A' ; i<='Z'; i++)
    ca[i] |= CH_UCLETTER;
  for(i='0' ; i<='9'; i++)
    ca[i] |= CH_DIGIT;
  
  ca['.'] |= CH_LCNMSTRT;
  ca['-'] |= CH_LCNMSTRT;

  ca['\t'] |= CH_BLANK;
  ca[' ']  |= CH_BLANK;
  ca['\r'] |= CH_BLANK;
  ca['\n'] |= CH_BLANK;

  return map;
}


dtd_charfunc *
new_charfunc()
{ dtd_charfunc *f = calloc(1, sizeof(*f));
  ichar *cf = f->func;

  cf[CF_STAGO]	= '<';
  cf[CF_STAGC]	= '>';
  cf[CF_ETAGO1]	= '<';
  cf[CF_ETAGO2]	= '/';
  cf[CF_VI]	= '=';
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
{ dtd_charmap *map = malloc(sizeof(*map));
  int i;

  for(i=0 ; i<INPUT_CHARSET_SIZE; i++)
    map->map[i] = i;

  return map;
}
