/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

#include <SWI-Prolog.h>
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include HAVE_MALLOC_H
#endif
#include "error.h"
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include "dtd.h"

#define CHARSET 256

static foreign_t
do_quote(term_t in, term_t quoted, char **map)
{ char *ins;
  unsigned len;
  unsigned  char *s;
  char outbuf[1024];
  char *out = outbuf;
  int outlen = sizeof(outbuf);
  int o = 0;
  int changes = 0;

  if ( !PL_get_nchars(in, &len, &ins, CVT_ATOMIC) )
    return sgml2pl_error(ERR_TYPE, "atom", in);
  if ( len == 0 )
    return PL_unify(in, quoted);

  for(s = (unsigned char*)ins ; len-- > 0; s++ )
  { int c = *s;
    
    if ( map[c] )
    { int l = strlen(map[c]);
      if ( o+l >= outlen )
      { outlen *= 2;
	  
	if ( out == outbuf )
	{ out = malloc(outlen);
	  memcpy(out, outbuf, sizeof(outbuf));
	} else
	{ out = realloc(out, outlen);
	}
      }
      memcpy(&out[o], map[c], l);
      o += l;
      changes++;
    } else
    { if ( o >= outlen-1 )
      { outlen *= 2;
	  
	if ( out == outbuf )
	{ out = malloc(outlen);
	  memcpy(out, outbuf, sizeof(outbuf));
	} else
	{ out = realloc(out, outlen);
	}
      }
      out[o++] = c;
    }
  }
  
  if ( changes > 0 )
    return PL_unify_atom_nchars(quoted, o, out);
  else
    return PL_unify(in, quoted);
}


static foreign_t
xml_quote_attribute(term_t in, term_t out)
{ static char **map;

  if ( !map )
  { int i;

    if ( !(map = malloc(CHARSET*sizeof(char*))) )
      return sgml2pl_error(ERR_ERRNO, errno);

    for(i=0; i<CHARSET; i++)
      map[i] = NULL;

    map['<']  = "&lt;";
    map['>']  = "&gt;";
    map['&']  = "&amp;";
    map['\''] = "&apos;";
    map['"']  = "&quot;";
  }

  return do_quote(in, out, map);
}


static foreign_t
xml_quote_cdata(term_t in, term_t out)
{ static char **map;

  if ( !map )
  { int i;

    if ( !(map = malloc(CHARSET*sizeof(char*))) )
      return sgml2pl_error(ERR_ERRNO, errno);

    for(i=0; i<CHARSET; i++)
      map[i] = NULL;

    map['<']  = "&lt;";
    map['>']  = "&gt;";
    map['&']  = "&amp;";
  }

  return do_quote(in, out, map);
}


static foreign_t
xml_name(term_t in)
{ char *ins;
  unsigned len;
  static dtd_charclass *map;
  int i;

  if ( !map )
    map = new_charclass();

  if ( !PL_get_nchars(in, &len, &ins, CVT_ATOMIC) )
    return FALSE;
  if ( len == 0 )
    return FALSE;
  
  if ( !(map->class[ins[0] & 0xff] & CH_NMSTART) )
    return FALSE;
  for(i=1; i<len; i++)
  { if ( !(map->class[ins[i] & 0xff] & CH_NAME) )
      return FALSE;
  }

  return TRUE;
}




install_t
install_xml_quote()
{ PL_register_foreign("xml_quote_attribute", 2, xml_quote_attribute, 0);
  PL_register_foreign("xml_quote_cdata",     2, xml_quote_cdata,     0);
  PL_register_foreign("xml_name",            1, xml_name,            0);
}
