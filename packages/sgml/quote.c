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

typedef struct charbuf
{ char     buffer[1024];
  char    *bufp;
  char    *end;
  int      size;
} charbuf;


static void
init_buf(charbuf *b)
{ b->bufp = b->end = b->buffer;
  b->size = sizeof(b->buffer);
}


static void
free_buf(charbuf *b)
{ if ( b->bufp != b->buffer )
    free(b->bufp);
}


static int
room_buf(charbuf *b, int room)
{ int used = b->end - b->bufp;

  if ( room + used > b->size )
  { if ( b->bufp == b->buffer )
    { b->size = sizeof(b->buffer)*2;
      if ( !(b->bufp = malloc(b->size)) )
	return sgml2pl_error(ERR_ERRNO);

      memcpy(b->bufp, b->buffer, used);
    } else
    { char *ptr;

      b->size *= 2;
      if ( !(ptr = realloc(b->bufp, b->size)) )
	return sgml2pl_error(ERR_ERRNO);
      b->bufp = ptr;
    }
    b->end = b->bufp + used;
  }

  return TRUE;    
}


static unsigned int
used_buf(const charbuf *b)
{ return b->end - b->bufp;
}


static int
add_char_buf(charbuf *b, int chr)
{ if ( room_buf(b, 1) )
  { *b->end++ = chr;

    return TRUE;
  }

  return FALSE;
}


static int
add_char_bufW(charbuf *b, int chr)
{ if ( room_buf(b, sizeof(wchar_t)) )
  { wchar_t *p = (wchar_t*)b->end;

    *p++ = chr;
    b->end = (char *)p;

    return TRUE;
  }

  return FALSE;
}


static int
add_str_buf(charbuf *b, const char *s)
{ int len = strlen(s);

  if ( room_buf(b, len+1) )
  { memcpy(b->end, s, len+1);
    b->end += len;

    return TRUE;
  }

  return FALSE;
}


static int
add_str_bufW(charbuf *b, const char *s)
{ int len = strlen(s);

  if ( room_buf(b, len*sizeof(wchar_t)) )
  { wchar_t *p = (wchar_t*)b->end;

    while(*s)
      *p++ = *s++;
    b->end = (char *)p;

    return TRUE;
  }

  return FALSE;
}



static foreign_t
do_quote(term_t in, term_t quoted, char **map)
{ char *inA = NULL;
  wchar_t *inW = NULL;
  unsigned len;
  const unsigned  char *s;
  charbuf buffer;
  int changes = 0;
  int rc;

  if ( !PL_get_nchars(in, &len, &inA, CVT_ATOMIC) &&
       !PL_get_wchars(in, &len, &inW, CVT_ATOMIC) )
    return sgml2pl_error(ERR_TYPE, "atom", in);
  if ( len == 0 )
    return PL_unify(in, quoted);

  init_buf(&buffer);

  if ( inA )
  { for(s = (unsigned char*)inA ; len-- > 0; s++ )
    { int c = *s;
      
      if ( map[c] )
      { if ( !add_str_buf(&buffer, map[c]) )
	  return FALSE;
	
	changes++;
      } else
      { add_char_buf(&buffer, c);
      }
    }

    if ( changes > 0 )
      rc = PL_unify_atom_nchars(quoted, used_buf(&buffer), buffer.bufp);
    else
      rc = PL_unify(in, quoted);
  } else
  { for( ; len-- > 0; inW++ )
    { int c = *inW;
      
      if ( c <= 0xff && map[c] )
      { if ( !add_str_bufW(&buffer, map[c]) )
	  return FALSE;
	
	changes++;
      } else
      { add_char_bufW(&buffer, c);
      }
    }
	 
    if ( changes > 0 )
      rc = PL_unify_wchars(quoted, PL_ATOM,
			   used_buf(&buffer)/sizeof(wchar_t),
			   (wchar_t*)buffer.bufp);
    else
      rc = PL_unify(in, quoted);
  }
  
  free_buf(&buffer);

  return rc;
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
  unsigned int i;

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
