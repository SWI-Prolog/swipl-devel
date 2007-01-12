/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

#include <SWI-Stream.h>			/* encoding */
#include <SWI-Prolog.h>
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include HAVE_MALLOC_H
#endif
#include "error.h"
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <wctype.h>
#include "dtd.h"

static atom_t ATOM_iso_latin_1;
static atom_t ATOM_utf8;
static atom_t ATOM_unicode;
static atom_t ATOM_ascii;

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
do_quote(term_t in, term_t quoted, char **map, int maxchr)
{ char *inA = NULL;
  wchar_t *inW = NULL;
  size_t len;
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
      } else if ( c > maxchr )
      { char buf[10];

	sprintf(buf, "&#%d;", c);
	if ( !add_str_buf(&buffer, buf) )
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
      } else if ( c > maxchr )
      { char buf[10];

	sprintf(buf, "&#%d;", c);
	if ( !add_str_bufW(&buffer, buf) )
	  return FALSE;
	
	changes++;
      }else
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


static int
get_max_chr(term_t t, int *maxchr)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_iso_latin_1 )
      *maxchr = 0xff;
    else if ( a == ATOM_utf8 )
      *maxchr = 0x7ffffff;
    else if ( a == ATOM_unicode )
      *maxchr = 0xffff;
    else if ( a == ATOM_ascii )
      *maxchr = 0x7f;
    else
      return sgml2pl_error(ERR_DOMAIN, "encoding", t);

    return TRUE;
  }

  return sgml2pl_error(ERR_TYPE, "atom", t);
}


static foreign_t
xml_quote_attribute(term_t in, term_t out, term_t encoding)
{ static char **map;
  int maxchr;

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

  if ( !get_max_chr(encoding, &maxchr) )
    return FALSE;

  return do_quote(in, out, map, maxchr);
}


static foreign_t
xml_quote_cdata(term_t in, term_t out, term_t encoding)
{ static char **map;
  int maxchr;

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

  if ( !get_max_chr(encoding, &maxchr) )
    return FALSE;

  return do_quote(in, out, map, maxchr);
}


static foreign_t
xml_name(term_t in, term_t encoding)
{ char *ins;
  wchar_t *inW;
  size_t len;
  static dtd_charclass *map;
  unsigned int i;
  int maxchr;

  if ( !get_max_chr(encoding, &maxchr) )
    return FALSE;

  if ( !map )
    map = new_charclass();

  if ( PL_get_nchars(in, &len, &ins, CVT_ATOMIC) )
  { int c;

    if ( len == 0 )
      return FALSE;

    c = ins[0] & 0xff;
    if ( c > maxchr )
      return FALSE;
    
    if ( !(map->class[c] & CH_NMSTART) )
      return FALSE;
    for(i=1; i<len; i++)
    { c = ins[i] & 0xff;

      if ( c > maxchr || !(map->class[c] & CH_NAME) )
	return FALSE;
    }

    return TRUE;
  }
  if ( PL_get_wchars(in, &len, &inW, CVT_ATOMIC) )
  { if ( len == 0 )
      return FALSE;
  
    if ( inW[0] > maxchr )
      return FALSE;

    if ( inW[0] <= 0xff &&
	 !(map->class[inW[0]] & CH_NMSTART) )
      return FALSE;
    if ( inW[0] > 0xff && !iswalpha(inW[0]) )
      return FALSE;
	 
    for(i=1; i<len; i++)
    { int c = inW[i];

      if ( c <= 0xff && !(map->class[c] & CH_NAME) )
	return FALSE;
      if ( c > 0xff && !iswalnum((wint_t)c) )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}




install_t
install_xml_quote()
{ ATOM_iso_latin_1 = PL_new_atom("iso_latin_1");
  ATOM_utf8        = PL_new_atom("utf8");
  ATOM_unicode     = PL_new_atom("unicode");
  ATOM_ascii       = PL_new_atom("ascii");

  PL_register_foreign("xml_quote_attribute", 3, xml_quote_attribute, 0);
  PL_register_foreign("xml_quote_cdata",     3, xml_quote_cdata,     0);
  PL_register_foreign("xml_name",            2, xml_name,            0);
}
