/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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


#include <h/kernel.h>
#include <h/utf8.h>

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These  functions  translate  CharArray  (String,  Name)  into  a  format
suitable to drive operating- or windowsystem   calls,  such as accessing
filenames, window titles, etc.

Both UTF-8 and locale-defined multibyte strings are not designed to deal
with embedded 0-bytes and APIs   generally  accept 0-terminated strings.
Only for wide-character arrays we work with sizes.

	* MB
	CTYPE Locale defined translation
	
	* UTF-8
	Well known UTF-8 encoding of UNICODE

	* WC
	wchar_t representation of UNICODE

The returned strings of this library are   stored in a ring of RING_SIZE
fields.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	      RING		*
		 *******************************/

#define RING_SIZE 16

typedef struct rcell
{ char 		*data;			/* actual data */
  char		*bufp;			/* pointer in buffer */
  char		*limitp;		/* pointer to end */
  size_t	allocated;		/* bytes allocated */
} rcell;

static rcell ring[RING_SIZE];
static int   ring_index;

static rcell *
find_ring()
{ rcell *c = &ring[ring_index++];

  if ( ring_index == RING_SIZE )
    ring_index = 0;

  if ( c->allocated == 0 )
  { c->allocated = 256;
    c->data = pceMalloc(c->allocated);
  } else if ( c->allocated >= 4096 )
  { c->allocated = 256;
    free(c->data);
    c->data = pceMalloc(c->allocated);
  }
  c->bufp   = c->data;
  c->limitp = &c->data[c->allocated];

  return c;
}


static void
roomBuffer(rcell *c, size_t room)
{ while ( c->bufp + room > c->limitp )
  { size_t size = c->bufp - c->data;

    c->allocated *= 2;
    c->data   = pceRealloc(c->data, c->allocated);
    c->limitp = &c->data[c->allocated];
    c->bufp   = &c->data[size];
  }
}


static void
addByte(rcell *c, int byte)
{ roomBuffer(c, 1);

  *c->bufp++ = byte;
}


		 /*******************************
		 *	  CHARARRAY --> 	*
		 *******************************/

typedef const unsigned char cuchar;
typedef const wchar_t       cwchar;

static char *
stringToUTF8(String str)
{ rcell *out;

  if ( isstrA(str) )
  { cuchar *s = (cuchar*) str->s_textA;
    cuchar *e = &s[str->size];

    for( ; s<e; s++ )			/* do we need conversion */
    { if ( *s & 0x80 )
	break;
    }
    if ( s == e )
      return str->s_textA;		/* no */

    out = find_ring();
    for( ; s<e; s++ )
    { roomBuffer(out, 2);		/* max bytes per UTF-8 < 256 */

      out->bufp = utf8_put_char(out->bufp, *s);
    } 
  } else
  { cwchar *s = str->s_textW;
    cwchar *e = &s[str->size];

    out = find_ring();
    for( ; s<e; s++ )
    { roomBuffer(out, 6);		/* max bytes per UTF-8 */

      out->bufp = utf8_put_char(out->bufp, *s);
    } 
  }

  addByte(out, 0);

  return out->data;
}


static char *
stringToMB(String str)
{ rcell *out;
  mbstate_t mbs;
  char b[MB_LEN_MAX];
  int rc;

  memset(&mbs, 0, sizeof(mbs));

  if ( isstrA(str) )
  { cuchar *s = (cuchar*) str->s_textA;
    cuchar *e = &s[str->size];

    for( ; s<e; s++ )			/* do we need conversion? */
    { if ( (rc=wcrtomb(b, *s, &mbs)) == 1 && b[0] == *s )
	continue;
      if ( rc == -1 )
	return NULL;			/* cannot convert */
    }
    if ( s == e )
      return str->s_textA;		/* no */

    memset(&mbs, 0, sizeof(mbs));
    out = find_ring();
    for( ; s <= e; s++ )		/* <=: also 0-byte! */
    { roomBuffer(out, MB_LEN_MAX);

      if ( (rc=wcrtomb(out->bufp, *s, &mbs)) < 0 )
	return NULL;
      out->bufp += rc;
    }
  } else
  { cwchar *s = str->s_textW;
    cwchar *e = &s[str->size];

    out = find_ring();
    for( ; s<e; s++ )
    { roomBuffer(out, MB_LEN_MAX);

      if ( (rc=wcrtomb(out->bufp, *s, &mbs)) < 0 )
	return NULL;
      out->bufp += rc;
    } 
  }

  roomBuffer(out, MB_LEN_MAX+1);	/* add restore state + 0-byte */
  wcrtomb(out->bufp, 0, &mbs);

  return out->data;
}


wchar_t *
charArrayToWC(CharArray ca, size_t *len)
{ String str = &ca->data;

  if ( len )
    *len = str->size;

  if ( isstrA(str) )
  { rcell *out = find_ring();
    cuchar *s = (cuchar*) str->s_textA;
    cuchar *e = &s[str->size];
    wchar_t *o;

    roomBuffer(out, str->size*sizeof(wchar_t));

    for(o=(wchar_t*)out->data ; s<e; )
    { *o++ = *s++;
    }
    *o = 0;

    return (wchar_t *)out->data;
  } else
    return str->s_textW;
}


char *
charArrayToUTF8(CharArray ca)
{ return stringToUTF8(&ca->data);
}


char *
charArrayToMB(CharArray ca)
{ return stringToMB(&ca->data);
}


		 /*******************************
		 *	    <-- NAME	  	*
		 *******************************/

Name
UTF8ToName(const char *utf8)
{ cuchar *in;
  cuchar *e;
  int len;
  int wide;

  for(in=utf8; *in; in++)
  { if ( (*in)&0x80 )
      break;
  }

  if ( *in == EOS )			/* simple ASCII string */
    return CtoName(utf8);

  e = in + strlen(in);
  for(in=utf8, len=0, wide=FALSE; in < e; )
  { int chr;

    in = utf8_get_char(in, &chr);
    if ( chr > 0xff )
      wide = TRUE;
    len++;
  }

  if ( wide )
  { wchar_t *ws, *o;
    int mlcd;
    string s;
    Name nm;

    if ( len < 1024 )
    { ws = alloca((len+1)*sizeof(wchar_t));
      mlcd = FALSE;
    } else
    { ws = pceMalloc((len+1)*sizeof(wchar_t));
      mlcd = TRUE;
    }

    for(in=utf8, o=ws; in < e; )
    { int chr;

      in = utf8_get_char(in, &chr);
      *o++ = chr;
    }

    str_set_n_wchar(&s, len, ws);
    nm = StringToName(&s);

    if ( mlcd )
      pceFree(ws);

    return nm;
  } else
  { char *as, *o;
    int mlcd;
    string s;
    Name nm;

    if ( len < 1024 )
    { as = alloca((len+1));
      mlcd = FALSE;
    } else
    { as = pceMalloc((len+1));
      mlcd = TRUE;
    }

    for(in=utf8, o=as; in < e; )
    { int chr;

      in = utf8_get_char(in, &chr);
      *o++ = (char)chr;
    }

    str_set_n_ascii(&s, len, as);
    nm = StringToName(&s);

    if ( mlcd )
      pceFree(as);

    return nm;
  }      
}


Name
MBToName(const char *mb)
{ size_t len;
  mbstate_t mbs;
  const char *in = mb;

  memset(&mbs, 0, sizeof(mbs));
  if ( (len = mbsrtowcs(NULL, &in, 0, &mbs)) >= 0 )
  { string s;
    wchar_t *ws;
    int mlcd;
    Name nm;

    if ( len < 1024 )
    { ws = alloca((len+1)*sizeof(wchar_t));
      mlcd = FALSE;
    } else
    { ws = pceMalloc((len+1)*sizeof(wchar_t));
      mlcd = TRUE;
    }

    memset(&mbs, 0, sizeof(mbs));
    in = mb;
    mbsrtowcs(ws, &in, len+1, &mbs);
    str_set_n_wchar(&s, len, ws);
    nm = StringToName(&s);

    if ( mlcd )
      pceFree(ws);

    return nm;
  }

  return NULL;
}


Name
WCToName(const wchar_t *wc, size_t len)
{ if ( wc )
  { string s;

    str_set_n_wchar(&s, len, (wchar_t *)wc);

    return StringToName(&s);
  }

  return NULL;
}


		 /*******************************
		 *	     FILE-NAMES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Turn  an  OS  filename  into  an  XPCE  name.  With  XOS,  the  filename
representation is always UTF-8
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Name
FNToName(const char *name)
{
#ifdef XOS
  return UTF8ToName(name);
#else
  return MBToName(name);
#endif
}


char *
charArrayToFN(CharArray ca)
{
#ifdef XOS
   return charArrayToUTF8(ca);
#else
   return charArrayToMB(ca);
#endif
}


char *
stringToFN(String s)
{
#ifdef XOS
   return stringToUTF8(s);
#else
   return stringToMB(s);
#endif
}
