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


static void
addBytes(rcell *c, char *bytes, size_t size)
{ roomBuffer(c, size);

  memcpy(c->bufp, bytes, size);
  c->bufp += size;
}



		 /*******************************
		 *	  CHARARRAY --> 	*
		 *******************************/

typedef const unsigned char cuchar;
typedef const wchar_t       cwchar;

char *
charArrayToUTF8(CharArray ca)
{ String str = &ca->data;
  rcell *out;

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

      c->bufp = utf8_put_char(c->bufp, *s);
    } 
  } else
  { cwchar *s = str->s_textW;
    cwchar *e = &s[str->size];

    out = find_ring();
    for( ; s<e; s++ )
    { roomBuffer(out, 6);		/* max bytes per UTF-8 */

      c->bufp = utf8_put_char(c->bufp, *s);
    } 
  }

  addByte(out, 0);

  return out->data;
}


char *
charArrayToMB(CharArray ca)
{ String str = &ca->data;
  rcell *out;
  mbstate_t mbs;
  int cmax = MB_CUR_MAX;
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

  return out->data;
}


wchar_t *
charArrayToWC(CharArray, size_t *len)
{ String str = &ca->data;

  if ( isstrA(str) )
  { rcell *out = find_ring();
    cuchar *s = (cuchar*) str->s_textA;
    cuchar *e = &s[str->size];
    wchar_t *o;

    roomBuffer(c, str->size*sizeof(wchar_t));

    for(o=(wchar_t*)c->data ; s<e; )
    { *o++ = *s++;
    }
    *o = 0;

    return out->data;
  } else
    return str->s_textW;
}


		 /*******************************
		 *	  <-- CHARARRAY  	*
		 *******************************/

CharArray
UTF8ToCharArray(const char *utf8)
{
}


CharArray
MBToCharArray(const char *mb)
{
}


CharArray
WCToCharArray(const wchar_t *wc, size_t len)
{
}



