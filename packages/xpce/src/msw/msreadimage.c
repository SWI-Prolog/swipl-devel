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
#include <h/graphics.h>
#ifndef __WATCOMC__
#include <memory.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read various bitmap-formats and  convert them into  an X11 bitmap-data
string to be used with XCreateBitmapFromData().  Functions provided:

unsigned char *read_bitmap_data(IOSTREAM *fd, int *w, int *h)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static unsigned char *read_x11_bitmap_file(IOSTREAM *, int *, int *);

#define Round(n, r) ((((n) + (r) - 1) / (r)) * (r))

		/********************************
		*          ENTRY POINT		*
		********************************/

unsigned char *
read_bitmap_data(IOSTREAM *fd, int *w, int *h)
{ long offset = Stell(fd);
  unsigned char *rval;

  if ( (rval = read_x11_bitmap_file(fd, w, h)) != NULL )
    return rval;
  Sseek(fd, offset, 0);

  return NULL;
}


		/********************************
		*         X10/X11 FORMAT	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below is copied from the MIT X11R5 distribution and modified for
the interface required in PCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_SIZE 255

/* shared data for the image read/parse logic */
static short hexTable[256];		/* conversion value */
static int initialized = FALSE;		/* easier to fill in at run time */

#define HT_NODIGIT (-2)

/*
 *	Table index for the hex values. Initialized once, first time.
 *	Used for translation value or delimiter significance lookup.
 */
static void
initHexTable(void)
{ int i;

  for(i=0; i<(sizeof(hexTable)/sizeof(short)); i++)
    hexTable[i] = -HT_NODIGIT;

  hexTable['0'] = 0;	hexTable['1'] = 1;
  hexTable['2'] = 2;	hexTable['3'] = 3;
  hexTable['4'] = 4;	hexTable['5'] = 5;
  hexTable['6'] = 6;	hexTable['7'] = 7;
  hexTable['8'] = 8;	hexTable['9'] = 9;
  hexTable['A'] = 10;	hexTable['B'] = 11;
  hexTable['C'] = 12;	hexTable['D'] = 13;
  hexTable['E'] = 14;	hexTable['F'] = 15;
  hexTable['a'] = 10;	hexTable['b'] = 11;
  hexTable['c'] = 12;	hexTable['d'] = 13;
  hexTable['e'] = 14;	hexTable['f'] = 15;

  /* delimiters of significance are flagged w/ negative value */
  hexTable[' '] = -1;	hexTable[','] = -1;
  hexTable['}'] = -1;	hexTable['\n'] = -1;
  hexTable['\t'] = -1;
	
  initialized = TRUE;
}

/*
 *	read next hex value in the input stream, return -1 if EOF
 */

static int
NextInt(IOSTREAM *fstream)
{ int ch;
  int value = 0;
  int gotone = 0;
  int done = 0;
    
    /* loop, accumulate hex value until find delimiter  */
    /* skip any initial delimiters found in read stream */

  while (!done)
  { if ( Sfeof(fstream) )
    { value = -1;
      done++;
    } else
    { int dvalue;

      ch = Sgetc(fstream) & 0xff;
      if ( ch == '\r' )
	continue;
      dvalue = hexTable[ch];
      if ( dvalue >= 0 )
      { value = (value << 4) + dvalue;
	gotone++;
      } else if ((hexTable[ch]) == -1 && gotone)
	done++;
    }
  }

  return value;
}


static unsigned char *
read_x11_bitmap_file(IOSTREAM *fd, int *w, int *h)
{ unsigned char *data = NULL;
  char line[LINESIZE];
  int size = 0;
  char name_and_type[LINESIZE];		/* an input line */
  char *type;				/* for parsing */
  int value;				/* from an input line */
  int version10p;			/* bool, old format */
  int padding;				/* to handle alignment */
  int bytes_per_line;			/* per scanline of data */
  unsigned int ww = 0;			/* width */
  unsigned int hh = 0;			/* height */
  int hx = -1;				/* x hotspot */
  int hy = -1;				/* y hotspot */

  if (initialized == FALSE)
    initHexTable();

#define	RETURN_ERROR { if (data) pceFree(data); return NULL; }

  while (Sfgets(line, LINESIZE, fd))
  { if ( sscanf(line,"#define %s %d",name_and_type,&value) == 2)
    { if (!(type = strrchr(name_and_type, '_')))
	type = name_and_type;
      else
	type++;

      if (!strcmp("width", type))
	ww = (unsigned int) value;
      if (!strcmp("height", type))
	hh = (unsigned int) value;
      if (!strcmp("hot", type))
      { if (type-- == name_and_type || type-- == name_and_type)
	  continue;
	if (!strcmp("x_hot", type))
	  hx = value;
	if (!strcmp("y_hot", type))
	  hy = value;
      }
      continue;
    }
    
    if (sscanf(line, "static short %s = {", name_and_type) == 1)
      version10p = 1;
    else if (sscanf(line,"static unsigned char %s = {",name_and_type) == 1)
      version10p = 0;
    else if (sscanf(line, "static char %s = {", name_and_type) == 1)
      version10p = 0;
    else
      continue;

    if (!(type = strrchr(name_and_type, '_')))
      type = name_and_type;
    else
      type++;

    if (strcmp("bits[]", type))
      continue;
    
    if (!ww || !hh)
      RETURN_ERROR;

    if ((ww % 16) && ((ww % 16) < 9) && version10p)
      padding = 1;
    else
      padding = 0;

    bytes_per_line = (ww+7)/8 + padding;

    size = bytes_per_line * hh;
    data = (unsigned char *) pceMalloc(size);

    if (version10p)
    { unsigned char *ptr;
      int bytes;

      for (bytes=0, ptr=data; bytes<size; (bytes += 2))
      { if ((value = NextInt(fd)) < 0)
	  RETURN_ERROR;
	*(ptr++) = value;
	if (!padding || ((bytes+2) % bytes_per_line))
	  *(ptr++) = value >> 8;
      }
    } else
    { unsigned char *ptr;
      int bytes;
      
      for (bytes=0, ptr=data; bytes<size; bytes++, ptr++)
      { if ((value = NextInt(fd)) < 0) 
	  RETURN_ERROR;
	*ptr=value;
      }
    }
  }

  if (data == NULL) {
    RETURN_ERROR;
  }

  *w = ww;
  *h = hh;

  return data;
}


