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

#ifdef WIN32
#include <msw/include.h>
#define  FOR_MSW 1
#include <msw/xpm.h>
#else
#include <h/kernel.h>
#include <X11/xpm.h>
#endif

#include "gif.h"
#include <stdlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#define XpmMalloc(size) (void *)malloc((size))

static int
alloc_colortable(int ncolors, void *closure)
{ XpmImage *img = closure;

  if ( ncolors < 0 || ncolors > 256 )
    return GIF_INVALID;

  img->ncolors    = ncolors;
  img->colorTable = XpmMalloc(sizeof(XpmColor) * ncolors);

  if ( img->colorTable )
  { memset(img->colorTable, 0, sizeof(XpmColor) * ncolors);

    return GIF_OK;
  }

  return GIF_NOMEM;
}


static int
alloc_color(int index, int r, int g, int b, void *closure)
{ XpmImage *img = closure;
  XpmColor *c;
  
  if ( index < 0 || index >= img->ncolors )
    return GIF_INVALID;
  c = &img->colorTable[index];

  if ( (c->c_color = XpmMalloc(8)) )
  { sprintf(c->c_color, "#%02x%02x%02x", r, g, b);

    return GIF_OK;
  }

  return GIF_NOMEM;
}


static int
gif_extension(int ext, void *data, void *closure)
{ XpmImage *img = closure;

  switch(ext)
  { case GIFEXT_TRANSPARENT:
    { XpmColor *c;
      int i = (int)data;

      DEBUG(NAME_gif, Cprintf("Using %d as transparent (ncolors=%d)\n",
			      i, img->ncolors));

      if ( i < 0 || i >= img->ncolors )
	return GIF_INVALID;

      c = &img->colorTable[(long)data];
      strcpy(c->c_color, "None");	/* malloced 8 bytes, so ok. */
      break;
    }
    default:
      assert(0);
  }

  return GIF_OK;
}


int
XpmReadGIF(IOSTREAM *fd, XpmImage *img)
{ long here = Stell(fd);

  img->ncolors    = 0;
  img->colorTable = NULL;
  img->data       = NULL;

  switch( GIFReadFD(fd,
		    &img->data,
		    &img->width,
		    &img->height,
		    alloc_colortable,
		    alloc_color,
		    gif_extension,
		    img) )
  { case GIF_OK:
      return XpmSuccess;
    case GIF_NOMEM:
      Sseek(fd, here, SIO_SEEK_SET);
      return XpmNoMemory;
    case GIF_INVALID:
    default:
      Sseek(fd, here, SIO_SEEK_SET);
      return XpmFileInvalid;
  }
}
