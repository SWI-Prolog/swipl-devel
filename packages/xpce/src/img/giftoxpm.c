/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
