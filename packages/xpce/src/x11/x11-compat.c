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

		/********************************
		*            HACKS ...		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Who  the   hell is using  these!?   It doesn't  seem  to be  the X11R5
libraries.  It certainly ain't PCE itself.  Nevertheless someone seems
to refer  to them.  Unfortunately they only  in a dynamic library  and
thus cannot be loaded through many foreign  language interfaces.  What
to do????
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(HAVE_LIBDL) && defined(__sun__) && XT_REVISION == 5

void *
dlopen(char *path, int mode)
{ Cprintf("dlopen(%s, %d)\n", path, mode);

  return NULL;
}


void *
dlsym(void *handle, char *symbol)
{ Cprintf("dlsym(%p, %s)\n", handle, symbol);

  return NULL;
}


void *
dlclose(void *handle)
{ Cprintf("dlclose (%p)\n", handle);

  return NULL;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
More of this nonsens.  RS6000 this time ...

	nm -pgo /usr/lib/libX11.a | grep _iconv_open
	shr4.o:         U __iconv_open
	shr4.o:0000fc18 T .__iconv_open
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if _AIX
void *
__iconv_open()
{ Cprintf("_iconv_open() called\n");

  return NULL;
}
#endif
