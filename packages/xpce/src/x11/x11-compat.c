/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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

#if !defined(HAVE_LIBDL) && defined(sun) && XT_REVISION == 5

void *
dlopen(char *path, int mode)
{ Cprintf("dlopen(%s, %d)\n", path, mode);

  return NULL;
}


void *
dlsym(void *handle, char *symbol)
{ Cprintf("dlsym(0x%lx, %s)\n", (ulong) handle, symbol);

  return NULL;
}


void *
dlclose(void *handle)
{ Cprintf("dlclose (0x%lx)\n", (ulong) handle);

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
