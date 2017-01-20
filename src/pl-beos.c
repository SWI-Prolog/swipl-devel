/*  Part of SWI-Prolog

    Author:        Alex Dörfler
    E-mail:        axeld@pinc-software.de
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2014, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is  written  by   Alex  Dörfler,  axeld@pinc-software.de and
integrated into SWI-Prolog by Jan Wielemaker.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __BEOS__
#include "pl-incl.h"


		 /*******************************
		 *	DLOPEN AND FRIENDS	*
		 *******************************/

#ifdef EMULATE_DLOPEN

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions emulate the bits from the ELF shared object interface we
need. They are used  by  pl-load.c,   which  defines  the  actual Prolog
interface.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void *
dlopen(const char *file, int flags)
{ image_id image = load_add_on(file);

  if ( image < B_OK )
  { LD->os.dl_error = image;
    return NULL;
  }

  LD->os.dl_error = B_OK;
  return (void *)image;
}


const char *
dlerror()
{ return strerror(LD->os.dl_error);
}


void *
dlsym(void *handle, char *symbol)
{ void *address;

  LD->os.dl_error = get_image_symbol((image_id)handle,
				     symbol,
				     B_SYMBOL_TYPE_TEXT,
				     &address);
  if ( LD->os.dl_error == B_OK )
    return address;

  return NULL;
}


int
dlclose(void *handle)
{ return unload_add_on((image_id)handle);
}

#endif	/* EMULATE_DLOPEN */

#endif	/* __BEOS__ */
