/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Prolog.h>
#include <stdio.h>
#include "error.h"
#include "table.h"
#include <string.h>
#ifdef __unix__
#include <errno.h>
#endif

#ifdef WIN32
#include <malloc.h>

		 /*******************************
		 *	       WINDOWS		*
		 *******************************/

static char *
winerror(int id)
{ char *msg;
  static WORD lang;
  static lang_initialised = 0;

  if ( !lang_initialised )
    lang = MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_UK);

again:
  if ( !FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|
		      FORMAT_MESSAGE_IGNORE_INSERTS|
		      FORMAT_MESSAGE_FROM_SYSTEM,
		      NULL,			/* source */
		      id,			/* identifier */
		      lang,
		      (LPTSTR) &msg,
		      0,			/* size */
		      NULL) )			/* arguments */
  { if ( lang_initialised == 0 )
    { lang = MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT);
      lang_initialised = 1;
      goto again;
    }

    msg = "Unknown Windows error";
  }

  return strcpy((char *)malloc(strlen(msg)+1), msg);
}
#endif

		 /*******************************
		 *            ERRORS		*
		 *******************************/

#if !defined(HAVE_STRERROR) && !defined(WIN32)
static char *
strerror(int err)
{ extern char *sys_errlist[];
  
  return sys_errlist[err];
}
#endif


int
error_func(int type, const char *pred, int argi, long argl)
{ switch(type)
  { case ERR_INSTANTIATION:
    { char buf[1024];

      sprintf(buf, "%s: instantiation error on argument %d", pred, argi);
      return PL_warning(buf);
    }
    case ERR_FORMAT:
    { char buf[1024];
      Field f = (Field) argl;

      sprintf(buf, "%s: bad record, field %d (%s), char-index %d",
	      pred, f->index, PL_atom_chars(f->name), argi);

      return PL_warning(buf);
    }
    case ERR_IO:
    { char buf[1024];

#ifdef WIN32
      char *msg = winerror(argi);
      sprintf(buf, "%s: IO error %s", pred, msg);
      free(msg);
#else
      sprintf(buf, "%s: IO error %s", pred, strerror(argi));
#endif

      return PL_warning(buf);
    }
  }

  return PL_warning("Table package: unknown error");
}
