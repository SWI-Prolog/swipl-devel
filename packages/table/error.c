/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

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
#endif
#ifdef __unix__
#ifdef SUNOS4
{     extern char *sys_errlist[];
      sprintf(buf, "%s: IO error %s", pred, sys_errlist[argi]);
}
#else
      sprintf(buf, "%s: IO error %s", pred, strerror(argi));
#endif
#endif

      return PL_warning(buf);
    }
  }

  return PL_warning("Table package: unknown error");
}
