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

#include "md.h"
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#define LINESIZE 256
#define EOS '\0'

#ifdef SYSLIB_H
#include SYSLIB_H
#endif

void
processInput(FILE *fd, FILE *ic, FILE *ih)
{ char name[LINESIZE];
  char prolog[LINESIZE];
  int cn = 0;				/* current-name index */

  for( ; fgets(name, LINESIZE, fd) != NULL ; )
  { char *s, *q;

    for(s=name, q=prolog; *s; s++)
    { if ( isupper(*s & 0xff) )
      { *q++ = '_';
	*q++ = *s + 'a' - 'A';
      } else if ( isspace(*s & 0xff) )
      { *s = EOS;
	break;
      } else
	*q++ = *s;
    }
    *q = EOS;

/*  if ( name[0] != EOS && prolog[0] != EOS ) */
    { fprintf(ic, "  BUILTIN_NAME(\"%s\")\n", prolog);
      fprintf(ih, "#define NAME_%s (&builtin_names[%d])\n", name, cn++);
    }
  }
}


int
main(int argc, char **argv)
{ FILE *fd = stdin;
  FILE *ic, *ih;
  char *program;

  program = argv[0];
  argc--, argv++;

  if ( argc != 2 )
  { fprintf(stderr, "Usage: %s ic-file ih-file\n", program);
    exit(1);
  }

  if ( (ic = fopen(argv[0], "w")) == NULL )
  { perror(argv[0]);
    exit(1);
  }
  if ( (ih = fopen(argv[1], "w")) == NULL )
  { perror(argv[1]);
    exit(1);
  }

  processInput(fd, ic, ih);
  fclose(ic);
  fclose(ih);

  exit( 0 );
}
