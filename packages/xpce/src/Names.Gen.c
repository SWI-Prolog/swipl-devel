/*  Names.Gen.c,v 1.1 1992/06/10 17:02:25 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include "md.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#define LINESIZE 256
#define EOS '\0'

#ifdef O_EXTRA_SYSTEM_TYPES
#include O_EXTRA_SYSTEM_TYPES
#endif

void
processInput(FILE *fd, FILE *ic, FILE *ih)
{ char name[LINESIZE];
  char prolog[LINESIZE];
  int cn = 0;				/* current-name index */

  for( ; fgets(name, LINESIZE, fd) != NULL ; )
  { char *s, *q;

    for(s=name, q=prolog; *s; s++)
    { if ( isupper(*s) )
      { *q++ = '_';
	*q++ = *s + 'a' - 'A';
      } else if ( isspace(*s) )
      { *s = EOS;
	break;
      } else
	*q++ = *s;
    }
    *q = EOS;

    if ( name[0] != EOS && prolog[0] != EOS )
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
