/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Select part of a file
*/

#include "pl-incl.h"
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a very simple stand alone program that picks  a  region  from  a
file on character index basis and sends this to the standard output.  It
is  part  of the help system to display long sections of the manual.  It
should be installed as `pl-bite' in a directory that is in the  path  of
the user (normally /usr/local/bin, next to Prolog itself).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void usage(void);

char *program;
int  escape = FALSE;

int
main(int argc, char **argv)
{ long from, to;
  char *s, *range, *file;
  FILE *fd;
  

  program = argv[0];
  argc--, argv++;

  if ( argc >= 1 && streq(argv[0], "-e") )
  { escape = TRUE;
    argc--, argv++;
  }
  if ( argc != 2 || (s = strchr(argv[0], ':')) == NULL )
  { usage();
    exit(1);
  }
  range = argv[0];
  file = argv[1];

  *s = '\0';
  if ( (from = atol(range)) == 0 || (to = atol(&s[1])) == 0 )
  { usage();
    exit(1);
  }

  if ( (fd = fopen(file, "rb")) == NULL || fseek(fd, from, 0) < 0 )
  { perror(argv[2]);
    exit(1);
  }

  if ( !escape )
  { int c2 = getc(fd);

    while(from < to)
    { int c = c2;
      
      c2 = getc(fd);
      if ( c2 == '\b' )
      {	c2 = getc(fd);
	from += 2;
	continue;
      }
      putchar(c);
      from++;
    }
  } else
  { while(from++ < to)
    { Char c;
  
      if ( (c = getc(fd)) == EOF )
      { fprintf(stderr, "%s: %s: premature EOF\n", program, file);
	exit(1);
      }
      putchar(c);
    }
  }
  fflush(stdout);
  fclose(fd);

  exit(0);
  return(0);
}

static void
usage(void)
{ fprintf(stderr, "usage: %s [-e] from:to file\n", program);
  exit(1);
}
