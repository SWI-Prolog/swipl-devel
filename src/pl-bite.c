/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Select part of a file
*/

#include "pl-incl.h"
#include <string.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a very simple stand alone program that picks  a  region  from  a
file on character index basis and sends this to the standard output.  It
is  part  of the help system to display long sections of the manual.  It
should be installed as `pl-bite' in a directory that is in the  path  of
the user (normally /usr/local/bin, next to Prolog itself).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void usage(void);

char *program;

int
main(int argc, char **argv)
{ long from, to;
  char *s;
  FILE *fd;

  program = argv[0];


  if ( argc != 3 || (s = strchr(argv[1], ':')) == NULL )
  { usage();
    exit(1);
  }

  *s = '\0';
  if ( (from = atol(argv[1])) == 0 || (to = atol(&s[1])) == 0 )
  { usage();
    exit(1);
  }

  if ( (fd = fopen(argv[2], "r")) == NULL || fseek(fd, from, 0) < 0 )
  { perror(argv[2]);
    exit(1);
  }

  while(from++ < to)
  { Char c;

    if ( (c = getc(fd)) == EOF )
    { fprintf(stderr, "%s: %s: premature EOF\n", program, argv[2]);
      exit(1);
    }
    putchar(c);
  }
  fflush(stdout);
  fclose(fd);

  exit(0);
}

static void
usage(void)
{ fprintf(stderr, "usage: %s from:to file\n", program);
  exit(1);
}
