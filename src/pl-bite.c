/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Select part of a file
*/

#include <stdio.h>
extern char *index();
typedef int Char;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a very simple stand alone program that picks  a  region  from  a
file on character index basis and sends this to the standard output.  It
is  part  of the help system to display long sections of the manual.  It
should be installed as `pl-bite' in a directory that is in the  path  of
the user (normally /usr/local/bin, next to Prolog itself).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *program;

main(argc, argv)
int argc;
char **argv;
{ long from, to;
  char *s;
  FILE *fd;

  program = argv[0];

  if ( argc != 3 || (s = index(argv[1], ':')) == NULL )
    usage();

  *s = '\0';
  if ( (from = atol(argv[1])) == 0 || (to = atol(&s[1])) == 0 )
    usage();

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

usage()
{ fprintf(stderr, "usage: %s from:to file\n");
  exit(1);
}
