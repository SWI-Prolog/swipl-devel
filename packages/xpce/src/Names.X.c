/*  Names.X.c,v 1.1 1993/04/05 12:16:45 jan Exp

    Copyright (c) 1993 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Extract name declrations from input files
*/

#include "md.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define LINESIZE 1024

#ifdef O_EXTRA_SYSTEM_TYPES
#include O_EXTRA_SYSTEM_TYPES
#endif

char *name = "NAME_";

int
main(int argc, char **argv)
{ char buf[LINESIZE];
  int len = strlen(name);

  while(fgets(buf, LINESIZE, stdin))
  { char *s;

    for(s = buf; *s; s++)
    { if ( strncmp(name, s, len) == 0 )
      { char *start = s = s + len;
	char nbuf[200];

	while(*s && (isalnum(*s) || *s == '_') )
	  s++;
	strncpy(nbuf, start, s-start);
	nbuf[s-start] = '\0';
	printf("%s\n", nbuf);
      }
    }
  }

  exit(0);
}
