/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "md.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define LINESIZE 1024

#ifdef SYSLIB_H
#include SYSLIB_H
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

	while(*s && (isalnum(*s & 0xff) || *s == '_') )
	  s++;
	strncpy(nbuf, start, s-start);
	nbuf[s-start] = '\0';
	printf("%s\n", nbuf);
      }
    }
  }

  exit(0);
}
