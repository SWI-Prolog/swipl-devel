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
