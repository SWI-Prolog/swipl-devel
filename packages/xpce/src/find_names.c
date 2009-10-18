/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2008, University of Amsterdam

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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This program is  used  to  generate   the  built-in  name  database from
NAME_<id> found in the  sources.  It  must   be  called  in  the  source
directory using

	find_names h/names.ic h/names.ih -- file ...

Older versions used POSIX scripting for this, but this has been moved to
a plain C program to facilitate   building  on on-POSIX platforms (read:
M$-Windows).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define LINESIZE 1024

const char *name = "NAME_";

char **names;
int allocated = 0;
int count = 0;

static void
save_name(const char *name)
{ if ( count >= allocated )
  { if ( allocated )
    { allocated *= 2;

      names = realloc(names, allocated*sizeof(char*));
    } else
    { allocated = 1024;
      names = malloc(allocated*sizeof(char*));
    }
  }

  names[count++] = strdup(name);
}



static int
scan_file(const char *file)
{ FILE *fd = fopen(file, "r");
  char buf[LINESIZE];
  int len = strlen(name);

  if ( !fd )
  { fprintf(stderr, "Warning: could not open %s (skipped)\n", file);
    return 0;
  }

  while(fgets(buf, sizeof(buf), fd))
  { const char *s;

    for(s = buf; *s; s++)
    { if ( s[0] == 'N' && strncmp(name, s, len) == 0 )
      { const char *start = s = s + len;
	char nbuf[200];

	if ( strncmp(s, "MAX", 3) == 0 ) /* avoid NAME_MAX */
	  continue;
	while(*s && (isalnum(*s & 0xff) || *s == '_') )
	  s++;
	strncpy(nbuf, start, s-start);
	nbuf[s-start] = '\0';
	save_name(nbuf);
      }
    }
  }

  fclose(fd);
  return 1;
}


static int
cmp_names(const void *p1, const void *p2)
{ return strcmp(*(char**)p1, *(char**)p2);
}


static void
sort_names()
{ if ( count > 0 )
  { int in, out;

    qsort(names, count, sizeof(char*), cmp_names);
    for(in=1, out=1; in<count; in++)
    { if ( strcmp(names[out-1], names[in]) == 0 )
      { free(names[in]);
      } else
      { names[out++] = names[in];
      }
    }

    count = out;
  }
}

static void
emit_names(FILE *ic, FILE *ih)
{ int i;

  for(i=0; i<count; i++)
  { const char *name = names[i];
    char prolog[LINESIZE];
    const char *s;
    char *q;

    for(s=name, q=prolog; *s; s++)
    { if ( isupper(*s & 0xff) )
      { *q++ = '_';
	*q++ = *s + 'a' - 'A';
      } else
      { *q++ = *s;
      }
    }
    *q = '\0';

    fprintf(ic, "  BUILTIN_NAME(\"%s\")\n", prolog);
    fprintf(ih, "#define NAME_%s (&builtin_names[%d])\n", name, i);
  }
}


int
main(int argc, char **argv)
{ int i;
  const char *program = argv[0];
  FILE *ic, *ih;

  argc--;				/* skip program */
  argv++;
  if ( argc < 3 || strcmp(argv[2], "--") )
  { fprintf(stderr, "Usage: %s ic-file ih-file -- file ...\n", program);
    exit(1);
  }

  if ( !(ic = fopen(argv[0], "w")) ||
       !(ih = fopen(argv[1], "w")) )
  { fprintf(stderr, "%s: Could not open output\n", program);
    exit(1);
  }

  argc -= 3;
  argv += 3;

  for(i=1; i<argc; i++)
    scan_file(argv[i]);

  sort_names();
  emit_names(ic, ih);

  fclose(ic);
  fclose(ih);

  return 0;
}
