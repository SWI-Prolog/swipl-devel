/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2018, University of Amsterdam
			      CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __WINDOWS__
#include <direct.h>
#else
#include <unistd.h>
#endif

#ifdef _MSC_VER
#pragma warning(disable : 4996)	/* deprecate open() etc */
#endif

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

int verbose = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file generates pl-atom.i[ch] and   pl-funct.i[ch] from ATOMS. Older
versions used an awk program for this,   but  to simplify portability to
Windows this is now converted into a little C program.

The program must be called in  the   source  directory  or with a single
argument that specifies the source directory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
cmp_file(const char *from, const char *to)
{ FILE *f1 = fopen(from, "r");
  FILE *f2 = fopen(to, "r");
  int rc = -1;

  if ( f1 && f2 )
  { char l1[1024];
    char l2[1024];

    while ( fgets(l1, sizeof(l1), f1) )
    { if ( fgets(l2, sizeof(l2), f2) )
      { if ( strcmp(l1, l2) == 0 )
	  continue;
      }
      goto out;
    }
    if ( !fgets(l2, sizeof(l2), f2) )
      rc = 0;
  }

out:
  if ( f1 ) fclose(f1);
  if ( f2 ) fclose(f2);
  return rc;
}


static int
update_file(const char *from, const char *to)
{ if ( cmp_file(from, to) == 0 )
  { if ( verbose )
      fprintf(stderr, "\t%s: no change\n", to);
    return remove(from);
  } else
  { remove(to);
    if ( verbose )
      fprintf(stderr, "\t%s: updated\n", to);
    return rename(from, to);
  }
}


int
main(int argc, char **argv)
{ FILE *aic, *aih, *fic, *fih;
  FILE *in;
  int atom=1;
  int functor=1;
  int line=0;
  char buf[256];
  int errors=0;
  char atom_defs[PATH_MAX];

  argc--;
  argv++;

  if ( argc >= 1 && strcmp(argv[0], "-v") == 0 )
  { argc--;
    argv++;
    verbose = 1;
  }

  if ( argc == 1 )
    snprintf(atom_defs, sizeof(atom_defs), "%s/%s", argv[0], "ATOMS");
  else
    snprintf(atom_defs, sizeof(atom_defs), "%s", "ATOMS");

  aic = fopen("pl-atom.ic.tmp", "w");
  aih = fopen("pl-atom.ih.tmp", "w");
  fic = fopen("pl-funct.ic.tmp", "w");
  fih = fopen("pl-funct.ih.tmp", "w");

  fprintf(aih, "#define ATOM_ MK_ATOM(%d)\n", atom++);
  fprintf(aic, "ATOM(\"\"),\n");

  in = fopen(atom_defs, "r");
  while(fgets(buf, sizeof(buf), in))
  { line++;

    switch(buf[0])
    { case '#':
	continue;
      case 'A':
      { char id[256], str[256];

	if ( sscanf(buf, "A%s \"%[^\"]\"", id, str) == 2 )
	{ fprintf(aih, "#define ATOM_%-12s MK_ATOM(%d)\n", id, atom++);
	  fprintf(aic, "ATOM(\"%s\"),\n", str);
	} else
	{ fprintf(stderr, "ERROR: ATOMS:%d: syntax error\n", line);
	  errors++;
	}

	continue;
      }
      case 'F':
      { char id[256];
	int arity;

	if ( sscanf(buf, "F%s%d", id, &arity) == 2 )
	{ char name[300];

	  snprintf(name, sizeof(name)-1, "%s%d", id, arity);
	  fprintf(fih, "#define FUNCTOR_%-12s MK_FUNCTOR(%d, %d)\n", name, functor++, arity);
	  fprintf(fic, "FUNCTOR(ATOM_%s, %d),\n", id, arity);
	} else
	{ fprintf(stderr, "ERROR: ATOMS:%d: syntax error\n", line);
	  errors++;
	}

	continue;
      }

      case '\r':
      case '\n':
	continue;
    }
  }

  fclose(in);
  fclose(aic);
  fclose(aih);
  fclose(fic);
  fclose(fih);

  if ( !errors )
  { errors += update_file("pl-atom.ic.tmp",  "pl-atom.ic");
    errors += update_file("pl-atom.ih.tmp",  "pl-atom.ih");
    errors += update_file("pl-funct.ic.tmp", "pl-funct.ic");
    errors += update_file("pl-funct.ih.tmp", "pl-funct.ih");
  }

  return errors ? 1 : 0;
}
