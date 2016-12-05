/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2011, University of Amsterdam
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
#ifdef __WINDOWS__
#include <direct.h>
#else
#include <unistd.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file generates pl-atom.i[ch] and   pl-funct.i[ch] from ATOMS. Older
versions used an awk program for this,   but  to simplify portability to
Windows this is now converted into a little C program.

The program must be called in  the   source  directory  or with a single
argument that specifies the source directory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
main(int argc, char **argv)
{ FILE *aic, *aih, *fic, *fih;
  FILE *in;
  int atom=1;
  int functor=1;
  int line=0;
  char buf[256];
  int errors=0;

  if ( argc == 2 )
  { if ( chdir(argv[1]) )
    { fprintf(stderr, "%s: Could not chdir to %s\n", argv[0], argv[1]);
      exit(1);
    }
  }

  aic = fopen("pl-atom.ic", "w");
  aih = fopen("pl-atom.ih", "w");
  fic = fopen("pl-funct.ic", "w");
  fih = fopen("pl-funct.ih", "w");

  fprintf(aih, "#define ATOM_ MK_ATOM(%d)\n", atom++);
  fprintf(aic, "ATOM(\"\"),\n");

  in = fopen("ATOMS", "r");
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
	{ char name[256];

	  sprintf(name, "%s%d", id, arity);
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

  return errors ? 1 : 0;
}
