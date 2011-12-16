/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
