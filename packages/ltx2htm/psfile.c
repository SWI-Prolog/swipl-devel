/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#include <SWI-Prolog.h>
#include <stdio.h>

#define MAXLINE 10240

static int
substr(const char *in, const char *sub)
{ int ls = strlen(sub);
  int l = strlen(in) - ls;

  for( ; l >= 0; in++, l-- )
  { if ( strncmp(in, sub, ls) == 0 )
      return 1;
  }

  return 0;
}


static foreign_t
pl_get_ps_parameters(term_t file, term_t iseps, term_t bb)
{ char *fname;
  FILE *fd;

  if ( !PL_get_chars(file, &fname, CVT_ALL) )
    return PL_warning("get_ps_parameters/3: invalid filename");

  if ( (fd = fopen(fname, "r")) )
  { char buf[MAXLINE];
    char *s;

    if ( (s=fgets(buf, sizeof(buf), fd)) )
    { if ( substr(s, "EPSF") )
	PL_unify_atom_chars(iseps, "eps");
      else
	PL_unify_atom_chars(iseps, "ps");
    }
    
    do
    { double a1, a2, a3, a4;

      if ( sscanf(buf, "%%%%BoundingBox: %lf %lf %lf %lf", &a1, &a2, &a3, &a4) == 4 )
      { fclose(fd);
	return PL_unify_term(bb,
			     PL_FUNCTOR, PL_new_functor(PL_new_atom("bb"), 4),
			     PL_FLOAT, a1,
			     PL_FLOAT, a2,
			     PL_FLOAT, a3,
			     PL_FLOAT, a4);
      }
    } while( (s=fgets(buf, sizeof(buf), fd)) );

    fclose(fd);
    PL_warning("get_ps_parameters/3: could not find %%%%BoundingBox in %s",
	       fname);

    PL_fail;
  }

  PL_warning("get_ps_parameters/3: could not open %s", fname);
  PL_fail;
}


void
install_ps()
{ PL_register_foreign("get_ps_parameters", 3, pl_get_ps_parameters, 0);
}

