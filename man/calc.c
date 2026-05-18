#include <string.h>
#include <stddef.h>
#include <SWI-Prolog.h>

#define MAXLINE 1024

int
main(int argc, char **argv)
{ char expression[MAXLINE] = "";
  char *e = expression;
  char *program = argv[0];
  char *plav[2];
  int n;

  /* combine all the arguments in a single string */

  for(n=1; n<argc; n++)
  { ptrdiff_t len_rem;
    if ( n != 1 )
    { len_rem = sizeof expression - (e - expression);
      if ( len_rem > 1 )
        *e++ = ' ';
    }
    len_rem = sizeof expression - (e - expression);
    if ( len_rem > 0 )
      e = strncpy(e, argv[n], len_rem);
  }
  expression[sizeof expression - 1] = '\0';

  /* make the argument vector for Prolog */

  plav[0] = program;
  plav[1] = NULL;

  /* initialise Prolog */

  if ( !PL_initialise(1, plav) )
    PL_halt(1);

  /* Lookup calc/1 and make the arguments and call */

  { predicate_t pred = PL_predicate("calc", 1, "user");
    term_t h0 = PL_new_term_refs(1);
    int rval;

    PL_put_atom_chars(h0, expression);
    rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, h0);

    PL_halt(rval ? 0 : 1);
  }

  return 0;
}
