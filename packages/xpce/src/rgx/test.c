#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
typedef wchar_t charW;
typedef unsigned char charA;
#include "regex.h"

#define INDIRECT 1

static wchar_t*
mkwide(const char *in)
{ size_t len = strlen(in);
  wchar_t *out = malloc((len+1)*sizeof(wchar_t));
  wchar_t *q;
  const unsigned char *s = (const unsigned char*)in;

  for(q=out; *s; )
    *q++ = *s++;
  *q = 0;

  return out;
}


#ifdef INDIRECT
static int
fetch(const charW *p, void *closure)
{ long n = (long)p;

  p = (charW*)((long)closure + n);

//printf("[%ld]: %c\n", n, *p);
  return *p;
}

#else

int
GETCHR(charW *p)
{ printf("[%p]: %c\n", p, *p);
  return *p;
}

#endif

int
main(int argc, char **argv)
{ regex_t re;
  int rc;
  char ebuf[1024];
  rm_detail_t details;
  regmatch_t matches[100];
  int i;
  charW *target, *offset;
  int dflags = REG_MTRACE|REG_FTRACE;
  
//  dflags = 0;

  if ( argc != 3 )
  { fprintf(stderr, "Usage: %s re string\n", argv[0]);
    exit(1);
  }

#if 0
  fprintf(stderr, "ANSI version\n");

  if ( (rc=re_compileA(&re, argv[1], strlen(argv[1]), REG_ADVANCED)) != REG_OKAY )
  { regerror(rc, &re, ebuf, sizeof(ebuf));
    fprintf(stderr, "Comp: %s\n", ebuf);
    exit(1);
  }

  if ( (rc = re_execA(&re, argv[2], strlen(argv[2]),
		      &details, 100, matches, 0)) != REG_OKAY )
  { regerror(rc, &re, ebuf, sizeof(ebuf));
    fprintf(stderr, "Exec: %s\n", ebuf);
    exit(0);
  }
  for(i=0; i<=re.re_nsub; i++)
  { printf("\tMatch %ld-%ld\n", matches[i].rm_so, matches[i].rm_eo);
  }

  fprintf(stderr, "WIDE version\n");
#endif

  if ( (rc=re_compileW(&re, mkwide(argv[1]), strlen(argv[1]), REG_ADVANCED)) != REG_OKAY )
  { regerror(rc, &re, ebuf, sizeof(ebuf));
    printf("error(compile, '%s').\n", ebuf);
    exit(1);
  }

  target = mkwide(argv[2]);
#ifdef INDIRECT
#if 0
  offset = target;
  target = NULL;
#else
  offset = target-10;
  target = ((charW*)0) + 10;
#endif
#endif
  if ( (rc = re_execW(&re, target, strlen(argv[2]),
		      fetch, offset,
		      &details, 100, matches, REG_MATCH|dflags)) != REG_OKAY )
  { regerror(rc, &re, ebuf, sizeof(ebuf));
    printf("error(execute, '%s').\n", ebuf);
    exit(0);
  }
  for(i=0; i<=re.re_nsub; i++)
  { printf("\tmatch(%ld,%ld).\n", matches[i].rm_so, matches[i].rm_eo);
  }

  return 0;
}
