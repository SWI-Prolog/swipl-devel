/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: File Name Expansion
*/

#if __TOS__
#include <tos.h>
#define HIDDEN	0x02
#define SUBDIR 0x10
#endif

#include "pl-incl.h"

#if unix || EMX
#include <sys/param.h>
#include <sys/stat.h>
#ifdef DIR_INCLUDE
#include DIR_INCLUDE
#else
#include <dirent.h>
#endif
#ifdef DIR_INCLUDE2
#include DIR_INCLUDE2
#endif
#ifndef IS_DIR_SEPARATOR
#define IS_DIR_SEPARATOR(c)	((c) == '/')
#endif
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unix Wildcard Matching.  Recognised:

  ?		matches one arbitrary character
  *		matches any number of any character
  [xa-z]	matches x and a-z
  {p1,p2}	matches pattern p1 or p2

  backslash (\) escapes a character.

First the pattern is compiled into an intermediate representation.  Next
this intermediate representation is matched  against  the  target.   The
non-ascii  characters  are  used  to  store  control  sequences  in  the
intermediate representation:

  ANY		Match any character
  STAR		Match (possibly empty) sequence
  ALT <offset>	Match, if fails, continue at <pc> + offset
  JMP <offset>	Jump <offset> instructions
  ANYOF		Next 16 bytes are bitmap
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXEXPAND	1024
#define NextIndex(i)	((i) < MAXEXPAND-1 ? (i)+1 : 0)

#define MAXCODE 512

#define ANY	128
#define STAR	129
#define ALT	130
#define JMP	131
#define ANYOF	132
#define EXIT	133

#define NOCURL	0
#define CURL	1

#if !O_UCHAR_PREDEFINED
typedef unsigned char uchar;
#endif

struct bag
{ int	 in;			/* in index */
  int	 out;			/* out index */
  int	 size;			/* number of entries */
  bool	 changed;		/* did bag change? */
  bool	 expanded;		/* Expanded bag? */
  char	*bag[MAXEXPAND];	/* bag of paths */
};

static struct out
{ int	size;
  uchar	code[MAXCODE];
} cbuf;

forwards char	*compile_pattern P((struct out *, char *, int));
forwards bool	match_pattern P((uchar *, char *));
forwards int	stringCompare P((char**, char**));
forwards bool	expand P((char *, char **, int *));
forwards bool	Exists P((char *));
forwards char	*change_string P((char *, char *));
forwards bool	expandBag P((struct bag *));

#define Output(c)	{ if ( Out->size > MAXCODE-1 ) \
			  { warning("pattern too large"); \
			    return (char *) NULL; \
			  } \
			  Out->code[Out->size++] = c; \
			}
#define setMap(c)	{ map[(c)/8] |= 1 << ((c) % 8); }

bool
compilePattern(p)
char *p;
{ cbuf.size = 0;
  if ( compile_pattern(&cbuf, p, NOCURL) == (char *) NULL )
    fail;

  succeed;
}


static char *
compile_pattern(Out, p, curl)
struct out *Out;
char *p;
int curl;
{ char c;

  for(;;)
  { switch(c = *p++)
    { case EOS:
	break;
      case '\\':
	Output(*p == EOS ? '\\' : (*p & 0x7f));
	if (*p == EOS )
	  break;
	p++;
	continue;
      case '?':
	Output(ANY);
	continue;
      case '*':
	Output(STAR);
	continue;
      case '[':
	{ uchar *map;
	  int n;

	  Output(ANYOF);
	  map = &Out->code[Out->size];
	  Out->size += 16;
	  if ( Out->size >= MAXCODE )
	  { warning("Pattern too long");
	    return (char *) NULL;
	  }

	  for( n=0; n < 16; n++)
	    map[n] = 0;

	  for(;;)
	  { switch( c = *p++ )
	    { case '\\':
		if ( *p == EOS )
		{ warning("Unmatched '['");
		  return (char *)NULL;
		}
		setMap(*p);
		p++;
		continue;
	      case ']':
		break;
	      default:
		if ( p[-1] != ']' && p[0] == '-' && p[1] != ']' )
		{ register int chr;

		  for ( chr=p[-1]; chr <= p[1]; chr++ )
		    setMap(chr);
		  p += 2;
		} else
		  setMap(c);
		continue;
	    }
	    break;
	  }

	  continue;
	}
      case '{':
	{ int ai, aj = -1;

	  for(;;)
	  { Output(ALT); ai = Out->size; Output(0);
	    if ( (p = compile_pattern(Out, p, CURL)) == (char *) NULL )
	      return (char *) NULL;
	    if ( aj > 0 )
	      Out->code[aj] = Out->size - aj;
	    if ( *p == ',' )
	    { Output(JMP); aj = Out->size; Output(0);
	      Out->code[ai] = Out->size - ai;
	      Output(ALT); ai = Out->size; Output(0);
	      p++;
	    } else if ( *p == '}' )
	    { p++;
	      break;
	    } else
	    { warning("Unmatched '{'");
	      return (char *) NULL;
	    }
	  }
	  
	  continue;
	}
      case '}':
      case ',':
	if ( curl == CURL )
	{ p--;
	  return p;
	}
	/*FALLTHROUGH*/
      default:
	Output(c & 0x7f);
	continue;
    }

    Output(EXIT);
    return p;
  }
}


bool
matchPattern(s)
char *s;
{ return match_pattern(cbuf.code, s);
}

static bool
match_pattern(p, s)
register uchar *p;
register char *s;
{ register uchar c;

  for(;;)
  { switch( c = *p++ )
    { case EXIT:
	  return (*s == EOS ? TRUE : FALSE);
      case ANY:						/* ? */
	  if ( *s == EOS )
	    fail;
	  s++;
	  continue;
      case ANYOF:					/* [...] */
	  if ( p[*s / 8] & (1 << (*s % 8)) )
	  { p += 16;
	    s++;
	    continue;
	  }
	  fail;
      case STAR:					/* * */
	  do
	  { if ( match_pattern(p, s) )
	      succeed;
	  } while( *s++ );
	  fail;
      case JMP:						/* { ... } */
	  p += *p;
	  continue;
      case ALT:
	  if ( match_pattern(p+1, s) )
	    succeed;
	  p += *p;
	  continue;	  
      default:						/* character */
	  if ( c != (uchar) *s )
	    fail;
	  s++;
	  continue;
    }
  }
}


word
pl_wildcard_match(pattern, string)
Word pattern, string;
{ char *p, *s;

  initAllocLocal();
  p = primitiveToString(*pattern, TRUE);
  s = primitiveToString(*string, TRUE);
  stopAllocLocal();

  if ( p == (char *)NULL || s == (char *)NULL )
    return warning("wildcard_match/2: instantiation fault");

  TRY( compilePattern(p) );

  return matchPattern(s);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
File Name Expansion.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_expand_file_name(f, l)
Word f, l;
{ char spec[MAXPATHLEN];
  char *s = primitiveToString(*f, FALSE);
  char *vector[MAXEXPAND];
  char **v;
  int filled;

  if ( s == (char *) NULL )
    return warning("expand_file_name/2: instantiation fault");
  if ( strlen(s) > MAXPATHLEN-1 )
    return warning("expand_file_name/2: name too long");
  TRY( expandVars(s, spec) );

  TRY( expand(spec, vector, &filled) );

  for( v = vector; filled > 0; filled--, v++ )
  { word w;

    w = (word) lookupAtom(*v);
    APPENDLIST(l, &w);
  }

  CLOSELIST(l);

  succeed;
}

static int
stringCompare(s1, s2)
register char **s1, **s2;
{ return strcmp(*s1, *s2);
}

static bool
expand(f, argv, argc)
char *f;
char **argv;
int *argc;
{ struct bag b;

  b.changed = b.expanded = FALSE;
  b.out = 0;			/* put the first entry in the bag */
  b.in = b.size = 1;
  b.bag[0] = store_string(f);
#if tos				/* case insensitive: do all lower case */
  strlwr(b.bag[0]);
#endif
/* Note for OS2: HPFS is case insensitive but case preserving. */

  do				/* expand till nothing to expand */
  { b.changed = FALSE;  
    TRY( expandBag(&b) );
  } while( b.changed );

  { char **r = argv;

    *argc = b.size;
    for( ; b.out != b.in; b.out = NextIndex(b.out) )
      *r++ = change_string(b.bag[b.out], PrologPath(b.bag[b.out]));
    *r = (char *) NULL;
    qsort(argv, b.size, sizeof(char *), stringCompare);

    succeed;
  }
}

#if unix || EMX
static bool
Exists(path)
char *path;
{ struct stat buf;
  extern int stat();

  if ( stat(OsPath(path), &buf) == -1 )
    fail;

  succeed;
}
#endif

#if tos
static bool
Exists(path)
char *path;
{ struct ffblk buf;

  DEBUG(2, printf("Checking existence of %s ... ", path));
  if ( findfirst(OsPath(path), &buf, SUBDIR|HIDDEN) == 0 )
  { DEBUG(2, printf("yes\n"));
    succeed;
  }
  DEBUG(2, printf("no\n"));

  fail;
}
#endif

static char *
change_string(old, new)
char *old, *new;
{ return store_string(new);
}

static bool
expandBag(b)
struct bag *b;
{ int high = b->in;

  for( ; b->out != high; b->out = NextIndex(b->out) )
  { char *head = b->bag[b->out];
    char *tail;
    register char *s = head;
    
    for(;;)
    { switch( *s++ )
      { case EOS:				/* no special characters */
	  if ( b->expanded == FALSE || Exists(b->bag[b->out]) )
	  { b->bag[b->in] = b->bag[b->out];
	    b->in = NextIndex(b->in);
	  } else
	  { b->size--;
	  }
	  goto next_bag;
#if tos || OS2
	case '\\':
#endif
	case '/':				/* no specials sofar */
	  head = s;
	  continue;
	case '[':				/* meta characters: expand */
	case '{':
	case '?':
	case '*':
	  break;
	default:
	  continue;
      }
      break;
    }

    b->expanded = b->changed = TRUE;
    b->size--;
    for( tail=s; *tail && !IS_DIR_SEPARATOR(*tail); tail++ )
      ;

/*  By now, head points to the start of the path holding meta characters,
    while tail points to the tail:

    ..../meta*path/....
	 ^        ^
       head     tail
*/

    { char prefix[MAXPATHLEN];
      char expanded[MAXPATHLEN];
      char *path;
      char *s, *q;
      int dot;

      for( q=prefix, s=b->bag[b->out]; s < head; )
	*q++ = *s++;
      *q = EOS;
      path = (prefix[0] == EOS ? "." : prefix);

      for( q=expanded, s=head; s < tail; )
        *q++ = *s++;
      *q = EOS;

      if ( compilePattern(expanded) == FALSE )		/* syntax error */
        fail;
      dot = (expanded[0] == '.');			/* do dots as well */

#if unix || EMX
      { DIR *d;
#if O_STRUCT_DIRECT
	struct direct *e;
	extern struct direct *readdir();
#else
	struct dirent *e;
	extern struct dirent *readdir();
#endif
	extern DIR *opendir();

	if ( (d = opendir(OsPath(path))) != (DIR *)NULL )
	{ for(e=readdir(d); e; e = readdir(d))
	  { if ( (dot || e->d_name[0] != '.') && matchPattern(e->d_name) )
	    { strcpy(expanded, prefix);
	      strcat(expanded, e->d_name);
	      strcat(expanded, tail);

	      b->bag[b->in] = change_string(b->bag[b->out], expanded);
	      b->in = NextIndex(b->in);
	      b->size++;
	      if ( b->in == b->out )
		return warning("Too many matches");
	    }
	  }
	  closedir(d);
	}
      }
#endif

#if tos
      { char dpat[MAXPATHLEN];
	struct ffblk buf;
	int r;

	strcpy(dpat, path);
	strcat(dpat, "\\*.*");

	DEBUG(2, printf("match path = %s\n", dpat));
	for(r=findfirst(OsPath(dpat), &buf, SUBDIR|HIDDEN); r == 0; r=findnext(&buf))
	{ char *name = buf.ff_name;
	  strlwr(name);		/* match at lower case */

	  DEBUG(2, printf("found %s\n", name));
	  if ( (dot || name[0] != '.') && matchPattern(name) )
	  { strcpy(expanded, prefix);
	    strcat(expanded, name);
	    strcat(expanded, tail);

	    DEBUG(2, printf("%s matches pattern\n", name));
	    b->bag[b->in] = change_string(b->bag[b->out], expanded);
	    b->in = NextIndex(b->in);
	    b->size++;
	    if ( b->in == b->out )
	      return warning("Too many matches");
	  }
	}
      }
#endif /* tos */
    }
  next_bag:;
  }

  succeed;
}
