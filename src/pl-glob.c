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
#include "pl-ctype.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __WATCOMC__
#include <direct.h>
#else /*__WATCOMC__*/
#if HAVE_DIRENT_H
# include <dirent.h>
#else
# define dirent direct
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif
#endif /*__WATCOMC__*/

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifndef IS_DIR_SEPARATOR
#define IS_DIR_SEPARATOR(c)	((c) == '/')
#endif

#define char_to_int(c)	(0xff & (int)(c))

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

#define MAXCODE 1024

#define ANY	128
#define STAR	129
#define ALT	130
#define JMP	131
#define ANYOF	132
#define EXIT	133

#define NOCURL	0
#define CURL	1

typedef unsigned char matchcode;

struct bag
{ int	 in;			/* in index */
  int	 out;			/* out index */
  int	 size;			/* number of entries */
  bool	 changed;		/* did bag change? */
  bool	 expanded;		/* Expanded bag? */
  char	*bag[MAXEXPAND];	/* bag of paths */
};

typedef struct
{ int		size;
  matchcode	code[MAXCODE];
} compiled_pattern;

forwards char	*compile_pattern(compiled_pattern *, char *, int);
forwards bool	match_pattern(matchcode *, char *);
forwards int	stringCompare(const void *, const void *);
forwards bool	expand(char *, char **, int *);
#ifdef O_EXPANDS_TESTS_EXISTS
forwards bool	Exists(char *);
#endif
forwards char	*change_string(char *, char *);
forwards bool	expandBag(struct bag *);

#define Output(c)	{ if ( Out->size > MAXCODE-1 ) \
			  { warning("pattern too large"); \
			    return (char *) NULL; \
			  } \
			  Out->code[Out->size++] = c; \
			}

static inline void
setMap(matchcode *map, int c)
{ if ( !trueFeature(FILE_CASE_FEATURE) )
    c = makeLower(c);

  map[(c)/8] |= 1 << ((c) % 8);
}


static bool
compilePattern(char *p, compiled_pattern *cbuf)
{ cbuf->size = 0;
  if ( compile_pattern(cbuf, p, NOCURL) == (char *) NULL )
    fail;

  succeed;
}


static char *
compile_pattern(compiled_pattern *Out, char *p, int curl)
{ int c;

  for(;;)
  { switch(c = char_to_int(*p++))
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
	{ matchcode *map;
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
		setMap(map, *p);
		p++;
		continue;
	      case ']':
		break;
	      default:
		if ( p[-1] != ']' && p[0] == '-' && p[1] != ']' )
		{ int chr;

		  for ( chr=p[-1]; chr <= p[1]; chr++ )
		    setMap(map, chr);
		  p += 2;
		} else
		  setMap(map, c);
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
      case ANY:
      case STAR:
      case ALT:
      case JMP:
      case ANYOF:
      case EXIT:
	PL_error(NULL, 0, "Reserved character",
		 ERR_REPRESENTATION, ATOM_pattern);
	return NULL;
      case '}':
      case ',':
	if ( curl == CURL )
	{ p--;
	  return p;
	}
	/*FALLTHROUGH*/
      default:
        if ( !trueFeature(FILE_CASE_FEATURE) )
	  c = makeLower(c);
	Output(c);
	continue;
    }

    Output(EXIT);
    return p;
  }
}


static inline bool
matchPattern(char *s, compiled_pattern *cbuf)
{ return match_pattern(cbuf->code, s);
}


static bool
match_pattern(matchcode *p, char *str)
{ matchcode c;
  matchcode *s = (matchcode *) str;

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
        { matchcode c2 = *s;

	  if ( !trueFeature(FILE_CASE_FEATURE) )
	    c2 = makeLower(c2);

	  if ( p[c2 / 8] & (1 << (c2 % 8)) )
	  { p += 16;
	    s++;
	    continue;
	  }
	  fail;
	}
      case STAR:					/* * */
	  do
	  { if ( match_pattern(p, (char *)s) )
	      succeed;
	  } while( *s++ );
	  fail;
      case JMP:						/* { ... } */
	  p += *p;
	  continue;
      case ALT:
	  if ( match_pattern(p+1, (char *)s) )
	    succeed;
	  p += *p;
	  continue;	  
      default:						/* character */
	  if ( c == *s ||
	       (!trueFeature(FILE_CASE_FEATURE) && c == makeLower(*s)) )
	  { s++;
	    continue;
	  }
          fail;
    }
  }
}


word
pl_wildcard_match(term_t pattern, term_t string)
{ char *p, *s;
  compiled_pattern buf;

  if ( !PL_get_chars_ex(pattern, &p, CVT_ALL) ||
       !PL_get_chars_ex(string,  &s, CVT_ALL) )
    fail;

  if ( compilePattern(p, &buf) )
  { return matchPattern(s, &buf);
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_pattern, pattern);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
File Name Expansion.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_expand_file_name(term_t f, term_t list)
{ char spec[MAXPATHLEN];
  char *s;
  char *vector[MAXEXPAND];
  char **v;
  int filled;
  term_t l    = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  if ( !PL_get_chars_ex(f, &s, CVT_ALL) )
    fail;
  if ( strlen(s) > MAXPATHLEN-1 )
    return PL_error(NULL, 0, "File name too long",
		    ERR_DOMAIN, ATOM_pattern, f);

  TRY( expandVars(s, spec) );
  TRY( expand(spec, vector, &filled) );

  for( v = vector; filled > 0; filled--, v++ )
  { if ( !PL_unify_list(l, head, l) ||
	 !PL_unify_atom_chars(head, *v) )
      fail;
  }

  return PL_unify_nil(l);
}


static int
stringCompare(const void *a1, const void *a2)
{ if ( trueFeature(FILE_CASE_FEATURE) )
    return strcmp(*((char **)a1), *((char **)a2));
  else
    return stricmp(*((char **)a1), *((char **)a2));
}

static bool
expand(char *f, char **argv, int *argc)
{ struct bag b;

  b.changed = b.expanded = FALSE;
  b.out = 0;			/* put the first entry in the bag */
  b.in = b.size = 1;
  b.bag[0] = store_string(f);
#if tos || defined(__MSDOS__)	/* case insensitive: do all lower case */
  strlwr(b.bag[0]);
#endif
/* Note for OS2: HPFS is case insensitive but case preserving. */

  do				/* expand till nothing to expand */
  { b.changed = FALSE;  
    TRY( expandBag(&b) );
  } while( b.changed );

  { char **r = argv;
    char plp[MAXPATHLEN];

    *argc = b.size;
    for( ; b.out != b.in; b.out = NextIndex(b.out) )
      *r++ = change_string(b.bag[b.out], PrologPath(b.bag[b.out], plp));
    *r = (char *) NULL;
    qsort(argv, b.size, sizeof(char *), stringCompare);

    succeed;
  }
}

#ifdef O_EXPANDS_TESTS_EXISTS
#if defined(HAVE_STAT) || defined(__unix__)
static bool
Exists(const char *path)
{ char tmp[MAXPATHLEN];
  struct stat buf;

  if ( stat(OsPath(path, tmp), &buf) == -1 )
    fail;

  succeed;
}
#endif

#if tos
static bool
Exists(const path)
char *path;
{ struct ffblk buf;
  char tmp[MAXPATHLEN];

  DEBUG(2, Sdprintf("Checking existence of %s ... ", path));
  if ( findfirst(OsPath(path, tmp), &buf, SUBDIR|HIDDEN) == 0 )
  { DEBUG(2, Sdprintf("yes\n"));
    succeed;
  }
  DEBUG(2, Sdprintf("no\n"));

  fail;
}
#endif
#endif /*O_EXPANDS_TESTS_EXISTS*/

static char *
change_string(char *old, char *new)
{ return store_string(new);
}

static bool
expandBag(struct bag *b)
{ int high = b->in;

  for( ; b->out != high; b->out = NextIndex(b->out) )
  { char *head = b->bag[b->out];
    char *tail;
    char *s = head;
    compiled_pattern cbuf;
    
    for(;;)
    { int c;

      switch( (c = *s++) )
      { case EOS:				/* no special characters */
#ifdef O_EXPANDS_TESTS_EXISTS
	  if ( b->expanded == FALSE || Exists(b->bag[b->out]) )
	  { b->bag[b->in] = b->bag[b->out];
	    b->in = NextIndex(b->in);
	  } else
	  { b->size--;
	  }
#else
	  b->bag[b->in] = b->bag[b->out];
	  b->in = NextIndex(b->in);
#endif
	  goto next_bag;
	case '[':				/* meta characters: expand */
	case '{':
	case '?':
	case '*':
	  break;
	default:
	  if ( IS_DIR_SEPARATOR(c) )
	    head = s;
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

      if ( !compilePattern(expanded, &cbuf) )		/* syntax error */
        fail;
      dot = (expanded[0] == '.');			/* do dots as well */

#ifdef HAVE_OPENDIR
      { DIR *d;
	char tmp[MAXPATHLEN];
	struct dirent *e;

	d = opendir(OsPath(path, tmp));

	if ( d != NULL )
	{ for(e=readdir(d); e; e = readdir(d))
	  {
#ifdef __MSDOS__
	    strlwr(e->d_name);
#endif
	    if ( (dot || e->d_name[0] != '.') &&
		 matchPattern(e->d_name, &cbuf) )
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
#endif /*HAVE_OPENDIR*/

#if tos
      { char dpat[MAXPATHLEN];
	struct ffblk buf;
	int r;
	char tmp[MAXPATHLEN];

	strcpy(dpat, path);
	strcat(dpat, "\\*.*");

	DEBUG(2, Sdprintf("match path = %s\n", dpat));
	for( r=findfirst(OsPath(dpat, tmp), &buf, SUBDIR|HIDDEN);
	     r == 0;
	     r=findnext(&buf) )
	{ char *name = buf.ff_name;
	  strlwr(name);		/* match at lower case */

	  DEBUG(2, Sdprintf("found %s\n", name));
	  if ( (dot || name[0] != '.') && matchPattern(name, &cbuf) )
	  { strcpy(expanded, prefix);
	    strcat(expanded, name);
	    strcat(expanded, tail);

	    DEBUG(2, Sdprintf("%s matches pattern\n", name));
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
