/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#define O_EXPANDS_TESTS_EXISTS 1

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

typedef struct
{ int		size;
  matchcode	code[MAXCODE];
} compiled_pattern;

static char	*compile_pattern(compiled_pattern *, char *, int);
static bool	match_pattern(matchcode *, char *);

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


		 /*******************************
		 *	EXPAND_FILE_NAME/2	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Wildcart expansion of a pattern to a list   of files. This code uses two
`buffers'  for  storing  the   intermediate    results   while  limiting
fragmentation. The `strings' buffer contains  all strings generated. The
files contains indices in the `strings' buffer  pointing to the start of
strings.  The indices in the range [start,end) are valid.

First  this  set  is   filled   with    the   empty   string.  Next  the
directory-segment with the first wildcart is   located.  If found, we go
through the current set, adding the segments without wildcarts, applying
the wildcart on the directory and adding   everything  found to the set.
The old set is deleted by incrementing info.start.

If we are at the end, we add the remaining non-wildcart segments to each
element of the set, deleting it if the result does not exits.

Finally we sort the result.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ tmp_buffer	files;			/* our files */
  tmp_buffer	strings;		/* our strings */
  int		start;			/* 1-st valid entry of files */
  int		end;			/* last valid entry of files */
} glob_info, *GlobInfo;

#define isspecial(c) \
	((c) == '[' || (c) == '{' || (c) == '?' || (c) == '*')

static void
free_expand_info(GlobInfo info)
{ discardBuffer(&info->files);
  discardBuffer(&info->strings);
}


static void
add_path(const char *path, GlobInfo info)
{ int idx = entriesBuffer(&info->strings, char);
  int n = strlen(path)+1;

  addMultipleBuffer(&info->strings, path, n, char);
  addBuffer(&info->files, idx, int);
  info->end++;
}


const char *
expand_str(GlobInfo info, int at)
{ char *s = &fetchBuffer(&info->strings, at, char);

  return (const char *)s;
}


const char *
expand_entry(GlobInfo info, int idx)
{ int at = fetchBuffer(&info->files, idx, int);

  return expand_str(info, at);
}


static void
un_escape(char *to, const char *from, const char *end)
{ while( from < end )
  { if ( *from == '\\' && (isspecial(from[1]) || from[1] == '\\') )
      from++;
    *to++ = *from++;
  }
  *to = EOS;
}


static int
expand(const char *pattern, GlobInfo info)
{ const char *pat = pattern;
  compiled_pattern cbuf;
  char prefix[MAXPATHLEN];		/* before first pattern */
  char patbuf[MAXPATHLEN];		/* pattern buffer */
  int end, dot;

  initBuffer(&info->files);
  initBuffer(&info->strings);
  info->start = 0;
  info->end = 0;

  add_path("", info);

  for(;;)
  { const char *s = pat, *head = pat, *tail;

    for(;;)
    { int c;
  
      switch( (c=*s++) )
      { case EOS:
	  if ( s > pat )		/* something left and expanded */
	  { un_escape(prefix, pat, s);
	  
	    end = info->end;
	    for( ; info->start < end; info->start++ )
	    { char path[MAXPATHLEN];
	      int plen;
  
	      strcpy(path, expand_entry(info, info->start));
	      plen = strlen(path);
	      if ( prefix[0] && plen > 0 && path[plen-1] != '/' )
		path[plen++] = '/';
	      strcpy(&path[plen], prefix);
	      if ( end == 1 || AccessFile(path, ACCESS_EXIST) )
		add_path(path, info);
	    }
	  }
	  succeed;
	case '[':				/* meta characters: expand */
	case '{':
	case '?':
	case '*':
	  break;
	case '\\':
	  if ( isspecial(*s) )
	  { s++;
	    continue;
	  }
	/*FALLTHROUGH*/
	default:
	  if ( IS_DIR_SEPARATOR(c) )
	    head = s;
	  continue;
      }
      break;
    }
    
    for( tail=s; *tail && !IS_DIR_SEPARATOR(*tail); tail++ )
      ;
  
/*  By now, head points to the start of the path holding meta characters,
    while tail points to the tail:

    ..../meta*path/....
	 ^        ^
       head     tail
*/
    un_escape(prefix, pat, head);
    un_escape(patbuf, head, tail);
  
    if ( !compilePattern(patbuf, &cbuf) )		/* syntax error */
      fail;
    dot = (patbuf[0] == '.');			/* do dots as well */
  
    end = info->end;
  
    for(; info->start < end; info->start++)
    { DIR *d;
      struct dirent *e;
      char path[MAXPATHLEN];
      char tmp[MAXPATHLEN];
      const char *current = expand_entry(info, info->start);

      strcpy(path, current);
      strcat(path, prefix);
      
      if ( (d=opendir(path[0] ? OsPath(path, tmp) : ".")) )
      { int plen = strlen(path);

	if ( plen > 0 && path[plen-1] != '/' )
	  path[plen++] = '/';

	for(e=readdir(d); e; e = readdir(d))
	{
#ifdef __MSDOS__
	  strlwr(e->d_name);
#endif
	  if ( (dot || e->d_name[0] != '.') &&
	       matchPattern(e->d_name, &cbuf) )
	  { char newp[MAXPATHLEN];
  
	    strcpy(newp, path);
	    strcpy(&newp[plen], e->d_name);
/*	    if ( !tail[0] || ExistsDirectory(newp) )
	    Saves memory, but involves one more file-access
*/
	      add_path(newp, info);
	  }
	}
	closedir(d);
      }
    }

    pat = tail;
    if ( IS_DIR_SEPARATOR(*pat) )
      pat++;
  }
}


static int
compareBagEntries(const void *a1, const void *a2)
{ GlobInfo info = LD->glob_info;
  int i1 = *(int *)a1;
  int i2 = *(int *)a2;
  const char *s1, *s2;

  s1 = expand_str(info, i1);
  s2 = expand_str(info, i2);
  
  if ( trueFeature(FILE_CASE_FEATURE) )
    return strcmp(s1, s2);
  else
    return stricmp(s1, s2);
}


static void
sort_expand(GlobInfo info)
{ int *ip = &fetchBuffer(&info->files, info->start, int);
  int is = info->end - info->start;
  
  LD->glob_info = info;
  qsort(ip, is, sizeof(int), compareBagEntries);
}


word
pl_expand_file_name(term_t f, term_t list)
{ char spec[MAXPATHLEN];
  char *s;
  glob_info info;
  term_t l    = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  int i;

  if ( !PL_get_chars_ex(f, &s, CVT_ALL) )
    fail;
  if ( strlen(s) > MAXPATHLEN-1 )
    return PL_error(NULL, 0, "File name too long",
		    ERR_DOMAIN, ATOM_pattern, f);

  if ( !expandVars(s, spec) )
    fail;
  if ( !expand(spec, &info) )
    goto failout;
  sort_expand(&info);

  for( i = info.start; i< info.end; i++ )
  { const char *e = expand_entry(&info, i);

    if ( !PL_unify_list(l, head, l) ||
	 !PL_unify_atom_chars(head, e) )
      goto failout;
  }

  if ( !PL_unify_nil(l) )
  { failout:
    free_expand_info(&info);
    fail;
  }

  free_expand_info(&info);
  succeed;
}
