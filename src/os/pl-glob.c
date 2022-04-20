/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2020, University of Amsterdam
                              VU University Amsterdam
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

#ifdef __WINDOWS__
#include <mbstring.h>
#endif

#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include "../pl-fli.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef O_XOS
# include "windows/dirent.h"
#else
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
#endif /*O_XOS*/

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

#define CBASE	 0x80000000
#define ANY	 (CBASE+1)
#define STAR	 (CBASE+2)
#define ALT	 (CBASE+3)
#define JMP	 (CBASE+4)
#define ANYOF	 (CBASE+5)
#define ANYRANGE (CBASE+6)
#define EXIT	 (CBASE+7)

#define NOCURL	0
#define CURL	1

#define M_IGNCASE	0x01

typedef unsigned int matchcode;

typedef struct
{ tmp_buffer	pattern;
} compiled_pattern;

static const char *compile_pattern(compiled_pattern *pattern, const char *s,
				   int curl, int flags);
static int match_pattern(matchcode *pattern, const char *s, int flags);

#define Output(c)  addBuffer(&Out->pattern, c, matchcode)
#define Here()	   entriesBuffer(&Out->pattern, matchcode)
#define CodeAt(o)  (baseBuffer(&Out->pattern, matchcode)[o])

static void
initPattern(compiled_pattern *cbuf)
{ initBuffer(&cbuf->pattern);
}

static void
discardPattern(compiled_pattern *cbuf)
{ discardBuffer(&cbuf->pattern);
  initBuffer(&cbuf->pattern);
}

static int
compilePattern(char *p, compiled_pattern *cbuf, int flags)
{ initPattern(cbuf);

  if ( compile_pattern(cbuf, p, NOCURL, flags) )
    return TRUE;

  discardPattern(cbuf);
  return FALSE;
}

static int
utf8_peek(const char *p, int n)
{ int c;

  while(n-- > 0)
    p = utf8_skip_char(p);

  PL_utf8_code_point(&p, NULL, &c);

  return c;
}

static const char *
compile_pattern(compiled_pattern *Out, const char *p, int curl, int mflags)
{ int c;

  for(;;)
  { PL_utf8_code_point(&p, NULL, &c);

    switch(c)
    { case EOS:
	p--;
	break;
      case '\\':
      { int c2;

	PL_utf8_code_point(&p, NULL, &c2);
	Output(c2 == EOS ? c : c2);
	if ( c2 == EOS )
	  break;
	continue;
      }
      case '?':
	Output(ANY);
	continue;
      case '*':
	Output(STAR);
	continue;
      case '[':
      { size_t mapsize_at;

	Output(ANYOF);
	mapsize_at = Here();
	Output(0);

	for(;;)
	{ PL_utf8_code_point(&p, NULL, &c);

	  switch( c )
	  { case EOS:
	      return PL_syntax_error("Unmatched '['", NULL),NULL;
	    case '\\':
	    { int c2;

	      PL_utf8_code_point(&p, NULL, &c2);
	      if ( c2 == EOS )
		return PL_syntax_error("Unmatched '['", NULL),NULL;
	      Output(c2);
	      continue;
	    }
	    case ']':
	    { size_t sz = Here();

	      sz -= mapsize_at+1;
	      CodeAt(mapsize_at) = sz;
	      break;
	    }
	    default:
	    { int ce;

	      if ( c != ']' &&
		   utf8_peek(p, 0) == '-' &&
		   (ce=utf8_peek(p, 1)) != ']' )
	      { Output(ANYRANGE);
		Output(c);
		Output(ce);
		p = utf8_skip_char(p);
		p = utf8_skip_char(p);
	      } else
		Output(c);
	      continue;
	    }
	  }
	  break;
	}

	continue;
      }
      case '{':
      { int ai, aj = -1;

	for(;;)
	{ Output(ALT); ai = Here(); Output(0);
	  if ( !(p = compile_pattern(Out, p, CURL, mflags)) )
	    return NULL;
	  if ( aj > 0 )
	    CodeAt(aj) = Here() - aj;
	  if ( *p == ',' )
	  { Output(JMP); aj = Here(); Output(0);
	    CodeAt(ai) = Here() - ai;
	    Output(ALT); ai = Here(); Output(0);
	    p++;
	  } else if ( *p == '}' )
	  { p++;
	    break;
	  } else
	  { return PL_syntax_error("Unmatched '{'", NULL),NULL;
	  }
	}

	continue;
      }
      case ANY:
      case STAR:
      case ALT:
      case JMP:
      case ANYOF:
      case ANYRANGE:
      case EXIT:
	assert(0);
	return PL_error(NULL, 0, "Reserved character",
			ERR_REPRESENTATION, ATOM_pattern),NULL;
      case '}':
      case ',':
	if ( curl == CURL )
	{ p--;
	  return p;
	}
	/*FALLTHROUGH*/
      default:
	if ( (mflags&M_IGNCASE) )
	  c = makeLowerW(c);
	Output(c);
	continue;
    }

    Output(EXIT);
    return p;
  }
}


static int
matchPattern(char *s, compiled_pattern *cbuf, int flags)
{ return match_pattern(baseBuffer(&cbuf->pattern, matchcode), s, flags);
}

static bool
match_pattern(matchcode *p, const char *s, int flags)
{ matchcode c;

  for(;;)
  { switch( c = *p++ )
    { case EXIT:
	return (*s == EOS ? TRUE : FALSE);
      case ANY:						/* ? */
	if ( *s == EOS )
	  return FALSE;
	s = utf8_skip_char(s);
	continue;
      case ANYOF:					/* [...] */
      { int c2;
	int sz = *p++;
	matchcode *anyend = &p[sz];

	PL_utf8_code_point(&s, NULL, &c2);
	if ( (flags&M_IGNCASE) )
	  c2 = makeLowerW(c2);

	while(p<anyend)
	{ if ( *p == ANYRANGE )
	  { if ( c2 >= p[1] && c2 <= p[2] )
	      goto match;
	    p += 3;
	  } else
	  { if ( *p == c2 )
	      goto match;
	    p++;
	  }
	}
	return FALSE;

      match:
	p = anyend;
	continue;
      }
      case STAR:					/* * */
      { int c2;
	do
	{ if ( match_pattern(p, s, flags) )
	    return TRUE;
	  PL_utf8_code_point(&s, NULL, &c2);
	} while(c2);

	return FALSE;
      }
      case JMP:						/* { ... } */
	p += *p;
        continue;
      case ALT:
	if ( match_pattern(p+1, s, flags) )
	  return TRUE;
        p += *p;
	continue;
      default:						/* character */
      { int c2;

	PL_utf8_code_point(&s, NULL, &c2);
	if ( (flags&M_IGNCASE) )
	  c2 = makeLowerW(c2);

	if ( c == c2 )
	  continue;

	return FALSE;
      }
    }
  }
}


/** wildcard_match(+Pattern, +Name [, +Options]) is semidet.
*/

static const opt_spec wildcard_options[] =
{ { ATOM_case_sensitive,    OPT_BOOL },
  { NULL_ATOM,		    0 }
};


#define wildcard_match(pattern, string, options) LDFUNC(wildcard_match, pattern, string, options)
static int
wildcard_match(DECL_LD term_t pattern, term_t string, term_t options)
{ char *p, *s;
  int rc = FALSE;
  int mflags = 0;
  int case_sensitive = TRUE;

  if ( options &&
       !scan_options(options, 0, ATOM_wildcard_option,
		     wildcard_options, &case_sensitive) )
    return FALSE;
  if ( !case_sensitive )
    mflags |= M_IGNCASE;

  PL_STRINGS_MARK();
  if ( PL_get_chars(pattern, &p, CVT_ALL|REP_UTF8|CVT_EXCEPTION) &&
       PL_get_chars(string,  &s, CVT_ALL|REP_UTF8|CVT_EXCEPTION) )
  { compiled_pattern buf;

    if ( (rc=compilePattern(p, &buf, mflags)) )
    { rc = matchPattern(s, &buf, mflags);
      discardPattern(&buf);
    }
  }
  PL_STRINGS_RELEASE();

  return rc;
}

static
PRED_IMPL("wildcard_match", 2, wildcard_match, 0)
{ PRED_LD

  return wildcard_match(A1, A2, 0);
}

static
PRED_IMPL("wildcard_match", 3, wildcard_match, 0)
{ PRED_LD

  return wildcard_match(A1, A2, A3);
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
  compiled_pattern pattern;		/* Pattern buffer */
  int		start;			/* 1-st valid entry of files */
  int		end;			/* last valid entry of files */
} glob_info, *GlobInfo;

#undef isspecial			/* play safe */
#define isspecial(c) \
	((c) == '[' || (c) == '{' || (c) == '?' || (c) == '*')

static void
free_expand_info(GlobInfo info)
{ discardBuffer(&info->files);
  discardBuffer(&info->strings);
  discardPattern(&info->pattern);
}


#ifndef __WINDOWS__
#define mb_add_path(path, info) LDFUNC(mb_add_path, path, info)
static void
mb_add_path(DECL_LD const char *path, GlobInfo info)
{ int idx = (int)entriesBuffer(&info->strings, char);
  PL_chars_t txt;

  PL_STRINGS_MARK();
  txt.text.t    = (char*)path;
  txt.length    = strlen(path);
  txt.encoding  = ENC_ANSI;
  txt.storage   = PL_CHARS_HEAP;
  txt.canonical = FALSE;
  if ( PL_canonicalise_text(&txt) &&
       PL_mb_text(&txt, REP_UTF8) )
  { addMultipleBuffer(&info->strings, txt.text.t, txt.length+1, char);
    addBuffer(&info->files, idx, int);
    info->end++;
  } else
  { assert(0);
  }
  PL_free_text(&txt);
  PL_STRINGS_RELEASE();
}
#endif

static void
utf8_add_path(const char *path, GlobInfo info)
{ int idx = (int)entriesBuffer(&info->strings, char);
  size_t n = strlen(path)+1;

  addMultipleBuffer(&info->strings, path, n, char);
  addBuffer(&info->files, idx, int);
  info->end++;
}

static const char *
expand_str(GlobInfo info, int at)
{ char *s = &fetchBuffer(&info->strings, at, char);

  return (const char *)s;
}


static const char *
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


#define utf8_exists_file(name) LDFUNC(utf8_exists_file, name)
static int
utf8_exists_file(DECL_LD const char *name)
{
#ifndef __WINDOWS__
  PL_chars_t txt;
  int rc;

  PL_STRINGS_MARK();
  txt.text.t    = (char*)name;
  txt.length    = strlen(name);
  txt.encoding  = ENC_UTF8;
  txt.storage   = PL_CHARS_HEAP;
  txt.canonical = FALSE;
  rc = ( PL_canonicalise_text(&txt) &&
	 PL_mb_text(&txt, REP_FN) &&
	 AccessFile(txt.text.t, ACCESS_EXIST) );
  PL_free_text(&txt);
  PL_STRINGS_RELEASE();

  return rc;
#else
  return AccessFile(name, ACCESS_EXIST);
#endif
}

#define utf8_opendir(name) LDFUNC(utf8_opendir, name)
static DIR*
utf8_opendir(DECL_LD const char *name)
{
#ifndef __WINDOWS__
  PL_chars_t txt;
  DIR *rc;

  PL_STRINGS_MARK();
  txt.text.t    = (char*)name;
  txt.length    = strlen(name);
  txt.encoding  = ENC_UTF8;
  txt.storage   = PL_CHARS_HEAP;
  txt.canonical = FALSE;
  if ( PL_canonicalise_text(&txt) &&
       PL_mb_text(&txt, REP_FN) )
    rc = opendir(txt.text.t);
  else
    rc = NULL;
  PL_free_text(&txt);
  PL_STRINGS_RELEASE();

  return rc;
#else
  return opendir(name);
#endif
}

static int
expand(const char *pattern, GlobInfo info)
{ GET_LD
  const char *pat = pattern;
  char prefix[PATH_MAX];		/* before first pattern */
  char patbuf[PATH_MAX];		/* pattern buffer */
  size_t prefix_len;
  int end, dot;
  int mflags = 0;

  if ( !truePrologFlag(PLFLAG_FILE_CASE) )
    mflags |= M_IGNCASE;

  initBuffer(&info->files);
  initBuffer(&info->strings);
  initPattern(&info->pattern);
  info->start = 0;
  info->end = 0;

  utf8_add_path("", info);

  for(;;)
  { const char *s = pat, *head = pat, *tail;

    for(;;)
    { int c;

      switch( (c=*s++) )
      { case EOS:
	  if ( s > pat )		/* something left and expanded */
	  { size_t prefix_len;

	    un_escape(prefix, pat, s);
	    prefix_len = strlen(prefix);

	    end = info->end;
	    for( ; info->start < end; info->start++ )
	    { char path[PATH_MAX];
	      const char *entry = expand_entry(info, info->start);
	      size_t plen = strlen(entry);

	      if ( plen+prefix_len+2 <= PATH_MAX )
	      { strcpy(path, entry);
		if ( prefix[0] && plen > 0 && path[plen-1] != '/' )
		  path[plen++] = '/';
		strcpy(&path[plen], prefix);
		if ( end == 1 || utf8_exists_file(path) )
		  utf8_add_path(path, info);
	      }
	    }
	  }
	  return TRUE;
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
    prefix_len = strlen(prefix);

    discardPattern(&info->pattern);
    if ( !compilePattern(patbuf, &info->pattern, mflags) ) /* syntax error */
      return FALSE;
    dot = (patbuf[0] == '.');			/* do dots as well */

    end = info->end;

    for(; info->start < end; info->start++)
    { DIR *d;
      struct dirent *e;
      char path[PATH_MAX];
      char tmp[PATH_MAX];
      const char *current = expand_entry(info, info->start);
      size_t clen = strlen(current);

      if ( clen+prefix_len+1 > sizeof(path) )
	continue;

      strcpy(path, current);
      strcpy(&path[clen], prefix);

      if ( (d=utf8_opendir((path[0] ? OsPath(path, tmp) : "."))) )
      { size_t plen = clen+prefix_len;

	if ( plen > 0 && path[plen-1] != '/' )
	  path[plen++] = '/';

	for(e=readdir(d); e; e = readdir(d))
	{
#ifdef __MSDOS__
	  strlwr(e->d_name);
#endif
	  if ( (dot || e->d_name[0] != '.') &&
	       matchPattern(e->d_name, &info->pattern, mflags) )
	  { char newp[PATH_MAX];

	    if ( plen+strlen(e->d_name)+1 < sizeof(newp) )
	    { strcpy(newp, path);
	      strcpy(&newp[plen], e->d_name);
#ifdef __WINDOWS__			/* readdir() emulation emits UTF-8 */
	      utf8_add_path(newp, info);
#else
	      mb_add_path(newp, info);
#endif
	    }
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
{ GET_LD
  GlobInfo info = LD->glob_info;
  int i1 = *(int *)a1;
  int i2 = *(int *)a2;
  const char *s1, *s2;

  s1 = expand_str(info, i1);
  s2 = expand_str(info, i2);

  if ( truePrologFlag(PLFLAG_FILE_CASE) )
    return mbscoll(s1, s2);
  else
    return mbscasecoll(s1, s2);
}


static void
sort_expand(GlobInfo info)
{ GET_LD
  int *ip = &fetchBuffer(&info->files, info->start, int);
  int is = info->end - info->start;

  LD->glob_info = info;
  qsort(ip, is, sizeof(int), compareBagEntries);
}


static
PRED_IMPL("expand_file_name", 2, expand_file_name, 0)
{ PRED_LD
  char spec[PATH_MAX];
  char *s;
  glob_info info;
  term_t l    = PL_copy_term_ref(A2);
  term_t head = PL_new_term_ref();
  int i;

  if ( !PL_get_chars(A1, &s, CVT_ALL|REP_UTF8|CVT_EXCEPTION) )
    fail;
  if ( strlen(s) > sizeof(spec)-1 )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
		    ATOM_max_path_length);

  if ( !expandVars(s, spec, sizeof(spec)) )
    fail;
  if ( !expand(spec, &info) )
    goto failout;
  sort_expand(&info);

  for( i = info.start; i< info.end; i++ )
  { const char *e = expand_entry(&info, i);

    if ( !PL_unify_list(l, head, l) ||
	 !PL_unify_chars(head, PL_ATOM|REP_UTF8, -1, e) )
      goto failout;
  }

  if ( !PL_unify_nil(l) )
  { failout:
    free_expand_info(&info);
    return FALSE;
  }

  free_expand_info(&info);
  return TRUE;
}


/** directory_files(+Dir, -Files) is det.

Files is a list of atoms that describe the entries in Dir.
*/

static
PRED_IMPL("directory_files", 2, directory_files, 0)
{ PRED_LD
  char *dname;
  DIR *dir;

  if ( !PL_get_file_name(A1, &dname, PL_FILE_READ|PL_FILE_OSPATH) )
    return FALSE;

  if ( (dir=opendir(dname)) )
  { struct dirent *e;
    term_t tail = PL_copy_term_ref(A2);
    term_t head = PL_new_term_ref();

    for(e=readdir(dir); e; e = readdir(dir))
    { PL_put_variable(head);
      if ( PL_handle_signals() < 0 ||
	   !PL_unify_list(tail, head, tail) ||
	   !PL_unify_chars(head, PL_ATOM|REP_FN, (size_t)-1, e->d_name) )
      { closedir(dir);
	return FALSE;
      }
    }
    closedir(dir);

    return PL_unify_nil(tail);
  }

  return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		  ATOM_open, ATOM_directory, A1);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(glob)
  PRED_DEF("expand_file_name", 2, expand_file_name, 0)
  PRED_DEF("wildcard_match",   2, wildcard_match,   0)
  PRED_DEF("wildcard_match",   3, wildcard_match,   0)
  PRED_DEF("directory_files",  2, directory_files,  0)
EndPredDefs
