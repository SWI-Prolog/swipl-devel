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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the source-file for  plld,   the  SWI-Prolog linker for embedded
applications. See plld(1) for details.  Feel   free  to comment and send
contributions.

The    file    pl-extend.c,    copied    by    the    installation    to
<home>/include/stub.c  contains  a  minimal  skeleton  for  creating  an
embedded application.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32
#include <process.h>
#include <io.h>

#define popen _popen
#define pclose _pclose
#define O_WRONLY _O_WRONLY
#define O_RDONLY _O_RDONLY
#define O_CREAT _O_CREAT
#define O_TRUNC _O_TRUNC
#define O_BINARY _O_BINARY

#ifndef PROG_PL
#define PROG_PL "plcon.exe"
#endif
#define PROG_LD "link.exe"
#define SO_LD "link.exe"
#define PROG_CC "cl.exe"
#define PROG_CXX "cl.exe"
#define PROG_OUT "plout.exe"
#define PROG_CPP "cl.exe -P"
#define LIB_PL	 "libpl.lib"
#define LIB_PLMT "libplmt.lib"
#define LIB_PL_DEBUG "libplD.lib"
#define EXT_OBJ "obj"
#define OPT_DEBUG "/DEBUG"
#else /*WIN32*/
#include "pl-incl.h"

#ifndef PROG_PL
#define PROG_PL "pl"
#endif
#define PROG_CC "cc"
#define PROG_CXX "c++"
#define PROG_OUT "a.out"
#define EXT_OBJ "o"
#define LIB_PL	"-lpl"
#define LIB_PLMT "-lplmt"
#define OPT_DEBUG "-g"

#ifndef SO_LDFLAGS
#define SO_LDFLAGS "-shared"
#endif

#endif /*WIN32*/

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#else
#ifdef HAVE_SYS_MALLOC_H
#include <sys/malloc.h>
#endif
#endif
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <signal.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#define CPBUFSIZE 8192

#ifndef streq
#define streq(s, q)     (strcmp((s), (q)) == 0)
#endif
#define strprefix(s, p) (strncmp((s), (p), strlen(p)) == 0)
#ifdef WIN32
#define strfeq(s, q)	(stricmp((s), (q)) == 0)
#else
#define strfeq(s, q)	streq(s, q)
#endif
#undef oserror				/* Irix name-clash */
#define oserror xoserror
#undef strdup
#define strdup plld_strdup
#undef strndup
#define strndup plld_strndup
#ifdef HAVE_DMALLOC_H
#undef xmalloc
#undef xrealloc
#undef xfree
#endif
#define xmalloc plld_xmalloc
#define xrealloc plld_xrealloc
#define xfree plld_xfree
#define CTOI(c) ((c)&0xff)

typedef struct
{ char **list;
  int  size;
} arglist;

static arglist tmpfiles;		/* list of temporary files */

static arglist ofiles;			/* object files */
static arglist cfiles;			/* C input files */
static arglist cppfiles;		/* C++ input files */
static arglist plfiles;			/* Prolog files */
static arglist qlfiles;			/* Prolog Quick Load Files */

static arglist coptions;		/* CC options */
static arglist cppoptions;		/* C++ options */
static arglist ldoptions;		/* LD options */
static arglist ploptions;		/* PL options for saved state */

static arglist libs;			/* (C) libraries */
static arglist lastlibs;		/* libs that must be at the end */
static arglist libdirs;			/* -L library directories */
static arglist includedirs;		/* -I include directories */

static char *pllib = LIB_PL;    /* libpl option -lpl, libpl.lib or libplD.lib */

static char *pl;			/* Prolog executable */
static char *cc;			/* CC executable */
static char *cxx;			/* C++ executable */
static char *ld;			/* The Linker */
static char *plld;			/* Thats me! */

static char *plbase;			/* Prolog home */
static char *plarch;			/* Prolog architecture id */
static char *plexe;			/* Path to the executable */

static char *plgoal;			/* -g goal */
static char *pltoplevel;		/* -t goal */
static char *plinitfile;		/* -f file */
static char *plclass;			/* -class <class> */
static char *plsysinit;			/* -F file */

static char *ctmp;			/* base executable */
static char *pltmp;			/* base saved state */
static char *out;			/* final output */

static int nostate = FALSE;		/* do not make a state */
static int nolink = FALSE;		/* do not link */
static int shared = FALSE;		/* -shared: make a shared-object/DLL */
static char *soext;			/* extension of shared object */

static int verbose = TRUE;		/* verbose operation */
static int fake = FALSE;		/* don't really do anything */

static void	removeTempFiles();
static void	parseOptions(int argc, char **argv);


		 /*******************************
		 *	       ERROR		*
		 *******************************/

static char *
oserror()
{
#ifdef HAVE_STRERROR
  return strerror(errno);
#else
  extern int sys_nerr;
  extern char *sys_errlist[];
  extern int errno;

  if ( errno < sys_nerr )
    return sys_errlist[errno];

  return "Unknown error";
#endif
}


static int
error(int status)
{ removeTempFiles();

  fprintf(stderr, "*** %s exit status %d\n", plld, status);

  exit(status);
  return 1;				/* not reached */
}


static void
catched_signal(int sig)
{ error(sig);
}


		 /*******************************
		 *	       MALLOC		*
		 *******************************/

static void
xfree(void *mem)
{ if ( mem )
    free(mem);
}


void *
xmalloc(size_t bytes)
{ void *mem;

  if ( !bytes )
    return NULL;
  if ( (mem = malloc(bytes)))
    return mem;

  fprintf(stderr, "%s: not enough memory\n", plld);
  error(1);
  return NULL;
}


void *
xrealloc(void *old, size_t bytes)
{ void *mem;

  if ( !bytes )
  { xfree(old);
    return NULL;
  }
  if ( !old )
  { if ( (mem = malloc(bytes)))
      return mem;
  } else
  { if ( (mem = realloc(old, bytes)))
      return mem;
  }

  fprintf(stderr, "%s: not enough memory\n", plld);
  error(1);
  return NULL;
}



		 /*******************************
		 *	TEXT MANIPULATION	*
		 *******************************/

static char *
strdup(const char *in)
{ return strcpy(xmalloc(strlen(in)+1), in);
}


static char *
strndup(const char *in, int len)
{ char *r = xmalloc(len+1);
 
  r[len] = '\0'; 

  return memcpy(r, in, len);
}


void
appendArgList(arglist *list, const char *arg)
{ if ( list->size == 0 )
  { list->list = xmalloc(sizeof(char*) * (list->size+2));
  } else
  { list->list = xrealloc(list->list, sizeof(char*) * (list->size+2));
  }

  list->list[list->size++] = strdup(arg);
  list->list[list->size]   = NULL;
}


void
prependArgList(arglist *list, const char *arg)
{ int n;

  if ( list->size == 0 )
  { list->list = xmalloc(sizeof(char*) * (list->size+2));
  } else
  { list->list = xrealloc(list->list, sizeof(char*) * (list->size+2));
  }
  for(n=++list->size; n>0; n--)
    list->list[n] = list->list[n-1];
  
  list->list[0] = strdup(arg);
}


void
concatArgList(arglist *to, const char *prefix, arglist *from)
{ int n;

  for(n=0; n<from->size; n++)
  { char buf[1024];

    if ( strchr(from->list[n], ' ') )
      sprintf(buf, "%s\"%s\"", prefix, from->list[n]);
    else
      sprintf(buf, "%s%s", prefix, from->list[n]);
    appendArgList(to, buf);
  }
}


static int
breakargs(const char *line, char **argv)
{ int argc = 0;

  while(*line)
  { while(*line && isspace(CTOI(*line)))
      line++;

    if ( *line == '"' )			/* Windows-95 quoted arguments */
    { const char *start = line+1;
      const char *end = start;

      while( *end && *end != '"' )
	end++;
      if ( *end == '"' )
      { argv[argc++] = strndup(start, end-start);
	line = end+1;
	continue;
      }
    }

    if ( *line )
    { const char *start = line;

      while(*line && !isspace(CTOI(*line)))
	line++;
      argv[argc++] = strndup(start, line-start);
    }
  }
  argv[argc] = NULL;			/* add trailing NULL pointer to argv */

  return argc;
}      



void
addOptionString(const char *s)
{ char *argv[256];
  int argc = breakargs(s, argv);

  parseOptions(argc, argv);
}


void
appendOptions(arglist *args, const char *from)
{ int sep = *from++;
  const char *f;
  char tmp[1024];

  while(*from)
  { f = from;
    while(*from && *from != sep)
      from++;
    if ( from > f )
    { strncpy(tmp, f, from-f);
      tmp[from-f] = '\0';
      appendArgList(args, tmp);
    }
    if ( *from == sep )
      from++;
  }
}


void
ensureOption(arglist *args, const char *opt)
{ int n;

  for(n=0; n<args->size; n++)
  { if ( streq(args->list[n], opt) )
      return;
  }

  appendArgList(args, opt);
}


arglist *
copyArgList(arglist *in)
{ arglist *out = xmalloc(sizeof(arglist));
  int n;

  out->size = in->size;
  out->list = xmalloc(sizeof(char *) * (in->size + 1));
  for(n=0; n<in->size; n++)
    out->list[n] = strdup(in->list[n]);
  out->list[n] = NULL;

  return out;
}

void
freeArgList(arglist *l)
{ int n;

  for(n=0; n<l->size; n++)
    xfree(l->list[n]);

  xfree(l);
}  


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If the filename has an extension, replace it, otherwise add the given
extension.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
replaceExtension(const char *base, const char *ext, char *buf)
{ char *e = NULL, *q = buf;
  const char *s = base;

  for( ; *s; s++, q++ )
  { *q = *s;
    if ( *q == '.' )
      e = q;
    else if ( *q == '/' || *q == '\\' )
      e = NULL;
  }
  *q = '\0';

  if ( e )
    e++;
  else
  { e = q + strlen(q);
    *e++ = '.';
  }
  
  strcpy(e, ext);

  return buf;
}



		 /*******************************
		 *	   OPTION DEFS		*
		 *******************************/

typedef struct
{ char *extension;
  arglist*list;
} extdef;

static extdef extdefs[] =
{ { EXT_OBJ,	&ofiles },
#ifdef WIN32
  { "lib",	&libs },
#else
  { "a",	&ofiles },
#endif
  { "c",	&cfiles },
  { "cpp",	&cppfiles },
  { "cxx",	&cppfiles },
  { "cc",	&cppfiles },
#ifndef WIN32
  { "C",	&cppfiles },
#endif
  { "pl",	&plfiles },
  { "qlf",	&qlfiles },
  { NULL,	NULL }
};

const char *
file_name_extension(const char *in)
{ const char *ext = NULL;
  
  for( ; *in; in++)
  { if ( *in == '.' )
      ext = in+1;
    else if ( *in == '/' || *in == '\\' )
      ext = NULL;
  }

  return ext;
}


int
dispatchFile(const char *name)
{ const char *ext;

  if ( (ext = file_name_extension(name)) )
  { extdef *d = extdefs;

    for( ; d->extension; d++ )
    { if ( strfeq(d->extension, ext) )
      { appendArgList(d->list, name);
	return TRUE;
      }
    }
  }
  
  return FALSE;
}


		 /*******************************
		 *	  OPTION PARSING	*
		 *******************************/

static void
usage()
{ fprintf(stderr,
	  "usage: %s -help\n"
	  "       %s [options] inputfile ...\n"
	  "       %s -E cppargument ...\n"
	  "\n"
	  "options:\n"
	  "       -o out           define output file\n"
	  "\n"
	  "       -v               verbose\n"
	  "       -f               fake (do not run any commands)\n"
	  "       -g               Compile/link for debugging\n"
	  "\n"
	  "       -pl prolog       Prolog to use\n"
	  "       -ld linker       link editor to use\n"
          "       -cc compiler     compiler for C source files\n"
	  "       -c++ compiler    compiler for C++ source files\n"
	  "\n"
	  "       -c               only compile C/C++ files, do not link\n"
	  "       -nostate         just relink the kernel\n"
	  "       -shared          link to shared object or DLL\n"
	  "       -fpic            compile small position-independent code\n"
	  "       -fPIC            compile large position-independent code\n"
	  "\n"
	  "       -pl-options,...  Add options for Prolog\n"
	  "       -ld-options,...  Add options for linker\n"
	  "       -cc-options,...  Add options for C/C++-compiler\n"
	  "       -F base          Load	swi(base.rc)"
	  "\n"
	  "       -goal goal       (Prolog) entry point\n"
	  "       -toplevel goal   (Prolog) abort toplevel goal\n"
	  "       -initfile file   (Prolog) profile file to load\n"
	  "       -class class     {runtime,kernel,development}\n"
	  "\n"
	  "       -O*              Optimization passed to compiler\n"
	  "       -W*              Warning options passed to compiler\n"
	  "\n"
	  "       -Dmacro          Define macro (C/C++)\n"
	  "       -Umacro          Undefine macro (C/C++)\n"
	  "       -Iincludedir     Include directory (C/C++)\n"
	  "       -Llibdir         Library directory (C/C++ link)\n"
	  "       -llib            library (C/C++)\n",
	plld, plld, plld);

  exit(1);
}


static void
parseOptions(int argc, char **argv)
{ for( ; argc > 0; argc--, argv++ )
  { char *opt = argv[0];
    
    if ( dispatchFile(opt) )
      continue;

    if ( streq(opt, "-help") )			/* -help */
    { usage();
    } else if ( streq(opt, "-v") )		/* -v */
    { verbose++;
    } else if ( streq(opt, "-f") )		/* -f */
    { fake++;
    } else if ( streq(opt, "-c") )		/* -c */
    { nolink++;
    } else if ( streq(opt, "-g") )		/* -g */
    { appendArgList(&coptions, OPT_DEBUG);
      appendArgList(&cppoptions, OPT_DEBUG);
#ifdef WIN32					/* MSVC DEBUG OPTIONS */
      appendArgList(&coptions, "/ZI");
      appendArgList(&coptions, "/Od");
      appendArgList(&cppoptions, "/ZI");
      appendArgList(&cppoptions, "/Od");
#endif
      appendArgList(&ldoptions, OPT_DEBUG);
#ifdef WIN32
      pllib = LIB_PL_DEBUG;
#endif
    } else if ( strprefix(opt, "-O") )		/* -O* */
    { appendArgList(&coptions, opt);
      appendArgList(&cppoptions, opt);
    } else if ( strprefix(opt, "-W") )		/* -W* */
    { appendArgList(&coptions, opt);
      appendArgList(&cppoptions, opt);
    } else if ( streq(opt, "-nostate") ) 	/* -nostate */
    { nostate = TRUE;
    } else if ( streq(opt, "-shared") )		/* -shared */
    { shared = TRUE;
      nostate = TRUE;
#ifdef SO_pic
      appendArgList(&coptions, SO_pic);
      appendArgList(&cppoptions, SO_pic);
#endif
    } else if ( streq(opt, "-SHARED") )		/* -SHARED */
    { shared = TRUE;
      nostate = TRUE;
#ifdef SO_PIC
      appendArgList(&coptions, SO_PIC);
      appendArgList(&cppoptions, SO_PIC);
#else
#ifdef SO_pic
      appendArgList(&coptions, SO_pic);
      appendArgList(&cppoptions, SO_pic);
#endif
#endif
    } else if ( streq(opt, "-fpic") )		/* -fpic */
    {
#ifdef SO_pic
      appendArgList(&coptions, SO_pic);
      appendArgList(&cppoptions, SO_pic);
#endif
    } else if ( streq(opt, "-fPIC") )		/* -fPIC */
    {
#ifdef SO_PIC
      appendArgList(&coptions, SO_PIC);
      appendArgList(&cppoptions, SO_PIC);
#endif
    } else if ( streq(opt, "-o") ) 		/* -o out */
    { if ( argc > 1 )
      { out = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-goal") ) 		/* -goal goal */
    { if ( argc > 1 )
      { plgoal = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-toplevel") ) 	/* -toplevel goal */
    { if ( argc > 1 )
      { pltoplevel = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-initfile") ) 	/* -initfile goal */
    { if ( argc > 1 )
      { plinitfile = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-F") ) 		/* -F base */
    { if ( argc > 1 )
      { plsysinit = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-class") ) 		/* -class runtime,kernel,
    							  development */
    { if ( argc > 1 )
      { plclass = argv[1];
	if ( !streq(plclass, "runtime") &&
	     !streq(plclass, "kernel") &&
	     !streq(plclass, "development")
	   )
	  usage();
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-pl") ) 		/* -pl prolog */
    { if ( argc > 1 )
      { pl = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-cc") ) 		/* -cc compiler */
    { if ( argc > 1 )
      { cc = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-c++") ) 		/* -c++ compiler */
    { if ( argc > 1 )
      { cxx = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( streq(opt, "-ld") ) 		/* -ld linker */
    { if ( argc > 1 )
      { ld = argv[1];
	argc--, argv++;
      } else
	usage();
    } else if ( strprefix(opt, "-cc-options") )
    { appendOptions(&coptions, opt+strlen("-cc-options"));
      appendOptions(&cppoptions, opt+strlen("-cc-options"));
    } else if ( strprefix(opt, "-ld-options") )
    { appendOptions(&ldoptions, opt+strlen("-ld-options"));
    } else if ( strprefix(opt, "-pl-options") )
    { appendOptions(&ploptions, opt+strlen("-pl-options"));
    } else if ( strprefix(opt, "-I") )		/* -I<include> */
    { appendArgList(&includedirs, &opt[2]);
    } else if ( strprefix(opt, "-D") )		/* -D<def> */
    { appendArgList(&coptions, opt);
      appendArgList(&cppoptions, opt);
    } else if ( strprefix(opt, "-U") )		/* -U<def> */
    { appendArgList(&coptions, opt);
      appendArgList(&cppoptions, opt);
    } else if ( strprefix(opt, "-L") )		/* -L<libdir> */
    { appendArgList(&libdirs, &opt[2]);
    } else if ( streq(opt, "-lccmalloc") )	/* -lccmalloc */
    { appendArgList(&lastlibs, &opt[2]);
    } else if ( strprefix(opt, "-l") )		/* -l<lib> */
    { appendArgList(&libs, &opt[2]);
    }
  }
}


static void
defaultProgram(char **store, char *def)
{ if ( !*store )
    *store = strdup(def);
}


static void
defaultPath(char **store, char *def)
{ if ( !*store )
  { char *s, *e;

    s = strdup(def);
    e = s + strlen(s);
    while(e>s+1 && e[-1] == '/')	/* strip terminating /'s */
      e--;
    *e = '\0';

    *store = s;
  }
}


static void
tmpPath(char **store, const char *base)
{ if ( !*store )
  { char tmp[MAXPATHLEN];

    sprintf(tmp, "%s%d", base, (int)getpid());
    *store = strdup(tmp);
  }
}


static void
fillDefaultOptions()
{ char tmp[1024];
  char *defcxx = PROG_CXX;

  defaultProgram(&cc,  PROG_CC);
  if ( streq(cc, "gcc") )
    defcxx = "g++";
  defaultProgram(&cxx, defcxx);

  if ( !ld )				/* not specified */
  { if ( shared )
    {
#ifndef SO_LD
       ld = (cppfiles.size > 0 ? cxx : cc);
#else
       ld = SO_LD;
#endif
    } else
#ifndef PROG_LD
       ld = (cppfiles.size > 0 ? cxx : cc);
#else
       ld = PROG_LD;
#endif
  }

#ifdef WIN32
  if (strcmp(LIB_PL_DEBUG,pllib) == 0) ensureOption(&coptions, "/MDd");
  else ensureOption(&coptions, "/MD");
  ensureOption(&coptions, "/DWIN32");
  ensureOption(&coptions, "/nologo");
  ensureOption(&ldoptions, "/nologo");
#endif

  tmpPath(&ctmp,   "ctmp-");
  tmpPath(&pltmp,  "pltmp-");
#if defined(__CYGWIN32__)
/* Compile generates .exe files on CygWin */
  replaceExtension(ctmp, "exe", tmp);
  free(ctmp);
  ctmp = strdup(tmp);
#endif
#if defined(WIN32) || defined(__CYGWIN32__)
/* Saved states have the .exe extension under Windows */
  replaceExtension(pltmp, "exe", tmp);
  free(pltmp);
  pltmp = strdup(tmp);
#endif
  if ( shared && !out && !nolink )
  { fprintf(stderr, "%s: \"-o out\" required for linking shared object\n", plld);
    exit(1);
  }
  defaultPath(&out, PROG_OUT);

  defaultProgram(&plgoal,     "$welcome");
  defaultProgram(&pltoplevel, "prolog");
  defaultProgram(&plinitfile, "none");
  defaultProgram(&plsysinit,  "none");

#ifdef WIN32
  sprintf(tmp, "%s/lib", plbase);
#else
  sprintf(tmp, "%s/runtime/%s", plbase, plarch);
#endif
  prependArgList(&libdirs, tmp);
  sprintf(tmp, "%s/include", plbase);
  prependArgList(&includedirs, tmp);
}

		 /*******************************
		 *	   PROLOG OPTIONS	*
		 *******************************/
static void
getPrologOptions()
{ FILE *fd;
  char cmd[512];

  sprintf(cmd, "%s -dump-runtime-variables", pl);
  if ( verbose )
    printf("\teval `%s`\n", cmd);

  if ( (fd = popen(cmd, "r")) )
  { char buf[256];

    while( fgets(buf, sizeof(buf), fd) )
    { char name[100];
      char value[256];
      char *v;

      if ( sscanf(buf, "%[^=]=%[^;\n]", name, value) == 2 )
      { v = value;
	if ( *v == '"' )
	{ char *e = ++v;

	  while(*e && *e != '"')
	    e++;
	  while(e>v && isspace(CTOI(e[-1])))
	    e--;
	  *e = '\0';
	}
	if ( streq(name, "CC") )
	  defaultProgram(&cc, v);
	else if ( streq(name, "PLBASE") )
	  defaultPath(&plbase, v);
	else if ( streq(name, "PLARCH") )
	  defaultPath(&plarch, v);
	else if ( streq(name, "PLLIBS") && !shared )
	  addOptionString(v);
	else if ( streq(name, "PLLDFLAGS") && !shared )
	  appendArgList(&ldoptions, v);
	else if ( streq(name, "PLSOEXT") )
	  soext = strdup(v);
	else if ( streq(name, "PLTHREADS") && streq(v, "yes") )
	{ ensureOption(&coptions, "-D_REENTRANT");
	  ensureOption(&cppoptions, "-D_REENTRANT");
	  pllib = LIB_PLMT;
	} else
	  continue;

	if ( verbose )
	  printf("\t\t%s=\"%s\"\n", name, v);
      }	
    }

    pclose(fd);

#ifdef WIN32
    sprintf(buf, "%s/bin/%s", plbase, PROLOG);
#else
    sprintf(buf, "%s/bin/%s/%s", plbase, plarch, PROG_PL);
#endif
    defaultPath(&plexe, buf);
  } else
  { fprintf(stderr, "%s: failed to run %s: %s", plld, cmd, oserror());
    error(1);
  }
}

		 /*******************************
		 *	     CALLING		*
		 *******************************/

char *
shell_quote(char *to, const char *arg)
{ static const char needq[] = "#!|<>*?$'";
  const char *s;
  int needquote = FALSE;

  for(s=arg; *s; s++)
  { if ( strchr(needq, *s) )
    { needquote = TRUE;
      break;
    }
  }

  if ( needquote )
  { *to++ = '"';
    for(s=arg; *s; s++)
    { if ( *s == '"' || *s == '$' )
	*to++ = '\\';
      *to++ = *s;
    }
    *to++ = '"';
    *to = '\0';
    
    return to;
  }

  strcpy(to, arg);

  return to+strlen(to);
}



static void
callprog(const char *ld, arglist *args)
{ char cmd[10240];
  char *e = cmd;
  int n, status;

  strcpy(e, ld);
  e = &e[strlen(e)];
  for(n=0; n<args->size; n++)
  { *e++ = ' ';
    e = shell_quote(e, args->list[n]);
  }

  if ( verbose )
    printf("\t%s\n", cmd);

  if ( !fake )
  { if ( (status=system(cmd)) != 0 )
    { fprintf(stderr, "%s returned code %d\n", ld, status);
      error(1);
    }
  }
}


		 /*******************************
		 *	    PROCESSING		*
		 *******************************/

static void
compileFile(const char *compiler, arglist *options, const char *cfile)
{ char ofile[MAXPATHLEN];
  char *ext;
  arglist *args = copyArgList(options);

  strcpy(ofile, cfile);
  if ( (ext = (char *)file_name_extension(ofile)) )
    strcpy(ext, EXT_OBJ);

  prependArgList(args, "-c");
  appendArgList(args, "-D__SWI_PROLOG__");
  if ( !shared )
    appendArgList(args, "-D__SWI_EMBEDDED__");
  concatArgList(args, "-I", &includedirs);

  appendArgList(args, "-o");
  appendArgList(args, ofile);
  appendArgList(args, cfile);

  callprog(compiler, args);
  appendArgList(&ofiles, ofile);
  if ( !nolink )
    appendArgList(&tmpfiles, ofile);
  freeArgList(args);
}


void
compileObjectFiles()
{ int n;

  for(n=0; n<cfiles.size; n++)
    compileFile(cc, &coptions, cfiles.list[n]);
  for(n=0; n<cppfiles.size; n++)
    compileFile(cxx, &cppoptions, cppfiles.list[n]);
}

#ifdef WIN32
char *
os_path(char *out, const char *in)
{ for(; *in; in++)
  { if ( *in == '/' )
      *out++ = '\\';
    else
      *out++ = *in;
  }
  *out = '\0';

  return out;
}

void
exportlibdirs()
{ char tmp[10240];
  char *s, *e;
  int n;

  strcpy(tmp, "LIB=");
  e = tmp + strlen(tmp);

  for(n=0; n<libdirs.size; n++)
  { e = os_path(e, libdirs.list[n]);
    *e++ = ';';
  }
  if ( (s = getenv("LIB")) )
    strcpy(e, s);

  if ( verbose )
    printf("\t%s\n", tmp);

  putenv(strdup(tmp));
}
#endif

void
linkBaseExecutable()
{ char *cout = (nostate ? out : ctmp);

#ifdef WIN32
{ char tmp[MAXPATHLEN];
  sprintf(tmp, "/out:%s", cout);
  prependArgList(&ldoptions, tmp);
}
  concatArgList(&ldoptions, "", &ofiles);	/* object files */
  exportlibdirs();
  appendArgList(&ldoptions, pllib);		/* -lpl */
  concatArgList(&ldoptions, "", &libs);		/* libraries */
  concatArgList(&ldoptions, "", &lastlibs);	/* libraries */
#else /*WIN32*/
  prependArgList(&ldoptions, cout);
  prependArgList(&ldoptions, "-o");		/* -o ctmp */
  concatArgList(&ldoptions, "", &ofiles);	/* object files */
  concatArgList(&ldoptions, "-L", &libdirs);    /* library directories */
  appendArgList(&ldoptions, pllib);		/* -lpl */
  concatArgList(&ldoptions, "-l", &libs);	/* libraries */
  concatArgList(&ldoptions, "-l", &lastlibs);	/* libraries */
#endif

  if ( !nostate )
  { appendArgList(&tmpfiles, ctmp);	/* register for deletion */
#ifdef WIN32
    {					/* schedule .exp file for deletion */
      char buf[MAXPATHLEN];
      appendArgList(&tmpfiles, replaceExtension(ctmp, "exp", buf));
      appendArgList(&tmpfiles, replaceExtension(ctmp, "lib", buf));
    }
#endif
  }

  callprog(ld, &ldoptions);
}


void
linkSharedObject()
{ char soname[MAXPATHLEN];
  char *soout;

  if ( !soext )
    soext = "so";			/* or give error? */

  if ( file_name_extension(out) )
  { soout = out;
  } else
  { soout = replaceExtension(out, soext, soname);
  }

#ifdef WIN32
  prependArgList(&ldoptions, "/dll");
{ char tmp[MAXPATHLEN];
  sprintf(tmp, "/out:%s", soout);
  prependArgList(&ldoptions, tmp);
}
  concatArgList(&ldoptions, "", &ofiles);	/* object files */
  exportlibdirs();
  appendArgList(&ldoptions, pllib);		/* libpl.lib */
  concatArgList(&ldoptions, "", &libs);		/* libraries */
  concatArgList(&ldoptions, "", &lastlibs);	/* libraries */
#else /*WIN32*/
#ifdef __CYGWIN32__
  prependArgList(&ldoptions, soout);
  prependArgList(&ldoptions, "-o");		/* -o ctmp */
  concatArgList(&ldoptions, "", &ofiles);	/* object files */
  appendArgList(&ldoptions, "-lplimp");		/* kernel import library */
  concatArgList(&ldoptions, "-L", &libdirs);    /* library directories */
  concatArgList(&ldoptions, "-l", &libs);	/* libraries */
  concatArgList(&ldoptions, "-l", &lastlibs);	/* libraries */
#else /*__CYGWIN32__*/
#ifdef SO_FORMAT_LDFLAGS
  { char tmp[MAXPATHLEN];
    sprintf(tmp, SO_FORMAT_LDFLAGS);
    prependArgList(&ldoptions, tmp);
  }
#else
  prependArgList(&ldoptions, SO_LDFLAGS);
#endif
  prependArgList(&ldoptions, soout);
  prependArgList(&ldoptions, "-o");		/* -o ctmp */
  concatArgList(&ldoptions, "", &ofiles);	/* object files */
  concatArgList(&ldoptions, "-L", &libdirs);    /* library directories */
  concatArgList(&ldoptions, "-l", &libs);	/* libraries */
  concatArgList(&ldoptions, "-l", &lastlibs);	/* libraries */
#endif /*__CYGWIN32__*/
#endif /*WIN32*/

  callprog(ld, &ldoptions);
}


static void
quoted_name(const char *name, char *plname)
{ int needquote = TRUE;

  if ( islower(CTOI(name[0])) )
  { const char *s = name+1;

    for( ; *s && (isalnum(CTOI(*s)) || *s == '_'); s++)
      ;
    if ( *s == '\0' )
      needquote = FALSE;
  }

  if ( !needquote )
    strcpy(plname, name);
  else
  { char *o = plname;

    *o++ = '\'';
    for( ; *name; name++)
    { if ( *name == '\'' )
	*o++ = *name;
      *o++ = *name;
    }
    *o++ = '\'';
    *o = '\0';
  }
}


char *
put_pl_option(char *to, const char* name, const char *value)
{ strcpy(to, name);
  to += strlen(to);
  *to++ = '=';
  quoted_name(value, to);
  to += strlen(to);

  return to;
}


void
createSavedState()
{ char buf[1024];
  char *e;
  int n;

  strcpy(buf, "consult([");
  e = buf + strlen(buf);
  for(n=0; n<plfiles.size; n++)
  { if ( n > 0 )
      *e++ = ',';
    quoted_name(plfiles.list[n], e);
    e += strlen(e);
  }
  strcpy(e, "]),qsave_program(");
  e += strlen(e);
  quoted_name(pltmp, e);
  e += strlen(e);
  strcpy(e, ",[");
  e += strlen(e);
  e = put_pl_option(e, "goal",     plgoal);
  *e++ = ',';
  e = put_pl_option(e, "toplevel", pltoplevel);
  *e++ = ',';
  e = put_pl_option(e, "initfile", plinitfile);
  if ( plclass )
  { *e++ = ',';
    e = put_pl_option(e, "class", plclass);
  }
  strcpy(e, "])");
  e += strlen(e);

  appendArgList(&ploptions, "-f");
  appendArgList(&ploptions, "none");
  appendArgList(&ploptions, "-F");
  appendArgList(&ploptions, plsysinit);
  appendArgList(&ploptions, "-g");
  appendArgList(&ploptions, "true");
  appendArgList(&ploptions, "-t");
  appendArgList(&ploptions, buf);
  appendArgList(&tmpfiles, pltmp);

  callprog(pl, &ploptions);
}


static void
copy_fd(int i, int o)
{ char buf[CPBUFSIZE];
  int n;

  while( (n=read(i, buf, sizeof(buf))) > 0 )
  { while( n > 0 )
    { int n2;

      if ( (n2 = write(o, buf, n)) > 0 )
      { n -= n2;
      } else
      { fprintf(stderr, "%s: write failed: %s\n", plld, oserror());
	error(1);
      }
    }
  }

  if ( n < 0 )
  { fprintf(stderr, "%s: read failed: %s\n", plld, oserror());
    error(1);
  }
}


void
createOutput()
{ int ifd, ofd = -1;

  if ( verbose )
  {
#ifdef WIN32
    printf("\tcopy /b %s+%s %s\n", ctmp, pltmp, out);
#else
    printf("\tcat %s %s > %s\n", ctmp, pltmp, out);
#endif
  }

  if ( !fake )
  { if ( (ofd = open(out, O_WRONLY|O_CREAT|O_TRUNC|O_BINARY, 0666)) < 0 )
    { fprintf(stderr, "Could not open %s: %s\n", out, oserror());
      error(1);
    }
    if ( (ifd = open(ctmp, O_RDONLY|O_BINARY)) < 0 )
    { close(ofd);
      remove(out);
      fprintf(stderr, "Could not open %s: %s\n", ctmp, oserror());
      error(1);
    }
    copy_fd(ifd, ofd);
    close(ifd);
    if ( (ifd = open(pltmp, O_RDONLY|O_BINARY)) < 0 )
    { close(ofd);
      remove(out);
      fprintf(stderr, "Could not open %s: %s\n", pltmp, oserror());
      error(1);
    }
    copy_fd(ifd, ofd);
    close(ifd);
  }

#ifdef HAVE_CHMOD
  { int mask = umask(0777);
    
    umask(mask);

    if ( verbose )
      printf("\tchmod %03o %s\n", 0777 & ~mask, out);

    if ( !fake )
    {
#ifdef HAVE_FCHMOD
      if ( fchmod(ofd, 0777 & ~mask) != 0 )
#else
      if ( chmod(out, 0777 & ~mask) != 0 )
#endif
      { fprintf(stderr, "Could make %s executable: %s\n", out, oserror());
	error(1);
      }
    }
  }
#endif

  if ( !fake )
    close(ofd);
}


static void
removeTempFiles()
{ int n;

  for(n = 0; n < tmpfiles.size; n++)
  { if ( remove(tmpfiles.list[n]) == 0 )
    { if ( verbose )
	printf("\trm %s\n", tmpfiles.list[n]);
    }
  }
}

		 /*******************************
		 *	       SIGNALS		*
		 *******************************/

static void
catchSignals()
{ signal(SIGINT,	catched_signal);
  signal(SIGSEGV,	catched_signal);
}


		 /*******************************
		 *	       MAIN		*
		 *******************************/

int
main(int argc, char **argv)
{ plld = argv[0];
  
  argc--;
  argv++;

  catchSignals();

  if ( argc == 0 )
  { fprintf(stderr, "No input files.  Use %s -help.\n", plld);
    exit(0);
  }

  putenv("PLLD=true");			/* for subprograms */

  verbose = FALSE;
					/* behave as cpp */
  if ( argc > 0 && streq(argv[0], "-E") )
  { arglist cppoptions;

    memset(&cppoptions, 0, sizeof(cppoptions));
    for(argc--, argv++ ; argc > 0; argc--, argv++)
      appendArgList(&cppoptions, argv[0]);

    callprog(PROG_CPP, &cppoptions);

    return 0;
  }

  parseOptions(argc, argv);
  defaultProgram(&pl, PROG_PL);
  getPrologOptions();
  fillDefaultOptions();

  compileObjectFiles();

  if ( !nolink )
  { if ( shared )
      linkSharedObject();
    else
    { linkBaseExecutable();
      
      if ( !nostate )
      { createSavedState();
	createOutput();
      }
    }
  }

  removeTempFiles();

  return 0;
}


