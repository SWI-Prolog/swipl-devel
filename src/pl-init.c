/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2017, University of Amsterdam
                              VU University Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the ball rolling.  The main task of  this  module  is  command  line
option  parsing,  initialisation  and  handling  of errors and warnings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* #define O_DEBUG 1 */

#include "rc/rc.h"
#include "pl-incl.h"
#include "pl-prof.h"
#include "os/pl-ctype.h"
#include <errno.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef __WINDOWS__
#include <process.h>			/* getpid() */
#endif

#if defined(_DEBUG) && defined(__WINDOWS__) && !defined(__MINGW32__)
#include <crtdbg.h>
#endif

#if defined(HAVE_MTRACE) && defined(O_MAINTENANCE)
#include <mcheck.h>
#endif

static int	usage(void);
static int	giveVersionInfo(const char *a);
static bool	vsysError(const char *fm, va_list args);

#define	optionString(s) { if (argc > 1) \
			  { if ( s ) remove_string(s); \
			    s = store_string(argv[1]); \
			    argc--; argv++; \
			  } else \
			  { return -1; \
			  } \
			}
#define	optionList(l)   { if (argc > 1) \
			  { opt_append(l, argv[1]); \
			    argc--; argv++; \
			  } else \
			  { return -1; \
			  } \
			}
#define K * 1024L

#define EXECVARMAGIC "$EXECVARS="
static const char exec_vars[512] = EXECVARMAGIC;

static const char *
exec_var(const char *name)
{ const char *s=exec_vars + strlen(EXECVARMAGIC);
  size_t l = strlen(name);

  while(s < exec_vars+sizeof(exec_vars))
  { if ( strncmp(name, s, l) == 0 && s[l] == '=' )
      return &s[l+1];
    while(s < exec_vars+sizeof(exec_vars) && *s)
      s++;
    while(s < exec_vars+sizeof(exec_vars) && *s == '\0')
      s++;
  }

  return NULL;
}


static const char *
longopt(const char *opt, int argc, const char **argv)
{ size_t optlen = strlen(opt);

  for(; argc > 0; argc--, argv++)
  { const char *a = argv[0];

    if ( *a++ == '-' && *a++ == '-' )
    { if ( *a == EOS )		/* --: end of args */
	return NULL;
      if ( strncmp(a, opt, optlen) == 0 && a[optlen] == '=' )
	return &a[optlen+1];
    }
  }

  return NULL;
}


const char *
is_longopt(const char *optstring, const char *name)
{ size_t len = strlen(name);

  if ( strncmp(optstring, name, len) == 0 )
  { if ( optstring[len] == '=' )
      return &optstring[len+1];
    if ( optstring[len] == EOS )
      return "";
  }

  return NULL;
}


int
opt_append(opt_list **l, const char *s)
{ opt_list *n = allocHeapOrHalt(sizeof(*n));

  n->opt_val = store_string(s);
  n->next = NULL;

  while(*l)
    l = &(*l)->next;
  *l = n;

  return TRUE;
}


static char *
findHome(const char *symbols, int argc, const char **argv)
{ const char *home = NULL;
  char envbuf[MAXPATHLEN];
  char plp[MAXPATHLEN];
  const char *val;

  if ( (val=longopt("home", argc, argv)) )
  { if ( (home=PrologPath(val, plp, sizeof(plp))) )
      return store_string(home);
    return NULL;
  }

  if ( (val  = exec_var("homevar")) &&
       (home = Getenv(val, envbuf, sizeof(envbuf))) &&
       (home = PrologPath(home, plp, sizeof(plp))) )
    return store_string(home);
  if ( (val = exec_var("home")) &&
       (home = PrologPath(val, plp, sizeof(plp))) )
    return store_string(home);

#ifdef PLHOMEVAR_1
  if ( !(home = Getenv(PLHOMEVAR_1, envbuf, sizeof(envbuf))) )
  {
#ifdef PLHOMEVAR_2
    home = Getenv(PLHOMEVAR_2, envbuf, sizeof(envbuf));
#endif
  }
  if ( home &&
       (home = PrologPath(home, plp, sizeof(plp))) &&
       ExistsDirectory(home) )
    return store_string(home);
#endif

#ifdef PLHOMEFILE
  if ( (home = symbols) )
  { char buf[MAXPATHLEN];
    char parent[MAXPATHLEN];
    IOSTREAM *fd;
    char *abshome;

    if ( !(abshome=AbsoluteFile(home, buf)) )
      fatalError("File name too long: %s", home);

    strcpy(parent, DirName(DirName(abshome, buf), buf));
    Ssnprintf(buf, sizeof(buf), "%s/" PLHOMEFILE, parent);

    if ( (fd = Sopen_file(buf, "r")) )
    { if ( Sfgets(buf, sizeof(buf), fd) )
      { size_t l = strlen(buf);

	while(l > 0 && buf[l-1] <= ' ')
	  l--;
	buf[l] = EOS;

#if O_XOS
      { char buf2[MAXPATHLEN];
	_xos_canonical_filename(buf, buf2, MAXPATHLEN, 0);
	strcpy(buf, buf2);
      }
#endif

	if ( !IsAbsolutePath(buf) )
	{ char buf2[MAXPATHLEN];

	  if ( Ssnprintf(buf2, sizeof(buf2), "%s/%s", parent, buf) < 0 ||
	       !(home = AbsoluteFile(buf2, plp)) )
	    fatalError("Path name too long: %s/%s", parent, buf);
	} else
	{ if ( !(home = AbsoluteFile(buf, plp)) )
	    fatalError("Path name too long: %s/%s", buf);
	}

	if ( ExistsDirectory(home) )
	{ Sclose(fd);
	  return store_string(home);
	}
      }
      Sclose(fd);
    }
  }
#endif /*PLHOMEFILE*/

  if ( (home = PrologPath(PLHOME, plp, sizeof(plp))) &&
       ExistsDirectory(home) )
    return store_string(home);

  return NULL;
}

/*
  -- atoenne -- convert state to an absolute path. This allows relative
  SWI_HOME_DIR and cleans up non-canonical paths.
*/

#ifndef IS_DIR_SEPARATOR
#define IS_DIR_SEPARATOR(c) ((c) == '/')
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The default name of the system init file `base.rc' is determined from the
basename of the running program, taking all the leading alnum characters.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
defaultSystemInitFile(const char *a0)
{ char plp[MAXPATHLEN];
  char *base = BaseName(PrologPath(a0, plp, sizeof(plp)));

  if ( base )
  { char buf[256];
    char *s = buf;
    size_t limit = sizeof(buf);

    while( (*base && (isAlpha(*base) || *base == '-')) && --limit > 0 )
      *s++ = *base++;
    *s = EOS;

    if ( buf[0] != EOS && limit > 0 )
      return store_string(buf);
  }

  return store_string("swipl");
}


#define MEMAREA_INVALID_SIZE (uintptr_t)(~0L)

static uintptr_t
memarea_limit(const char *s)
{ number n;
  unsigned char *q;

  if ( str_number((unsigned char *)s, &q, &n, FALSE) == NUM_OK &&
       intNumber(&n) )
  { switch((int)*q)
    { case 'k':
      case 'K':
      case EOS:
	return (intptr_t)n.value.i K;
      case 'm':
      case 'M':
	return (intptr_t)n.value.i K K;
      case 'g':
      case 'G':
	return (intptr_t)n.value.i K K K;
      case 'b':
      case 'B':
	return (intptr_t)n.value.i;
    }
  }

  return MEMAREA_INVALID_SIZE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When detected to run under a  GNU-Emacs   shell  or using M-x run-prolog
from GNU-Emacs, don't pretend we can manipulate the TTY settings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
setupGNUEmacsInferiorMode()
{ char envbuf[4];
  char *s;
  int val;

  if ( ((s = Getenv("EMACS", envbuf, sizeof(envbuf))) && s[0]) ||
       ((s = Getenv("INSIDE_EMACS", envbuf, sizeof(envbuf))) && s[0]) ||
       ((s = Getenv("INFERIOR", envbuf, sizeof(envbuf))) && streq(s, "yes")) )
  { GET_LD

    clearPrologFlagMask(PLFLAG_TTY_CONTROL);
    val = TRUE;
  } else
  { val = FALSE;
  }

  PL_set_prolog_flag("emacs_inferior_process", PL_BOOL|FF_READONLY, val);
}


static void
initPaths(int argc, const char **argv)
{ char plp[MAXPATHLEN];

  if ( argc > 0 )
  { char plp1[MAXPATHLEN];
    const char *symbols = NULL;		/* The executable */

    if ( !(symbols = findExecutable(argv[0], plp1, sizeof(plp1))) ||
	 !(symbols = DeRefLink(symbols, plp)) )
      symbols = argv[0];

    DEBUG(MSG_INITIALISE, Sdprintf("rc-module: %s\n", symbols));

    systemDefaults.home	       = findHome(symbols, argc, argv);

#ifdef __WINDOWS__			/* we want no module but the .EXE */
    GD->paths.module	       = store_string(symbols);
    symbols = findExecutable(NULL, plp, sizeof(plp));
    DEBUG(MSG_INITIALISE, Sdprintf("Executable: %s\n", symbols));
#endif
    GD->paths.executable       = store_string(symbols);
    GD->options.systemInitFile = defaultSystemInitFile(argv[0]);
  } else
  { systemDefaults.home	       = findHome(NULL, argc, argv);
    GD->options.systemInitFile = store_string("none");
#ifdef __WINDOWS__			/* we want no module but the .EXE */
    GD->paths.module	       = store_string("libswipl.dll");
#endif
  }

  systemDefaults.startup = store_string(PrologPath(DEFSTARTUP, plp, sizeof(plp)));

#ifdef O_XOS
  if ( systemDefaults.home )
  { char buf[MAXPATHLEN];
    _xos_limited_os_filename(systemDefaults.home, buf);
    systemDefaults.home = store_string(buf);
  }
#endif
}


static void
cleanupStringP(char **loc)
{ char *s;

  if ( (s=*loc) )
  { *loc = NULL;
    remove_string(s);
  }
}

static void
cleanupOptListP(opt_list **listp)
{ opt_list *l, *n;

  if ( (l=*listp) )
  { *listp = NULL;

    for(; l; l=n)
    { n = l->next;

      remove_string(l->opt_val);
      freeHeap(l, sizeof(*l));
    }
  }
}


static void
cleanupPaths(void)
{ cleanupStringP(&GD->paths.executable);
  cleanupStringP(&systemDefaults.home);
  cleanupStringP(&systemDefaults.startup);
  cleanupStringP(&GD->options.systemInitFile);
  cleanupStringP(&GD->options.compileOut);
  cleanupStringP(&GD->options.topLevel);
  cleanupStringP(&GD->options.initFile);
  cleanupStringP(&GD->options.saveclass);
  cleanupStringP(&GD->os.myhome);
#ifdef __WINDOWS__
  cleanupStringP(&GD->paths.module);
#endif

  cleanupOptListP(&GD->options.scriptFiles);
  cleanupOptListP(&GD->options.goals);
}


static void
initDefaults(void)
{ GET_LD

  systemDefaults.arch	     = PLARCH;
  systemDefaults.local       = DEFLOCAL;
  systemDefaults.global      = DEFGLOBAL;
  systemDefaults.trail       = DEFTRAIL;
  systemDefaults.table       = DEFTABLE;
  systemDefaults.goal	     = NULL;
  systemDefaults.toplevel    = "default";
  systemDefaults.notty       = NOTTYCONTROL;

#ifdef __WINDOWS__
  getDefaultsFromRegistry();
#endif

  GD->io_initialised	     = FALSE;
  GD->initialised	     = FALSE;
  GD->bootsession	     = FALSE;
#ifdef SIG_ALERT
  GD->signals.sig_alert      = SIG_ALERT;
#endif

  if ( systemDefaults.notty )
    clearPrologFlagMask(PLFLAG_TTY_CONTROL);
  else
    setPrologFlagMask(PLFLAG_TTY_CONTROL);

  setPrologFlagMask(PLFLAG_DEBUGINFO|PLFLAG_GCTHREAD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the default values  for  the   various  options.  First,  the
options are initialised from the  code.  Next,   if  this  is not a boot
session, the resource $system:options is opened, which contains lines of
the format

	<name>=<value>

This  file  is  parsed,  and  the    values  are  interpreted.  See  the
if-then-else below for the defined values.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
getVarFromStream(IOSTREAM *s, tmp_buffer *name, tmp_buffer *value)
{ int c;

again:
  initBuffer(name);
  initBuffer(value);

  for(;;)
  { switch(c = Sgetc(s))
    { case EOF:
	return FALSE;
      case '=':
	addBuffer(name, EOS, char);
        goto do_value;
      case '\n':
	discardBuffer(name);
	goto again;
      default:
	addBuffer(name, c, char);
    }
  }

do_value:
  for(;;)
  { switch(c = Sgetc(s))
    { case EOF:
      case '\n':
	addBuffer(value, EOS, char);
        return TRUE;
      default:
	addBuffer(value, c, char);
    }
  }
}


static void
initDefaultOptions(void)
{ GD->options.compileOut    = store_string("a.out");
  GD->options.localSize     = systemDefaults.local    K;
  GD->options.globalSize    = systemDefaults.global   K;
  GD->options.trailSize     = systemDefaults.trail    K;
  GD->options.tableSpace    = systemDefaults.table    K;
  GD->options.topLevel      = store_string(systemDefaults.toplevel);
  GD->options.initFile      = store_string(systemDefaults.startup);
  GD->options.scriptFiles   = NULL;
  GD->options.saveclass	    = store_string("none");

  if ( systemDefaults.goal )
    opt_append(&GD->options.goals, systemDefaults.goal);


  if ( !GD->bootsession && GD->resourceDB )
  { IOSTREAM *op = SopenRC(GD->resourceDB, "$options", "$prolog", RC_RDONLY);

    if ( op )
    { tmp_buffer name;
      tmp_buffer val;

      while( getVarFromStream(op, &name, &val) )
      { set_pl_option(baseBuffer(&name, char), baseBuffer(&val, char));
	discardBuffer(&name);
	discardBuffer(&val);
      }

      Sclose(op);
    }
  }
}


int
setTraditional(void)
{ GD->options.traditional = TRUE;
  if ( GD->atoms.table )
    resetListAtoms();

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Does the commandline option parsing.  Actually   we  should  use the GNU
getopt package and deal nicely with intptr_t   arguments  as well as shorts,
but these options are  too  widely  used   as  they  are  to change them
overnight. Returns -1 on error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
isoption(const char *av, const char *opt)
{ return (streq(av, opt) || (av[0] == '-' && streq(av+1, opt)));
}


static int
parseCommandLineOptions(int argc0, char **argv, int *compile)
{ GET_LD
  int argc = argc0;

  DEBUG(MSG_INITIALISE,
	{ Sdprintf("parseCommandLineOptions");
	  for(int i=0; i<argc; i++)
	    Sdprintf("%s ", argv[i]);
	});

  for( ; argc > 0 && (argv[0][0] == '-' || argv[0][0] == '+'); argc--, argv++ )
  { char *s = &argv[0][1];

    if ( streq(s, "-" ) )		/* swipl <plargs> -- <app-args> */
    { break;
    }

    if ( streq(s, "tty") )	/* +/-tty */
    { if ( s[-1] == '+' )
	setPrologFlagMask(PLFLAG_TTY_CONTROL);
      else
	clearPrologFlagMask(PLFLAG_TTY_CONTROL);

      continue;
    } else if ( isoption(s, "nosignals") )
    { clearPrologFlagMask(PLFLAG_SIGNALS);
      clearPrologFlagMask(PLFLAG_GCTHREAD);
      continue;
    } else if ( isoption(s, "nodebug") )
    { clearPrologFlagMask(PLFLAG_DEBUGINFO);
      continue;
    } else if ( streq(s, "-quiet") )
    { GD->options.silent = TRUE;
      continue;
    }

    if ( *s == '-' )
    { const char *optval;

      s++;

      if ( (optval=is_longopt(s, "pldoc")) )
      { GD->options.pldoc_server = store_string(optval);
      } else if ( is_longopt(s, "home") )
      { /* already handled */
#ifdef __WINDOWS__
      } else if ( (optval=is_longopt(s, "win_app")) )
      { GD->options.win_app = TRUE;
#endif
      } else if ( (optval=is_longopt(s, "traditional")) )
      { setTraditional();
      } else if ( (optval=is_longopt(s, "sigalert")) )
      { char *e;
	long sig = strtol(optval, &e, 10);

	if ( e > optval && *e == EOS && sig >= 0 && sig < 32 )
	  GD->signals.sig_alert = sig;
	else
	  return -1;
      }

      continue;				/* don't handle --long=value */
    }

    while(*s)
    { switch(*s)
      { case 'd':	if (argc > 1)
			{ prolog_debug_from_string(argv[1], TRUE);
			  argc--, argv++;
			} else
			  return -1;
			break;
	case 'p':	optionList(&GD->options.search_paths);
			break;
	case 'O':	GD->cmdline.optimise = TRUE; /* see initFeatures() */
			break;
	case 'x':
	case 'o':	optionString(GD->options.compileOut);
			break;
	case 'f':	optionString(GD->options.initFile);
			break;
	case 'F':	optionString(GD->options.systemInitFile);
			break;
	case 'l':
	case 's':	optionList(&GD->options.scriptFiles);
			break;
	case 'g':	optionList(&GD->options.goals);
			break;
	case 't':	optionString(GD->options.topLevel);
			break;
	case 'c':	*compile = TRUE;
			break;
	case 'b':	GD->bootsession = TRUE;
			break;
	case 'q':	GD->options.silent = TRUE;
			break;
	case 'L':
	case 'G':
	case 'T':
	case 'M':
	case 'A':
	case 'H':
        { uintptr_t size = memarea_limit(&s[1]);

	  if ( size == MEMAREA_INVALID_SIZE )
	    return -1;

	  switch(*s)
	  { case 'L':	GD->options.localSize    = size; goto next;
	    case 'G':	GD->options.globalSize   = size; goto next;
	    case 'T':	GD->options.trailSize    = size; goto next;
	    case 'M':	GD->options.tableSpace   = size; goto next;
	    case 'H':
	    case 'A':
	      Sdprintf("%% Warning: -%csize is no longer supported\n", *s);
	      goto next;
	  }
	}
      }
      s++;
    }
    next:;
  }

  return argc0-argc;
}


static void
replace_extension(char *path, const char *ext)
{ char *s = path + strlen(path);

  for( ; s > path; s-- )
  { if ( s[-1] == '.' )
    { strcpy(s, ext);
      return;
    }
    if ( s[-1] == '/' )
      break;
  }

  s += strlen(s);
  *s++ = '.';
  strcpy(s, ext);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the resource database.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef BOOTFILE		/* normally delivered through config.h */
#if SIZEOF_VOIDP == 4
#define BOOTFILE "boot32.prc"
#else
#if SIZEOF_VOIDP == 8
#define BOOTFILE "boot64.prc"
#else
#define BOOTFILE "boot.prc"
#endif
#endif
#endif

RcArchive
openResourceDB(int argc, char **argv)
{ RcArchive rc;
  char *xfile = NULL;
  int flags = (GD->bootsession ? RC_WRONLY|RC_CREATE|RC_TRUNC : RC_RDONLY);
  char tmp[MAXPATHLEN];
  int n;

  if ( !GD->bootsession )
  {
#ifdef __WINDOWS__
    if ( GD->paths.module &&
	 !streq(GD->paths.module, GD->paths.executable) &&
	 (rc = rc_open_archive(GD->paths.module, flags)) )
      return rc;
#endif
    if ( (rc = rc_open_archive(GD->paths.executable, flags)) )
      return rc;
  }

  for(n=0; n<argc-1; n++)
  { if ( argv[n][0] == '-' && argv[n][2] == EOS ) /* -? */
    { if ( argv[n][1] == '-' )
	break;				/* trapped -- */
      if ( GD->bootsession )
      { if ( argv[n][1] == 'o' )
	{ xfile = argv[n+1];
	  break;
	}
      } else
      { if ( argv[n][1] == 'x' )
	{ xfile = argv[n+1];
	  break;
	}
      }
    }
  }

  if ( xfile )
  { if ( !(rc = rc_open_archive(xfile, flags)) )
      fatalError("Could not open resource database \"%s\": %s",
		 xfile, OsError());

    return rc;
  }

  strcpy(tmp, GD->paths.executable);
  replace_extension(tmp, "prc");

  if ( (rc=rc_open_archive(tmp, flags)) )
    return rc;

  if ( systemDefaults.home )
  { if ( strlen(systemDefaults.home)+1+strlen(BOOTFILE) < MAXPATHLEN )
    { strcpy(tmp, systemDefaults.home);
      strcat(tmp, "/");
      strcat(tmp, BOOTFILE);

      return rc_open_archive(tmp, flags);
    } else
      errno = ENAMETOOLONG;
  }

  return NULL;
}


int
PL_is_initialised(int *argc, char ***argv)
{ if ( GD->initialised )
  { if ( argc )
      *argc = GD->cmdline.os_argc;
    if ( argv )
      *argv = GD->cmdline.os_argv;

    return TRUE;
  }

  return FALSE;
}


int
PL_initialise(int argc, char **argv)
{ int n;
  bool compile = FALSE;
  const char *rcpath = "<none>";

  if ( GD->initialised )
    succeed;

  initAlloc();
  initPrologThreads();			/* initialise thread system */
  SinitStreams();

  GD->cmdline.os_argc = argc;
  GD->cmdline.os_argv = argv;

  initOs();				/* Initialise OS bindings */
  initDefaults();			/* Initialise global defaults */
  initPaths(argc, (const char**)argv);	/* fetch some useful paths */

  { GET_LD
#ifdef HAVE_SIGNAL
    setPrologFlagMask(PLFLAG_SIGNALS);	/* default: handle signals */
#endif

  if ( !GD->resourceDB )
  { if (    (GD->resourceDB = rc_open_archive(GD->paths.executable, RC_RDONLY))
#ifdef __WINDOWS__
         || (GD->resourceDB = rc_open_archive(GD->paths.module, RC_RDONLY))
#endif
       )
    { rcpath = ((RcArchive)GD->resourceDB)->path;
    }
  }
  if ( GD->resourceDB )
    initDefaultOptions();

  if ( !GD->resourceDB ||
       !streq(GD->options.saveclass, "runtime") )
  { int done;
    argc--; argv++;

    if ( argc == 1 && giveVersionInfo(argv[0]) ) /* -help, -v, etc */
    { exit(0);
    }

    for(n=0; n<argc; n++)		/* need to check this first */
    { if ( streq(argv[n], "--" ) )	/* --: terminates argument list */
	break;
      if ( streq(argv[n], "-b" ) )	/* -b: boot compilation */
      { GD->bootsession = TRUE;
	break;
      }
    }

    DEBUG(MSG_INITIALISE, if (GD->bootsession) Sdprintf("Boot session\n"););

    if ( !GD->resourceDB )
    { if ( !(GD->resourceDB = openResourceDB(argc, argv)) )
      { fatalError("Could not find system resources");
      }
      rcpath = ((RcArchive)GD->resourceDB)->path;

      initDefaultOptions();
    }

    if ( (done = parseCommandLineOptions(argc, argv, &compile)) < 0 )
    { usage();
      fail;
    }
    argc -= done;
    argv += done;
  }

  GD->cmdline.appl_argc = argc;
  GD->cmdline.appl_argv = argv;

  setupGNUEmacsInferiorMode();		/* Detect running under EMACS */

  if ( !setupProlog() )
    return FALSE;
#ifdef O_PLMT
  aliasThread(PL_thread_self(), ATOM_thread, ATOM_main);
  enableThreads(TRUE);
#endif
  PL_set_prolog_flag("resource_database", PL_ATOM|FF_READONLY, rcpath);
  initialiseForeign(GD->cmdline.os_argc, /* PL_initialise_hook() functions */
		    GD->cmdline.os_argv);
  setAccessLevel(ACCESS_LEVEL_SYSTEM);

  if ( GD->bootsession )
  { IOSTREAM *s = SopenRC(GD->resourceDB, "$state", "$prolog", RC_WRONLY);
    char *rcpathcopy = store_string(rcpath); /* rcpath is destroyed on close */

    if ( !compileFileList(s, argc, argv) )
    { PL_halt(1);
    }
    if ( Sclose(s) != 0 || !rc_save_archive(GD->resourceDB, NULL) )
    {
#ifdef __WINDOWS__
      PlMessage("Failed to save system resources: %s", rc_strerror(rc_errno));
#else
      Sfprintf(Serror,
	       "[ERROR: Failed to save system resources %s]\n",
	       rc_strerror(rc_errno));
#endif
      PL_halt(1);
    }
#ifdef __WINDOWS__
    PlMessage("Boot compilation has created %s", rcpathcopy);
#else
    Sfprintf(Serror,
	     "Boot compilation has created %s\n", rcpathcopy);
#endif
    PL_halt(0);
  } else
  { IOSTREAM *statefd = SopenRC(GD->resourceDB, "$state", "$prolog", RC_RDONLY);

    if ( statefd )
    { GD->bootsession = TRUE;
      if ( !loadWicFromStream(statefd) )
	return FALSE;
      GD->bootsession = FALSE;

      Sclose(statefd);
    } else
    { fatalError("Resource database \"%s\" does not contain a saved state",
		 rcpath);
    }
  }

  debugstatus.styleCheck = (SINGLETON_CHECK|SEMSINGLETON_CHECK|
			    DISCONTIGUOUS_STYLE|
			    NOEFFECT_CHECK);
  setAccessLevel(ACCESS_LEVEL_USER);
  GD->initialised = TRUE;
  registerForeignLicenses();

  DEBUG(MSG_INITIALISE, Sdprintf("Starting Prolog Part of initialisation\n"));

  if ( compile )
  { int status = prologToplevel(PL_new_atom("$compile")) ? 0 : 1;

    PL_halt(status);
    fail;				/* make compiler happy */
  } else
  { int status = prologToplevel(PL_new_atom("$initialise"));
    return status;
  }
  }					/* { GET_LD } */
}


int
PL_set_resource_db_mem(const unsigned char *data, size_t size)
{ if ( (GD->resourceDB = rc_open_archive_mem(data, size, RC_RDONLY)) )
    return TRUE;

  return FALSE;
}


typedef const char *cline;

static int
usage(void)
{ static const cline lines[] = {
    "%s: Usage:\n",
    "    1) %s --help     Display this message (also -h)\n",
    "    2) %s --version  Display version information (also -v)\n",
    "    3) %s --arch     Display architecture\n",
    "    4) %s --dump-runtime-variables[=format]\n"
    "                     Dump link info in sh(1) format\n",
    "    5) %s [options] prolog-file ... [-- arg ...]\n",
    "    6) %s [options] [-o output] -c prolog-file ...\n",
    "    7) %s [options] [-o output] -b bootfile -c prolog-file ...\n",
    "\n",
    "Options:\n",
    "    -x state         Start from state (must be first)\n",
    "    -[LGT]size[KMG]  Specify {Local,Global,Trail} limits\n",
    "    -t toplevel      Toplevel goal\n",
    "    -g goal          Initialisation goal\n",
    "    -f file          User initialisation file\n",
    "    -F file          System initialisation file\n",
    "    -l file          Script source file\n",
    "    -s file          Script source file\n",
    "    -p alias=path    Define file search path 'alias'\n",
    "    [+/-]tty         Allow tty control\n",
    "    -O               Optimised compilation\n",
    "    --nosignals      Do not modify any signal handling\n",
    "    --nodebug        Omit generation of debug info\n",
    "    --quiet          Quiet operation (also -q)\n",
    "    --traditional    Disable extensions of version 7\n",
    "    --home=DIR       Use DIR as SWI-Prolog home\n",
    "    --pldoc[=port]   Start PlDoc server [at port]\n",
#ifdef __WINDOWS__
    "    --win_app	  Behave as Windows application\n",
#endif
#ifdef O_DEBUG
    "    -d level|topic   Enable maintenance debugging\n",
#endif
    NULL
  };
  const cline *lp = lines;
  char *prog;

  if ( GD->cmdline.os_argc > 0 )
    prog = BaseName(GD->cmdline.os_argv[0]);
  else
    prog = "swipl";

  for(lp = lines; *lp; lp++)
    Sfprintf(Serror, *lp, prog);

  return TRUE;
}

#ifndef PLVERSION_TAG
#define PLVERSION_TAG ""
#endif

static int
version(void)
{ const char *tag = PLVERSION_TAG;

  if ( !tag ) tag = "";
  Sprintf("SWI-Prolog version %d.%d.%d%s%s for %s\n",
	  PLVERSION / 10000,
	  (PLVERSION / 100) % 100,
	  PLVERSION % 100,
	  tag[0] ? "-" : "", tag,
	  PLARCH);

  return TRUE;
}


static int
arch()
{ Sprintf("%s\n", PLARCH);

  return TRUE;
}

#define FMT_SH 1			/* Unix sh: name="value" */
#define FMT_CMD 2			/* Windows cmd.exe: set name=value */

static void
printvar(const char *name, const char *value, int format)
{ switch(format)
  { case FMT_SH:
      Sprintf("%s=\"%s\";\n", name, value);
      break;
    case FMT_CMD:
      Sprintf("SET %s=%s\n", name, value);
      break;
    default:
      assert(0);
  }
}


static int
runtime_vars(int format)
{ char *home;
#ifdef O_XOS
  char base[MAXPATHLEN];
#endif
  char version[20];
  char *tag = PLVERSION_TAG;

  if ( systemDefaults.home )
  {
#ifdef O_XOS
    if ( format == FMT_CMD )
    { _xos_os_filename(systemDefaults.home, base, MAXPATHLEN);
      home = base;
    } else
      home = systemDefaults.home;
#else
    home = systemDefaults.home;
#endif
  } else
  { home = "<no home>";
  }

  Ssprintf(version, "%d", PLVERSION);

  printvar("CC",	C_CC, format);
  printvar("PLBASE",	home, format);
  printvar("PLARCH",	PLARCH, format);
  printvar("PLLIBS",	C_LIBS, format);
  printvar("PLLIB",	C_PLLIB, format);
  printvar("PLCFLAGS",  C_CFLAGS, format);
  printvar("PLLDFLAGS", C_LDFLAGS, format);
#ifdef SO_EXT
  printvar("PLSOEXT",	SO_EXT, format);
#endif
#ifdef SO_PATH
  printvar("PLSOPATH",	SO_PATH, format);
#endif
  printvar("PLVERSION", version, format);
  if ( tag[0] )
    printvar("PLVERSIONTAG", tag, format);
#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD) || defined(EMULATE_DLOPEN)
  printvar("PLSHARED",	"yes", format);
#else
  printvar("PLSHARED",	"no", format);
#endif
#ifdef O_PLMT
  printvar("PLTHREADS", "yes", format);
#else
  printvar("PLTHREADS", "no", format);
#endif

  return TRUE;
}


static int
giveVersionInfo(const char *a)
{ if ( *a != '-' )
    return FALSE;

  if ( streq(a, "-help") || streq(a, "--help") || streq(a, "-h") )
    return usage();
  if ( streq(a, "-arch") || streq(a, "--arch") )
    return arch();
  if ( streq(a, "--version") || streq(a, "-v") )
    return version();

  if ( a[1] == '-' )			/* allow for --name versions */
    a++;

  if ( streq(a, "-dump-runtime-variables") )
    return runtime_vars(FMT_SH);
  if ( streq(a, "-dump-runtime-variables=sh") )
    return runtime_vars(FMT_SH);
  if ( streq(a, "-dump-runtime-variables=cmd") )
    return runtime_vars(FMT_CMD);

  return FALSE;
}


		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

typedef int (*halt_function)(int, void*);

struct on_halt
{ halt_function	function;
  void*		argument;
  OnHalt	next;
};

void
register_halt(OnHalt *where, halt_function f, void *arg)
{ if ( !GD->os.halting )
  { OnHalt h = allocHeapOrHalt(sizeof(struct on_halt));

    h->function = f;
    h->argument = arg;
    h->next = *where;
    *where = h;
  }
}


void
PL_on_halt(halt_function f, void *arg)
{ return register_halt(&GD->os.on_halt_list, f, arg);
}

void
PL_exit_hook(halt_function f, void *arg)
{ return register_halt(&GD->os.exit_hooks, f, arg);
}


int
run_on_halt(OnHalt *handlers, int rval)
{ OnHalt h, next;

  h = *handlers;
  *handlers = NULL;

  for(; h; h=next)
  { int rc;

    next = h->next;
    if ( (rc=(*h->function)(rval, h->argument)) != 0 )
      Sdprintf("Foreign halt function %p returned %d\n", h->function, rc);
    freeHeap(h, sizeof(*h));
  }

  return TRUE;				/* not yet cancelling */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cleanup Prolog. The reclaim_memory  argument   says  whether  the system
tries to reclaim memory. This  is   true  when called from PL_cleanup().
Ideally, this would allow for  restarting   the  system  without loosing
memory. In practice, this is hard,   especially if foreign libraries are
loaded.

When called from PL_halt(),  reclaim_memory   is  FALSE, unless compiled
with -DGC_DEBUG.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_HALT_CANCELLED 10

int
cleanupProlog(int rval, int reclaim_memory)
{ GET_LD

  if ( GD->cleaning != CLN_NORMAL )
    return FALSE;

#ifdef __WINDOWS__
  if ( rval != 0 && !hasConsole() )
    PlMessage("Exit status is %d", rval);
#endif

  PL_LOCK(L_INIT);
  if ( GD->cleaning != CLN_NORMAL )
  { PL_UNLOCK(L_INIT);
    return FALSE;
  }

#ifdef O_PLMT
  if ( !LD )
  { PL_thread_attach_engine(NULL);
    LD = GLOBAL_LD;
    if ( !LD )
      goto emergency;
  }
#endif

  GD->cleaning = CLN_PROLOG;
  debugmode(FALSE, NULL);		/* avoid recursive tracing */

  if ( GD->initialised )
  { DEBUG(MSG_CLEANUP, Sdprintf("Running at_halt hooks\n"));

    if ( LD->outofstack )
      emptyStacks();

    PL_set_prolog_flag("exit_status", PL_INTEGER, rval);
    if ( query_loop(PL_new_atom("$run_at_halt"), FALSE) == FALSE &&
	 rval == 0 )
    { if ( ++GD->halt_cancelled	< MAX_HALT_CANCELLED )
      { GD->cleaning = CLN_NORMAL;
	PL_UNLOCK(L_INIT);
	return FALSE;
      }
    }

    GD->cleaning = CLN_FOREIGN;
    if ( !run_on_halt(&GD->os.on_halt_list, rval) && rval == 0 )
    { if ( ++GD->halt_cancelled	< MAX_HALT_CANCELLED )
      { GD->cleaning = CLN_NORMAL;
	PL_UNLOCK(L_INIT);
	return FALSE;
      }
    }
  }

  emptyStacks();			/* no need for this and we may be */
					/* out of stack */
#ifdef O_PROFILE
  resetProfiler();			/* don't do profiling anymore */
#endif
#ifdef O_PLMT
  exitPrologThreads();

emergency:
#endif
  GD->cleaning = CLN_IO;

  Scurout = Soutput;			/* reset output stream to user */

  qlfCleanup();				/* remove errornous .qlf files */
  dieIO();				/* streams may refer to foreign code */
					/* Standard I/O is only flushed! */

  GD->cleaning = CLN_SHARED;

  if ( GD->initialised )
  { fid_t cid = PL_open_foreign_frame();
    predicate_t proc = PL_predicate("unload_all_foreign_libraries", 0,
				    "shlib");
    if ( isDefinedProcedure(proc) )
      PL_call_predicate(MODULE_system, FALSE, proc, 0);
    PL_discard_foreign_frame(cid);
  }

  GD->cleaning = CLN_DATA;

  RemoveTemporaryFiles();

  if ( GD->resourceDB )
  { rc_close_archive(GD->resourceDB);
    GD->resourceDB = NULL;
  }

  cleanupSignals();
#ifdef HAVE_DMALLOC_H
  dmalloc_verify(0);
#endif

  if ( reclaim_memory )
  { freeStacks(PASS_LD1);
#ifdef O_PLMT
    cleanupLocalDefinitions(LD);
#endif
    freePrologLocalData(LD);
    cleanupSourceFiles();
    cleanupModules();
    cleanupPrologFlags();
    cleanupFlags();
    cleanupRecords();
    cleanupTerm();
    cleanupAtoms();
    cleanupFunctors();
    cleanupArith();
    cleanupInitialiseHooks();
    cleanupExtensions();
    cleanupOs();
    Scleanup();
#ifdef O_PLMT
    cleanupThreads();
#endif
    cleanupForeign();
    cleanupPaths();
    cleanupCodeToAtom();
#ifdef O_GMP
    cleanupGMP();
#endif
    cleanupDebug();
  }

  PL_UNLOCK(L_INIT);				/* requires GD->thread.enabled */

  if ( reclaim_memory )
  { memset(&PL_global_data, 0, sizeof(PL_global_data));
    memset(&PL_local_data,  0, sizeof(PL_local_data));
  }

  return TRUE;
}


int
PL_cleanup(int rc)
{ return cleanupProlog(rc, rc == 0);
}


		 /*******************************
		 *	ERRORS AND WARNINGS	*
		 *******************************/

#include <stdarg.h>

bool
sysError(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vsysError(fm, args);
  va_end(args);

  PL_fail;
}


void
fatalError(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vfatalError(fm, args);
/*va_end(args);*/
}


int
warning(const char *fm, ...)
{ int rc;
  va_list args;

  va_start(args, fm);
  rc = vwarning(fm, args);
  va_end(args);

  return rc;
}


#if !defined(HAVE_CTIME_R) && !defined(ctime_r)
#define ctime_r(timep, buf) strcpy(buf, ctime(timep))
#endif

static bool
vsysError(const char *fm, va_list args)
{ GET_LD
  static int active = 0;
  time_t now;
  char tbuf[48];

  switch ( active++ )
  { case 1:
      PL_halt(3);
    case 2:
      abort();
  }

  now = time(NULL);
  ctime_r(&now, tbuf);
  tbuf[24] = '\0';

#ifdef O_PLMT
{ int tid = PL_thread_self();
  atom_t alias;
  const pl_wchar_t *name = L"";

  if ( PL_get_thread_alias(tid, &alias) )
    name = PL_atom_wchars(alias, NULL);

  Sfprintf(Serror, "[PROLOG SYSTEM ERROR:  Thread %d (%Ws) at %s\n\t",
	   tid, name, tbuf);
}
#else
  Sfprintf(Serror, "[PROLOG SYSTEM ERROR: at %s\n\t", tbuf);
#endif
  Svfprintf(Serror, fm, args);
  if ( gc_status.active )
  { Sfprintf(Serror,
	    "\n[While in %ld-th garbage collection]\n",
	    gc_status.collections);
    unblockSignals(&LD->gc.saved_sigmask);
  }

#if defined(O_DEBUGGER)
  Sfprintf(Serror, "\n\nPROLOG STACK:\n");
  PL_backtrace(10, 0);
  Sfprintf(Serror, "]\n");
#endif /*O_DEBUGGER*/

  if ( GD->bootsession )
    PL_halt(1);

action:
#ifdef HAVE_GETPID
  Sfprintf(Serror, "\n[pid=%d] Action? ", getpid());
#else
  Sfprintf(Serror, "\nAction? ");
#endif
  Sflush(Soutput);
  ResetTty();

  switch(getSingleChar(Sinput, FALSE))
  { case EOF:
      Sfprintf(Serror, "EOF: exit\n");
    case 'e':
      PL_halt(3);
      break;
    default:
      Sfprintf(Serror,
	       "Unknown action.  Valid actions are:\n"
	      "\te\texit Prolog\n");
      goto action;
  }

  return FALSE;					/* not reached */
}


void
vfatalError(const char *fm, va_list args)
{ static int active = 0;
  time_t now;
  char tbuf[48];

  switch ( active++ )
  { case 1:
      exit(2);
    case 2:
      abort();
  }

  now = time(NULL);
  ctime_r(&now, tbuf);
  tbuf[24] = '\0';

#ifdef __WINDOWS__
  { char msg[500];
    Ssnprintf(msg, sizeof(msg), "[FATAL ERROR: at %s\n\t", tbuf);
    Svsnprintf(&msg[strlen(msg)], sizeof(msg)-strlen(msg), fm, args);
    Ssnprintf(&msg[strlen(msg)], sizeof(msg)-strlen(msg), "]");

    PlMessage(msg);
  }
#else
  Sfprintf(Serror, "[FATAL ERROR: at %s\n\t", tbuf);
  Svfprintf(Serror, fm, args);
  Sfprintf(Serror, "]\n");
#endif

  PL_halt(2);
  exit(2);				/* in case PL_halt() does not */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
One day, warning() should be replaced by   PL_error()  or direct call to
print_message/2. For now we make warning call print_message/2, so we can
move the rest of the warnings gradually. For this reason we make a term

	message_lines(ListOfLines)

Where ListOfLines is a list of string objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
vwarning(const char *fm, va_list args)
{ GET_LD
  toldString();				/* play safe */

  if ( truePrologFlag(PLFLAG_REPORT_ERROR) )
  { fid_t cid = 0;
    char *s = NULL;

    if ( !GD->bootsession && GD->initialised &&
	 !LD->outofstack &&		/* cannot call Prolog */
	 fm[0] != '$')			/* explicit: don't call Prolog */
    { char message[LINESIZ];
      term_t av, head, tail;

      if ( !(cid = PL_open_foreign_frame()) )
	goto nospace;
      av   = PL_new_term_refs(2);
      tail = PL_copy_term_ref(av+1);
      head = PL_new_term_ref();

      Svsnprintf(message, sizeof(message), fm, args);
      s = message;

      for(;;)
      { char *eol = strchr(s, '\n');

	if ( eol )
	{ if ( !PL_unify_list(tail, head, tail) ||
	       !PL_unify_string_nchars(head, eol-s, s) )
	    goto nospace;
	  s = eol+1;
	} else
	{ if ( *s )
	  { if ( !PL_unify_list(tail, head, tail) ||
		 !PL_unify_string_chars(head, s) )
	      goto nospace;
	  }
	  if ( !PL_unify_nil(tail) )
	    goto nospace;
	  break;
	}
      }
      if ( !PL_cons_functor(av+1, FUNCTOR_message_lines1, av+1) )
	goto nospace;
      PL_put_atom(av, ATOM_error);	/* error? */

      PL_call_predicate(NULL, PL_Q_NODEBUG, PROCEDURE_print_message2, av);
      PL_discard_foreign_frame(cid);
    } else
    { nospace:
      if ( cid )
	PL_discard_foreign_frame(cid);
      Sfprintf(Suser_error, "ERROR: ");
      if ( s )
        Sfprintf(Suser_error, s);
      else
        Svfprintf(Suser_error, fm, args);
      Sfprintf(Suser_error, "\n");
      Pause(0.2);
    }
  }

  if ( !ReadingSource && truePrologFlag(PLFLAG_DEBUG_ON_ERROR) )
    pl_trace();

  return FALSE;
}
