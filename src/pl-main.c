/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Prologs main module
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the ball rolling.  The main task of  this  module  is  command  line
option  parsing,  initialisation  and  handling  of errors and warnings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*#define O_DEBUG 1*/

#include "pl-incl.h"
#include "pl-ctype.h"
#include "rc/rc.h"
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef FORCED_MALLOC_BASE
#include "morecore.c"
#endif

static int	usage(void);
static int	giveVersionInfo(const char *a);
static bool	vsysError(const char *fm, va_list args);

#define	optionString(s) { if (argc > 1) \
			  { s = argv[1]; argc--; argv++; \
			  } else \
			  { usage(); \
			    exit(1); \
			  } \
			}
#define K * 1024L

#define EXECVARMAGIC "$EXECVARS="
static const char exec_vars[512] = EXECVARMAGIC;

static const char *
exec_var(const char *name)
{ const char *s=exec_vars + strlen(EXECVARMAGIC);
  int l = strlen(name);

  while(s < exec_vars+sizeof(exec_vars))
  { if ( strncmp(name, s, l) == 0 && s[l] == '=' )
      return &s[l+1];
    while(*s && s< exec_vars+sizeof(exec_vars))
      s++;
    while(*s == '\0' && s< exec_vars+sizeof(exec_vars))
      s++;
  }

  return NULL;
}


static char *
findHome(char *symbols)
{ char *home = NULL;
  char envbuf[MAXPATHLEN];
  char plp[MAXPATHLEN];
  const char *val;
  
  if ( (val  = exec_var("homevar")) &&
       (home = getenv3(val, envbuf, sizeof(envbuf))) &&
       (home = PrologPath(home, plp)) )
    return store_string(home);
  if ( (val = exec_var("home")) &&
       (home = PrologPath(home, plp)) )
    return store_string(home);

  if ( !(home = getenv3("SWI_HOME_DIR", envbuf, sizeof(envbuf))) )
    home = getenv3("SWIPL", envbuf, sizeof(envbuf));
  if ( home && (home = PrologPath(home, plp)) && ExistsDirectory(home) )
    return store_string(home);

  if ( (home = symbols) )
  { char buf[MAXPATHLEN];
    char parent[MAXPATHLEN];
    IOSTREAM *fd;

    strcpy(parent, DirName(DirName(AbsoluteFile(home, buf), buf), buf));
    Ssprintf(buf, "%s/swipl", parent);

    if ( (fd = Sopen_file(buf, "r")) )
    { if ( Sfgets(buf, sizeof(buf), fd) )
      { int l = strlen(buf);

	while(l > 0 && buf[l-1] <= ' ')
	  l--;
	buf[l] = EOS;

#if O_XOS
      { char buf2[MAXPATHLEN];
	_xos_canonical_filename(buf, buf2);
	strcpy(buf, buf2);
      }
#endif

	if ( !IsAbsolutePath(buf) )
	{ char buf2[MAXPATHLEN];

	  Ssprintf(buf2, "%s/%s", parent, buf);
	  home = AbsoluteFile(buf2, plp);
	} else
	  home = AbsoluteFile(buf, plp);

	if ( ExistsDirectory(home) )
	{ Sclose(fd);
	  return store_string(home);
	}
      }
      Sclose(fd);
    }
  }

  if ( (home = PrologPath(PLHOME, plp)) &&
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
defaultSystemInitFile(char *a0)
{ char plp[MAXPATHLEN];
  char *base = BaseName(PrologPath(a0, plp));
  char buf[256];
  char *s = buf;

  while(*base && isAlpha(*base))
    *s++ = *base++;
  *s = EOS;

  if ( strlen(buf) > 0 )
    return store_string(buf);

  return "pl";
}


#define MEMAREA_INVALID_SIZE (unsigned long)(~0L)

static unsigned long
memarea_limit(const char *s)
{ number n;
  unsigned char *q;

  if ( get_number((unsigned char *)s, &q, &n) && intNumber(&n) )
  { switch((int)*q)
    { case 'k':
      case 'K':
      case EOS:
	return n.value.i K;
      case 'm':
      case 'M':
	return n.value.i K K;
      case 'b':
      case 'B':
	return n.value.i;
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

  if ( ((s = getenv3("EMACS", envbuf, sizeof(envbuf))) && streq(s, "t")) ||
       ((s = getenv3("INFERIOR", envbuf, sizeof(envbuf))) && streq(s, "yes")) )
    GD->cmdline.notty = TRUE;
}


static void
initPaths()
{ char plp[MAXPATHLEN];
  char *symbols = NULL;			/* The executable */

  if ( !(symbols = findExecutable(GD->cmdline.argv[0], plp)) ||
       !(symbols = DeRefLink(symbols, plp)) )
    symbols = GD->cmdline.argv[0];

  DEBUG(2, Sdprintf("rc-module: %s\n", symbols));

  systemDefaults.home	     = findHome(symbols);

#ifdef __WIN32__			/* we want no module but the .EXE */
  symbols = findExecutable(NULL, plp);
  DEBUG(2, Sdprintf("Executable: %s\n", symbols));
#endif
  GD->paths.executable	     = store_string(symbols);
  systemDefaults.startup     = store_string(PrologPath(DEFSTARTUP, plp));
  GD->options.systemInitFile = defaultSystemInitFile(GD->cmdline.argv[0]);

#ifdef O_XOS
  if ( systemDefaults.home )
  { char buf[MAXPATHLEN];
    _xos_limited_os_filename(systemDefaults.home, buf);
    systemDefaults.home = store_string(buf);
  }
#endif
}


static void
initDefaults()
{ systemDefaults.arch	     = ARCH;
  systemDefaults.local       = DEFLOCAL;
  systemDefaults.global      = DEFGLOBAL;
  systemDefaults.trail       = DEFTRAIL;
  systemDefaults.argument    = DEFARGUMENT;
  systemDefaults.heap	     = DEFHEAP;
  systemDefaults.goal	     = "'$welcome'";
  systemDefaults.toplevel    = "prolog";
  systemDefaults.notty       = NOTTYCONTROL;

  GD->io_initialised	     = FALSE;
  GD->initialised	     = FALSE;
  GD->cmdline.notty	     = systemDefaults.notty;
  GD->bootsession	     = FALSE;
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

#define MAXVARNAME 256
#define MAXVARVAL  1024

static int
getVarFromStream(IOSTREAM *s, char *name, char *value)
{ char *q;
  int l;
  int c;

again:
  for(l=MAXVARNAME, q=name; --l > 0; )
  { switch(c = Sgetc(s))
    { case EOF:
	return FALSE;
      case '=':
	*q = EOS;
        goto do_value;
      case '\n':
	goto again;
      default:
	*q++ = c;
    }
  }
  return FALSE;

do_value:
  for(l=MAXVARVAL, q=value; --l > 0; )
  { switch(c = Sgetc(s))
    { case EOF:
      case '\n':
	*q = EOS;
        return TRUE;
      default:
	*q++ = c;
    }
  }
  return FALSE;
}


static void
initDefaultOptions()
{ GD->options.compileOut    = "a.out";
  GD->options.localSize     = systemDefaults.local    K;
  GD->options.globalSize    = systemDefaults.global   K;
  GD->options.trailSize     = systemDefaults.trail    K;
  GD->options.argumentSize  = systemDefaults.argument K;
  GD->options.heapSize      = systemDefaults.heap     K;
  GD->options.goal	    = systemDefaults.goal;
  GD->options.topLevel      = systemDefaults.toplevel;
  GD->options.initFile      = systemDefaults.startup;
  GD->options.saveclass	    = "none";

  if ( !GD->bootsession && GD->resourceDB )
  { IOSTREAM *op = SopenRC(GD->resourceDB, "$options", "$prolog", RC_RDONLY);

    if ( op )
    { char name[MAXVARNAME];
      char val[MAXVARVAL];

      while( getVarFromStream(op, name, val) )
	set_pl_option(name, val);

      Sclose(op);
    }
  }
}


static int
parseCommandLineOptions(int argc0, char **argv, int *compile)
{ int argc = argc0;

  for( ; argc > 0 && (argv[0][0] == '-' || argv[0][0] == '+'); argc--, argv++ )
  { char *s = &argv[0][1];

    if ( streq(s, "-" ) )		/* pl <plargs> -- <app-args> */
      break;
    if ( *s == '-' )
      continue;				/* don't handle --long=value */

    if ( streq(s, "tty") )	/* +/-tty */
    { GD->cmdline.notty = (s[-1] == '-');
      continue;
    }

    while(*s)
    { switch(*s)
      { case 'd':	if (argc > 1)
			{ GD->debug_level = atoi(argv[1]);
			  argc--, argv++;
			} else
			{ usage();
			  exit(1);
			}
			break;
	case 'p':	if (!argc)	/* handled in Prolog */
			{ usage();
			  exit(1);
			}
			argc--, argv++;
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
	case 'g':	optionString(GD->options.goal);
			break;
	case 't':	optionString(GD->options.topLevel);
			break;
	case 'c':	*compile = TRUE;
			break;
	case 'b':	GD->bootsession = TRUE;
			break;
	case 'L':
	case 'G':
	case 'T':
	case 'A':
	case 'H':
        { unsigned long size = memarea_limit(&s[1]);
	  
	  if ( size == MEMAREA_INVALID_SIZE )
	  { usage();
	    exit(1);
	  }

	  switch(*s)
	  { case 'L':	GD->options.localSize    = size; goto next;
	    case 'G':	GD->options.globalSize   = size; goto next;
	    case 'T':	GD->options.trailSize    = size; goto next;
	    case 'A':	GD->options.argumentSize = size; goto next;
	    case 'H':	GD->options.heapSize     = size; goto next;
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

RcArchive
openResourceDB(int argc, char **argv)
{ RcArchive rc;
  char *xfile = NULL;
  int flags = (GD->bootsession ? RC_WRONLY|RC_CREATE|RC_TRUNC : RC_RDONLY);
  char tmp[MAXPATHLEN];
  int n;

  if ( !GD->bootsession && 
       (rc = rc_open_archive(GD->paths.executable, flags)) )
    return rc;
  
  for(n=0; n<argc-1; n++)
  { if ( argv[n][0] == '-' && argv[n][2] == EOS ) /* -? */
    { if ( GD->bootsession )
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
  { strcpy(tmp, systemDefaults.home);
    strcat(tmp, "/boot.prc");

    return rc_open_archive(tmp, flags);
  }

  return NULL;
}


int
PL_initialise(int argc, char **argv)
{ int n;
  bool compile = FALSE;
  const char *rcpath = "<none>";

#if defined(_DEBUG) && defined(WIN32)
  extern void initHeapDebug(void);
  initHeapDebug();
#endif

  GD->cmdline.argc = argc;
  GD->cmdline.argv = argv;
  GD->debug_level  = 0;			/* 1-9: debug, also -d <level> */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FORCED_MALLOC_BASE is a debugging aid for  me   to  force  the system to
allocate memory starting from a specific   address.  Probably only works
properly on Linux. Don't bother with it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef FORCED_MALLOC_BASE
  start_memory((void *)FORCED_MALLOC_BASE);
  Sdprintf("FORCED_MALLOC_BASE at 0x%08x\n", FORCED_MALLOC_BASE);
#endif
#if O_MALLOC_DEBUG
  malloc_debug(O_MALLOC_DEBUG);
#endif

#ifdef O_PLMT
  initPrologThreads();			/* initialise thread system */
#endif

  initOs();				/* Initialise OS bindings */
  initDefaults();			/* Initialise global defaults */
  initPaths();				/* fetch some useful paths */
  setupGNUEmacsInferiorMode();		/* Detect running under EMACS */

  if ( (GD->resourceDB = rc_open_archive(GD->paths.executable, RC_RDONLY)) )
  { rcpath = ((RcArchive)GD->resourceDB)->path;
    initDefaultOptions();
  }

  if ( !GD->resourceDB ||
       !streq(GD->options.saveclass, "runtime") )
  { int done;
    argc--; argv++;

    if ( argc == 1 && giveVersionInfo(argv[0]) ) /* -help, -v, etc */
      exit(0);

    for(n=0; n<argc; n++)		/* need to check this first */
    { if ( streq(argv[n], "--" ) )	/* --: terminates argument list */
	break;
      if ( streq(argv[n], "-b" ) )	/* -b: boot compilation */
      { GD->bootsession = TRUE;
	break;
      }
    }

    DEBUG(1, if (GD->bootsession) Sdprintf("Boot session\n"););

    if ( !GD->resourceDB )
    { if ( !(GD->resourceDB = openResourceDB(argc, argv)) )
	fatalError("Could not find system resources");
      rcpath = ((RcArchive)GD->resourceDB)->path;

      initDefaultOptions();
    }

    done = parseCommandLineOptions(argc, argv, &compile);
    argc -= done;
    argv += done;
  }

  setupProlog();
#ifdef O_PLMT
  aliasThread(PL_thread_self(), PL_new_atom("main"));
#endif
  CSetFeature("resource_database", rcpath);
  initialiseForeign(GD->cmdline.argc, /* PL_initialise_hook() functions */
		    GD->cmdline.argv);
  systemMode(TRUE);

  if ( GD->bootsession )
  { IOSTREAM *s = SopenRC(GD->resourceDB, "$state", "$prolog", RC_WRONLY);
    char *rcpathcopy = store_string(rcpath); /* rcpath is destroyed on close */

    if ( !compileFileList(s, argc, argv) )
      Halt(1);
    if ( Sclose(s) != 0 ||
	 !rc_close_archive(GD->resourceDB) )
    { 
#if defined(__WINDOWS__) || defined(__WIN32__)
      PlMessage("Failed to save system resources: %s", rc_strerror(rc_errno));
#else
      Sfprintf(Serror,
	       "[ERROR: Failed to save system resources %s]\n",
	       rc_strerror(rc_errno));
#endif
      Halt(1);
    }
#if defined(__WINDOWS__) || defined(__WIN32__)
    PlMessage("Boot compilation has created %s", rcpathcopy);
#else
    Sfprintf(Serror,
	     "Boot compilation has created %s\n", rcpathcopy);
#endif
    Halt(0);
  } else
  { IOSTREAM *statefd = SopenRC(GD->resourceDB, "$state", "$prolog", RC_RDONLY);

    if ( statefd )
    { GD->bootsession = TRUE;
      if ( !loadWicFromStream(statefd) )
	Halt(1);
      GD->bootsession = FALSE;

      Sclose(statefd);
    } else
      fatalError("Resource database \"%s\" does not contain a saved state",
		 rcpath);
  }

  debugstatus.styleCheck = (LONGATOM_CHECK|
			    SINGLETON_CHECK|
			    DISCONTIGUOUS_STYLE);
  systemMode(FALSE);
  GD->initialised = TRUE;

  DEBUG(1, Sdprintf("Starting Prolog Part of initialisation\n"));

  if ( compile )
  { Halt(prologToplevel(lookupAtom("$compile")) ? 0 : 1);
  }
    
  return prologToplevel(lookupAtom("$initialise"));
}

typedef const char *cline;

static int
usage()
{ static const cline lines[] = {
    "%s: Usage:\n",
    "    1) %s -help      Display this message\n",
    "    2) %s -v         Display version information\n",
    "    3) %s -arch      Display architecture\n",
    "    4) %s -dump-runtime-variables\n"
    "                     Dump link info in sh(1) format\n",
    "    5) %s [options]\n",
    "    6) %s [options] [-o output] -c file ...\n",
    "    7) %s [options] [-o output] -b bootfile -c file ...\n",
    "Options:\n",
    "    -x state         Start from state (must be first)\n",
    "    -[LGTA]size[KM]  Specify {Local,Global,Trail,Argument} limits\n",
    "    -t toplevel      Toplevel goal\n",
    "    -g goal          Initialisation goal\n",
    "    -f file          Initialisation file\n",
    "    -F file          System Initialisation file\n",
    "    [+/-]tty         Allow tty control\n",
    "    -O               Optimised compilation\n",
    NULL
  };
  const cline *lp = lines;

  for(lp = lines; *lp; lp++)
    Sfprintf(Serror, *lp, BaseName(GD->cmdline.argv[0]));

  return TRUE;
}

static int
version()
{ Sprintf("SWI-Prolog version %d.%d.%d for %s\n",
	  PLVERSION / 10000,
	  (PLVERSION / 100) % 100,
	  PLVERSION % 100,
	  ARCH);

  return TRUE;
}


static int
arch()
{ Sprintf("%s\n", ARCH);

  return TRUE;
}


static int
runtime_vars()
{ Sprintf("CC=\"%s\";\n"
	  "PLBASE=\"%s\";\n"
	  "PLARCH=\"%s\";\n"
	  "PLLIBS=\"%s\";\n"
	  "PLLDFLAGS=\"%s\";\n"
	  "PLVERSION=\"%d\";\n"
#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD)
	  "PLSHARED=\"yes\";\n",
#else
	  "PLSHARED=\"no\";\n",
#endif
	  C_CC,
	  systemDefaults.home ? systemDefaults.home : "<no home>",
	  ARCH,
	  C_LIBS,
	  C_LDFLAGS,
	  PLVERSION);

  return TRUE;
}


static int
giveVersionInfo(const char *a)
{ if ( streq(a, "-help") )
    return usage();
  if ( streq(a, "-arch") )
    return arch();
  if ( streq(a, "-v") )
    return version();
  if ( streq(a, "-dump-runtime-variables") )
    return runtime_vars();

  return FALSE;
}


#include <stdarg.h>

bool
sysError(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vsysError(fm, args);
  va_end(args);

  PL_fail;
}


bool
fatalError(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vfatalError(fm, args);
  va_end(args);

  PL_fail;
}


bool
warning(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vwarning(fm, args);
  va_end(args);

  PL_fail;
}


static bool
vsysError(const char *fm, va_list args)
{ Sfprintf(Serror, "[PROLOG INTERNAL ERROR:\n\t");
  Svfprintf(Serror, fm, args);
  if ( gc_status.active )
  { Sfprintf(Serror,
	    "\n[While in %ld-th garbage collection; skipping stacktrace]\n",
	    gc_status.collections);
  }

#if 0
  if ( /*GD->bootsession ||*/ !GD->initialised )
  { Sfprintf(Serror,
	     "\n[While initialising; quitting]\n");

    Halt(1);
  }
#endif

#if defined(O_DEBUGGER)
  if ( !gc_status.active )
  { debugstatus.styleCheck |= DOLLAR_STYLE;
    Sfprintf(Serror, "\n\nPROLOG STACK:\n");
    backTrace(NULL, 10);
    Sfprintf(Serror, "]\n");
  }
#endif /*O_DEBUGGER*/

  if ( GD->bootsession )
    Halt(1);

action:
  Sfprintf(Serror, "\nAction? "); Sflush(Soutput);
  ResetTty();
  switch(getSingleChar(Sinput))
  { case 'a':
      pl_abort();
      break;
    case EOF:
      Sfprintf(Serror, "EOF: exit\n");
    case 'e':
      Halt(3);
      break;
    default:
      Sfprintf(Serror,
	       "Unknown action.  Valid actions are:\n"
	      "\ta\tabort to toplevel\n"
	      "\te\texit Prolog\n");
      goto action;
  }

  pl_abort();
  Halt(3);
  PL_fail;
}


bool
vfatalError(const char *fm, va_list args)
{
#if defined(__WINDOWS__) || defined(__WIN32__)
  char msg[500];
  Ssprintf(msg, "[FATAL ERROR:\n\t");
  Svsprintf(&msg[strlen(msg)], fm, args);
  Ssprintf(&msg[strlen(msg)], "]");
  
  PlMessage(msg);
#else
  Sfprintf(Serror, "[FATAL ERROR:\n\t");
  Svfprintf(Serror, fm, args);
  Sfprintf(Serror, "]\n");
#endif

  Halt(2);
  PL_fail;
}


bool
vwarning(const char *fm, va_list args)
{ toldString();

  if ( trueFeature(REPORT_ERROR_FEATURE) )
  { if ( ReadingSource &&
	 !GD->bootsession && GD->initialised &&
	 !LD->outofstack )		/* cannot call Prolog */
    { fid_t cid = PL_open_foreign_frame();
      term_t argv = PL_new_term_refs(3);
      predicate_t pred = PL_pred(FUNCTOR_exception3, MODULE_user);
      term_t a = PL_new_term_ref();
      char message[LINESIZ];
      qid_t qid;
      int rval;
  
      Svsprintf(message, fm, args);
  
      PL_put_atom(   argv+0, ATOM_warning);
      PL_put_functor(argv+1, FUNCTOR_warning3);
      PL_get_arg(1, argv+1, a); PL_unify_atom(a, source_file_name);
      PL_get_arg(2, argv+1, a); PL_unify_integer(a, source_line_no);
      PL_get_arg(3, argv+1, a); PL_unify_string_chars(a, message);
      
      qid = PL_open_query(MODULE_user, PL_Q_NODEBUG, pred, argv);
      rval = PL_next_solution(qid);
      PL_close_query(qid);
      PL_discard_foreign_frame(cid);
  
      if ( !rval )
      { Sfprintf(Serror, "[WARNING: (%s:%d)\n\t%s]\n",
		 stringAtom(source_file_name), source_line_no, message);
      }
  
      PL_fail;				/* handled */
    }
  
    Sfprintf(Serror, "[WARNING: ");
    Svfprintf(Serror, fm, args);
    Sfprintf(Serror, "]\n");
  }

  if ( trueFeature(DEBUG_ON_ERROR_FEATURE) )
    pl_trace();

  PL_fail;
}
