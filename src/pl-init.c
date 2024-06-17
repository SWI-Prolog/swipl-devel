/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2022, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include "pl-incl.h"
#include "pl-arith.h"
#include "pl-zip.h"
#include "pl-prof.h"
#include "pl-read.h"
#include "pl-prims.h"
#include "pl-comp.h"
#include "pl-setup.h"
#include "pl-fli.h"
#include "pl-qlf.h"
#include "pl-pro.h"
#include "pl-trace.h"
#include "pl-proc.h"
#include "pl-modul.h"
#include "pl-flag.h"
#include "pl-event.h"
#include "pl-rec.h"
#include "pl-tabling.h"
#include "pl-term.h"
#include "pl-funct.h"
#include "pl-ext.h"
#include "pl-cont.h"
#include "pl-srcfile.h"
#include "pl-load.h"
#include "pl-nt.h"
#include "os/pl-prologflag.h"
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "os/pl-cstack.h"
#include <errno.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef __WINDOWS__
#include <process.h>			/* getpid() */
#endif

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if defined(_DEBUG) && defined(__WINDOWS__) && !defined(__MINGW32__)
#include <crtdbg.h>
#endif

#if defined(HAVE_MTRACE) && defined(O_MAINTENANCE)
#include <mcheck.h>
#endif

static int	usage(void);
static int	giveVersionInfo(const char *a);
static bool	vsysError(const char *errtype, const char *fm, va_list args);

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

static const char *
optcmp(const char *av, const char *opt)
{ for(; *av && *opt && *av != '='; av++, opt++)
  { if ( *av == *opt ||
	 (*av == '-' && *opt == '_') ||
	 (*av == '_' && *opt == '-') )
      continue;

    return NULL;
  }

  if ( !*opt )
  { if ( *av == '=' )
      av++;
    return av;
  } else
  { return NULL;
  }
}


static const char *
find_longopt(const char *opt, int argc, const char **argv)
{ for(; argc > 0; argc--, argv++)
  { const char *a = argv[0];

    if ( *a++ == '-' && *a++ == '-' )
    { if ( *a == EOS )		/* --: end of args */
	return NULL;
      if ( optcmp(a, opt) )
	return a;
    }
  }

  return NULL;
}


static const char *
is_longopt(const char *optstring, const char *name)
{ const char *v;

  if ( (v=optcmp(optstring, name)) )
  { return *v ? v : "";
  }

  return NULL;
}


static const char *
skip_wsep(const char *s)
{ if ( *s == '-' || *s == '_' )
    s++;
  return s;
}

static int
is_bool_opt(const char *opt, const char *name, int *val)
{ const char *optval;

  if ( (optval=is_longopt(opt,name)) )
  { if ( *optval == EOS ||
	 strcasecmp(optval, "true") == 0 ||
	 strcasecmp(optval, "yes") == 0 ||
	 strcasecmp(optval, "y") == 0 )
    { *val = TRUE;
      return TRUE;
    }
    if ( strcasecmp(optval, "false") == 0 ||
	 strcasecmp(optval, "no") == 0 ||
	 strcasecmp(optval, "n") == 0 )
    { *val = FALSE;
      return TRUE;
    }

    return -1;
  } else if ( strncmp(opt, "no", 2) == 0 &&
	      (optval=is_longopt(skip_wsep(opt+2),name)) )
  { if ( *optval == EOS )
    { *val = FALSE;
      return TRUE;
    }

    return -1;
  }

  return FALSE;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the installation location  of   the  SWI-Prolog  resources, notably
where boot.prc and the library reside.  Tries:

  - --home=DIR
  - One of $SWI_HOME_DIR or $SWIPL
  - Find ../swipl.home from the executable, and resolve its content
    similar to a symbolic link
  - Try the compiled-in location where Prolog should be installed

If `--home` is given (without a dir),   follow the above steps exept for
`--home=DIR` and report the location or  print   an  error and exit with
status 1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
findHome(const char *symbols, int argc, const char **argv)
{ char *home = NULL;
  char *maybe_home;
  char envbuf[PATH_MAX];
  char plp[PATH_MAX];
  const char *homeopt = find_longopt("home", argc, argv);
  const char *val;
  const char *envvar;

  if ( homeopt && (val=is_longopt(homeopt, "home")) && val[0] )
  { if ( (home=PrologPath(val, plp, sizeof(plp))) )
      return store_string(home);
    return NULL;
  }

#ifdef PLHOMEVAR_1
  if ( !(maybe_home = Getenv((envvar=PLHOMEVAR_1), envbuf, sizeof(envbuf))) )
  {
#ifdef PLHOMEVAR_2
    maybe_home = Getenv((envvar=PLHOMEVAR_2), envbuf, sizeof(envbuf));
#endif
  }
  if ( maybe_home &&
       (maybe_home = PrologPath(maybe_home, plp, sizeof(plp))) &&
       ExistsDirectory(maybe_home) )
  { home = maybe_home;
    DEBUG(MSG_INITIALISE,
	  Sdprintf("Found home using env %s\n", envvar));
    goto out;
  }
  (void)envvar;
#endif

#ifdef PLHOMEFILE
  if ( (maybe_home = (char*)symbols) )
  { char buf[PATH_MAX];
    char parent[PATH_MAX];
    IOSTREAM *fd;
    char *pparent;

    if ( !(pparent=DirName(DirName(AbsoluteFile(maybe_home,parent,sizeof(parent)),
				   parent),
			   parent)) ||
	 strlen(PLHOMEFILE) + 1 + strlen(pparent) + 1 > sizeof(parent) )
      fatalError("File name too long: %s", home);

    Ssnprintf(buf, sizeof(buf), "%s/" PLHOMEFILE, pparent);

    if ( (fd = Sopen_file(buf, "r")) )
    { DEBUG(MSG_INITIALISE, Sdprintf("Found home link file %s\n", buf));

      if ( Sfgets(buf, sizeof(buf), fd) )
      { size_t l = strlen(buf);

	while(l > 0 && buf[l-1] <= ' ')
	  l--;
	buf[l] = EOS;

#if O_XOS
      { char buf2[PATH_MAX];
	_xos_canonical_filename(buf, buf2, PATH_MAX, 0);
	strcpy(buf, buf2);
      }
#endif

	if ( !IsAbsolutePath(buf) )
	{ char buf2[PATH_MAX];

	  if ( Ssnprintf(buf2, sizeof(buf2), "%s/%s", parent, buf) < 0 ||
	       !(maybe_home = AbsoluteFile(buf2, plp, sizeof(plp))) )
	    fatalError("Path name too long: %s/%s", parent, buf);
	} else
	{ if ( !(maybe_home = AbsoluteFile(buf, plp, sizeof(plp))) )
	    fatalError("Path name too long: %s/%s", buf);
	}

	if ( ExistsDirectory(maybe_home) )
	{ home = maybe_home;
	  DEBUG(MSG_INITIALISE,
		Sdprintf("Found home %s using link file\n", home));
	}
      }
      Sclose(fd);
    }
  }
#endif /*PLHOMEFILE*/
#ifdef PLRELHOME
  if ( !home && symbols )
  { char bindir[PATH_MAX];
    char *o;

    strcpy(bindir, symbols);
    DirName(bindir, bindir);
    if ( strlen(bindir)+strlen(PLRELHOME)+2 > sizeof(bindir) )
      fatalError("Executable path name too long");
    o = bindir+strlen(bindir);
    *o++ = '/';
    strcpy(o, PLRELHOME);
    if ( ExistsDirectory(bindir) )
    { if ( !(home=AbsoluteFile(bindir, plp, sizeof(plp))) )
	fatalError("Executable path name too long");
      DEBUG(MSG_INITIALISE,
	    Sdprintf("Found home using %s from %s\n", PLRELHOME, symbols));
    }
  }
#endif

#ifdef PLHOME
  if ( !home &&
       ( (maybe_home = PrologPath(PLHOME, plp, sizeof(plp))) &&
	 ExistsDirectory(maybe_home)
       ) )
  { home = maybe_home;
    DEBUG(MSG_INITIALISE, Sdprintf("Using compiled-in home at %s\n", PLHOME));
  }
#endif

out:
  if ( home )
    home = store_string(home);

  if ( homeopt )
  { if ( home )
    { Sprintf("%s\n", home);
      exit(0);
    } else
    { Sdprintf("ERROR: Could not find SWI-Prolog home directory\n");
      exit(1);
    }
  }

  return home;
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
{ char plp[PATH_MAX];
  char *base = BaseName(PrologPath(a0, plp, sizeof(plp)), plp);

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


static int
is_hash_bang_file(const char *s)
{ char fb[PATH_MAX];
  char *fn;
  IOSTREAM *fd;
  int rc = FALSE;

  if ( (fn = PrologPath(s, fb, sizeof(fb))) &&
       (fd = Sopen_file(fb, "r")) )
  { if ( Sgetc(fd) == '#' &&
	 Sgetc(fd) == '!' )
      rc = TRUE;

    Sclose(fd);
  }

  return rc;
}



#define MEMAREA_INVALID_SIZE (uintptr_t)(~0L)

static size_t
memarea_limit(const char *s)
{ number n;
  unsigned char *q;

  if ( str_number((unsigned char *)s, &q, &n, 0) == NUM_OK &&
       intNumber(&n) )
  { switch((int)*q)
    { case 0:
	return (size_t)n.value.i;
      case 'k':
      case 'K':
	return (size_t)n.value.i K;
      case 'm':
      case 'M':
	return (size_t)n.value.i K K;
      case 'g':
      case 'G':
	return (size_t)n.value.i K K K;
      case 'b':
      case 'B':
	return (size_t)n.value.i;
    }
  }

  return MEMAREA_INVALID_SIZE;
}


static int
on_error_style(const char *s)
{ return ( strcmp(s, "print") == 0 ||
	   strcmp(s, "halt") == 0 ||
	   strcmp(s, "status") == 0 );
}


PL_EXPORT_DATA(int) plugin_is_GPL_compatible;
int plugin_is_GPL_compatible;

PL_EXPORT(int) emacs_module_init(void*a);

int
emacs_module_init(void*a)
{ (void)a;

  return 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When detected to run under a  GNU-Emacs   shell  or using M-x run-prolog
from GNU-Emacs, don't pretend we  can   manipulate  the TTY settings. On
Windows, do pretend we have a tty, so the prompt is displayed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
setupGNUEmacsInferiorMode(void)
{ char envbuf[80];
  char *s;
  int val;

  if ( ((s = Getenv("EMACS", envbuf, sizeof(envbuf))) && s[0]) ||
       ((s = Getenv("INSIDE_EMACS", envbuf, sizeof(envbuf))) && s[0]) ||
       ((s = Getenv("INFERIOR", envbuf, sizeof(envbuf))) && streq(s, "yes")) )
  { GET_LD

    clearPrologFlagMask(PLFLAG_TTY_CONTROL);
    val = TRUE;
#ifdef __WINDOWS__
    Sinput->flags  |= SIO_ISATTY;
    Soutput->flags |= SIO_ISATTY;
    Serror->flags  |= SIO_ISATTY;
#endif
  } else
  { val = FALSE;
  }

  PL_set_prolog_flag("emacs_inferior_process", PL_BOOL|FF_READONLY, val);
}


static void
initPaths(int argc, const char **argv)
{ char plp[PATH_MAX];

  if ( argc > 0 )
  { char plp1[PATH_MAX];
    const char *symbols = NULL;		/* The executable */

    if ( !(symbols = findExecutable(argv[0], plp1, sizeof(plp1))) ||
	 !(symbols = DeRefLink(symbols, plp)) )
      symbols = argv[0];

    DEBUG(MSG_INITIALISE, Sdprintf("rc-module: %s\n", symbols));

#ifdef __WINDOWS__			/* we want no module but the .EXE */
    GD->paths.module	       = store_string(symbols);
    symbols = findExecutable(NULL, plp, sizeof(plp));
    DEBUG(MSG_INITIALISE, Sdprintf("Executable: %s\n", symbols));
#endif
    GD->paths.executable       = store_string(symbols);
    GD->options.systemInitFile = defaultSystemInitFile(argv[0]);

    systemDefaults.home	       = findHome(symbols, argc, argv);
  } else
  { systemDefaults.home	       = findHome(NULL, argc, argv);
    GD->options.systemInitFile = store_string("none");
#ifdef __WINDOWS__			/* we want no module but the .EXE */
    GD->paths.module	       = store_string("libswipl.dll");
#endif
  }

#ifdef DEFSTARTUP
  systemDefaults.startup = store_string(DEFSTARTUP);
#else
  systemDefaults.startup = NULL;
#endif

#ifdef O_XOS
  if ( systemDefaults.home )
  { char buf[PATH_MAX];

    if ( !_xos_limited_os_filename(systemDefaults.home, buf, sizeof(buf)) )
      fatalError("Home path too long");
    systemDefaults.home = store_string(buf);
  }
#endif
}


static void
setStringP(char **loc, const char *val)
{ char *s = *loc;

  if ( s && val && streq(s,val) )
    return;
  *loc = val ? store_string(val) : NULL;
  if ( s )
    remove_string(s);
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
{ setStringP(&GD->paths.executable, NULL);
  setStringP(&systemDefaults.home, NULL);
  setStringP(&systemDefaults.startup, NULL);
  setStringP(&GD->options.systemInitFile, NULL);
  setStringP(&GD->options.compileOut, NULL);
  setStringP(&GD->options.topLevel, NULL);
  setStringP(&GD->options.initFile, NULL);
  setStringP(&GD->options.saveclass, NULL);
  setStringP(&GD->options.on_error, NULL);
  setStringP(&GD->options.on_warning, NULL);
  setStringP(&GD->os.myhome, NULL);
#ifdef __WINDOWS__
  setStringP(&GD->paths.module, NULL);
#endif

  cleanupOptListP(&GD->options.scriptFiles);
  cleanupOptListP(&GD->options.goals);
  cleanupOptListP(&GD->options.search_paths);
}


static void
initDefaults(void)
{ GET_LD

  systemDefaults.arch		    = PLARCH;
  systemDefaults.stack_limit	    = DEFSTACKLIMIT;
  systemDefaults.table_space	    = DEFTABLE;
#ifdef O_PLMT
  systemDefaults.shared_table_space = DEFTABLE;
#endif
  systemDefaults.goal		    = NULL;
  systemDefaults.toplevel	    = "default";
  systemDefaults.notty		    = NOTTYCONTROL;

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

  setPrologFlagMask(PLFLAG_DEBUGINFO);
  setPrologFlagMask(PLFLAG_GCTHREAD);
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
	addBuffer(name, (char)c, char);
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
	addBuffer(value, (char)c, char);
    }
  }
}


static int
loadStateOptions(IOSTREAM *opts)
{ int rc = FALSE;
  tmp_buffer name;
  tmp_buffer val;

  while( getVarFromStream(opts, &name, &val) )
  { set_pl_option(baseBuffer(&name, char), baseBuffer(&val, char));
    discardBuffer(&name);
    discardBuffer(&val);
    rc = TRUE;		/* check really modified? */
  }

  return rc;
}


static void
initDefaultOptions(void)
{ GD->options.stackLimit       = systemDefaults.stack_limit;
  GD->options.tableSpace       = systemDefaults.table_space;
#ifdef O_PLMT
  GD->options.sharedTableSpace = systemDefaults.shared_table_space;
#endif
  setStringP(&GD->options.compileOut, "a.out");
  setStringP(&GD->options.topLevel,   systemDefaults.toplevel);
  setStringP(&GD->options.initFile,   systemDefaults.startup);
  setStringP(&GD->options.saveclass,  "none");
  setStringP(&GD->options.on_error,   "print");
  setStringP(&GD->options.on_warning, "print");
  setStringP(&GD->options.bootFrom,   NULL);

  cleanupOptListP(&GD->options.scriptFiles);
  cleanupOptListP(&GD->options.goals);

  if ( systemDefaults.goal )
    opt_append(&GD->options.goals, systemDefaults.goal);
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
getopt package and deal nicely with long   arguments  as well as shorts,
but these options are  too  widely  used   as  they  are  to change them
overnight. Returns -1 on error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
parseCommandLineOptions(int argc0, char **argv0, char **argvleft)
{ GET_LD
  int argc;
  char **argv;
  int argcleft = 0;

  DEBUG(MSG_INITIALISE,
	{ int i;
	  Sdprintf("parseCommandLineOptions");
	  for(i=0; i<argc0; i++)
	    Sdprintf("%s ", argv0[i]);
	});

  GD->options.xpce = -1;

  for(argc=argc0,argv=argv0; argc > 0 && argv[0][0] == '-'; argc--, argv++ )
  { const char *s = &argv[0][1];

    if ( *s == '-' )			/* long options */
    { const char *optval;
      int rc, b;

      s++;
      if ( s[0] == EOS )		/* swipl <plargs> -- <app-args>  */
	break;

      if ( (rc=is_bool_opt(s, "quiet", &b)) )
      { if ( rc == TRUE )
	{ if ( b )
	    GD->options.silent = TRUE;
	} else
	  return -1;
      } else if ( (rc=is_bool_opt(s, "debug_on_interrupt", &b)) )
      { if ( rc == TRUE )
	{ if ( b )
	    setPrologFlagMask(PLFLAG_DEBUG_ON_INTERRUPT);
	} else
	  return -1;
      } else if ( (rc=is_bool_opt(s, "debug", &b)) )
      { if ( rc == TRUE )
	{ if ( !b )
	    clearPrologFlagMask(PLFLAG_DEBUGINFO);
	} else
	  return -1;
      } else if ( (rc=is_bool_opt(s, "signals", &b)) )
      { if ( rc == TRUE )
	{ if ( !b )
	  { clearPrologFlagMask(PLFLAG_SIGNALS);
	    GD->options.nosignals = TRUE;
	  }
	} else
	  return -1;
      } else if ( (rc=is_bool_opt(s, "threads", &b)) )
      { if ( rc == TRUE )
	{ if ( !b )
	    GD->options.nothreads = TRUE;
	} else
	  return -1;
      } else if ( (rc=is_bool_opt(s, "tty", &b)) )
      { if ( rc == TRUE )
	{ if ( b )
	    setPrologFlagMask(PLFLAG_TTY_CONTROL);
	  else
	    clearPrologFlagMask(PLFLAG_TTY_CONTROL);
	} else
	  return -1;
      } else if ( (rc=is_bool_opt(s, "pce", &b)) )
      { if ( rc == TRUE )
	  GD->options.xpce = b;
	else
	  return -1;
      } else if ( (rc=is_bool_opt(s, "packs", &b)) )
      { if ( rc == TRUE )
	  GD->cmdline.packs = b;
	else
	  return -1;
      } else if ( (optval=is_longopt(s, "pldoc")) )
      { GD->options.pldoc_server = store_string(optval);
      } else if ( is_longopt(s, "home") )
      { /* already handled */
#ifdef __WINDOWS__
      } else if ( (optval=is_longopt(s, "win_app")) )
      { GD->options.win_app = TRUE;
#endif
      } else if ( (optval=is_longopt(s, "traditional")) )
      { setTraditional();
#if O_SIGNALS && defined(SIG_ALERT)
      } else if ( (optval=is_longopt(s, "sigalert")) )
      { char *e;
	long sig = strtol(optval, &e, 10);

	if ( e > optval && *e == EOS && sig >= 0 && sig < 32 )
	  GD->signals.sig_alert = sig;
	else
	  return -1;
#endif
      } else if ( (optval=is_longopt(s, "stack_limit")) )
      { size_t size = memarea_limit(optval);

	if ( size == MEMAREA_INVALID_SIZE )
	  return -1;

	GD->options.stackLimit = size;
      } else if ( (optval=is_longopt(s, "table_space")) )
      { size_t size = memarea_limit(optval);

	if ( size == MEMAREA_INVALID_SIZE )
	  return -1;

	GD->options.tableSpace = size;
#ifdef O_PLMT
      } else if ( (optval=is_longopt(s, "shared_table_space")) )
      { size_t size = memarea_limit(optval);

	if ( size == MEMAREA_INVALID_SIZE )
	  return -1;

	GD->options.sharedTableSpace = size;
#endif
      } else if ( (optval=is_longopt(s, "dump-runtime-variables")) )
      { GD->options.config = store_string(optval);
      } else if ( (optval=is_longopt(s, "on-error")) )
      { if ( on_error_style(optval) )
	  setStringP(&GD->options.on_error, optval);
	else
	  return -1;
      } else if ( (optval=is_longopt(s, "on-warning")) )
      { if ( on_error_style(optval) )
	  setStringP(&GD->options.on_warning, optval);
	else
	  return -1;
      } else if ( !GD->options.compile )
      { argvleft[argcleft++] = argv[0];
      }

      continue;
    }

    if ( streq(s, "tty") )
    { Sdprintf("Warning: `-tty` is deprecated.  Please use `--no-tty`\n");
      clearPrologFlagMask(PLFLAG_TTY_CONTROL);
      continue;
    }

    if ( *s == 'D' )
    { const char *def = s+1;

      if ( *def )
	opt_append(&GD->options.defines, def);
      else
	optionList(&GD->options.defines);

      continue;
    }

    while(*s)
    { switch(*s)
      { case 'd':	if ( argc > 1 )
			{ prolog_debug_from_string(argv[1], TRUE);
			  argc--, argv++;
			} else
			  return -1;
			break;
	case 'p':	optionList(&GD->options.search_paths);
			break;
	case 'O':	GD->cmdline.optimise = TRUE; /* see initPrologFlags() */
			break;
	case 'x':	optionString(GD->options.bootFrom);
			break;
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
	case 'c':	GD->options.compile = TRUE;
			break;
	case 'b':	GD->bootsession = TRUE;
			break;
	case 'q':	GD->options.silent = TRUE;
			break;
	default:
	{ if ( s == &argv[0][1] )
	  { argvleft[argcleft++] = argv[0];
	    goto next;
	  } else
	    return -1;
	}
      }
      s++;
    }
  next:;
  }

  for(; argc>0; argc--, argv++)
    argvleft[argcleft++] = argv[0];

  return argcleft;
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

#ifndef SWIPL_BOOT_BASE		/* normally delivered through config.h */
#define SWIPL_BOOT_BASE "boot.prc"
#endif

static zipper *
openResourceDB(int is_hash_bang)
{ zipper *rc;
  char tmp[PATH_MAX];
  char plp[PATH_MAX];
  char *exe, *exedir;

  errno = 0;
  if ( !is_hash_bang && GD->options.bootFrom )	/* -x file */
  { if ( !(rc = zip_open_archive(GD->options.bootFrom, RC_RDONLY)) )
      fatalError("Could not open resource database \"%s\": %s",
		 GD->options.bootFrom, errno ? OsError() : "not a ZIP file");
    return rc;
  }

  strcpy(tmp, GD->paths.executable);
  replace_extension(tmp, "prc");

  if ( (rc=zip_open_archive(tmp, RC_RDONLY)) )
    return rc;
  if ( (exe = PrologPath(tmp, plp, sizeof(plp))) &&
       (exedir = DirName(exe, plp)) &&
       strlen(exedir)+strlen("/swipl.prc")+1 < PATH_MAX )
  { strcat(exedir, "/swipl.prc");
    if ( (rc=zip_open_archive(exedir, RC_RDONLY)) )
      return rc;
  }

  if ( systemDefaults.home )
  { if ( strlen(systemDefaults.home)+1+strlen(SWIPL_BOOT_BASE) < PATH_MAX )
    { strcpy(tmp, systemDefaults.home);
      strcat(tmp, "/");
      strcat(tmp, SWIPL_BOOT_BASE);

      return zip_open_archive(tmp, RC_RDONLY);
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
PL_winitialise(int argc, wchar_t **wargv)
{ char **argv;
  tmp_buffer b;
  int i;
  size_t sz;
  char *abuf, *op;

  if ( !(argv = malloc((argc+1)*sizeof(*argv))) )
    return FALSE;

  initBuffer(&b);
  for(i=0; i<argc; i++)
  { wchar_t *w;

    for(w = wargv[i]; *w; w++)
    { if ( *w < 0x7f )
      { char c = (char)*w;
	addBuffer(&b, c, char);
      } else
      { char wb[6];
	char *e;

	e = utf8_put_char(wb, *w);
	addMultipleBuffer(&b, wb, e-wb, char);
      }
    }
    addBuffer(&b, 0, char);
  }

  sz = entriesBuffer(&b, char);
  if ( !(abuf = malloc(sz)) )
    return FALSE;
  memcpy(abuf, baseBuffer(&b, char), sz);
  discardBuffer(&b);
  op = abuf;

  for(i=0; i<argc; i++)
  { argv[i] = op;
    op += strlen(op)+1;
  }
  argv[i] = NULL;

  return PL_initialise(argc, argv);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialize  the Prolog  system.   Commandline processing  is a  little
complicated as  commandline options may  come from the  commandline as
well as from the loaded state.  Conceptually:

  - If we have a `runtime` state on our back, process the Prolog flags
    from ``$prolog/options.txt``.  All commandline arguments are for
    the application.
  - If we have a `development` state, first process the options from
    ``$prolog/options.txt`` and next from the commandline.  Only leave
    options we do not know to the application.

Steps:

  - If argv[1] == -d, set debug flags (with -DO_DEBUG)
  - If there is a state at the end of the executable, load it and
    set the default options, possibly from the state using
    initDefaultOptions().  If the state is a _runtime_ state, we are
    done with argv as far as Prolog is concerned.
  - Else, parse the commandline options and
    - If we have a boot session, create the resource file
    - Else, find and open the resource DB
      - If the resource DB contains a ``$prolog/options.txt``
        - Redo the option processing, first processing the options
          from options.txt.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_initialise(int argc, char **argv)
{ int is_hash_bang = FALSE;
  const char *rcpath = "<none>";

  if ( GD->initialised )
    succeed;

#ifdef O_DEBUG
  /* Initialize debug flag early, if first argument */
  if (argc > 2 && strcmp(argv[1],"-d") == 0)
    /* One's complement tells p_d_f_s not to bail on error, just return */
    prolog_debug_from_string(argv[2], ~TRUE);
#endif

  initAlloc();
  initPrologThreads();			/* initialise thread system */
  SinitStreams();

  GD->cmdline.os_argc = argc;
  GD->cmdline.os_argv = argv;
  GD->cmdline.packs   = TRUE;

  initOs();				/* Initialise OS bindings */
  initDefaults();			/* Initialise global defaults */
  initPaths(argc, (const char**)argv);	/* fetch some useful paths */

  { GET_LD
#ifdef O_SIGNALS
    setPrologFlagMask(PLFLAG_SIGNALS);	/* default: handle signals */
#endif

  if ( !GD->resources.DB )
  { if ( (GD->resources.DB = zip_open_archive(GD->paths.executable, RC_RDONLY)) )
      rcpath = GD->paths.executable;
#ifdef __WINDOWS__
    else if ( !streq(GD->paths.module, GD->paths.executable) &&
	      (GD->resources.DB = zip_open_archive(GD->paths.module, RC_RDONLY)) )
      rcpath = GD->paths.module;
#endif
  }
  initDefaultOptions();
  if ( GD->resources.DB )
  { IOSTREAM *opts;

    if ( (opts=SopenZIP(GD->resources.DB, "$prolog/options.txt", RC_RDONLY)) )
    { loadStateOptions(opts);
      Sclose(opts);
    }
  }

  if ( !GD->resources.DB ||
       !streq(GD->options.saveclass, "runtime") )
  { int done;
    argc--; argv++;
    char **argvleft;

    if ( argc == 1 && giveVersionInfo(argv[0]) ) /* --help, --version, etc */
      exit(0);

    if ( argc > 1 && argv[0][0] != '-' && is_hash_bang_file(argv[0]) )
      is_hash_bang = TRUE;

    argvleft = PL_malloc(argc*sizeof(*argvleft));
    if ( (done = parseCommandLineOptions(argc, argv, argvleft)) < 0 )
    { usage();
      fail;
    }
    argc = done;
    argv = argvleft;
    GD->cmdline.appl_malloc = TRUE;

    if ( GD->bootsession )
    { DEBUG(MSG_INITIALISE, Sdprintf("Boot session\n"));

      errno = 0;
      if ( !(GD->resources.DB=zip_open_archive(GD->options.compileOut,
					       RC_WRONLY|RC_CREATE|RC_TRUNC)) )
	fatalError("Could not create boot file \"%s\": %s",
		   GD->options.bootFrom, OsError());
    } else if ( !GD->resources.DB )
    { IOSTREAM *opts = NULL;

      if ( !(GD->resources.DB = openResourceDB(is_hash_bang)) )
	fatalError("Could not find system resources");
      rcpath = zipper_file(GD->resources.DB);

      opts = SopenZIP(GD->resources.DB, "$prolog/options.txt", RC_RDONLY);
      if ( opts )
      { initDefaultOptions();
	loadStateOptions(opts);
	Sclose(opts);

	argc = GD->cmdline.os_argc-1;
	argv = GD->cmdline.os_argv+1;
	if ( (done = parseCommandLineOptions(argc, argv, argvleft)) < 0 )
	{ usage();
	  fail;
	}
	argc = done;
	argv = argvleft;
      }
    }
  } else
  { argc--;				/* saved state: only drop program */
    argv++;
  }

  GD->cmdline.appl_argc = argc;
  GD->cmdline.appl_argv = argv;

  setupGNUEmacsInferiorMode();		/* Detect running under EMACS */

  if ( !setupProlog() )
    return FALSE;
#ifdef O_PLMT
  aliasThread(PL_thread_self(), ATOM_thread, ATOM_main);
  enableThreads(!GD->options.nothreads);
#endif
  PL_set_prolog_flag("resource_database", PL_ATOM|FF_READONLY, rcpath);
  initialiseForeign(GD->cmdline.os_argc, /* PL_initialise_hook() functions */
		    GD->cmdline.os_argv);
  setAccessLevel(ACCESS_LEVEL_SYSTEM);
  if ( GD->options.nosignals )
  { GET_LD
    clearPrologFlagMask(PLFLAG_SIGNALS);
  }

  if ( GD->bootsession )
  { IOSTREAM *s = SopenZIP(GD->resources.DB, "$prolog/state.qlf", RC_WRONLY);
    char *rcpathcopy = store_string(rcpath); /* rcpath is destroyed on close */
    zipper *rca = GD->resources.DB;

    if ( !compileFileList(s, argc, argv) )
    { PL_halt(1);
    }

    GD->resources.DB = NULL;
    if ( Sclose(s) != 0 || zip_close_archive(rca) != 0 )
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

    predicate_t boot_message2 = PL_predicate("$boot_message", 2, "system");
    term_t av = PL_new_term_refs(2);
    PL_halt((PL_put_string_chars(av+0, "Boot compilation has created ~w~n") &&
	     PL_put_string_chars(av+1, rcpathcopy) &&
	     PL_call_predicate(NULL, PL_Q_NODEBUG, boot_message2, av)) ? 0 : 1);
  } else
  { IOSTREAM *statefd = SopenZIP(GD->resources.DB, "$prolog/state.qlf", RC_RDONLY);

    if ( statefd )
    { GD->bootsession = TRUE;
      if ( !loadWicFromStream(rcpath, statefd) )
	return FALSE;
      GD->bootsession = FALSE;

      Sclose(statefd);
    } else
    { fatalError("Resource database \"%s\" does not contain "
		 "\"$prolog/state.qlf\"", rcpath);
    }
  }

  debugstatus.styleCheck = (SINGLETON_CHECK|SEMSINGLETON_CHECK|
			    DISCONTIGUOUS_STYLE|
			    NOEFFECT_CHECK);
  setAccessLevel(ACCESS_LEVEL_USER);
  GD->initialised = TRUE;
  registerForeignLicenses();

  DEBUG(MSG_INITIALISE, Sdprintf("Starting Prolog Part of initialisation\n"));

  if ( GD->options.config )
  { int status = prologToplevel(PL_new_atom("$config")) ? 0 : 1;
    PL_halt(status);
    fail;				/* make compiler happy */
  } else if ( GD->options.compile )
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
{ if ( (GD->resources.DB = zip_open_archive_mem(data, size, RC_RDONLY)) )
    return TRUE;

  return FALSE;
}


typedef const char *cline;

static int
usage(void)
{ char tmp[PATH_MAX];

  static const cline lines[] = {
    "%s: Usage:\n",
    "    1) %s [options] prolog-file ... [-- arg ...]\n",
    "    2) %s [options] [-o executable] -c prolog-file ...\n",
    "    3) %s app ...        Use ", "\"%s app list\" for available apps\n",
    "    4) %s --help         Display this message\n",
    "    5) %s --version      Display version information\n",
    "    6) %s --abi-version  Display ABI version key\n",
    "    7) %s --arch         Display architecture\n",
    "    8) %s --dump-runtime-variables[=format]\n"
    "                            Dump link info in sh(1) format\n",
    "\n",
    "Options:\n",
    "    -x state                 Start from state (must be first)\n",
    "    -g goal                  Run goal (may be repeated)\n",
    "    -t toplevel              Toplevel goal\n",
    "    -f file                  User initialisation file\n",
    "    -F file                  Site initialisation file\n",
    "    -l file                  Script source file\n",
    "    -s file                  Script source file\n",
    "    -p alias=path            Define file search path 'alias'\n",
    "    -D name=value            Set a Prolog flag\n",
    "    -O                       Optimised compilation\n",
    "    --on-error=style         One of print, halt or status\n",
    "    --on-warning=style       One of print, halt or status\n",
    "    --tty[=bool]             (Dis)allow tty control\n",
    "    --packs[=bool]           Do (not) attach add-ons\n",
    "    --signals[=bool]         Do (not) modify signal handling\n",
    "    --sigalert[=num]         Use signal num for alerting threads\n",
    "    --threads[=bool]         Do (not) allow for threads\n",
    "    --debug[=bool]           Do (not) generate debug info\n",
    "    --debug-on-interrupt[=bool] Trap the debugger on interrupt\n",
    "    --quiet[=bool] (-q)      Do (not) suppress informational messages\n",
    "    --traditional            Disable extensions of version 7\n",
    "    --home[=DIR]             Print home or use DIR as SWI-Prolog home\n",
    "    --stack-limit=size[BKMG] Specify maximum size of Prolog stacks\n",
    "    --table-space=size[BKMG] Specify maximum size of SLG tables\n",
#ifdef O_PLMT
    "    --shared-table-space=size[BKMG] Maximum size of shared SLG tables\n",
#endif
    "    --pce[=bool]             Make the xpce gui available\n",
    "    --pldoc[=port]           Start PlDoc server [at port]\n",
#ifdef __WINDOWS__
    "    --win-app		  Behave as Windows application\n",
#endif
#ifdef O_DEBUG
    "    -d topic,topic,...       Enable C-source DEBUG channels\n",
#endif
    "\n",
    "Boolean options may be written as --name=bool, --name, --no-name ",
    "or --noname.\n",
    "Both '-' or '_' are accepted as word-separator for long options.\n",
    NULL
  };
  const cline *lp = lines;
  char *prog;

  if ( (GD->cmdline.os_argc <= 0 ||
       (prog = BaseName(GD->cmdline.os_argv[0], tmp))) )
    prog = "swipl";

  for(lp = lines; *lp; lp++)
    Sfprintf(Serror, *lp, prog);

  return TRUE;
}


static
PRED_IMPL("$usage", 0, usage, 0)
{ usage();

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


#ifndef PLPKGNAME
#define PLPKGNAME "swipl"
#endif

static int
abi_version(void)
{ initDefaultOptions();
  setupProlog();
  Sprintf(PLPKGNAME "-abi-%d-%d-%08x-%08x\n",
	  PL_FLI_VERSION,
	  PL_QLF_LOADVERSION,
	  GD->foreign.signature,
	  VM_SIGNATURE);
  PL_cleanup(0);

  return TRUE;
}


static int
arch(void)
{ Sprintf("%s\n", PLARCH);

  return TRUE;
}


static int
giveVersionInfo(const char *a)
{ const char *v;

  if ( *a++ != '-' || *a++ != '-' )
    return FALSE;

  if ( (v=is_longopt(a, "help")) && !*v )
    return usage();
  if ( (v=is_longopt(a, "arch")) && !*v )
    return arch();
  if ( (v=is_longopt(a, "version")) && !*v )
    return version();
  if ( (v=is_longopt(a, "abi_version")) && !*v )
    return abi_version();

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

static void
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
{ register_halt(&GD->os.on_halt_list, f, arg);
}

void
PL_exit_hook(halt_function f, void *arg)
{ register_halt(&GD->os.exit_hooks, f, arg);
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
Cleanup Prolog.  `status` carries the exit status as well as flags.  See
the docs for PL_cleanup() for a description.

When called from PL_halt(), PL_CLEANUP_NO_RECLAIM_MEMORY  is set, unless
compiled for debugging or using ASAN.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_HALT_CANCELLED 10

int
PL_cleanup(int status)
{ int rval = status&PL_CLEANUP_STATUS_MASK;
  int asked_reclaim_memory = (status&PL_CLEANUP_NO_RECLAIM_MEMORY) == 0;
  int reclaim_memory = asked_reclaim_memory;

  if ( GD->cleaning != CLN_NORMAL )
    return PL_CLEANUP_RECURSIVE;

  checkPrologFlagsAccess();

#ifdef __WINDOWS__
  if ( rval != 0 && !hasConsole() )
    PlMessage("Exit status is %d", rval);
#endif

  PL_LOCK(L_INIT);
  if ( GD->cleaning != CLN_NORMAL )
  { PL_UNLOCK(L_INIT);
    return PL_CLEANUP_RECURSIVE;
  }

#ifdef O_PLMT
  if ( !GLOBAL_LD )
  { PL_thread_attach_engine(NULL);
  }
  GET_LD
  if ( !LD )
    goto emergency;
#else
  GET_LD
#endif

  GD->cleaning = CLN_PROLOG;
  debugmode(FALSE, NULL);		/* avoid recursive tracing */

  if ( GD->initialised )
  { DEBUG(MSG_CLEANUP, Sdprintf("Running at_halt hooks\n"));

    if ( LD->outofstack )
      emptyStacks();

    PL_set_prolog_flag("exit_status", PL_INTEGER, (intptr_t)rval);
    if ( query_loop(PL_new_atom("$run_at_halt"), FALSE) == FALSE &&
	 !(status&PL_CLEANUP_NO_CANCEL) )
    { if ( ++GD->halt_cancelled	< MAX_HALT_CANCELLED )
      { GD->cleaning = CLN_NORMAL;
	PL_UNLOCK(L_INIT);
	return PL_CLEANUP_CANCELED;
      }
    }

    GD->cleaning = CLN_FOREIGN;
    if ( !run_on_halt(&GD->os.on_halt_list, rval) &&
	 !(status&PL_CLEANUP_NO_CANCEL) )
    { if ( ++GD->halt_cancelled	< MAX_HALT_CANCELLED )
      { GD->cleaning = CLN_NORMAL;
	PL_UNLOCK(L_INIT);
	return PL_CLEANUP_CANCELED;
      }
    }
  }

  emptyStacks();			/* no need for this and we may be */
					/* out of stack */
#ifdef O_PROFILE
  resetProfiler();			/* don't do profiling anymore */
#endif
#ifdef O_PLMT
  if ( !exitPrologThreads() )		/* reclaim memory while a thread */
  { if ( reclaim_memory )
    { Sdprintf("WARNING: Failed to stop Prolog threads. "
	       "Not reclaming memory.\n");
      reclaim_memory = FALSE;
    }
  }

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
      PL_call_predicate(MODULE_system, PL_Q_NODEBUG, proc, 0);
    PL_discard_foreign_frame(cid);
  }

  GD->cleaning = CLN_DATA;

  RemoveTemporaryFiles();

  cleanupSignals();

  if ( reclaim_memory )
  { cleanupOs();
    freeStacks();
    cleanupBreakPoints();
#ifdef O_PLMT
    cleanupLocalDefinitions(LD);
#endif
    cleanupModules();
    cleanupProcedures();
    cleanupPrologFlags();
    cleanupFlags();
    cleanupRecords();
    cleanupTerm();
    freePrologLocalData(LD);
    cleanupEvents();
    cleanupTabling();
    cleanupAtoms();
    cleanupFunctors();
    cleanupArith();
    cleanupInitialiseHooks();
    cleanupExtensions();
    cleanupWamTable();
    cleanupLocale();
    cleanupSourceFiles();
    Scleanup();
#ifdef O_PLMT
    cleanupThreads();
#endif
    cleanupForeign();
    cleanupPaths();
    cleanupCodeToAtom();
    cleanupCont();
#ifdef O_GMP
    cleanupGMP();
#endif
    cleanupDebug();

    if ( GD->cmdline.appl_malloc )
      free(GD->cmdline.appl_argv);
  }

  if ( GD->resources.DB )
  { zip_close_archive(GD->resources.DB);
    GD->resources.DB = NULL;
  }

  PL_UNLOCK(L_INIT);				/* requires GD->thread.enabled */

  DEBUG(0, assert(GD->clauses.lingering == 0));

  if ( reclaim_memory )
  { memset(&PL_global_data, 0, sizeof(PL_global_data));
    memset(&PL_local_data,  0, sizeof(PL_local_data));
  }

#ifdef __SANITIZE_ADDRESS__
  char *s;

  if ( (s=getenv("ASAN_OPTIONS")) && strstr(s,"detect_leaks=1") )
  { fprintf(stderr, "Running LSAN memory leak check (reclaim_memory=%d)\n",
	   reclaim_memory);
    if ( __lsan_do_recoverable_leak_check() )
    { fprintf(stderr, "Leaks detected; sleeping 60 sec.  Attach using\n"
	     "   gdb -p %d\n", getpid());
      sleep(60);
    } else
    { fprintf(stderr, "No leaks detected\n");
    }
  }
#endif

  return reclaim_memory == asked_reclaim_memory ? PL_CLEANUP_SUCCESS
						: PL_CLEANUP_FAILED;
}


		 /*******************************
		 *	ERRORS AND WARNINGS	*
		 *******************************/

#include <stdarg.h>

bool
sysError(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vsysError("system", fm, args);
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

void
printCrashContext(const char *btname)
{ GET_LD
  time_t now;
  char tbuf[48];
  int btflags = 0;
  static bool running = FALSE;

  if ( running )
  { Sdprintf("Recursive crash; omitting crash report\n");
    return;
  }
  running = TRUE;

  now = time(NULL);
  ctime_r(&now, tbuf);
  tbuf[24] = '\0';
  Sdprintf("Time: %s\n", tbuf);

  if ( LD )
  { Sdprintf("Inferences: %" PRIu64 "\n", LD->statistics.inferences);
    if ( gc_status.active )
    { Sdprintf("Running GC: %" PRIu64 "-th garbage collection]\n",
	       LD->gc.stats.totals.collections);
      unblockSignals(&LD->gc.saved_sigmask);
      btflags |= PL_BT_SAFE;
    }
  }

#ifdef O_PLMT
  int tid = PL_thread_self();
  atom_t alias;
  const pl_wchar_t *name = L"";

  if ( PL_get_thread_alias(tid, &alias) )
    name = PL_atom_wchars(alias, NULL);

  SdprintfX("Thread: %d (%Ws)\n", tid, name);
#endif

  print_backtrace_named(btname);
  if ( LD )
  { if ( getenv("SWIPL_DEBUG_GC_STACK") )
    { if ( LD->shift_status.inferences )
      { Sdprintf("Last stack shift at %" PRIu64 " inferences\n",
		 LD->shift_status.inferences);
	print_backtrace_named("SHIFT");
      }
      if ( LD->gc.inferences )
      { Sdprintf("Last garbage collect at %" PRIu64 " inferences\n",
		 LD->gc.inferences);
	print_backtrace_named("GC");
      }
    }
    Sdprintf("\n\nPROLOG STACK:\n");
    PL_backtrace(10, btflags);
  }

  running = FALSE;
}

#ifdef HAVE_SETITIMER
static void
abort_sig(int sig)
{ (void)sig;
  abort();
}
#endif

static void
set_cleanup_timeout(int sec)
{
#ifdef HAVE_SETITIMER
  struct itimerval timeout = {0};
  struct sigaction act = {0};

  timeout.it_value.tv_sec = sec;
  act.sa_handler = abort_sig;

  sigaction(SIGALRM, &act, NULL);
  setitimer(ITIMER_REAL, &timeout, NULL);
#endif
}

static bool
vsysError(const char *errtype, const char *fm, va_list args)
{ static int active = 0;

  if ( active++ )
    abort();

  set_cleanup_timeout(30);

  Sfprintf(Serror, "\nERROR: %s error: ", errtype);
  Svfprintf(Serror, fm, args);
  Sfprintf(Serror, "\n");

  save_backtrace(errtype);
  printCrashContext(errtype);

  if ( !(true(Sinput, SIO_ISATTY) &&
	 true(Serror, SIO_ISATTY)) ||
       GD->bootsession )
    PL_abort_process();			/* non-interactive or booting */

action:
  Sflush(Soutput);
  ResetTty();
#ifdef HAVE_GETPID
  Sfprintf(Serror, "\n[pid=%d] Action? ", getpid());
#else
  Sfprintf(Serror, "\nAction? ");
#endif

  switch(getSingleChar(Sinput, FALSE))
  { case EOF:
      Sfprintf(Serror, "EOF: exit (status 134)\n");
    case 'e':
      PL_abort_process();
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
PL_system_error(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vsysError("system", fm, args);
  va_end(args);
}

void
PL_api_error(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vsysError("API", fm, args);
  va_end(args);
}

void
vfatalError(const char *fm, va_list args)
{ static int active = 0;
  time_t now;
  char tbuf[48];

  if ( active++ )
  { abort();
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

  PL_abort_process();
  assert(0); /* not reached */
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
      Sfprintf(Suser_error, "%s", "ERROR: ");
      if ( s )
	Sfprintf(Suser_error, "%s", s);
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




		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(init)
  PRED_DEF("$usage", 0, usage, 0)
EndPredDefs
