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

#include "pl-incl.h"
#include "pl-itf.h"
#include "pl-save.h"
#if unix || EMX
#include <sys/param.h>
#endif

forwards void	usage P((void));
forwards char * findHome P((char *));
forwards char *	findState P((char *));

#define	optionString(s) { if (argc > 1) \
			  { s = argv[1]; argc--; argv++; \
			  } else \
			    usage(); \
			  break; \
			}

static char *
findHome(def)
char *def;
{ char *h;

  if ( ExistsDirectory(def) )
    return def;

  if ( (h=PrologPath(getenv("SWI_HOME_DIR"))) != NULL && ExistsDirectory(h) )
    return store_string(h);

#if tos
  { char *drv;
    static char drvs[] = "cdefghijklmnopab";
    char home[MAXPATHLEN];

    for(drv = drvs; *drv; drv++)
    { sprintf(home, "%c:/pl", *drv);
      if ( Drvmap() & (1 << (*drv - 'a')) && ExistsDirectory(home) )
        return store_string(home);
    }
  }
#endif

  fatalError("Can't find Prolog Home Directory");
  return (char *) NULL;
}

/*
  -- atoenne -- convert state to an absolute path. This allows relative
  SWI_HOME_DIR and cleans up non-canonical paths.
*/

static char *
findState(base)
char *base;
{ char state[MAXPATHLEN];
  char *full;

  sprintf(state, "%s.%s", base, systemDefaults.machine);
  if ( ExistsFile(state) )
    return store_string(state);

  if ( ExistsFile(base) )
    return store_string(base);

  sprintf(state, "%s/startup/%s.%s",
	  systemDefaults.home, base, systemDefaults.machine);
  full = AbsoluteFile(state);
  if ( ExistsFile(full) )
    return store_string(full);

  sprintf(state, "%s/startup/%s", systemDefaults.home, base);
  full = AbsoluteFile(state);
  if ( ExistsFile(full) )
    return store_string(full);

  return base;
}


#if O_LINK_PCE
foreign_t
pl_pce_init()
{ prolog_pce_init(mainArgc, mainArgv);

  succeed;
}
#endif

int
startProlog(argc, argv, env)
int argc;
char **argv;
char **env;
{ char *s;
  int n;
  char *state;
  bool compile;

  mainArgc			= argc;
  mainArgv			= argv;
  mainEnv			= env;

 /* status.debugLevel = 9; */

  if ( status.dumped == FALSE )
  { systemDefaults.machine	    = MACHINE;
    systemDefaults.home		    = findHome(store_string(PrologPath(SYSTEMHOME)));
    systemDefaults.state	    = findState("startup");
    systemDefaults.startup	    = store_string(PrologPath(DEFSTARTUP));
    systemDefaults.version	    = store_string(PLVERSION);
    systemDefaults.local	    = DEFLOCAL;
    systemDefaults.global	    = DEFGLOBAL;
    systemDefaults.trail	    = DEFTRAIL;
    systemDefaults.argument	    = DEFARGUMENT;
    systemDefaults.lock		    = DEFLOCK;
    systemDefaults.goal		    = "'$welcome'";
    systemDefaults.toplevel	    = "prolog";
    systemDefaults.notty	    = FALSE;
    systemDefaults.operating_system = OPERATING_SYSTEM;

  } else
  { DEBUG(1, printf("Restarting from dumped state\n"));
    SECURE( extern char *sbrk();
	    extern char *thebreak;
	    if ( thebreak != (char *)sbrk(0) )
	      printf("Saved break = %ld; reloaded = %ld\n", thebreak, sbrk(0))
	  );
  }

  SECURE(
    malloc_debug(1);
    if ( malloc_verify() != 1 )
      sysError("Memory allocation corrupted");
  );

  compile			= FALSE;
  state				= systemDefaults.state;
  status.io_initialised		= FALSE;
  status.initialised		= FALSE;
  status.notty			= systemDefaults.notty;
  status.boot			= FALSE;
  status.extendMode		= TRUE;

  argc--; argv++;

  if ( (s = getenv("EMACS")) != NULL && streq(s, "t") )
    status.notty = TRUE;

  for(n=0; n<argc; n++)			/* need to check this first */
  { DEBUG(2, printf("argv[%d] = %s\n", n, argv[n]));
    if (streq(argv[n], "-b") )
      status.boot = TRUE;
  }

  DEBUG(1, {if (status.boot) printf("Boot session\n");});

  if ( argc >= 2 && streq(argv[0], "-r") )
  { loaderstatus.restored_state = lookupAtom(AbsoluteFile(PrologPath(argv[1])));
    argc -= 2, argv += 2;		/* recover; we've done this! */
  }

  if ( argc >= 2 && streq(argv[0], "-x") )
  { state = argv[1];
    argc -= 2, argv += 2;
    DEBUG(1, printf("Startup file = %s\n", state));
  }

  if ( argc >= 1 && streq(argv[0], "-help") )
    usage();

#define K * 1024L

  if ( state != NULL && status.boot == FALSE )
  { DEBUG(1, printf("Scanning %s for options\n", state));
    if ( loadWicFile(state, TRUE, TRUE) == FALSE )
      Halt(1);
    DEBUG(2, printf("options.localSize    = %ld\n", options.localSize));
    DEBUG(2, printf("options.globalSize   = %ld\n", options.globalSize));
    DEBUG(2, printf("options.trailSize    = %ld\n", options.trailSize));
    DEBUG(2, printf("options.argumentSize = %ld\n", options.argumentSize));
    DEBUG(2, printf("options.lockSize	  = %ld\n", options.lockSize));
    DEBUG(2, printf("options.goal         = %s\n",  options.goal));
    DEBUG(2, printf("options.topLevel     = %s\n",  options.topLevel));
    DEBUG(2, printf("options.initFile     = %s\n",  options.initFile));
  } else
  { options.compileOut	  = "a.out";
    options.localSize	  = systemDefaults.local    K;
    options.globalSize	  = systemDefaults.global   K;
    options.trailSize	  = systemDefaults.trail    K;
    options.argumentSize  = systemDefaults.argument K;
    options.lockSize	  = systemDefaults.lock	    K;
    options.goal	  = systemDefaults.goal;
    options.topLevel	  = systemDefaults.toplevel;
    options.initFile      = systemDefaults.startup;
  }

  for( ; argc > 0 && (argv[0][0] == '-' || argv[0][0] == '+'); argc--, argv++ )
  { if ( streq(&argv[0][1], "tty") )
    { status.notty = (argv[0][0] == '-');
      continue;
    }

    s = &argv[0][1];
    while(*s)
    { switch(*s)
      { case 'd':	if (argc > 1)
			{ status.debugLevel = atoi(argv[1]);
			  argc--, argv++;
			} else
			  usage();
			break;
	case 'O':	status.optimise = TRUE;
			break;
  	case 'o':	optionString(options.compileOut);
	case 'f':	optionString(options.initFile);
	case 'g':	optionString(options.goal);
	case 't':	optionString(options.topLevel);
	case 'c':	compile = TRUE;
			break;
	case 'b':	status.boot = TRUE;
			break;
	case 'B':
#if !O_DYNAMIC_STACKS
			options.localSize    = 32 K;
			options.globalSize   = 8 K;
			options.trailSize    = 8 K;
			options.argumentSize = 1 K;
			options.lockSize     = 1 K;
#endif
			goto next;
	case 'L':	options.localSize    = atoi(++s) K; goto next;
	case 'G':	options.globalSize   = atoi(++s) K; goto next;
	case 'T':	options.trailSize    = atoi(++s) K; goto next;
	case 'A':	options.argumentSize = atoi(++s) K; goto next;
	case 'P':	options.lockSize     = atoi(++s) K; goto next;
      }
      s++;
    }
    next:;
  }
#undef K
  
  DEBUG(1, printf("Command line options parsed\n"));

  setupProlog();

#if LINK_THIEF
  { extern long pl_thief();

    if ( status.dumped == FALSE )
      PL_register_foreign("$thief", 1, pl_thief, 0);
  }
#endif

  systemMode(TRUE);

  if ( status.boot )
  { if (compileFileList(options.compileOut, argc, argv) == TRUE)
      Halt(0);

    Halt(1);
  }

  if ( state != NULL )
  { status.boot = TRUE;
    if ( loadWicFile(state, TRUE, FALSE) == FALSE )
      Halt(1);
    status.boot = FALSE;
  }

  if ( PL_foreign_reinit_function != NULL )
    (*PL_foreign_reinit_function)(mainArgc, mainArgv);

  systemMode(FALSE);
  status.dumped = TRUE;
  status.initialised = TRUE;

#if O_LINK_PCE
  PL_register_foreign("$pce_init", 0, pl_pce_init, PL_FA_TRANSPARENT, 0);
#endif

  DEBUG(1, printf("Starting Prolog Engine\n"));

  if ( prolog(PL_new_atom(compile ? "$compile" : "$init")) == TRUE )
    Halt(0);
  else
    Halt(1);

  return 0;
}

static void
usage()
{ static char *lines[] = {
    "%s: Usage:\n",
    "    1) %s -help\n",
    "    2) %s [options]\n",
    "    3) %s [options] [-o output] -c file ...\n",
    "    4) %s [options] [-o output] -b file ...\n",
    "Options:\n",
    "    -x state        Start from state (must be first)\n",
    "    -[LGTA]kbytes   Specify [Local, Global, Trail, Argument] stack sizes\n",
    "    -B              Small stack sizes to prepare for boot\n",
    "    -t toplevel     Toplevel goal\n",
    "    -g goal         Initialisation goal\n",
    "    -f file         Initialisation file\n",
    "    [+/-]tty        Allow tty control\n",
    "    -O              Optimised compilation\n",
    NULL
  };
  char **lp = lines;

  for(lp = lines; *lp; lp++)
    fprintf(stderr, *lp, BaseName(mainArgv[0]));

  Halt(1);
}

#if ANSI
#include <stdarg.h>

bool
sysError(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vsysError(fm, args);
  va_end(args);

  PL_fail;
}

bool
fatalError(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vfatalError(fm, args);
  va_end(args);

  PL_fail;
}

bool
warning(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vwarning(fm, args);
  va_end(args);

  PL_fail;
}
#else
#if mips
#include "/usr/include/varargs.h"
#else
#include <varargs.h>
#endif

bool
sysError(va_alist)
va_dcl
{ va_list args;
  char *fm;

  va_start(args);
  fm = va_arg(args, char *);
  vsysError(fm, args);
  va_end(args);

  PL_fail;
}

bool
fatalError(va_alist)
va_dcl
{ va_list args;
  char *fm;

  va_start(args);
  fm = va_arg(args, char *);
  vfatalError(fm, args);
  va_end(args);

  PL_fail;
}

bool
warning(va_alist)
va_dcl
{ va_list args;
  char *fm;

  va_start(args);
  fm = va_arg(args, char *);
  vwarning(fm, args);
  va_end(args);

  PL_fail;
}
#endif ANSI

bool
vsysError(fm, args)
char *fm;
va_list args;
{ fprintf(stderr, "[PROLOG INTERNAL ERROR:\n\t");
  vfprintf(stderr, fm, args);
  fprintf(stderr, "\n[Switched to system mode: style_check(+dollar)]\n");
  debugstatus.styleCheck |= DOLLAR_STYLE;
  fprintf(stderr, "PROLOG STACK:\n");
  backTrace(NULL, 10);
  fprintf(stderr, "]\n");

  pl_abort();
  Halt(3);
  PL_fail;
}

bool
vfatalError(fm, args)
char *fm;
va_list args;
{ fprintf(stderr, "[FATAL ERROR:\n\t");
  vfprintf(stderr, fm, args);
  fprintf(stderr, "]\n");

  Halt(2);
  PL_fail;
}

bool
vwarning(fm, args)
char *fm;
va_list args;
{ toldString();

  if ( ReadingSource && !status.boot && status.initialised )
  { word goal = globalFunctor(FUNCTOR_exception3);
    char message[LINESIZ];
    word arg;

    vsprintf(message, fm, args);

    unifyAtomic(argTermP(goal, 0), ATOM_warning);
    unifyFunctor(argTermP(goal, 1), FUNCTOR_warning3);
    arg = argTerm(goal, 1);
    unifyAtomic(argTermP(arg, 0), source_file_name);
    unifyAtomic(argTermP(arg, 1), consNum(source_line_no));
    unifyAtomic(argTermP(arg, 2), globalString(message));

    if ( callGoal(MODULE_user, goal, FALSE) == FALSE )
    { extern int Output;
      int old = Output;
    
      Output = 2;
      Putf("[WARNING: (%s:%d)\n\t",
	    stringAtom(source_file_name), source_line_no);
      vPutf(fm, args);
      Putf("]\n");
      Output = old;
    }

    PL_fail;				/* handled */
  }


  if ( status.io_initialised )
  { extern int Output;
    int old = Output;
    
    Output = 2;
    Putf("[WARNING: ");
    vPutf(fm, args);
    Putf("]\n");
    Output = old;
  } else
  { fprintf(stderr, "[WARNING: ");
    vfprintf(stderr, fm, args);
    fprintf(stderr, "]\n");
  }

  pl_trace();

  PL_fail;
}
