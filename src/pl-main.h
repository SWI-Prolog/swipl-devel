/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: System dependent parameters
*/

GLOBAL int	mainArgc;		/* arguments to main() */
GLOBAL char  ** mainArgv;

		/********************************
		*           STRUCTURES		*
		********************************/

GLOBAL struct
{ char *state;				/* system's boot file */
  char *startup;			/* default user startup file */
  int  local;				/* default local stack size (K) */
  int  global;				/* default global stack size (K) */
  int  trail;				/* default trail stack size (K) */
  int  argument;			/* default argument stack size (K) */
  int  heap;				/* default heap size (K) */
  char *goal;				/* default initialisation goal */
  char *toplevel;			/* default top level goal */
  bool notty;				/* use tty? */
  char *arch;				/* machine/OS we are using */
  char *home;				/* systems home directory */
} systemDefaults; 

GLOBAL struct options
{ long		localSize;		/* size of local stack */
  long		globalSize;		/* size of global stack */
  long		trailSize;		/* size of trail stack */
  long		argumentSize;		/* size of argument stack */
  long		heapSize;		/* size of the heap */
  char *	goal;			/* initial goal */
  char *	topLevel;		/* toplevel goal */
  char *	initFile;		/* -f initialisation file */
  char *	systemInitFile;		/* -F initialisation file */
  char *	compileOut;		/* file to store compiler output */
} options;

GLOBAL struct
{ bool		boot;			/* boot cycle */
  bool		extendMode;		/* extend using ^[ and ^D */
  bool		doExtend;		/* currently using extend mode ? */
  bool		beep;			/* beep if extend fails */
  int		debugLevel;		/* internal debuglevel (0-9) */
  bool		dumped;			/* created from a dump? */
  bool		notty;			/* do not use ioctl() calls */
  bool		optimise;		/* use optimised compiler */
  int		arithmetic;		/* inside arithmetic code ? */
  bool		io_initialised;		/* I/O initoalisation has finished */
  bool		initialised;		/* Initialisation completed */
  bool		outofstack;		/* System is out of a stack */
  bool		autoload;		/* autoloading traps enabled */
  bool		case_sensitive_files;   /* FileNames are case sensitive? */
  bool		case_preserving_files;  /* FileNames case is preserved */
  bool		dos_files;		/* FileNames have 8+3 convention */
} status;


		/********************************
		*           PARAMETERS		*
		********************************/

#ifndef DEFSTARTUP
#define DEFSTARTUP ".plrc"
#endif
#ifndef SYSTEMHOME
#define SYSTEMHOME "/usr/local/lib/pl"
#endif

#ifndef ARCH
#define ARCH "unknown"
#endif
#ifndef OS
#define OS "unknown"
#endif

#if O_DYNAMIC_STACKS || O_SHIFT_STACKS
#define DEF_DEFLOCAL	2000
#define DEF_DEFGLOBAL	4000
#define DEF_DEFTRAIL	4000
#define DEF_DEFHEAP        0		/* unlimited */
#if O_DYNAMIC_STACKS
#define DEF_DEFARGUMENT 1000
#else
#define DEF_DEFARGUMENT 16
#endif /*O_DYNAMIC_STACKS*/
#else					/* static stack areas */
#define DEF_DEFLOCAL	200
#define DEF_DEFGLOBAL	400
#define DEF_DEFTRAIL	200
#define DEF_DEFARGUMENT  16
#define DEF_DEFHEAP       0		/* unlimited */
#endif

#ifndef DEFLOCAL
#define DEFLOCAL    DEF_DEFLOCAL
#endif
#ifndef DEFGLOBAL
#define DEFGLOBAL   DEF_DEFGLOBAL
#endif
#ifndef DEFTRAIL
#define DEFTRAIL    DEF_DEFTRAIL
#endif
#ifndef DEFARGUMENT
#define DEFARGUMENT DEF_DEFARGUMENT
#endif
#ifndef DEFHEAP
#define DEFHEAP     DEF_DEFHEAP
#endif
