/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: System dependent parameters
*/

#if !PL_KERNEL
#ifdef MD				/* get machine parameters */
#include MD
#else
#include "md.h"
#endif

#if PROTO
#define P(type) type
#else
#define P(type) ()
#endif

#if !__GNUC__ || !__STDC__
#define volatile
#define inline
#endif

#if O_NO_VOID_POINTER
typedef char *			Void;
#else
typedef void *			Void;
#endif

#define forwards static
#define GLOBAL extern

#include <stdio.h>
#include "pl-itf.h"

#endif PL_KERNEL

GLOBAL int	mainArgc;		/* arguments to main() */
GLOBAL char  ** mainArgv;
GLOBAL char  ** mainEnv;
GLOBAL void	(*PL_foreign_reinit_function) P((int argc, char **argv));

GLOBAL struct
{ char *state;				/* system's boot file */
  char *startup;			/* default user startup file */
  char *version;			/* version of this prolog */
  int  local;				/* default local stack size (K) */
  int  global;				/* default global stack size (K) */
  int  trail;				/* default trail stack size (K) */
  int  argument;			/* default argument stack size (K) */
  int  lock;				/* foreign code locks (K) */
  char *goal;				/* default initialisation goal */
  char *toplevel;			/* default top level goal */
  bool notty;				/* use tty? */
  char *machine;			/* machine we are using */
  char *operating_system;		/* operating system it is running */
  char *home;				/* systems home directory */
} systemDefaults; 

GLOBAL struct options
{ long		localSize;		/* size of local stack */
  long		globalSize;		/* size of global stack */
  long		trailSize;		/* size of trail stack */
  long		argumentSize;		/* size of argument stack */
  long		lockSize;		/* size of lock stack */
  char *	goal;			/* initial goal */
  char *	topLevel;		/* toplevel goal */
  char *	initFile;		/* initialisation file */
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
} status;

		/********************************
		*          FUNCTIONS		*
		*********************************/

#if !PL_KERNEL
		/* imported functions */
bool		prolog P((atomic));
bool		toldString P((void));
bool		loadWicFile P((char *, bool, bool));
void		backTrace P((void *));
bool		compileFileList P((char *out, int argc, char **argv));
unsigned long	pl_trace P((void));
void		systemMode P((bool));
void		setupProlog P((void));
unsigned long	Putf P((char *, ...));
unsigned long	vPutf P((char *, va_list));
char *		store_string P((char *));
bool		ExistsDirectory P((char *));
bool		ExistsFile P((char *));
volatile void	Halt P((int));
char *		BaseName P((char *));
		/* OS functions */
#if !ANSI && !LINUX
extern char	*getenv P((char *));
extern char	*sprintf P((char *, char *, ...));
extern int	strcmp P((char*, char*));
extern int	atoi P((char *));
extern int	printf P((char *, ...));
extern int	fprintf P((FILE *, char *, ...));
extern int	vfprintf P((FILE *, char *, va_list));
volatile void	abort P((void));
extern void	bzero P((Void, size_t));
extern int	select P((int, fd_set *,fd_set *,fd_set *, struct timeval *));
extern int	fflush P((FILE *));
#endif
#if O_LINK_PCE
int		prolog_pce_init P((int, char**));
#endif
#endif !PL_KERNEL

		/* exported functions */
bool		vsysError P((char *, va_list));
bool		vfatalError P((char *, va_list));
bool		vwarning P((char *, va_list));
bool		sysError P((char *, ...));
bool		fatalError P((char *, ...));
bool		warning P((char *, ...));

#ifndef DEBUG
#if O_DEBUG
#define DEBUG(n, g) { if (status.debugLevel >= n) { g; fflush(stdout); } }
#else
#define DEBUG(n, g)
#endif
#endif

		/********************************
		*           PARAMETERS		*
		********************************/

#ifndef DEFSTARTUP
#define DEFSTARTUP ".plrc"
#endif
#ifndef SYSTEMSTATE
#define SYSTEMSTATE "startup"
#endif
#ifndef SYSTEMHOME
#define SYSTEMHOME "/usr/local/lib/pl"
#endif

#if O_CAN_MAP || O_SHARED_MEMORY
#define DEF_DEFLOCAL	2000
#define DEF_DEFGLOBAL	4000
#define DEF_DEFTRAIL	4000
#define DEF_DEFARGUMENT 8
#define DEF_DEFLOCK	8
#else
#define DEF_DEFLOCAL	200
#define DEF_DEFGLOBAL	200
#define DEF_DEFTRAIL	200
#define DEF_DEFARGUMENT 5
#define DEF_DEFLOCK	5
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
#ifndef DEFLOCK
#define DEFLOCK     DEF_DEFLOCK
#endif
