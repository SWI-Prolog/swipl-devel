/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2022, University of Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_INIT_H
#define _PL_INIT_H

		/********************************
		*           STRUCTURES		*
		********************************/

typedef struct
{ char	 *state;			/* system's boot file */
  char   *startup;			/* default user startup file */
  size_t  stack_limit;			/* default stack limit (bytes) */
  size_t  table_space;			/* default table space (bytes) */
#ifdef O_PLMT
  size_t  shared_table_space;		/* default space for shared tables */
#endif
  char   *goal;				/* default initialisation goal */
  char   *toplevel;			/* default top level goal */
  bool    notty;			/* use tty? */
  char	 *arch;				/* machine/OS we are using */
  char   *home;				/* systems home directory */
} pl_defaults_t;

typedef struct opt_list
{ struct opt_list *next;
  char *opt_val;
} opt_list;

typedef struct
{ size_t	stackLimit;		/* Total stack limit */
  size_t	tableSpace;		/* table space */
#ifdef O_PLMT
  size_t	sharedTableSpace;	/* table space for shared tables */
#endif
  opt_list     *goals;			/* initialization goals */
  char *	topLevel;		/* toplevel goal */
  char *	initFile;		/* -f initialisation file */
  char *	systemInitFile;		/* -F initialisation file */
  char *	config;			/* Show config info */
  opt_list     *scriptFiles;
  opt_list     *search_paths;		/* -p path */
  char *	pldoc_server;		/* --pldoc=Server */
  char *	compileOut;		/* file to store compiler output */
  char *	saveclass;		/* Type of saved state */
  bool		silent;			/* -q: quiet operation */
  bool		traditional;		/* --traditional: no version 7 exts */
  bool		nothreads;		/* --no-threads */
  bool		nosignals;		/* --no-signals */
  char *	on_error;
  char *	on_warning;
  int		xpce;			/* --no-pce */
#ifdef __WINDOWS__
  bool		win_app;		/* --win_app: be Windows application */
#endif
} pl_options_t;

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

int		startProlog(int argc, char **argv);
bool		sysError(const char *fm, ...);
void		fatalError(const char *fm, ...) NORETURN;
bool		warning(const char *fm, ...);
void		vfatalError(const char *fm, va_list args) NORETURN;
bool		vwarning(const char *fm, va_list args);
int		run_on_halt(OnHalt *handlers, int rval);
int		setTraditional(void);
int		opt_append(opt_list **l, const char *s);


		/********************************
		*           PARAMETERS		*
		********************************/

#ifndef SYSTEMHOME
#define SYSTEMHOME "/usr/lib/swipl"
#endif
#ifndef NOTTYCONTROL
#define NOTTYCONTROL FALSE
#endif

#ifndef PLARCH
#define PLARCH "unknown"
#endif
#ifndef OS
#define OS "unknown"
#endif


#define DEF_DEFDEFSTACKLIMIT	(((size_t)1024/8)*1024*1024*SIZEOF_VOIDP)
#define DEF_DEFTABLE		(((size_t)1024/8)*1024*1024*SIZEOF_VOIDP)

#ifndef DEFSTACKLIMIT
#define DEFSTACKLIMIT   DEF_DEFDEFSTACKLIMIT
#endif
#ifndef DEFTABLE
#define DEFTABLE	DEF_DEFTABLE
#endif

/* Parameters that control findHome() */

#define PLHOMEVAR_1	"SWI_HOME_DIR"
#define PLHOMEVAR_2	"SWIPL"
#define PLHOMEFILE	"swipl.home"

#endif /*_PL_INIT_H*/
