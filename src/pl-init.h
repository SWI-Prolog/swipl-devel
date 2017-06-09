/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2016, University of Amsterdam
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

		/********************************
		*           STRUCTURES		*
		********************************/

typedef struct
{ char *state;				/* system's boot file */
  char *startup;			/* default user startup file */
  int  local;				/* default local stack size (K) */
  int  global;				/* default global stack size (K) */
  int  trail;				/* default trail stack size (K) */
  int  table;				/* default table space (K) */
  char *goal;				/* default initialisation goal */
  char *toplevel;			/* default top level goal */
  bool notty;				/* use tty? */
  char *arch;				/* machine/OS we are using */
  char *home;				/* systems home directory */
} pl_defaults_t;

typedef struct opt_list
{ struct opt_list *next;
  char *opt_val;
} opt_list;

typedef struct
{ size_t	localSize;		/* size of local stack */
  size_t	globalSize;		/* size of global stack */
  size_t	trailSize;		/* size of trail stack */
  size_t	tableSpace;		/* table space */
  opt_list     *goals;			/* initialization goals */
  char *	topLevel;		/* toplevel goal */
  char *	initFile;		/* -f initialisation file */
  char *	systemInitFile;		/* -F initialisation file */
  opt_list     *scriptFiles;
  opt_list     *search_paths;		/* -p path */
  char *	pldoc_server;		/* --pldoc=Server */
  char *	compileOut;		/* file to store compiler output */
  char *	saveclass;		/* Type of saved state */
  bool		silent;			/* -q: quiet operation */
  bool		traditional;		/* --traditional: no version 7 exts */
#ifdef __WINDOWS__
  bool		win_app;		/* --win_app: be Windows application */
#endif
} pl_options_t;

COMMON(int)	opt_append(opt_list **l, const char *s);


		/********************************
		*           PARAMETERS		*
		********************************/

#ifndef DEFSTARTUP
#define DEFSTARTUP ".swiplrc"
#endif
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Default stack-limits. On 32-bit systems, SWI-Prolog  is limited to 128Mb
per stack due to the tagging   and  data-representation choices. 3*128Mb
can be handled with ease by almost any  PC still in use and therefore we
use the maximum as the limit.  64-bit   systems  can  handle limits that
typically exceed the capabilities of the   hardware (i.e., the amount of
physical memory). We use the same limit   counted in `cells' for maximal
portability.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define DEF_DEFLOCAL	(32*1024*SIZEOF_VOIDP)
#define DEF_DEFGLOBAL	(32*1024*SIZEOF_VOIDP)
#define DEF_DEFTRAIL	(32*1024*SIZEOF_VOIDP)
#define DEF_DEFTABLE	((1024/8)*1024*SIZEOF_VOIDP)

#ifndef DEFLOCAL
#define DEFLOCAL    DEF_DEFLOCAL
#endif
#ifndef DEFGLOBAL
#define DEFGLOBAL   DEF_DEFGLOBAL
#endif
#ifndef DEFTRAIL
#define DEFTRAIL    DEF_DEFTRAIL
#endif
#ifndef DEFTABLE
#define DEFTABLE    DEF_DEFTABLE
#endif

/* Parameters that control findHome() */

#define PLHOMEVAR_1	"SWI_HOME_DIR"
#define PLHOMEVAR_2	"SWIPL"
#define PLHOMEFILE	"swipl.home"
