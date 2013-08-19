/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
  char *	goal;			/* initial goal */
  char *	topLevel;		/* toplevel goal */
  char *	initFile;		/* -f initialisation file */
  char *	systemInitFile;		/* -F initialisation file */
  opt_list     *scriptFiles;
  opt_list     *search_paths;		/* -p path */
  char *	pldoc_server;		/* --pldoc=Server */
  char *	compileOut;		/* file to store compiler output */
  char *	saveclass;		/* Type of saved state */
  bool		silent;			/* -q: quiet operation */
#ifdef __WINDOWS__
  bool		win_app;		/* --win_app: be Windows application */
#endif
} pl_options_t;


		/********************************
		*           PARAMETERS		*
		********************************/

#ifndef DEFSTARTUP
#define DEFSTARTUP ".plrc"
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

#ifndef DEFLOCAL
#define DEFLOCAL    DEF_DEFLOCAL
#endif
#ifndef DEFGLOBAL
#define DEFGLOBAL   DEF_DEFGLOBAL
#endif
#ifndef DEFTRAIL
#define DEFTRAIL    DEF_DEFTRAIL
#endif
#ifndef DEFHEAP
#define DEFHEAP     DEF_DEFHEAP
#endif

/* Parameters that control findHome() */

#define PLHOMEVAR_1	"SWI_HOME_DIR"
#define PLHOMEVAR_2	"SWIPL"
#define PLHOMEFILE	"swipl.home"
