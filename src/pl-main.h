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

		/********************************
		*           STRUCTURES		*
		********************************/

typedef struct
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
} pl_defaults_t; 

typedef struct
{ long		localSize;		/* size of local stack */
  long		globalSize;		/* size of global stack */
  long		trailSize;		/* size of trail stack */
  long		argumentSize;		/* size of argument stack */
  long		heapSize;		/* size of the heap */
  char *	goal;			/* initial goal */
  char *	topLevel;		/* toplevel goal */
  char *	initFile;		/* -f initialisation file */
  char *	systemInitFile;		/* -F initialisation file */
  char *	scriptFile;		/* -s script file */
  char *	compileOut;		/* file to store compiler output */
  char *	saveclass;		/* Type of saved state */
  bool		silent;			/* -q: quiet operation */
} pl_options_t;


		/********************************
		*           PARAMETERS		*
		********************************/

#ifndef DEFSTARTUP
#define DEFSTARTUP ".plrc"
#endif
#ifndef SYSTEMHOME
#define SYSTEMHOME "/usr/local/lib/pl"
#endif
#ifndef NOTTYCONTROL
#define NOTTYCONTROL FALSE
#endif

#ifndef ARCH
#define ARCH "unknown"
#endif
#ifndef OS
#define OS "unknown"
#endif

#if O_DYNAMIC_STACKS || O_SHIFT_STACKS
#define DEF_DEFLOCAL	(500*SIZEOF_LONG)
#define DEF_DEFGLOBAL	(1000*SIZEOF_LONG)
#define DEF_DEFTRAIL	(1000*SIZEOF_LONG)
#define DEF_DEFHEAP        0		/* unlimited */
#if O_DYNAMIC_STACKS
#define DEF_DEFARGUMENT (250*SIZEOF_LONG)
#else
#define DEF_DEFARGUMENT (4*SIZEOF_LONG)
#endif /*O_DYNAMIC_STACKS*/
#else					/* static stack areas */
#define DEF_DEFLOCAL	(50*SIZEOF_LONG)
#define DEF_DEFGLOBAL	(80*SIZEOF_LONG)
#define DEF_DEFTRAIL	(80*SIZEOF_LONG)
#define DEF_DEFARGUMENT	(4*SIZEOF_LONG)
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
