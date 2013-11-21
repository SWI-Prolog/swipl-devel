/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

#ifdef HAVE_SYS_PARAM_H			/* get MAXPATHLEN */
#include <sys/param.h>
#endif


		/********************************
		*         MISCELLANEOUS         *
		*********************************/

extern char *OsError(void);
extern bool initOs(void);


		/********************************
		*              FILES            *
		*********************************/

#ifndef STREAM_OPEN_BIN_READ
#define STREAM_OPEN_BIN_READ "rb"
#endif

#ifndef STREAM_OPEN_BIN_WRITE
#define STREAM_OPEN_BIN_WRITE "wb"
#endif

#ifdef HAVE_POPEN
#define PIPE 1
#define Popen(path, m)	Sopen_pipe(OsPath(path), m)
#define Pclose(fd)	pclose(fd)
#endif

#ifndef MAXPATHLEN
#ifdef PATH_MAX
#define MAXPATHLEN PATH_MAX
#else
#ifdef PATHSIZE
#define MAXPATHLEN PATHSIZE
#endif
#endif
#endif

COMMON(char*)	canonicaliseFileName(char *path);


		/********************************
		*        TIME CONVERSION        *
		*********************************/

typedef enum
{ CPU_USER,
  CPU_SYSTEM
} cputime_kind;

extern double	  CpuTime(cputime_kind);
extern double	  WallTime(void);


		 /*******************************
		 *	      MEMORY		*
		 *******************************/

extern uintptr_t	UsedMemory(void);
extern uintptr_t	FreeMemory(void);


		/********************************
		*       IOSTREAM DESCR. SETS	*
		********************************/

#if !defined(FD_ZERO) && !defined(__WINDOWS__)
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#else
#define FD_ZERO(s)	{ *((uintptr_t *)(s)) = (0L); }
#define FD_SET(fd, s)	{ *((uintptr_t *)(s)) |= ((uintptr_t)L << (fd)); }
#define FD_ISSET(fd, s) ( (*((uintptr_t *)(s)) & ((uintptr_t)L << (fd))) != 0 )
#endif
#endif

		/********************************
		*        TERMINAL CONTROL       *
		*********************************/

#define TTY_COOKED	 1		/* Initial mode: echo */
#define TTY_RAW		 2		/* Non-blocking, non-echo */
#define TTY_OUTPUT	 3		/* enable post-processing */
#define TTY_SAVE	 4		/* just save status */

typedef struct
{ void *state;				/* Saved state */
  int   mode;				/* Prolog;'s view on mode */
} ttybuf;

extern ttybuf	ttytab;			/* saved tty status */
extern int	ttymode;		/* Current tty mode */

#define IsaTty(fd)	isatty(fd)

extern bool PushTty(IOSTREAM *s, ttybuf *buf, int mode);
extern bool PopTty(IOSTREAM *s, ttybuf *buf, int do_free);
extern void ResetTty(void);


		/********************************
		*        PROCESS CONTROL        *
		*********************************/

extern int System(char *command);
