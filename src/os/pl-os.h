/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2015, University of Amsterdam
                              VU University Amsterdam
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
extern int	ttymodified;		/* Did we modify the tty */

#define IsaTty(fd)	isatty(fd)

extern bool PushTty(IOSTREAM *s, ttybuf *buf, int mode);
extern bool PopTty(IOSTREAM *s, ttybuf *buf, int do_free);
extern void ResetTty(void);


		/********************************
		*        PROCESS CONTROL        *
		*********************************/

extern int System(char *command);
