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
#ifdef HAVE_SYS_PARAM_H			/* get MAXPATHLEN */
#include <sys/param.h>
#endif


		/********************************
		*        MEMORY MANAGEMENT      *
		*********************************/

extern Void Allocate(long);

		/********************************
		*         MISCELLANEOUS         *
		*********************************/

extern char *OsError(void);
extern bool initOs(void);

		/********************************
		*           ARITHMETIC          *
		*********************************/

extern long Random(void);


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


#define Fflush(fd)		Sflush(fd)
#define Fclose(fd)		Sclose(fd)
#define Open(path, how, mode)	open(OsPath(path), how, mode)
#define Read(fd, buf, size)	read(fd, buf, size)
#define Write(fd, buf, size)	write(fd, buf, size)
#define Getc(fd)		Sgetc(fd)
#define Putw(w, fd)		Sputw((long)(w), fd)
#define Getw(fd)		Sgetw(fd)

		 /*******************************
		 *      PAGE AND TABLE-SIZE	*
		 *******************************/

#ifdef HAVE_SYSCONF
#if defined(_SC_OPEN_MAX) && !defined(HAVE_GETPAGESIZE)
#undef getdtablesize
#define getdtablesize() sysconf(_SC_OPEN_MAX)
#ifndef HAVE_GETDTABLESIZE
#define HAVE_GETDTABLESIZE 1
#endif
#endif
#if defined(_SC_PAGESIZE) && !defined(HAVE_GETPAGESIZE)
#undef getpagesize
#define getpagesize() sysconf(_SC_PAGESIZE)
#ifndef HAVE_GETPAGESIZE
#define HAVE_GETPAGESIZE 1
#endif
#endif
#endif /*HAVE_SYSCONF*/

#ifndef HAVE_GETDTABLESIZE
extern int	getdtablesize(void);
#endif
#ifndef HAVE_GETPAGESIZE
extern int	getpagesize(void);
#endif

		 /*******************************
		 *	    FILE ACCESS		*
		 *******************************/

#define ACCESS_EXIST	0
#define ACCESS_EXECUTE	1
#define ACCESS_READ	2
#define ACCESS_WRITE	4

		 /*******************************
		 *               TYPES		*
		 *******************************/

#if 0 && defined(__sun__) && defined(__svr4__)
extern long random(void);
extern int srandom(unsigned seed);
#endif

		/********************************
		*        TIME CONVERSION        *
		*********************************/

typedef enum
{ CPU_USER,
  CPU_SYSTEM
} cputime_kind;

extern double	  CpuTime(cputime_kind);
extern double	  WallTime(void);
extern struct tm *LocalTime(long *, struct tm *r);


		 /*******************************
		 *	      MEMORY		*
		 *******************************/

extern unsigned long	UsedMemory(void);
extern unsigned long	FreeMemory(void);


		/********************************
		*       IOSTREAM DESCR. SETS	*
		********************************/

#if !defined(FD_ZERO) && !defined(WIN32)
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#else
#define FD_ZERO(s)	{ *((unsigned long *)(s)) = (0L); }
#define FD_SET(fd, s)	{ *((unsigned long *)(s)) |= (1L << (fd)); }
#define FD_ISSET(fd, s) ( (*((unsigned long *)(s)) & (1L << (fd))) != 0 )
#endif
#endif

		/********************************
		*        TERMINAL CONTROL       *
		*********************************/

#define TTY_COOKED	 1		/* Initial mode: echo */
#define TTY_RAW		 2		/* Non-blocking, non-echo */
#define TTY_OUTPUT	 3		/* enable post-processing */
#define TTY_SAVE	 4		/* just save status */

#ifdef HAVE_TCSETATTR
#include <termios.h>
#include <unistd.h>
#define O_HAVE_TERMIO 1
#else /*HAVE_TCSETATTR*/
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#define termios termio
#define O_HAVE_TERMIO 1
#else
#ifdef HAVE_SYS_TERMIOS_H
#include <sys/termios.h>
#define O_HAVE_TERMIO 1
#endif
#endif
#endif /*HAVE_TCSETATTR*/

#ifdef O_HAVE_TERMIO

typedef struct
{ struct termios tab;		/* saved tty status */
  int		mode;		/* Prolog;'s view on mode */
} ttybuf;

#else /* !O_HAVE_TERMIO */

#ifdef HAVE_SGTTYB
#include <sys/ioctl.h>
typedef struct
{ struct sgttyb tab;		/* saved tty status */
  int		mode;		/* Prolog;'s view on mode */
} ttybuf;

#else

typedef struct
{ int		mode;		/* Prolog;'s view on mode */
} ttybuf;

#endif /*HAVE_SGTTYB*/
#endif /*O_HAVE_TERMIO*/

extern ttybuf	ttytab;			/* saved tty status */
extern int	ttymode;		/* Current tty mode */

#define IsaTty(fd)	isatty(fd)

extern bool PushTty(IOSTREAM *s, ttybuf *, int mode);
extern bool PopTty(IOSTREAM *s, ttybuf *);
extern void ResetTty(void);


		/********************************
		*        PROCESS CONTROL        *
		*********************************/

#define Wait(stat)	wait(stat)

extern int System(char *command);
