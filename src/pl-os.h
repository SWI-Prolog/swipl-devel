/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Describe your OS here
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
extern volatile void Halt(int);

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
#define Putc(c, fd)		Sputc((char)(c), fd)
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

extern struct tm *LocalTime(long *);
extern real	  CpuTime(void);


		/********************************
		*       IOSTREAM DESCR. SETS	*
		********************************/

#ifndef FD_ZERO
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

extern bool PushTty(ttybuf *, int mode);
extern bool PopTty(ttybuf *);
extern void ResetTty(void);


		/********************************
		*        PROCESS CONTROL        *
		*********************************/

#define Wait(stat)	wait(stat)

extern int System(char *command);
extern char *Which(const char *cmd, char *buf);
