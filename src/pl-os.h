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

#ifndef HAVE_GETTIMEOFDAY
struct timeval
{ long tv_sec;
  long tv_usec;
};
#endif



		/********************************
		*             OS-TYPES		*
		********************************/

extern int	puti(int, FILE *);
extern int	geti(FILE *);

		/********************************
		*        MEMORY MANAGEMENT      *
		*********************************/

#define alloc_t		size_t		/* argument type of Prolog's alloc */

#define Malloc(n)	malloc((size_t) (n))
#define Free(p)		free((char *)(p))
#define Realloc(p, n)	realloc((char *)p, (size_t)(n))

#define allocHeap(n)	alloc_heap((alloc_t) (n))
#define freeHeap(p, n)	free_heap((char *)(p), (alloc_t)(n))
#define allocGlobal(n)	alloc_global((alloc_t) (n))
#define allocLocal(n)	alloc_local((alloc_t) (n))

extern Void Allocate(long);

		/********************************
		*         MISCELLANEOUS         *
		*********************************/

extern char *Setenv(char *name, char *value);
extern char *Unsetenv(char *name);

extern long Time(void);
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
#define Popen(path, m)	popen(OsPath(path), m)
#define Pclose(fd)	pclose(fd)
#endif

#if tos
#define MAXPATHLEN	PATH_MAX
#endif


#define Fflush(fd)		fflush(fd)
#define Fopen(path, m)		fopen(OsPath(path), m)
#define Fclose(fd)		fclose(fd)
#define Open(path, how, mode)	open(OsPath(path), how, mode)
#define Close(fd)		close(fd)
#define Read(fd, buf, size)	read(fd, buf, size)
#define Write(fd, buf, size)	write(fd, buf, size)
#define Getc(fd)		getc(fd)
#define Putc(c, fd)		putc((char)(c), fd)
#define Putw(w, fd)		putw((long)(w), fd)
#define Getw(fd)		getw(fd)

Char		GetChar(void);
Atom		TemporaryFile(char *key);
void		RemoveTemporaryFiles(void);
long		LastModifiedFile(char *name),
		SizeFile(char *name);
bool		AccessFile(char *name, int how),
		ExistsFile(char *name),
		ExistsDirectory(char *name),
		DeleteFile(char *name),
		RenameFile(char *old, char *new),
		SameFile(char *file1, char *file2),
		OpenStream(int index),
		MarkExecutable(char *name),
		expandVars(char *pattern, char *expanded),
		ChDir(char *dir);
char 		*AbsoluteFile(char *),
		*ExpandOneFile(char *),
		*BaseName(char *),
		*DirName(char *),
		*PrologPath(char *),
		*OsPath(char *),
		*ReadLink(char *),
		*DeRefLink(char *);

#ifndef HAVE_GETDTABLESIZE
extern int	getdtablesize(void);
#endif
#ifndef HAVE_GETPAGESIZE
extern int	getpagesize(void);
#endif

#define ACCESS_EXECUTE	1
#define ACCESS_READ	2
#define ACCESS_WRITE	4

		/********************************
		*        TIME CONVERSION        *
		*********************************/

extern struct tm *LocalTime(long *);
extern real	  CpuTime(void);


		/********************************
		*       FILE DESCR. SETS	*
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

#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#define termios termio
#define O_TERMIO 1
#else
#ifdef HAVE_SYS_TERMIOS_H
#include <sys/termios.h>
#define O_TERMIO 1
#endif
#endif

#ifdef O_TERMIO

typedef struct
{
  struct termios tab;		/* saved tty status */
  int		mode;		/* Prolog;'s view on mode */
} ttybuf;

#else /* !O_TERMIO */

typedef struct
{ int		mode;		/* Prolog;'s view on mode */
} ttybuf;

#endif /* O_TERMIO */

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
extern void Sleep(real time);
extern char *Symbols(void);
