/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Describe your OS here
*/

#ifdef TIME_INCLUDE
#include TIME_INCLUDE
#else
#include <sys/time.h>
#endif

#if tos
struct timeval
{ long tv_sec;
  long tv_usec;
};
#endif

		/********************************
		*             OS-TYPES		*
		********************************/

extern int	puti P((int, FILE *));
extern int	geti P((FILE *));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The types below should (mostly) be in stdlib.h.  They are not and this
file keeps GCC silent while using the -Wall flag.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if sun
extern int	getpid P((void));
extern int	isatty P((int));
extern int	fclose P(( FILE * ));
extern int	pclose P((FILE *));
extern int	_filbuf P((FILE *));
extern int	_flsbuf P((unsigned char, FILE *));
extern int	fflush P((FILE *));
extern char *   vsprintf P((char *, char *, va_list));
extern void	bzero P((Void, int));
extern void	exit P((int));
extern int	close P((int));
extern int	read P((int, Void, int));
extern int	access P((char *, int));
extern unsigned	sleep P((unsigned));
extern int	fprintf P((FILE *, char *, ...));
extern int	printf P((char *, ...));
extern long	putw P((long, FILE *));
extern long	getw P((FILE *));
extern char *	index P((char *, char));
extern int	write P((int, Void, int));
extern int	gettimeofday P((struct timeval *, struct timeval *));
extern long	strtol P((char *, char**, int));
extern int	vfprintf P((FILE *, char *, ...));
#endif


		/********************************
		*        MEMORY MANAGEMENT      *
		*********************************/

#if !ANSI
#define memcpy(to, from, n)	bcopy(from, to, n)
#endif


#define malloc_t	size_t		/* Argument type of malloc(), etc */
#define alloc_t		size_t		/* argument type of Prolog's alloc */

#define Malloc(n)	malloc((malloc_t) (n))
#define Free(p)		free((char *)(p))
#define Realloc(p, n)	realloc((char *)p, (malloc_t)(n))

#define allocHeap(n)	alloc_heap((alloc_t) (n))
#define freeHeap(p, n)	free_heap((char *)(p), (alloc_t)(n))
#define allocGlobal(n)	alloc_global((alloc_t) (n))
#define allocLocal(n)	alloc_local((alloc_t) (n))

extern Void Allocate P((long));

		/********************************
		*         MISCELLANEOUS         *
		*********************************/

extern char *Setenv P((char *name, char *value));
extern char *Unsetenv P((char *name));

extern long Time P((void));
extern char *OsError P((void));
extern bool initOs P((void));
extern volatile void Halt P((int));

		/********************************
		*           ARITHMETIC          *
		*********************************/

extern long Random P((void));


		/********************************
		*              FILES            *
		*********************************/

#ifndef STREAM_OPEN_BIN_READ
#define STREAM_OPEN_BIN_READ "rb"
#endif

#ifndef STREAM_OPEN_BIN_WRITE
#define STREAM_OPEN_BIN_WRITE "wb"
#endif

#if unix
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

Char		GetChar P((void));
Atom		TemporaryFile P((char *key));
void		RemoveTemporaryFiles P((void));
int		GetDTableSize P((void));
long		LastModifiedFile P((char *name)),
		SizeFile P((char *name));
bool		AccessFile P((char *name, int how)),
		ExistsFile P((char *name)),
		ExistsDirectory P((char *name)),
		DeleteFile P((char *name)),
		RenameFile P((char *old, char *new)),
		SameFile P((char *file1, char *file2)),
		OpenStream P((int index)),
		MarkExecutable P((char *name)),
		expandVars P((char *pattern, char *expanded)),
		ChDir P((char *dir));
char 		*AbsoluteFile P((char *)),
		*ExpandOneFile P((char *)),
		*BaseName P((char *)),
		*DirName P((char *)),
		*PrologPath P((char *)),
		*OsPath P((char *));

#define ACCESS_EXECUTE	1
#define ACCESS_READ	2
#define ACCESS_WRITE	4

		/********************************
		*        TIME CONVERSION        *
		*********************************/

extern struct tm *LocalTime P((long *));
extern real	  CpuTime P((void));


		/********************************
		*       FILE DESCR. SETS	*
		********************************/

#ifndef FD_ZERO
/* typedef ulong fd_set;		prior SunOs 4.0 compatibility */
#define FD_ZERO(s)	{ *((ulong *)(s)) = (0L); }
#define FD_SET(fd, s)	{ *((ulong *)(s)) |= (1L << (fd)); }
#define FD_ISSET(fd, s) ( (*((ulong *)(s)) & (1L << (fd))) != 0 )
#endif

		/********************************
		*        TERMINAL CONTROL       *
		*********************************/

#define TTY_COOKED	 1		/* Initial mode: echo */
#define TTY_EXTEND_ATOMS 2		/* Atom-completion Mode: echo */
#define TTY_APPEND	 3		/* Add input from Prolog: echo */
#define TTY_RAW		 4		/* Non-blocking, non-echo */
#define TTY_RETYPE	 5		/* Retype input: non-echo */
#define	TTY_SAVE	 6		/* Save parameters only */

#if unix
#if O_TERMIOS
#ifdef TERMIO_INCLUDE
#include TERMIO_INCLUDE
#else
#include <sys/termio.h>
#endif

typedef struct
{ struct termio tab;		/* saved tty status */
  int		mode;		/* Prolog;'s view on mode */
} ttybuf;

#else O_TERMIOS

#include <sgtty.h>

typedef struct
{ struct sgttyb tab;		/* saved tty flags */
  struct tchars chars;		/* tty characters */
  int		mode;		/* Prolog's view on mode */
} ttybuf;
#endif O_TERMIOS

#else  unix

typedef struct
{ int		mode;		/* Prolog's view on mode */
} ttybuf;
#endif unix

extern ttybuf	ttytab;			/* saved tty status */
extern int	ttymode;		/* Current tty mode */

#define IsaTty(fd)	isatty(fd)

extern bool PushTty P((ttybuf *, int mode));
extern bool PopTty P((ttybuf *));
extern void PretendTyped P((char));
extern void ResetTty P((void));
extern void TtyAddChar P((Char));


		/********************************
		*        PROCESS CONTROL        *
		*********************************/

#define Wait(stat)	wait(stat)

extern int System P((char *command));
extern void Sleep P((real time));
extern char *Symbols P((void));
