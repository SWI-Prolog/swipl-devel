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

#ifndef _PL_STREAM_H
#define _PL_STREAM_H

#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

		 /*******************************
		 *	       EXPORT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See SWI-Prolog.h, containing the same code   for  an explanation on this
stuff.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef _PL_EXPORT_DONE
#define _PL_EXPORT_DONE
#if defined(WIN32) && !defined(__WIN32__)
#define __WIN32__
#endif

#if (defined(__WIN32__) || defined(__CYGWIN32__)) && !defined(__LCC__)
#define HAVE_DECLSPEC
#endif

#ifdef HAVE_DECLSPEC
# ifdef PL_KERNEL
#define PL_EXPORT(type)		__declspec(dllexport) type
#define PL_EXPORT_DATA(type)	__declspec(dllexport) type
#define install_t	 	void
# else
#  ifdef __BORLANDC__
#define PL_EXPORT(type)	 	type _stdcall
#define PL_EXPORT_DATA(type)	extern type
#  else
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	__declspec(dllimport) type
#  endif
#define install_t	 	__declspec(dllexport) void
# endif
#else /*HAVE_DECLSPEC*/
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	extern type
#define install_t	 	void
#endif /*HAVE_DECLSPEC*/
#endif /*_PL_EXPORT_DONE*/

		 /*******************************
		 *	    CONSTANTS		*
		 *******************************/

#ifndef EOF
#define EOF (-1)
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

#if defined(__WIN32__) && !defined(EWOULDBLOCK)
#define EWOULDBLOCK	1000		/* Needed for socket handling */
#endif
#define EPLEXCEPTION	1001		/* errno: pending Prolog exception */

#define SIO_BUFSIZE	(4096)		/* buffering buffer-size */
#define SIO_LINESIZE	(1024)		/* Sgets() default buffer size */
#define SIO_MAGIC	(7212676)	/* magic number */
#define SIO_CMAGIC	(42)		/* we are close (and thus illegal!) */

typedef int   (*Sread_function)(void *handle, char *buf, int bufsize);
typedef int   (*Swrite_function)(void *handle, char*buf, int bufsize);
typedef long  (*Sseek_function)(void *handle, long pos, int whence);
typedef int   (*Sclose_function)(void *handle);
typedef int   (*Scontrol_function)(void *handle, int action, void *arg);

#if defined(O_PLMT) && defined(PL_KERNEL)
#include "pl-mutex.h"
#define IOLOCK recursiveMutex
#else
typedef void *		IOLOCK;		/* Definition for external use */
#endif

typedef struct io_functions
{ Sread_function	read;		/* fill the buffer */
  Swrite_function	write;		/* empty the buffer */
  Sseek_function	seek;		/* seek to position */
  Sclose_function	close;		/* close stream */
  Scontrol_function	control;	/* Info/control */
} IOFUNCTIONS;

typedef struct io_position
{ long			charno;		/* character position in file */
  int			lineno;		/* lineno in file */
  int			linepos;	/* position in line */
} IOPOS;

typedef enum
{ ENC_UNKNOWN = 0,			/* invalid/unknown */
  ENC_NONE,				/* raw 8 bit input */
  ENC_ASCII,
  ENC_ISO_LATIN_1,
  ENC_UTF8,
  ENC_UNICODE_BE,			/* big endian unicode file */
  ENC_UNICODE_LE,			/* little endian unicode file */
  ENC_WCHAR				/* pl_wchar_t */
} IOENC;

typedef struct io_stream
{ char		       *bufp;		/* `here' */
  char		       *limitp;		/* read/write limit */
  char		       *buffer;		/* the buffer */
  char		       *unbuffer;	/* Sungetc buffer */
  int			lastc;		/* last character written */
  int			magic;		/* magic number SIO_MAGIC */
  int  			bufsize;	/* size of the buffer */
  int			flags;		/* Status flags */
  IOPOS			posbuf;		/* location in file */
  IOPOS *		position;	/* pointer to above */
  void		       *handle;		/* function's handle */
  IOFUNCTIONS	       *functions;	/* open/close/read/write/seek */
  int		        locks;		/* lock/unlock count */
  IOLOCK *		mutex;		/* stream mutex */
					/* SWI-Prolog 4.0.7 */
  void			(*close_hook)(void* closure);
  void *		closure;
					/* SWI-Prolog 5.1.3 */
  int			timeout;	/* timeout (milliseconds) */
					/* SWI-Prolog 5.4.4 */
  char *		message;	/* error/warning message */
  IOENC			encoding;	/* character encoding used */
  long			reserved[8];	/* reserved for extension */
} IOSTREAM;


#define SmakeFlag(n)	(1<<(n-1))

#define SIO_FBUF	SmakeFlag(1)	/* full buffering */
#define SIO_LBUF	SmakeFlag(2)	/* line buffering */
#define SIO_NBUF	SmakeFlag(3)	/* no buffering */
#define SIO_FEOF	SmakeFlag(4)	/* end-of-file */
#define SIO_FERR	SmakeFlag(5)	/* error ocurred */
#define SIO_USERBUF	SmakeFlag(6)	/* buffer is from user */
#define SIO_INPUT	SmakeFlag(7)	/* input stream */
#define SIO_OUTPUT	SmakeFlag(8)	/* output stream */
#define SIO_NOLINENO	SmakeFlag(9)	/* line no. info is void */
#define SIO_NOLINEPOS	SmakeFlag(10)	/* line pos is void */
#define SIO_STATIC	SmakeFlag(11)	/* Stream in static memory */
#define SIO_RECORDPOS	SmakeFlag(12)	/* Maintain position */
#define SIO_FILE	SmakeFlag(13)	/* Stream refers to an OS file */
#define SIO_PIPE	SmakeFlag(14)	/* Stream refers to an OS pipe */
#define SIO_NOFEOF	SmakeFlag(15)	/* don't set SIO_FEOF flag */
#define SIO_TEXT	SmakeFlag(16)	/* text-mode operation */
#define SIO_FEOF2	SmakeFlag(17)	/* attempt to read past eof */
#define SIO_FEOF2ERR	SmakeFlag(18)	/* Sfpasteof() */
#define SIO_NOCLOSE     SmakeFlag(19)	/* Do not close on abort */
#define SIO_APPEND	SmakeFlag(20)	/* opened in append-mode */
#define SIO_UPDATE	SmakeFlag(21)	/* opened in update-mode */
#define SIO_ISATTY	SmakeFlag(22)	/* Stream is a tty */
#define SIO_CLOSING	SmakeFlag(23)	/* We are closing the stream */
#define SIO_TIMEOUT	SmakeFlag(24)	/* We had a timeout */
#define SIO_NOMUTEX	SmakeFlag(25)	/* Do not allow multi-thread access */
#define SIO_ADVLOCK	SmakeFlag(26)	/* File locked with advisory lock */
#define SIO_WARN	SmakeFlag(27)	/* Pending warning */
#define SIO_CLEARERR	SmakeFlag(28)	/* Clear error after reporting */

#define	SIO_SEEK_SET	0	/* From beginning of file.  */
#define	SIO_SEEK_CUR	1	/* From current position.  */
#define	SIO_SEEK_END	2	/* From end of file.  */

PL_EXPORT(IOSTREAM *)	S__getiob(void);	/* get DLL's __iob[] address */

PL_EXPORT_DATA(IOFUNCTIONS)  Sfilefunctions;	/* OS file functions */
PL_EXPORT_DATA(int)	     Slinesize;		/* Sgets() linesize */
#if defined(__CYGWIN32__) && !defined(PL_KERNEL)
#define S__iob S__getiob()
#else
PL_EXPORT_DATA(IOSTREAM)    S__iob[3];		/* Libs standard streams */
#endif

#define Sinput  (&S__iob[0])		/* Stream Sinput */
#define Soutput (&S__iob[1])		/* Stream Soutput */
#define Serror  (&S__iob[2])		/* Stream Serror */

#define Sgetchar()	Sgetc(Sinput)
#define Sputchar(c)	Sputc((c), Soutput)

#define S__updatefilepos(s, c) \
	((s)->position ? S__fupdatefilepos((s), (c)) \
		       : (c))

#define Snpgetc(s) ((s)->bufp < (s)->limitp ? (int)(*(s)->bufp++)&0xff \
					    : S__fillbuf(s))
#define Sgetc(s) S__updatefilepos((s), Snpgetc(s))

/* Control-operations */
#define SIO_GETSIZE	(1)		/* get size of underlying object */
#define SIO_GETFILENO	(2)		/* get underlying file (if any) */

/* Sread_pending() */
#define SIO_RP_BLOCK 0x1		/* wait for new input */

#if IOSTREAM_REPLACES_STDIO

#undef FILE
#undef stdin
#undef stdout
#undef stderr
#undef putc
#undef getc
#undef putchar
#undef getchar
#undef feof
#undef ferror
#undef fileno
#undef clearerr

#define FILE		IOSTREAM
#define stdin		Sinput
#define stdout		Soutput
#define stderr		Serror

#define	putc		Sputc
#define	getc		Sgetc
#define	fputc		Sputc
#define	fgetc		Sgetc
#define getw		Sgetw
#define putw		Sputw
#define fread		Sfread
#define fwrite		Sfwrite
#define	ungetc		Sungetc
#define putchar		Sputchar
#define getchar		Sgetchar
#define feof		Sfeof
#define ferror		Sferror
#define clearerr	Sclearerr
#define	fflush		Sflush
#define	fseek		Sseek
#define	ftell		Stell
#define	fclose		Sclose
#define fgets		Sfgets
#define gets		Sgets
#define	fputs		Sfputs
#define	puts		Sputs
#define	fprintf		Sfprintf
#define	printf		Sprintf
#define	vprintf		Svprintf
#define	vfprintf	Svfprintf
#define	sprintf		Ssprintf
#define	vsprintf	Svsprintf
#define fopen		Sopen_file
#define fdopen		Sfdopen
#define	fileno		Sfileno
#define popen		Sopen_pipe

#endif /*IOSTREAM_REPLACES_STDIO*/

		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

PL_EXPORT(void)		SinitStreams();
PL_EXPORT(void)		Scleanup(void);
PL_EXPORT(int)		S__fupdatefilepos(IOSTREAM *s, int c);
PL_EXPORT(int)		S__fillbuf(IOSTREAM *s);
					/* byte I/O */
PL_EXPORT(int)		Sputc(int c, IOSTREAM *s);
PL_EXPORT(int)		Sfgetc(IOSTREAM *s);
PL_EXPORT(int)		Sungetc(int c, IOSTREAM *s);
					/* multibyte I/O */
PL_EXPORT(int)		Sputcode(int c, IOSTREAM *s);
PL_EXPORT(int)		Sgetcode(IOSTREAM *s);
PL_EXPORT(int)		Sungetcode(int c, IOSTREAM *s);
					/* word I/O */
PL_EXPORT(int)		Sputw(int w, IOSTREAM *s);
PL_EXPORT(int)		Sgetw(IOSTREAM *s);
PL_EXPORT(int)		Sfread(void *data, int size, int elems, IOSTREAM *s);
PL_EXPORT(int)		Sfwrite(const void *data, int size, int elems,
				IOSTREAM *s);
PL_EXPORT(int)		Sfeof(IOSTREAM *s);
PL_EXPORT(int)		Sfpasteof(IOSTREAM *s);
PL_EXPORT(int)		Sferror(IOSTREAM *s);
PL_EXPORT(void)		Sclearerr(IOSTREAM *s);
PL_EXPORT(void)		Sseterr(IOSTREAM *s, int which, const char *message);
PL_EXPORT(int)		Sflush(IOSTREAM *s);
PL_EXPORT(long)		Ssize(IOSTREAM *s);
PL_EXPORT(long)		Sseek(IOSTREAM *s, long pos, int whence);
PL_EXPORT(long)		Stell(IOSTREAM *s);
PL_EXPORT(int)		Sclose(IOSTREAM *s);
PL_EXPORT(char *)	Sfgets(char *buf, int n, IOSTREAM *s);
PL_EXPORT(char *)	Sgets(char *buf);
PL_EXPORT(int)		Sread_pending(IOSTREAM *s,
				      char *buf, int limit, int flags);
PL_EXPORT(int)		Sfputs(const char *q, IOSTREAM *s);
PL_EXPORT(int)		Sputs(const char *q);
PL_EXPORT(int)		Sfprintf(IOSTREAM *s, const char *fm, ...);
PL_EXPORT(int)		Sprintf(const char *fm, ...);
PL_EXPORT(int)		Svprintf(const char *fm, va_list args);
PL_EXPORT(int)		Svfprintf(IOSTREAM *s, const char *fm, va_list args);
PL_EXPORT(int)		Ssprintf(char *buf, const char *fm, ...);
PL_EXPORT(int)		Svsprintf(char *buf, const char *fm, va_list args);
PL_EXPORT(int)		Svdprintf(const char *fm, va_list args);
PL_EXPORT(int)		Sdprintf(const char *fm, ...);
PL_EXPORT(int)		Slock(IOSTREAM *s);
PL_EXPORT(int)		StryLock(IOSTREAM *s);
PL_EXPORT(int)		Sunlock(IOSTREAM *s);
PL_EXPORT(IOSTREAM *)	Snew(void *handle, int flags, IOFUNCTIONS *functions);
PL_EXPORT(IOSTREAM *)	Sopen_file(const char *path, const char *how);
PL_EXPORT(IOSTREAM *)	Sfdopen(int fd, const char *type);
PL_EXPORT(int)	   	Sfileno(IOSTREAM *s);
PL_EXPORT(IOSTREAM *)	Sopen_pipe(const char *command, const char *type);
PL_EXPORT(IOSTREAM *)	Sopenmem(char **buffer, int *sizep, const char *mode);
PL_EXPORT(IOSTREAM *)	Sopen_string(IOSTREAM *s, char *buf, int sz, const char *m);
PL_EXPORT(int)		Sclosehook(void (*hook)(IOSTREAM *s));
PL_EXPORT(void)		Sfree(void *ptr);

#ifdef __cplusplus
}
#endif

#endif /*_PL_STREAM_H*/
