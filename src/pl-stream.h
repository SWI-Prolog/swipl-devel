/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog IO streams
*/

#ifndef _PL_STREAM_H
#define _PL_STREAM_H

#include <stdarg.h>

#ifndef _export
#if defined(_WIN32) && defined(_MAKE_DLL)
#define _export _declspec(dllexport)
#else
#define _export extern
#endif
#endif

#ifndef EOF
#define EOF (-1)
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

#define SIO_BUFSIZE	(4096)		/* buffering buffer-size */
#define SIO_LINESIZE	(1024)		/* Sgets() default buffer size */
#define SIO_MAGIC	(7212676)	/* magic number */

typedef int   (*Sread_function)(void *handle, char *buf, int bufsize);
typedef int   (*Swrite_function)(void *handle, char*buf, int bufsize);
typedef long  (*Sseek_function)(void *handle, long pos, int whence);
typedef int   (*Sclose_function)(void *handle);

typedef struct io_functions
{ Sread_function	read;		/* fill the buffer */
  Swrite_function	write;		/* empty the buffer */
  Sseek_function	seek;		/* seek to position */
  Sclose_function	close;		/* close stream */
} IOFUNCTIONS;

typedef struct io_position
{ long			charno;		/* character position in file */
  int			lineno;		/* lineno in file */
  int			linepos;	/* position in line */
} IOPOS;

typedef struct io_stream
{ unsigned char	       *bufp;		/* `here' */
  unsigned char	       *limitp;		/* read/write limit */
  unsigned char	       *buffer;		/* the buffer */
  unsigned char	       *unbuffer;	/* Sungetc buffer */
  int			magic;		/* magic number SIO_MAGIC */
  int  			bufsize;	/* size of the buffer */
  int			flags;		/* Status flags */
  IOPOS			posbuf;		/* location in file */
  IOPOS *		position;	/* pointer to above */
  void		       *handle;		/* function's handle */
  IOFUNCTIONS	       *functions;	/* open/close/read/write/seek */
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

#define	SIO_SEEK_SET	0	/* From beginning of file.  */
#define	SIO_SEEK_CUR	1	/* From current position.  */
#define	SIO_SEEK_END	2	/* From end of file.  */

extern IOFUNCTIONS Sfilefunctions;	/* OS file functions */
extern int	   Slinesize;		/* Sgets() linesize (SIO_LINESIZE) */

_export IOSTREAM *S__getiob(void);	/* get DLL's S__iob[] address */

#if defined(_WIN32) && !defined(PL_KERNEL) && !defined(_MAKE_DLL)
extern IOSTREAM    *S__iob;		/* Standard IO */
#else
extern IOSTREAM	    S__iob[];
#endif

#define Sinput  (&S__iob[0])		/* Stream Sinput */
#define Soutput (&S__iob[1])		/* Stream Soutput */
#define Serror  (&S__iob[2])		/* Stream Serror */

#define Sgetchar()	Sgetc(Sinput)
#define Sputchar(c)	Sputc((c), Soutput)

#define S__updatefilepos(s, c) \
	((s)->position ? S__fupdatefilepos((s)->position, (c)) \
		       : (c))

#define Snpgetc(s) ((s)->bufp < (s)->limitp ? *(s)->bufp++&0xff \
					    : S__fillbuf(s))
#define Sgetc(s) S__updatefilepos((s), Snpgetc(s))

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

_export int	S__fupdatefilepos(IOPOS *p, int c);
_export int	S__fillbuf(IOSTREAM *s);
_export int	Sputc(int c, IOSTREAM *s);
_export int	Sfgetc(IOSTREAM *s);
_export int	Sungetc(int c, IOSTREAM *s);
_export int	Sputw(int w, IOSTREAM *s);
_export int	Sgetw(IOSTREAM *s);
_export int	Sfread(void *data, int size, int elems, IOSTREAM *s);
_export int	Sfwrite(void *data, int size, int elems, IOSTREAM *s);
_export int	Sfeof(IOSTREAM *s);
_export int	Sferror(IOSTREAM *s);
_export void	Sclearerr(IOSTREAM *s);
_export int	Sflush(IOSTREAM *s);
_export long	Sseek(IOSTREAM *s, long pos, int whence);
_export long	Stell(IOSTREAM *s);
_export int	Sclose(IOSTREAM *s);
_export char *	Sfgets(char *buf, int n, IOSTREAM *s);
_export char *	Sgets(char *buf);
_export int	Sfputs(const char *q, IOSTREAM *s);
_export int	Sputs(const char *q);
_export int	Sfprintf(IOSTREAM *s, const char *fm, ...);
_export int	Sprintf(const char *fm, ...);
_export int	Svprintf(const char *fm, va_list args);
_export int	Svfprintf(IOSTREAM *s, const char *fm, va_list args);
_export int	Ssprintf(char *buf, const char *fm, ...);
_export int	Svsprintf(char *buf, const char *fm, va_list args);
_export int	Svdprintf(const char *fm, va_list args);
_export int	Sdprintf(const char *fm, ...);
_export IOSTREAM * Snew(void *handle, int flags, IOFUNCTIONS *functions);
_export IOSTREAM * Sopen_file(char *path, char *how);
_export IOSTREAM * Sfdopen(int fd, char *type);
_export int	   Sfileno(IOSTREAM *s);
_export IOSTREAM * Sopen_pipe(const char *command, const char *type);
_export IOSTREAM * Sopenmem(char **buffer, int *sizep, char *mode);
_export IOSTREAM * Sopen_string(IOSTREAM *s, char *buf, int size, char *mode);


#endif /*_PL_STREAM_H*/
