/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog IO streams
*/

#ifndef _PL_STREAM_H
#define _PL_STREAM_H

#include <stdarg.h>

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
  unsigned char	       *unbuffer;	/* ungetc buffer */
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
extern IOSTREAM    S__iob[];		/* Standard IO */
extern int	   Slinesize;		/* Sgets() linesize (SIO_LINESIZE) */

#define Sinput  (&S__iob[0])		/* Stream stdin */
#define Soutput (&S__iob[1])		/* Stream stdout */
#define Serror  (&S__iob[2])		/* Stream stderr */

#define Sgetchar()	Sgetc(Sinput)
#define Sputchar(c)	Sputc((c), Soutput)

#ifdef IOSTREAM_REPLACES_STDIO

#undef FILE
#undef stdin
#undef stderr
#undef putc
#undef getc

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

int		Sputc(int c, IOSTREAM *s);
int		Sgetc(IOSTREAM *s);
int		Sungetc(int c, IOSTREAM *s);
int		Sputw(int w, IOSTREAM *s);
int		Sgetw(IOSTREAM *s);
int		Sfeof(IOSTREAM *s);
int		Sferror(IOSTREAM *s);
void		Sclearerr(IOSTREAM *s);
int		Sflush(IOSTREAM *s);
long		Sseek(IOSTREAM *s, long pos, int whence);
long		Stell(IOSTREAM *s);
int		Sclose(IOSTREAM *s);
char *		Sfgets(char *buf, int n, IOSTREAM *s);
char *		Sgets(char *buf);
int		Sfputs(const char *q, IOSTREAM *s);
int		Sputs(const char *q);
int		Sfprintf(IOSTREAM *s, const char *fm, ...);
int		Sprintf(const char *fm, ...);
int		Svprintf(const char *fm, va_list args);
int		Svfprintf(IOSTREAM *s, const char *fm, va_list args);
int		Ssprintf(char *buf, const char *fm, ...);
int		Svsprintf(char *buf, const char *fm, va_list args);
IOSTREAM *	Snew(void *handle, int flags, IOFUNCTIONS *functions);
IOSTREAM *	Sopen_file(char *path, char *how);
IOSTREAM *	Sfdopen(int fd, char *type);
int		Sfileno(IOSTREAM *s);
IOSTREAM *	Sopen_pipe(const char *command, const char *type);
IOSTREAM *	Sopenmem(char **buffer, int *sizep, char *mode);
IOSTREAM *	Sopen_string(IOSTREAM *s, char *buf, int size, char *mode);


#endif /*_PL_STREAM_H*/
