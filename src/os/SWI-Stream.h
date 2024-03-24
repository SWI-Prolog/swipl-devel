/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2023, University of Amsterdam
			      VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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

#ifndef _SWI_STREAM_H
#define _SWI_STREAM_H

/* This appears to make the wide-character support compile and work
   on HPUX 11.23.  There really should be a cleaner way ...
*/
#if defined(__hpux)
#include <sys/_mbstate_t.h>
#endif

#ifndef __WINDOWS__
#if defined(_MSC_VER) || defined(__MINGW32__)
#define __WINDOWS__ 1
#endif
#endif

#ifdef __WINDOWS__
#include <winsock2.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <stdarg.h>
#include <wchar.h>
#include <stddef.h>
#include <inttypes.h>			/* more portable than stdint.h */

#ifdef _MSC_VER
typedef __int32 int32_t;
typedef unsigned __int32 uint32_t;
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
typedef intptr_t ssize_t;
typedef uintptr_t size_t;
#endif

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

#if defined(_MSC_VER) || defined(__MINGW32__)
#define HAVE_DECLSPEC
#else
#if !defined(HAVE_VISIBILITY_ATTRIBUTE) && (__GNUC__ >= 4 || defined(__clang__))
#define HAVE_VISIBILITY_ATTRIBUTE 1
#endif
#endif

#ifdef HAVE_DECLSPEC
# ifdef PL_KERNEL
#define PL_EXPORT(type)		__declspec(dllexport) extern type
#define PL_EXPORT_DATA(type)	__declspec(dllexport) extern type
# else
#  ifdef __BORLANDC__
#define PL_EXPORT(type)		type _stdcall
#define PL_EXPORT_DATA(type)	extern type
#  else
#   ifdef __MINGW32__
#define PL_EXPORT(type)		extern type
#define PL_EXPORT_DATA(type)	extern type
#   else
#define PL_EXPORT(type)		extern type
#define PL_EXPORT_DATA(type)	__declspec(dllimport) type
#   endif
#  endif
#define install_t		__declspec(dllexport) void
# endif
#else /*HAVE_DECLSPEC*/
# ifdef PL_SO_EXPORT
#define PL_EXPORT(type)		extern PL_SO_EXPORT type
#define PL_EXPORT_DATA(type)	extern PL_SO_EXPORT type
# else
#define PL_EXPORT(type)		extern type
#define PL_EXPORT_DATA(type)	extern type
# endif
#ifdef HAVE_VISIBILITY_ATTRIBUTE
#define install_t		__attribute__((visibility("default"))) void
#else
#define install_t		void
#endif
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

#if defined(__WINDOWS__) && !defined(EWOULDBLOCK)
#define EWOULDBLOCK	140		/* Needed for socket handling */
					/* 140 is compatible to VS2010 */
#endif
#define EPLEXCEPTION	1001		/* errno: pending Prolog exception */

#define SIO_BUFSIZE	(4096)		/* buffering buffer-size */
#define SIO_LINESIZE	(1024)		/* Sgets() default buffer size */
#define SIO_OMAGIC	(7212676)	/* old magic number */
#define SIO_MAGIC	(7212677)	/* magic number */
#define SIO_CMAGIC	(42)		/* we are close (and thus illegal!) */

typedef ssize_t (*Sread_function)(void *handle, char *buf, size_t bufsize);
typedef ssize_t (*Swrite_function)(void *handle, char*buf, size_t bufsize);
typedef long  (*Sseek_function)(void *handle, long pos, int whence);
typedef int64_t (*Sseek64_function)(void *handle, int64_t pos, int whence);
typedef int   (*Sclose_function)(void *handle);
typedef int   (*Scontrol_function)(void *handle, int action, void *arg);

typedef struct recursiveMutex IOLOCK;

#ifndef PL_HAVE_TERM_T
#define PL_HAVE_TERM_T
typedef uintptr_t	term_t;		/* opaque term handle */
#endif
#ifndef PL_HAVE_ATOM_T
#define PL_HAVE_ATOM_T
typedef uintptr_t	atom_t;		/* opaque handle to an atom */
#endif

typedef struct io_functions
{ Sread_function	read;		/* fill the buffer */
  Swrite_function	write;		/* empty the buffer */
  Sseek_function	seek;		/* seek to position */
  Sclose_function	close;		/* close stream */
  Scontrol_function	control;	/* Info/control */
  Sseek64_function	seek64;		/* seek to position (large files) */
} IOFUNCTIONS;

typedef struct io_position
{ int64_t		byteno;		/* byte-position in file */
  int64_t		charno;		/* character position in file */
  int			lineno;		/* lineno in file */
  int			linepos;	/* position in line */
  intptr_t		reserved[2];	/* future extensions */
} IOPOS;

					/* NOTE: check with encoding_names */
					/* in pl-file.c */
#undef IOENC
typedef enum
{ ENC_UNKNOWN = 0,			/* invalid/unknown */
  ENC_OCTET,				/* raw 8 bit input */
  ENC_ASCII,				/* US-ASCII (0..127) */
  ENC_ISO_LATIN_1,			/* ISO Latin-1 (0..256) */
  ENC_ANSI,				/* default (multibyte) codepage */
  ENC_UTF8,
  ENC_UTF16BE,				/* big endian UTF-16 */
  ENC_UTF16LE,				/* little endian UTF-16 file */
  ENC_WCHAR				/* wchar_t */
} IOENC;

#define ENC_UNICODE_BE ENC_UTF16BE
#define ENC_UNICODE_LE ENC_UTF16LE


#define SIO_NL_POSIX  0			/* newline as \n */
#define SIO_NL_DOS    1			/* newline as \r\n */
#define SIO_NL_DETECT 3			/* detect processing mode */

typedef struct io_stream
{ char		       *bufp;		/* `here' */
  char		       *limitp;		/* read/write limit */
  char		       *buffer;		/* the buffer */
  char		       *unbuffer;	/* Sungetc buffer */
  int			lastc;		/* last character written */
  int			magic;		/* magic number SIO_MAGIC */
  int			bufsize;	/* size of the buffer */
  unsigned int		flags;		/* Status flags */
  IOPOS			posbuf;		/* location in file */
  IOPOS *		position;	/* pointer to above */
  void		       *handle;		/* function's handle */
  IOFUNCTIONS	       *functions;	/* open/close/read/write/seek */
  int			timeout;	/* timeout (milliseconds) */
  IOENC			encoding;	/* character encoding used */
  int			locks;		/* lock/unlock count */
  int			references;	/* Reference-count */
  IOLOCK *		mutex;		/* stream mutex */
  void			(*close_hook)(void* closure);
  void *		closure;
  mbstate_t *		mbstate;	/* ENC_ANSI decoding */
  struct io_stream *	tee;		/* copy data to this stream */
  struct io_stream *	upstream;	/* stream providing our input */
  struct io_stream *	downstream;	/* stream providing our output */
  unsigned		newline : 2;	/* Newline mode */
  unsigned		erased : 1;	/* Stream was erased */
  int			io_errno;	/* Save errno value */
  char *		message;	/* error/warning message */
  void *		exception;	/* pending exception (record_t) */
  void *		context;	/* getStreamContext() */
  struct PL_locale *	locale;		/* Locale associated to stream */
  intptr_t		fileno;		/* File number if this is associated to a file */
  intptr_t		reserved[3];	/* reserved for extension */
} IOSTREAM;


#define SmakeFlag(n)	((unsigned int)1<<(n-1))

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
#define SIO_NOERROR	SmakeFlag(14)	/* Ignore write errors */
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
#define SIO_RAW		SmakeFlag(28)	/* TTY Stream is in raw mode */
#define SIO_REPXML	SmakeFlag(29)	/* Bad char --> XML entity */
#define SIO_REPPL	SmakeFlag(30)	/* Bad char --> Prolog \hex\ */
#define SIO_BOM		SmakeFlag(31)	/* BOM was detected/written */
#define SIO_REPPLU	SmakeFlag(32)	/* Bad char --> Prolog \uXXXX */

#define	SIO_SEEK_SET	0	/* From beginning of file.  */
#define	SIO_SEEK_CUR	1	/* From current position.  */
#define	SIO_SEEK_END	2	/* From end of file.  */

PL_EXPORT(IOSTREAM *)	S__getiob(void);	/* get DLL's __iob[] address */

PL_EXPORT_DATA(IOFUNCTIONS)	Sfilefunctions;	/* OS file functions */
PL_EXPORT_DATA(int)		Slinesize;		/* Sgets() linesize */
#if defined(__CYGWIN__) && !defined(PL_KERNEL)
#define S__iob S__getiob()
#else
PL_EXPORT_DATA(IOSTREAM)	S__iob[3];		/* Libs standard streams */
#endif

/* WARNING: Sinput, Soutput, Serror use the OS's files directly.
            If you wish to use Prolog's streams, use Suser_input,
            Scurrent_output, etc. in SWI-Prolog.h
*/

#define Sinput  (&S__iob[0])		/* Stream Sinput */
#define Soutput (&S__iob[1])		/* Stream Soutput */
#define Serror  (&S__iob[2])		/* Stream Serror */

#define Sgetchar()	Sgetc(Sinput)
#define Sputchar(c)	Sputc((c), Soutput)

#define S__updatefilepos_getc(s, c) \
	((s)->position ? S__fupdatefilepos_getc((s), (c)) \
		       : (c))

#define Snpgetc(s) ((s)->bufp < (s)->limitp ? (int)(*(s)->bufp++)&0xff \
					    : S__fillbuf(s))
#define Sgetc(s) S__updatefilepos_getc((s), Snpgetc(s))

/* Control-operations */
#define SIO_GETSIZE	  (1)		/* get size of underlying object */
#define SIO_GETFILENO	  (2)		/* get underlying file (if any) */
#define SIO_SETENCODING	  (3)		/* modify encoding of stream */
#define SIO_FLUSHOUTPUT	  (4)		/* flush output */
#define SIO_LASTERROR	  (5)		/* string holding last error */
#ifdef __WINDOWS__
#define SIO_GETWINSOCK    (6)		/* get underlying SOCKET object */
#endif
#define SIO_GETPENDING    (7)		/* get #pending bytes */
#define SIO_GETREPOSITION (8)		/* Test if stream is repositionable */

/* Sread_pending() */
#define SIO_RP_BLOCK 0x1		/* wait for new input */
#define SIO_RP_NOPOS 0x2		/* Do not update position */

#define SIO_CLOSE_TRYLOCK	0x1	/* Sgcclose(): fail if we cannot lock */
#define SIO_CLOSE_FORCE		0x2	/* Sgcclose(): force regardless of lock */
/*#define #define SIO_CLOSE_GC	0x4        Sgcclose(): used internally */

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

#if !defined(WPRINTF12)
/* these macros are duplicated in SWI-Prolog.h */
#if defined(CHECK_FORMAT)
#define WPRINTF12  __attribute__ ((format (printf, 1, 2)))
#define WPRINTF23  __attribute__ ((format (printf, 2, 3)))
#define WPRINTF34  __attribute__ ((format (printf, 3, 4)))
#else
#define WPRINTF12
#define WPRINTF23
#define WPRINTF34
#endif
#endif

PL_EXPORT(void)		SinitStreams(void);
PL_EXPORT(void)		Scleanup(void);
PL_EXPORT(void)		Sreset(void);
PL_EXPORT(int)		S__fupdatefilepos_getc(IOSTREAM *s, int c);
PL_EXPORT(int)		S__fillbuf(IOSTREAM *s);
PL_EXPORT(int)		Sset_timeout(IOSTREAM *s, int tmo);
PL_EXPORT(int)		Sunit_size(IOSTREAM *s);
					/* byte I/O */
PL_EXPORT(int)		Sputc(int c, IOSTREAM *s);
PL_EXPORT(int)		Sfgetc(IOSTREAM *s);
PL_EXPORT(int)		Sungetc(int c, IOSTREAM *s);
					/* multibyte I/O */
PL_EXPORT(int)		Scanrepresent(int c, IOSTREAM *s);
PL_EXPORT(int)		Sputcode(int c, IOSTREAM *s);
PL_EXPORT(int)		Sgetcode(IOSTREAM *s);
PL_EXPORT(int)		Speekcode(IOSTREAM *s);
					/* word I/O */
PL_EXPORT(int)		Sputw(int w, IOSTREAM *s);
PL_EXPORT(int)		Sgetw(IOSTREAM *s);
PL_EXPORT(size_t)	Sfread(void *data, size_t size, size_t elems,
			       IOSTREAM *s);
PL_EXPORT(size_t)	Sfwrite(const void *data, size_t size, size_t elems,
				IOSTREAM *s);
PL_EXPORT(int)		Sfeof(IOSTREAM *s);
PL_EXPORT(int)		Sfpasteof(IOSTREAM *s);
PL_EXPORT(int)		Sferror(IOSTREAM *s);
PL_EXPORT(void)		Sclearerr(IOSTREAM *s);
PL_EXPORT(int)		Sseterr(IOSTREAM *s, int which, const char *message);
PL_EXPORT(int)		Sset_exception(IOSTREAM *s, term_t ex);
PL_EXPORT(int)		Ssetenc(IOSTREAM *s, IOENC new_enc, IOENC *old_enc);
PL_EXPORT(int)		Ssetlocale(IOSTREAM *s,
				   struct PL_locale *new_loc,
				   struct PL_locale **old_loc);
PL_EXPORT(int)		Sflush(IOSTREAM *s);
PL_EXPORT(int64_t)	Ssize(IOSTREAM *s);
PL_EXPORT(int)		Sseek(IOSTREAM *s, long pos, int whence); /* WDEPRECATED */ /* use Sseek64() */
PL_EXPORT(long)		Stell(IOSTREAM *s); /* WDEPRECATED */ /* use Stell64() */
PL_EXPORT(int)		Sclose(IOSTREAM *s);
PL_EXPORT(int)		Sgcclose(IOSTREAM *s, int flags);
PL_EXPORT(char *)	Sfgets(char *buf, int n, IOSTREAM *s);
PL_EXPORT(char *)	Sgets(char *buf);
PL_EXPORT(ssize_t)	Sread_pending(IOSTREAM *s,
				      char *buf, size_t limit, int flags);
PL_EXPORT(size_t)	Spending(IOSTREAM *s);
PL_EXPORT(int)		Sfputs(const char *q, IOSTREAM *s);
PL_EXPORT(int)		Sputs(const char *q);
PL_EXPORT(int)		Sfprintf(IOSTREAM *s, const char *fm, ...) WPRINTF23;
PL_EXPORT(int)		SfprintfX(IOSTREAM *s, const char *fm, ...);
PL_EXPORT(int)		Sprintf(const char *fm, ...) WPRINTF12;
PL_EXPORT(int)		Svprintf(const char *fm, va_list args);
PL_EXPORT(int)		Svfprintf(IOSTREAM *s, const char *fm, va_list args);
PL_EXPORT(int)		Ssprintf(char *buf, const char *fm, ...) WPRINTF23;
PL_EXPORT(int)		Ssnprintf(char *buf, size_t size, const char *fm, ...) WPRINTF34;
PL_EXPORT(int)		SsnprintfX(char *buf, size_t size, const char *fm, ...);
PL_EXPORT(int)		Svsprintf(char *buf, const char *fm, va_list args);
PL_EXPORT(int)		Svsnprintf(char *buf, size_t size, const char *fm, va_list args);
PL_EXPORT(int)		Svdprintf(const char *fm, va_list args);
PL_EXPORT(int)		Sdprintf(const char *fm, ...) WPRINTF12;
PL_EXPORT(int)		SdprintfX(const char *fm, ...);
PL_EXPORT(int)		Slock(IOSTREAM *s);
PL_EXPORT(int)		StryLock(IOSTREAM *s);
PL_EXPORT(int)		Sunlock(IOSTREAM *s);
PL_EXPORT(IOSTREAM *)	Snew(void *handle, int flags, IOFUNCTIONS *functions);
PL_EXPORT(IOSTREAM *)	Sopen_file(const char *path, const char *how);
PL_EXPORT(IOSTREAM *)	Sopen_iri_or_file(const char *path, const char *how);
PL_EXPORT(IOSTREAM *)	Sfdopen(int fd, const char *type);
PL_EXPORT(int)		Sfileno(IOSTREAM *s);
#ifdef __WINDOWS__
PL_EXPORT(int)		Swin_open_osfhandle(HANDLE h, int flags);
PL_EXPORT(IOSTREAM *)	Swin_open_handle(HANDLE h, const char *mode);
PL_EXPORT(HANDLE)	Swinhandle(IOSTREAM *s);
#if defined(_WINSOCKAPI_) || defined(NEEDS_SWINSOCK) /* have SOCKET */
PL_EXPORT(SOCKET)	Swinsock(IOSTREAM *s);
#endif
#endif
PL_EXPORT(IOSTREAM *)	Sopen_pipe(const char *command, const char *type);
PL_EXPORT(IOSTREAM *)	Sopenmem(char **buffer, size_t *sizep, const char *mode);
PL_EXPORT(IOSTREAM *)	Sopen_string(IOSTREAM *s, char *buf, size_t sz, const char *m);
PL_EXPORT(int)		Sclosehook(void (*hook)(IOSTREAM *s));
PL_EXPORT(void)		Sfree(void *ptr);
PL_EXPORT(int)		Sset_filter(IOSTREAM *parent, IOSTREAM *filter);
PL_EXPORT(void)		Ssetbuffer(IOSTREAM *s, char *buf, size_t size);

PL_EXPORT(int64_t)	Stell64(IOSTREAM *s);
PL_EXPORT(int)		Sseek64(IOSTREAM *s, int64_t pos, int whence);

PL_EXPORT(int)		ScheckBOM(IOSTREAM *s);
PL_EXPORT(int)		SwriteBOM(IOSTREAM *s);

#ifdef _SWI_PROLOG_H
PL_EXPORT(IOENC)	PL_atom_to_encoding(atom_t name);
PL_EXPORT(atom_t)	PL_encoding_to_atom(IOENC enc);
#endif

PL_EXPORT(int)		PL_qlf_get_int64(IOSTREAM *s, int64_t *ip);
PL_EXPORT(int)		PL_qlf_get_int32(IOSTREAM *s, int32_t *ip);
PL_EXPORT(int)		PL_qlf_get_uint32(IOSTREAM *s, uint32_t *ip);
PL_EXPORT(int)		PL_qlf_get_double(IOSTREAM *s, double *fp);
PL_EXPORT(int)		PL_qlf_get_atom(IOSTREAM *s, atom_t *a);

PL_EXPORT(int)		PL_qlf_put_int64(int64_t i, IOSTREAM *s);
PL_EXPORT(int)		PL_qlf_put_int32(int32_t i, IOSTREAM *s);
PL_EXPORT(int)		PL_qlf_put_uint32(uint32_t i, IOSTREAM *s);
PL_EXPORT(int)		PL_qlf_put_double(double f, IOSTREAM *s);
PL_EXPORT(int)		PL_qlf_put_atom(atom_t a, IOSTREAM *s);


#ifdef __cplusplus
}
#endif

#endif /*_SWI_STREAM_H*/
