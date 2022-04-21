/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2022, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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

#ifndef _PL_OS_H
#define _PL_OS_H

/* Get PATH_MAX for legacy BSD systems */
#ifndef PATH_MAX
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#if defined(MAXPATHLEN)
#define PATH_MAX MAXPATHLEN
#elif defined(PATHSIZE)
#define PATH_MAX PATHSIZE
#endif
#endif

#if WIN_PATH_MAX
#undef PATH_MAX
#define PATH_MAX WIN_PATH_MAX
#endif

		/********************************
		*         MISCELLANEOUS         *
		*********************************/

char *		OsError(void);
bool		initOs(void);
void		cleanupOs(void);
int		CpuCount(void);
void		setRandom(unsigned int *seed);
uint64_t	_PL_Random(void);
void		setOSPrologFlags(void);
int		Pause(double time);

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

char *		canonicaliseFileName(char *path);
char *		canonicalisePath(char *path);
char *		OsPath(const char *plpath, char *ospath);
char *		PrologPath(const char *ospath, char *plpath, size_t len);
void		RemoveTemporaryFiles(void);
char *		expandVars(const char *pattern, char *expanded, int len);
char *		AbsoluteFile(const char *spec, char *path);
int		IsAbsolutePath(const char *spec);
char *		BaseName(const char *f, char *buf);
char *		DirName(const char *f, char *buf);
atom_t		TemporaryFile(const char *id,
			      const char *ext, int *fdp);
int		DeleteTemporaryFile(atom_t name);
char *		findExecutable(const char *module, char *buf, size_t len);


		/********************************
		*        TIME CONVERSION        *
		*********************************/

typedef enum
{ CPU_USER,
  CPU_SYSTEM
} cputime_kind;

double		CpuTime(cputime_kind);
double		WallTime(void);
struct tm *	PL_localtime_r(const time_t *t, struct tm *r);
char *		PL_asctime_r(const struct tm *tm, char *buf);


		 /*******************************
		 *	      MEMORY		*
		 *******************************/

uintptr_t	UsedMemory(void);
uintptr_t	FreeMemory(void);


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
#define TTY_SAVE	 3		/* just save status */

typedef struct
{ void *state;				/* Saved state */
  int   mode;				/* Prolog;'s view on mode */
} ttybuf;

extern ttybuf	ttytab;			/* saved tty status */
extern int	ttymodified;		/* Did we modify the tty */
extern int	ttyfileno;		/* Main TTY file number */

#define IsaTty(fd)	isatty(fd)

bool		PushTty(IOSTREAM *s, ttybuf *buf, int mode);
bool		PopTty(IOSTREAM *s, ttybuf *buf, int do_free);
void		ResetTty(void);
int		Sttymode(IOSTREAM *s);
int		hasConsole(void);


		/********************************
		*        PROCESS CONTROL        *
		*********************************/

bool		ChDir(const char *path);
size_t		getenv3(const char *, char *buf, size_t buflen);
char *		Getenv(const char *, char *buf, size_t buflen);
int		Setenv(char *name, char *value);
int		Unsetenv(char *name);
int		System(char *command);

#endif /*_PL_OS_H*/
