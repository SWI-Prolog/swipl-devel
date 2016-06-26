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

#ifndef _XNT_H_INCLUDED
#define _XNT_H_INCLUDED

#undef _export
#if defined(_UXNT_KERNEL) && !defined(__MINGW32__)
#define _export _declspec(dllexport)
#else
#define _export extern
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <io.h>
#include <direct.h>
#ifdef _MSC_VER
#if (_MSC_VER < 1300)
typedef long intptr_t;
typedef unsigned long uintptr_t;
#endif
typedef intptr_t ssize_t;		/* signed version of size_t */
#endif

#ifndef _UXNT_KERNEL

#undef remove
#undef rename
#undef open
#undef close
#undef read
#undef write
#undef lseek
#undef tell
#undef access
#undef chmod
#undef remove
#undef rename
#undef chdir
#undef mkdir
#undef rmdir
#undef getcwd

#define remove _xos_remove
#define rename _xos_rename
#define open _xos_open
#define close _xos_close
#define read _xos_read
#define write _xos_write
#define lseek _xos_lseek
#define tell _xos_tell
#define access _xos_access
#define chmod _xos_chmod
#define remove _xos_remove
#define rename _xos_rename
#define statfunc _xos_stat
#define chdir _xos_chdir
#define mkdir _xos_mkdir
#define rmdir _xos_rmdir
#define getcwd _xos_getcwd
#define setenv _xos_setenv
#define fopen(p, m) _xos_fopen(p, m)

#ifndef HAVE_SETENV
#define HAVE_SETENV 1
#endif

#endif /*_UXNT_KERNEL*/

#ifndef F_OK
#define F_OK 00				/* access() fields */
#define X_OK 01
#define W_OK 02
#define R_OK 04
#endif

		 /*******************************
		 *	     TYPES, ETC		*
		 *******************************/

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

#define XOS_ACCESS_ACCESS		0
#define XOS_ACCESS_GETFILESECURITY	1
#define XOS_ACCESS_OPENCLOSE		2

#undef _xos_stat

_export char *  _xos_home(void);
_export wchar_t*_xos_os_filenameW(const char *cname,
				  wchar_t *osname, size_t len);
_export char *	_xos_os_filename(const char *cname,
				 char *osname, size_t len);
_export char *	_xos_canonical_filenameW(const wchar_t *cname,
					 char *osname, size_t len, int flags);
_export char *	_xos_canonical_filename(const char *cname,
					char *osname, size_t len, int flags);
_export int	_xos_is_absolute_filename(const char *spec);
_export char *	_xos_absolute_filename(const char *local, char *absolute, size_t len);
_export char *	_xos_limited_os_filename(const char *spec, char *limited);
_export wchar_t*_xos_long_file_nameW(const wchar_t *n8and3,
				     wchar_t *name, size_t len);
_export char*	_xos_long_file_name_toA(const wchar_t *n8and3,
					char *name, size_t len);
_export char *  _xos_long_file_name(const char *file, char *longname,
				    size_t len);
_export int	_xos_same_file(const char *p1, const char *p2);
_export int	_xos_open(const char *path, int access, ...);
_export FILE*	_xos_fopen(const char *path, const char *mode);
_export int	_xos_close(int handle);
_export ssize_t	_xos_read(int handle, void *buf, size_t size);
_export ssize_t	_xos_write(int handle, const void *buf, size_t size);
_export long	_xos_lseek(int handle, long offset, int whence);
_export long	_xos_tell(int handle);
_export int	_xos_access(const char *path, int mode);
_export int	_xos_access_dir(const char *path, int mode);
_export int	_xos_chmod(const char *path, int mode);
_export int	_xos_remove(const char *path);
_export int	_xos_rename(const char *old, const char *newname);
_export int	_xos_stat(const char *path, struct _stati64 *sbuf);
_export int	_xos_chdir(const char *path);
_export int	_xos_mkdir(const char *path, int mode);
_export int	_xos_rmdir(const char *path);
_export char *	_xos_getcwd(char *buf, size_t len);
_export int	_xos_errno(void);
_export int	_xos_exists(const char *path, int flags);
_export size_t  _xos_getenv(const char *name, char *buf, size_t buflen);
_export int	_xos_setenv(const char *name, char *value, int overwrite);
_export int	_xos_set_win_file_access_check(int new);
_export int	_xos_get_win_file_access_check(void);

#define _XOS_ISFILE	0x01
#define _XOS_ISDIR	0x02

#define _XOS_FILE	0x0001		/* is a file */
#define _XOS_DIR	0x0002		/* is a directory */

#define XOS_DOWNCASE	0x01		/* _xos_canonical_filename() */

#endif /*_XNT_H_INCLUDED*/
