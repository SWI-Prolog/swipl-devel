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

#ifndef _XNT_H_INCLUDED
#define _XNT_H_INCLUDED

#undef _export
#ifdef _UXNT_KERNEL
#define _export _declspec(dllexport)
#else
#define _export extern
#endif

#include <sys/types.h>
#include <stdio.h>
#include <io.h>
#include <direct.h>

#ifndef _UXNT_KERNEL
#include <sys/stat.h>

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
#undef stat
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

#define fopen(p, m) fopen(_xos_os_existing_filename((p), alloca(PATH_MAX)), \
			  (m))

#endif /*_UXNT_KERNEL*/

#define F_OK 00
#define R_OK 04				/* access() fields */
#define W_OK 06

		 /*******************************
		 *	     TYPES, ETC		*
		 *******************************/

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

#undef _xos_stat

_export char *  _xos_home(void);
_export char *	_xos_os_filename(const char *cname, char *osname);
_export char *	_xos_canonical_filename(const char *cname, char *osname);
_export int	_xos_is_absolute_filename(const char *spec);
_export char *	_xos_absolute_filename(const char *local, char *absolute);
_export char *	_xos_limited_os_filename(const char *spec, char *limited);
_export char *  _xos_long_file_name(const char *n8and3, char *name);
_export int	_xos_same_file(const char *p1, const char *p2);
_export int	_xos_open(const char *path, int access, ...);
_export int	_xos_close(int handle);
_export int	_xos_read(int handle, void *buf, unsigned int size);
_export int	_xos_write(int handle, const void *buf, unsigned int size);
_export long	_xos_lseek(int handle, long offset, int whence);
_export long	_xos_tell(int handle);
_export int	_xos_access(const char *path, int mode);
_export int	_xos_chmod(const char *path, int mode);
_export int	_xos_remove(const char *path);
_export int	_xos_rename(const char *old, const char *newname);
_export int	_xos_stat(const char *path, struct stat *sbuf);
_export int	_xos_chdir(const char *path);
_export int	_xos_mkdir(const char *path, int mode);
_export int	_xos_rmdir(const char *path);
_export char *	_xos_getcwd(char *buf, int len);
_export int	_xos_errno(void);
_export int	_xos_exists(const char *path, int flags);

#define _XOS_ISFILE	0x01
#define _XOS_ISDIR	0x02

		 /*******************************
		 *   WIN32 LONG FILENAME MAPS	*
		 *******************************/

#define _XOS_FILE		0x0001		/* is a file */
#define _XOS_DIR		0x0002		/* is a directory */

_export int	_xos_make_filemap(const char *dir);
_export char *	_xos_os_existing_filename(const char *cname, char *osname);


#endif /*_XNT_H_INCLUDED*/
