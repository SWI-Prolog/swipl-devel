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

#ifndef RC_H_INCLUDED
#define RC_H_INCLUDED

#ifdef WIN32
#  include "../config/win32.h"
#  ifdef RC_KERNEL
#    include <windows.h>
#  else
#    define HANDLE void *
#  endif
#else
#  if defined(HAVE_CONFIG_H) || defined(RC_KERNEL)
#    include <config.h>
#  endif
#endif

#ifdef HAVE_DMALLOC_H
#include <dmalloc.h>			/* Use www.dmalloc.com debugger */
#endif

#include <stdio.h>
#include <time.h>

/* Flags for rc_open()
*/

#define RC_RDONLY	0x01
#define RC_WRONLY	0x02
#define RC_CREATE	0x04
#define RC_TRUNC	0x08
#define RC_RDWR		(RC_RDONLY|RC_WRONLY)

#define RCE_ERRBASE	1024		/* Lower: system errors */
#define RCE_MKERRNO(n)	(RCE_ERRBASE+(n))

#define RCE_ERRNO	errno
#define RCE_NOERROR	RCE_MKERRNO(0)
#define RCE_NOARCHIVE	RCE_MKERRNO(1)
#define RCE_NOENT	RCE_MKERRNO(2)
#define RCE_SHORT	RCE_MKERRNO(3)
#define RCE_RDIO	RCE_MKERRNO(4)
#ifdef WIN32
#define RCE_WINERRNO	RCE_MKERRNO(5)
#endif

#if defined(HAVE_MMAP) || defined(WIN32)
#define MAPPED_ARCHIVE 1
#endif

extern int	rc_errno;
const char *rc_strerror(int e);

typedef unsigned long rc_size;		/* size of resource objects */
typedef unsigned long rc_offset;	/* offset in the archive */

typedef struct _rc_member
{ char	       *name;			/* name of the member */
  char	       *rc_class;		/* type of the data */
  char	       *encoding;		/* used encoding technique */
  time_t	modified;		/* time-stamp for modified time */
  rc_size	size;			/* Size of the resource */
  char         *file;			/* underlying file (creating) */
  rc_size	allocated;		/* allocated size */
  char	       *data;			/* underlying data (creating) */
  struct _rc_archive *archive;		/* archive I belong to */
  rc_offset	offset;			/* Offset in the archive file */
  struct _rc_member *next;		/* Pointer to next */
} rc_stat_buf, *RcStatBuf, rc_member, *RcMember;

typedef struct _rc_archive
{ const char   *path;			/* Underlying file */
  int		flags;			/* Access flags */
  rc_offset	offset;			/* Offset of the archive in the file */
  rc_size	size;			/* Size of the archive */
  int		modified;		/* Archive was modified */
  RcMember      members;		/* Linked list of members */
  RcMember	members_tail;
  FILE	       *fd;			/* Accessing file-stream */
  void	       *map_start;		/* Start of the map */
  rc_size	map_size;		/* Size of the map */
  void	       *data;			/* Handle to data */
#ifdef WIN32
  HANDLE	hfile;			/* handle to the file */
  HANDLE	hmap;			/* handle to the map */
#endif
} rc_archive, *RcArchive;

typedef struct
{ RcMember	member;			/* represented member */
  rc_offset	offset;			/* current offset */
  void	       *data;			/* data (if opened) */
} rc_object, *RcObject;

RcArchive	rc_open_archive(const char *file, int flags);
int		rc_close_archive(RcArchive rca);
int		rc_save_archive(RcArchive rca, const char *to);

RcObject        rc_open(RcArchive rca,
			const char *name, const char *rcclass, int flags);
int             rc_close(RcObject o);
void            rc_stat(RcObject o, RcStatBuf stat);
rc_offset       rc_seek(RcObject o, rc_offset to, int whence);
int             rc_read(RcObject o, void *buf, int bytes);
int             rc_write(RcObject o, void *buf, int bytes);
void *          rc_data(RcObject o, int *size);

int		rc_append_file(RcArchive rca,
			       const char *name,
			       const char *rcclass, const char *enc,
			       const char *file);
int		rc_delete(RcArchive rca, const char *name, const char *cl);

#endif /*RC_H_INCLUDED*/
