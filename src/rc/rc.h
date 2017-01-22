/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2011, University of Amsterdam
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

#ifndef RC_H_INCLUDED
#define RC_H_INCLUDED

#if defined(__MINGW32__) && !defined(__WINDOWS__)
#define __WINDOWS__ 1
#endif

#ifdef __WINDOWS__
#  ifdef WIN64
#    include "../config/win64.h"
#  else
#    include "../config/win32.h"
#  endif
#  ifdef RC_KERNEL
#  define WINDOWS_LEAN_AND_MEAN 1
#  include <windows.h>
#  endif
#  ifdef _WINDOWS_
#    define WIN_HANDLE HANDLE
#  else
     typedef void *WIN_HANDLE;
#  endif
#  if defined(_MSC_VER)
#    if (_MSC_VER < 1300)
typedef long intptr_t;
typedef unsigned long uintptr_t;
#    else
#      include <stddef.h>
#    endif
typedef intptr_t ssize_t;		/* signed version of size_t */
#  elif defined(__MINGW32__)
#    include <stdint.h>
#  endif
#else
#include <config.h>
#include <unistd.h>
#include <inttypes.h>			/* more portable than stdint.h */
#endif

#ifdef HAVE_DMALLOC_H
#include <dmalloc.h>			/* Use www.dmalloc.com debugger */
#endif
#ifdef HAVE_UXNT_H
#include "../os/windows/uxnt.h"
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
#ifdef __WINDOWS__
#define RCE_WINERRNO	RCE_MKERRNO(5)
#endif

#ifndef MAPPED_ARCHIVE
#if defined(CAN_MMAP_FILES) || defined(__WINDOWS__)
#define MAPPED_ARCHIVE 1
#endif
#endif

extern int	rc_errno;
const char *rc_strerror(int e);

typedef uintptr_t rc_size;		/* size of resource objects */
typedef uintptr_t rc_offset;		/* offset in the archive */

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
#ifdef __WINDOWS__
  WIN_HANDLE	hfile;			/* handle to the file */
  WIN_HANDLE	hmap;			/* handle to the map */
#endif
} rc_archive, *RcArchive;

typedef struct
{ RcMember	member;			/* represented member */
  rc_offset	offset;			/* current offset */
  void	       *data;			/* data (if opened) */
} rc_object, *RcObject;

RcArchive	rc_open_archive(const char *file, int flags);
RcArchive	rc_open_archive_mem(const unsigned char *mem,
				    size_t mem_size, int flags);
int		rc_close_archive(RcArchive rca);
int		rc_save_archive(RcArchive rca, const char *to);

RcObject        rc_open(RcArchive rca,
			const char *name, const char *rcclass, int flags);
int             rc_close(RcObject o);
void            rc_stat(RcObject o, RcStatBuf stat);
rc_offset       rc_seek(RcObject o, rc_offset to, int whence);
ssize_t         rc_read(RcObject o, void *buf, size_t bytes);
ssize_t         rc_write(RcObject o, void *buf, size_t bytes);
void *          rc_data(RcObject o, uintptr_t *size);

int		rc_append_file(RcArchive rca,
			       const char *name,
			       const char *rcclass, const char *enc,
			       const char *file);
int		rc_delete(RcArchive rca, const char *name, const char *cl);

#endif /*RC_H_INCLUDED*/
