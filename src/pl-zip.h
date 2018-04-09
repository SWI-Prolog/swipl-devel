/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

#ifndef H_PLZIP_INCLUDED
#define H_PLZIP_INCLUDED 1

#include "minizip/zip.h"
#include "minizip/unzip.h"

typedef enum zipper_state
{ ZIP_IDLE = 0,
  ZIP_SCAN,					/* goto executing */
  ZIP_ENTRY					/* Entry open */
} zipper_state;

typedef struct zipper
{ zipFile	 writer;
  unzFile	 reader;
  const char    *path;
  IOSTREAM      *stream;
  zipper_state	 state;
  recursiveMutex lock;
} zipper;

#define RC_RDONLY	0x01
#define RC_WRONLY	0x02
#define RC_CREATE	0x04
#define RC_TRUNC	0x08
#define RC_RDWR		(RC_RDONLY|RC_WRONLY)

extern int rc_errno;

COMMON(zipper *)	zip_open_archive(const char *file, int flags);
COMMON(zipper *)	rc_open_archive_mem(const unsigned char *mem,
					    size_t mem_size, int flags);

COMMON(int)		zip_close_archive(zipper *z);
COMMON(IOSTREAM *)	SopenZIP(zipper *z, const char *name, int flags);
COMMON(char *)		rc_strerror(int);

/* Allow using in existing system */

typedef zipper* RcArchive;
#define rc_open_archive zip_open_archive
#define rc_close_archive zip_close_archive

#endif /*H_PLZIP_INCLUDED*/
