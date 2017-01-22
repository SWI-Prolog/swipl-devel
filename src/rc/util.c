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

#define RC_KERNEL 1
#include "rc.h"

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#ifdef HAVE_SYS_MALLOC_H
#include <sys/malloc.h>
#else
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#endif
#include <errno.h>
#include "rcutil.h"
#include <string.h>

int rc_errno;

static const char *rc_errlist[] =
{ "No Error",				/* RCE_NOERROR */
  "Not a resource archive",		/* RCE_NOARCHIVE */
  "No such resource",			/* RCE_NOENT */
  "Could not read enough data from resource", /* RCE_SHORT */
  "Read failed",			/* RCE_RDIO */
  "Windows error",			/* RCE_WINERRNO */
  NULL
};


#ifndef HAVE_STRERROR
char *
strerror(int e)
{ extern int sys_nerr;
  extern char *sys_errlist[];
  extern int errno;

  if ( errno >= 0 && errno < sys_nerr )
    return sys_errlist[errno];

  return "Unknown error";
}
#endif

const char *
rc_strerror(int e)
{
#ifdef __WINDOWS__
  if ( e == RCE_WINERRNO )
  {					/* TBD */
  }
#endif

  if ( e < RCE_ERRBASE )
    return strerror(e);

  e -= RCE_ERRBASE;
  if ( (unsigned) e >= sizeof(rc_errlist)/sizeof(char *)-1 )
    return "Unknown error";

  return rc_errlist[e];
}


RcMember
rc_find_member(RcArchive rca, const char *name, const char *rcclass)
{ RcMember m;

  for(m = rca->members; m; m = m->next)
  { if ( strcmp(name, m->name) == 0 &&
	 (!rcclass || strcmp(rcclass, m->rc_class) == 0 ) )
      return m;
  }

  rc_errno = RCE_NOENT;
  return NULL;
}


RcMember
rc_register_member(RcArchive archive, RcMember member)
{ RcMember copy;

  if ( (copy = rc_find_member(archive, member->name, member->rc_class)) )
  {					/* release? */
  } else
  { if ( !(copy = malloc(sizeof(*member))) )
      return FALSE;

    copy->next    = NULL;
    copy->archive = archive;

    if ( !archive->members )
    { archive->members = archive->members_tail = copy;
    } else
    { archive->members_tail->next = copy;
      archive->members_tail = copy;
    }
  }

  copy->name     = member->name;
  copy->rc_class = member->rc_class;
  copy->encoding = member->encoding;
  copy->modified = member->modified;
  copy->file	 = member->file;
  copy->allocated= member->allocated;
  copy->data     = member->data;
  copy->offset   = member->offset;
  copy->size     = member->size;

  return copy;
}


