/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <sys/types.h>
#include <malloc.h>
#include <errno.h>
#define RC_KERNEL 1
#include "rc.h"
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
#ifdef WIN32
  if ( e == RCE_WINERRNO )
  {					/* TBD */
  }
#endif

  if ( e < RCE_ERRBASE )
    return strerror(e);

  e -= RCE_ERRBASE;
  if ( e > sizeof(rc_errlist)/sizeof(char *) )
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


