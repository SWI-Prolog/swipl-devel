/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2014, University of Amsterdam
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
#include <string.h>
#ifdef HAVE_SYS_MALLOC_H
#include <sys/malloc.h>
#else
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#endif
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdarg.h>
#include "rcutil.h"
#include "html.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef __WINDOWS__
#include <io.h>
#include <process.h>
#endif

int
rc_append_file(RcArchive rca,
	       const char *name, const char *rcclass, const char *enc,
	       const char *file)
{ rc_member hdr;
  struct stat buf;

  if ( stat(file, &buf)	< 0 )
  { rc_errno = RCE_ERRNO;
    return FALSE;			/* cannot add */
  }

  memset(&hdr, 0, sizeof(hdr));
  hdr.name     = strdup(name);
  hdr.rc_class = strdup(rcclass);
  hdr.encoding = strdup(enc);
  hdr.file     = strdup(file);
  hdr.size     = buf.st_size;
  hdr.modified = buf.st_mtime;

  rca->modified = TRUE;

  return rc_register_member(rca, &hdr) ? TRUE : FALSE;
}


int
rc_delete(RcArchive rca, const char *name, const char *class)
{ RcMember m = rc_find_member(rca, name, class);

  if ( m )
  { RcMember prev;

    rca->modified = TRUE;

    if ( m == rca->members )		/* we are the head */
    { rca->members = m->next;

      if ( !m->next )			/* and the tail as well */
	rca->members_tail = NULL;

      return TRUE;
    } else
    { for(prev = rca->members; prev; prev = prev->next)
      { if ( prev->next == m )
	{ prev->next = m->next;

	  if ( !m->next )		/* we are the tail */
	    rca->members_tail = prev;

	  return TRUE;
	}
      }
    }
  }

  return FALSE;
}


#ifndef O_BINARY
#define O_BINARY 0
#endif

static int
rc_save_data(RcMember m, FILE *fd)
{ if ( m->file )
  { int in = open(m->file, O_RDONLY|O_BINARY);

    if ( in >= 0 )
    { char buf[8192];
      size_t size = m->size;
      ssize_t n;

      while( size > 0 )
      { if ( (n=read(in, buf, sizeof(buf))) > 0 )
	{ if ( fwrite(buf, sizeof(char), n, fd) != n )
	  { rc_errno = RCE_ERRNO;
	    close(in);
	    return FALSE;
	  }
	  size -= n;
	  continue;
	}
	rc_errno = (n == 0 ? RCE_SHORT : RCE_ERRNO);
	close(in);
	return FALSE;
      }

      close(in);
    } else
    { rc_errno = RCE_ERRNO;
      return FALSE;
    }
  } else
  { RcObject o = rc_open(m->archive, m->name, m->rc_class, RC_RDONLY);
    char buf[8192];
    size_t size = m->size;
    size_t n;

    while( size > 0 )
    { if ( (n=rc_read(o, buf, sizeof(buf))) > 0 )
      { if ( fwrite(buf, sizeof(char), n, fd) != n )
	{ rc_errno = RCE_ERRNO;
	  rc_close(o);
	  return FALSE;
	}
	size -= n;
	continue;
      }
      rc_close(o);
      rc_errno = (n == 0 ? RCE_SHORT : RCE_RDIO);
      return FALSE;
    }

    rc_close(o);
  }

  return TRUE;
}


static int
rc_save_member(RcMember m, FILE *fd)
{ fprintf(fd, "\n<file name=\"%s\" class=\"%s\" encoding=\"%s\" size=%ld",
	  m->name, m->rc_class, m->encoding, (long)m->size);
  if ( m->modified )
    fprintf(fd, " modified=%ld", (long)m->modified);
  fprintf(fd, ">\n");

  if ( !rc_save_data(m, fd) )
    return FALSE;

  fprintf(fd, "\n</file>\n");

  return TRUE;
}


int
rc_save_archive(RcArchive rca, const char *to)
{ FILE *fd;
  char tmp[200];

  sprintf(tmp, "__tmp%d.prc", (int)getpid());

  if ( to == NULL )
    to = rca->path;

  if ( (fd=fopen(tmp, "wb")) )
  { RcMember member;
    rc_size size;
    RcMember hdr;
    rc_size hdrlen = 0;

    if ( (hdr = rc_find_member(rca, "$header", "$rc")) )
    { rc_save_data(hdr, fd);
      hdrlen = hdr->size;
    }

    fprintf(fd, "<archive>\n");
    for( member = rca->members; member; member = member->next )
    { if ( strcmp(member->name, "$header") == 0 &&
	   strcmp(member->rc_class, "$rc") == 0 )
	continue;

      if ( !rc_save_member(member, fd) )
      { fclose(fd);
	return FALSE;
      }
    }
    fprintf(fd, "</archive>\n");
    size = ftell(fd) - hdrlen;
    fprintf(fd, "<foot contentlength=%ld>\n", (long)size);
    if ( fclose(fd) == EOF )
    { rc_errno = RCE_ERRNO;
      return FALSE;
    }

    remove(to);
    if ( rename(tmp, to) != 0 )
    { rc_errno = RCE_ERRNO;
      remove(tmp);
      return FALSE;
    }

    if ( to == rca->path )
      rca->modified = FALSE;

    return TRUE;
  }

  rc_errno = RCE_ERRNO;
  return FALSE;
}
