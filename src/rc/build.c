/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <malloc.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdarg.h>
#define RC_KERNEL 1
#include "rc.h"
#include "rcutil.h"
#include "html.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef WIN32
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
      int size = m->size;
      size_t n;

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
    int size = m->size;
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
{ fprintf(fd, "\n<FILE NAME=\"%s\" CLASS=\"%s\" ENCODING=\"%s\" SIZE=%ld",
	  m->name, m->rc_class, m->encoding, m->size);
  if ( m->modified )
    fprintf(fd, " MODIFIED=%ld", m->modified);
  fprintf(fd, ">\n");

  if ( !rc_save_data(m, fd) )
    return FALSE;

  fprintf(fd, "\n</FILE>\n");

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

    fprintf(fd, "<ARCHIVE>\n");
    for( member = rca->members; member; member = member->next )
    { if ( strcmp(member->name, "$header") == 0 &&
	   strcmp(member->rc_class, "$rc") == 0 )
	continue;

      if ( !rc_save_member(member, fd) )
      { fclose(fd);
	return FALSE;
      }
    }
    fprintf(fd, "</ARCHIVE>\n");
    size = ftell(fd) - hdrlen;
    fprintf(fd, "<FOOT CONTENTLENGTH=%ld>\n", size);
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
