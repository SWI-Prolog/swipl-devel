/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/types.h>
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

#define ARCHIVE_TAG	"<archive>"

static HtmlTagDef file_tag_def = NULL;

static int attach_archive(RcArchive rca);

RcArchive
rc_open_archive(const char *file, int flags)
{ RcArchive rca = malloc(sizeof(rc_archive));

  if ( rca )
  { memset(rca, 0, sizeof(*rca));
    rca->path = strdup(file);
    rca->flags = flags;

    if ( !(flags & RC_TRUNC) )
    { if ( !attach_archive(rca) && !(flags & RC_CREATE) )
      { rc_close_archive(rca);
	return NULL;
      }
    }
  } else
    rc_errno = RCE_ERRNO;

  return rca;
}


int
rc_close_archive(RcArchive rca)
{ int rval = TRUE;
  RcMember m, next;

  if ( rca->fd )
  { fclose(rca->fd);
    rca->fd = NULL;
  }
#ifdef HAVE_MMAP
  if ( rca->map_start )
    munmap(rca->map_start, rca->map_size);
#else
#ifdef WIN32
  if ( rca->map_start )
    UnmapViewOfFile(rca->map_start);
  if ( rca->hmap )
    CloseHandle(rca->hmap);
  if ( rca->hfile )
    CloseHandle(rca->hfile);
#endif /*WIN32*/
#endif /*HAVE_MMAP*/

  for(m=rca->members; m; m=next)
  { next = m->next;
    if ( m->name     ) free(m->name);
    if ( m->rc_class ) free(m->rc_class);
    if ( m->encoding ) free(m->encoding);
    if ( m->file     ) free(m->file);
    free(m);
  }

  free((void *)rca->path);
  free(rca);

  if ( file_tag_def )			/* normally we won't need this */
  { void *p = file_tag_def;		/* any longer and if we need it */
					/* it will be recreated */
    file_tag_def = NULL;
    free(p);
  }
					/* TBD: deallocate structures */
  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the archive, filling a member structure  and returning a pointer to
a point after the next construct,  or  NULL   if  we  hit the end of the
archive without finding anything useful.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

HtmlTagDef
make_file_tag_def()
{ if ( !file_tag_def && (file_tag_def = malloc(sizeof(htmltagdef)*6)) )
  { HtmlTagDef d = file_tag_def;

    d->tag     = "name";
    d->convert = html_cvt_malloc_string;
    d->offset  = offsetof(rc_member, name);
    d++;
    d->tag     = "class";
    d->convert = html_cvt_malloc_string;
    d->offset  = offsetof(rc_member, rc_class);
    d++;
    d->tag     = "encoding";
    d->convert = html_cvt_malloc_string;
    d->offset  = offsetof(rc_member, encoding);
    d++;
    d->tag     = "modified";
    d->convert = html_cvt_date;
    d->offset  = offsetof(rc_member, modified);
    d++;
    d->tag     = "size";
    d->convert = html_cvt_long;
    d->offset  = offsetof(rc_member, size);
    d++;
    d->tag     = NULL;
  } 

  return file_tag_def;
}


#ifdef MAPPED_ARCHIVE

static char *
decode_member_header(RcArchive rca, const char *p0, RcMember mbr)
{ const char *p = p0;
  const char *end = &((const char *)rca->data)[rca->size];

  memset(mbr, 0, sizeof(*mbr));

  if ( (p = html_find_tag(p0, end, "file")) )
  { p = html_decode_tag(p, make_file_tag_def(), mbr)+1;

    if ( mbr->name )
    { mbr->offset = p - (const char *)rca->data;
      if ( !mbr->size )
      { const char *p1 = html_find_close_tag(p, "file");

	mbr->size = p1-strlen("</file>")-p-1; /* 1 for final newline */
	return (char *)p1;
      } else
      { p += mbr->size;
	
	return html_find_close_tag(p, "file");
      }      
    } else
      return html_find_close_tag(p, "file");
  }

  return NULL;
}

#else /*MAPPED_ARCHIVE*/

static int
decode_member_header(RcArchive rca, RcMember mbr)
{ char tag[MAXTAGLEN];
  char properties[MAXTAGPROPLEN];

  memset(mbr, 0, sizeof(*mbr));

  while( html_fd_next_tag(rca->fd, tag, properties) )
  { if ( strcasecmp(tag, "file") == 0 )
    { html_decode_tag(properties, make_file_tag_def(), mbr);

      if ( mbr->name )
      { mbr->offset = ftell(rca->fd) + 1;

	if ( !mbr->size )
	{ if ( html_fd_find_close_tag(rca->fd, "file") )
	  { rc_offset offset = ftell(rca->fd);

	    mbr->size = offset-strlen("</file>")-mbr->offset-1;
	    return TRUE;
	  }
	  return FALSE;			/* error. restart? */
	} else
	{ fseek(rca->fd, mbr->size, SEEK_CUR);
	  
	  return html_fd_find_close_tag(rca->fd, "file");
	}      
      } else
	return html_fd_find_close_tag(rca->fd, "file");
    }
  }
  
  return FALSE;
}

#endif /*MAPPED_ARCHIVE*/


static int
register_header(RcArchive rca, rc_size hdrlen)
{ rc_member hdr;

  memset(&hdr, 0, sizeof(hdr));
  hdr.name      = strdup("$header");
  hdr.rc_class  = strdup("$rc");
  hdr.encoding  = strdup("none");
  hdr.modified  = time(NULL);
  hdr.size      = hdrlen;
  hdr.offset    = -(long)hdrlen;

  rc_register_member(rca, &hdr);

  return TRUE;
}


static rc_size
contentlength(const char *tag)
{ htmltagdef def[2];
  long len = 0;

  def[0].tag     = "contentlength";
  def[0].convert = html_cvt_long;
  def[0].offset  = 0;
  def[1].tag     = NULL;

  html_decode_tag(tag, def, &len);

  return len;
}

#ifdef MAPPED_ARCHIVE

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the location of the real  archive.   The  real  archive starts with
<archive>, but there may be data (script   or the executable) before the
archive. In that case, the end   contains  <foot contentlength=len>, and
the archive starts at <len> before the *start* of this tag.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
find_archive_dimensions(RcArchive rca)
{ const char *data = rca->data;
  int alen = strlen(ARCHIVE_TAG);

  if ( strncasecmp(data, ARCHIVE_TAG, alen) != 0 )
  { const char *rc_end = &data[rca->size];
    const char *end = rc_end-1;
    const char *s;

    while(end > data && *end != '<')
      end--;
    if ( end <= data )
    { rc_errno = RCE_NOARCHIVE;
      return FALSE;
    }
    
    if ( (s = html_find_tag(end, rc_end, "foot")) )
    { rc_size len = contentlength(s);

      if ( len )
      { const char *start = end - len;
	rc_size hdrlen = start - (const char *)rca->map_start;

	if ( strncasecmp(start, ARCHIVE_TAG, alen) == 0 )
	{ rca->data  = (void *)start;
	  rca->size -= hdrlen;

	  return register_header(rca, hdrlen);
	}
      }
    }

    rc_errno = RCE_NOARCHIVE;
    return FALSE;
  }

  return TRUE;
}

#else /*MAPPED_ARCHIVE*/

static int
at_archive_start(RcArchive rca)
{ char buf[100];
  char *atag = ARCHIVE_TAG;
  int alen = strlen(atag);

  if ( fread(buf, sizeof(char), alen, rca->fd) != alen )
  { rc_errno = RCE_NOARCHIVE;		/* too short */
    return FALSE;
  }

  if ( strncasecmp(buf, atag, alen) == 0 )
    return TRUE;			/* fine, this is the start */

  rc_errno = RCE_NOERROR;
  return FALSE;
}


static int
find_archive_dimensions(RcArchive rca)
{ char buf[100];
  char *end, *theend, *s;
  rc_size bufstart;

  if ( at_archive_start(rca) )
    return TRUE;
  if ( rc_errno == RCE_NOARCHIVE )
    return FALSE;

  bufstart = fseek(rca->fd, sizeof(buf), SEEK_END);
  theend = buf + fread(buf, sizeof(char), sizeof(buf), rca->fd);
  end = theend;

  while(end > buf && *end != '<')	/* find the last tag */
    end--;

  if ( end <= buf )
  { rc_errno = RCE_NOARCHIVE;
    return FALSE;
  }

  if ( (s = html_find_tag(end, theend, "foot")) )
  { rc_size len = contentlength(s);

    if ( len )
    { rc_size offset = bufstart + (end-buf) - len;

      if ( fseek(rca->fd, offset, SEEK_SET) == offset )
      { if ( at_archive_start(rca) )
	{ rca->offset = offset;		/* base offset in file */
	  rca->size  -= offset;

	  return register_header(rca, offset);
	}
      }
    }
  }

  rc_errno = RCE_NOARCHIVE;
  return FALSE;
}

#endif /*MAPPED_ARCHIVE*/


#ifdef MAPPED_ARCHIVE

static int
scan_archive(RcArchive rca)
{ if ( find_archive_dimensions(rca) )
  { const char *p = rca->data;

    while(p)
    { rc_member hdr;

      if ( (p = decode_member_header(rca, p, &hdr)) && hdr.name )
	rc_register_member(rca, &hdr);
    }

    return TRUE;
  }
  
  return FALSE;
}

#else /*MAPPED_ARCHIVE*/

static int
scan_archive(RcArchive rca)
{ if ( find_archive_dimensions(rca) )
  { rc_member hdr;

    while( decode_member_header(rca, &hdr) && hdr.name )
      rc_register_member(rca, &hdr);

    return TRUE;
  }
  
  return FALSE;
}

#endif /*MAPPED_ARCHIVE*/


#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif
#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif

static int
attach_archive(RcArchive rca)
{
#ifdef HAVE_MMAP
  int fd;

  if ( (fd = open(rca->path, O_RDONLY)) >= 0 )
  { struct stat buf;

    if ( fstat(fd, &buf) == 0 )
    { rca->map_size = buf.st_size;
      rca->size     = rca->map_size;
      rca->offset   = 0;
      if ( (rca->map_start = mmap(NULL,
				  rca->size,
				  PROT_READ,
				  MAP_SHARED|MAP_NORESERVE,
				  fd,
				  0)) != MAP_FAILED )
      { close(fd);
	rca->data = rca->map_start;
	return scan_archive(rca);
      }
    }
  }

  rc_errno = RCE_ERRNO;
  return FALSE;
#else /*HAVE_MMAP*/
#ifdef WIN32
  DWORD fsize;

  rca->hfile = CreateFile(rca->path,
			  GENERIC_READ,
			  FILE_SHARE_READ,
			  NULL,
			  OPEN_EXISTING,
			  FILE_ATTRIBUTE_NORMAL,
			  NULL);
  if ( !rca->hfile )
    goto errio;

  if ( (fsize = GetFileSize(rca->hfile, NULL)) == (DWORD)~0L )
    goto errio;
    
  rca->map_size = fsize;
  rca->size     = rca->map_size;
  rca->offset   = 0;
      
  rca->hmap = CreateFileMapping(rca->hfile,
				NULL,
				PAGE_READONLY,
				0L,
				rca->size,
				NULL);
  if ( !rca->hmap )
    goto errio;

  rca->map_start = MapViewOfFile(rca->hmap,
				 FILE_MAP_READ,
				 0L, 0L, /* offset */
				 0L);	/* size (0=all) */
				  
  if ( !rca->map_start )
      goto errio;

  rca->data = rca->map_start;
  return scan_archive(rca);

errio:
  { int id = GetLastError();

    if ( rca->hmap )
      CloseHandle(rca->hmap);
    if ( rca->hfile )
      CloseHandle(rca->hfile);
    rca->map_start = NULL;
    rca->hfile     = NULL;
    rca->hmap      = NULL;

    rc_errno = RCE_WINERRNO;
    return FALSE;
  }
#else  /*WIN32*/
					/* bottom line, use files */
  if ( (rca->fd = fopen(rca->path, "rb")) )
  { struct stat buf;

    if ( fstat(fileno(rca->fd), &buf) == 0 )
    { rca->size   = buf.st_size;
      rca->offset = 0;
    }

    return scan_archive(rca);
  }

  rc_errno = RCE_ERRNO;
  return FALSE;

#endif /*WIN32*/
#endif /*HAVE_MMAP*/
}

		 /*******************************
		 *	ACCESSING MEMBERS	*
		 *******************************/

RcObject
rc_open(RcArchive rca, const char *name, const char *rcclass, int flags)
{ RcMember m = NULL;

  if ( (flags & RC_RDONLY) )
    m = rc_find_member(rca, name, rcclass);
  else if ( (flags & RC_WRONLY) )
  { rc_member hdr;

    memset(&hdr, 0, sizeof(hdr));
    hdr.name     = strdup(name);
    hdr.rc_class = strdup(rcclass);
    hdr.encoding = strdup("none");
    hdr.file	 = NULL;
    hdr.size	 = 0;
    hdr.modified = time(NULL);

    rca->modified = TRUE;
    m = rc_register_member(rca, &hdr);
  }

  if ( m )
  { RcObject o = malloc(sizeof(rc_object));

    if ( o )
    { o->member = m;
      o->offset = 0;
      o->data   = NULL;
    } else
      rc_errno = RCE_ERRNO;
      
    return o;
  }

  return NULL;
}
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function returns an int (0), so it can be used from Sclose() without
problems.  Pointed by Tamas Laufer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
rc_close(RcObject o)
{ o->member = NULL;			/* crash on access attempt */
  free(o);

  return 0;
}


void
rc_stat(RcObject o, RcStatBuf stat)
{ memcpy(stat, o->member, sizeof(*stat));
}


rc_offset
rc_seek(RcObject o, rc_offset to, int whence)
{ switch(whence)
  { case SEEK_CUR:
      to += o->offset;
      /*FALLTHROUGH*/
    case SEEK_SET:
    set:
      if ( to > o->member->size )
	return (rc_offset) -1L;
      o->offset = to;
      return o->offset;
    case SEEK_END:
      to += o->member->size;
      goto set;
    default:
      return (rc_offset) -1L;
  }
}


#ifndef MAPPED_ARCHIVE

int
updateFilePtr(RcObject o)
{ RcMember m = o->member;
  RcArchive rca = m->archive;
  rc_offset apos = o->offset + m->offset + rca->offset;

  if ( ftell(rca->fd) == apos )
    return 0;

  if ( fseek(rca->fd, apos, SEEK_SET) != 0 )
  { rc_errno = RCE_ERRNO;
    return -1;
  }

  return 0;
}

#endif



int
rc_read(RcObject o, void *buf, int bytes)
{ RcMember m = o->member;
  const char *mdata;

  if ( bytes < 0 )
    return -1;
  if ( o->offset + bytes > m->size )
    bytes = m->size - o->offset;

  if ( m->data )
  { mdata = m->data;
  } else
#ifdef MAPPED_ARCHIVE
  { mdata = (char *)m->archive->data + m->offset;
  }
#else
  { RcArchive rca = m->archive;
    int n;

    if ( updateFilePtr(o) < 0 )
      return -1;

    n = fread(buf, sizeof(char), bytes, rca->fd);
    if ( n > 0 )
      o->offset += n;
    else if ( n < 0 )
      rc_errno = RCE_ERRNO;
    
    return n;
  }
#endif

  memcpy(buf, mdata + o->offset, bytes);
  o->offset += bytes;

  return bytes;
}


int
rc_write(RcObject o, void *buf, int bytes)
{ RcMember m = o->member;

  if ( bytes < 0 )
    return -1;
  
  if ( o->offset + bytes > m->allocated )
  { rc_size size = m->allocated;
    
    if ( size == 0 )
      size = 1024;

    while( o->offset + bytes > size )
      size *= 2;
    m->allocated = size;

    if ( !m->data )
      m->data = malloc(m->allocated);
    else
      m->data = realloc(m->data, m->allocated);

    if ( !m->data )
    { rc_errno = RCE_ERRNO;
      return -1;
    }
  }
  if ( o->offset + bytes > m->size )
    m->size = o->offset + bytes;

  memcpy(m->data + o->offset, buf, bytes);
  o->offset += bytes;

  return bytes;
}


void *
rc_data(RcObject o, int *size)
{ RcMember m = o->member;

  if ( size )
    *size = m->size;

  if ( m->data )
    return m->data;
  else
#ifdef MAPPED_ARCHIVE
    return (char *)m->archive->data + m->offset;
#else
  { if ( (m->data = malloc(m->size)) )
    { RcArchive rca = m->archive;

      o->offset = 0;			/* dubious */
      updateFilePtr(o);

      if ( fread(m->data, sizeof(char), m->size, rca->fd) != m->size )
      { rc_errno = RCE_ERRNO;
	free(m->data);
	m->data = NULL;
	return NULL;
      }

      return m->data;
    }

    rc_errno = RCE_ERRNO;
    return NULL;
  }
#endif
}
