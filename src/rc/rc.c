/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

#include "rc.h"
#include <stdio.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

char *program;

static char *
basename(const char *f)
{ const char *base;

  for(base = f; *f; f++)
  { if (*f == '/')
      base = f+1;
  }

  return (char *)base;
}


void
error(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  fprintf(stderr, "%s: error: ", basename(program));
  vfprintf(stderr, fm, args);
  fprintf(stderr, "\n");
  va_end(args);
}


void
verbose(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vfprintf(stderr, fm, args);
  va_end(args);
}


static int
badarchive(const char *name)
{ error("Could not open archive \"%s\": %s", name, rc_strerror(rc_errno));

  return 1;
}


static void
usage()
{ printf("usage: %s command resourcefile members\n", program);
  printf("commands:\n\n");
  printf("\tl\tList archive members\n");
  printf("\tx\tExtract members\n");
  printf("\ta\tAdd members to archive\n");
  printf("\td\tDelete members from archive\n");
}


static int
memberOfList(const char *name, char **list)
{ if ( list )
  { for( ; *list; list++ )
    { if ( strcmp(name, *list) == 0 )
	return TRUE;
    }

    return FALSE;
  }

  return TRUE;
}


static int
rcls(const char *archive, char **members)
{ RcArchive rca = rc_open_archive(archive, RC_RDONLY);
  RcMember m;

  if ( rca )
  { char *fmt  = "%8ld %-10s %-10s %-14s\n";
    char *sfmt = "%8s %-10s %-10s %-14s\n";

    printf(sfmt, "size", "class", "encoding", "name");

    for(m=rca->members; m; m = m->next)
    { if ( memberOfList(m->name, members) )
      { char *rcclass = m->rc_class ? m->rc_class : "data";
	char *enc     = m->encoding ? m->encoding : "none";

	printf(fmt, m->size, rcclass, enc, m->name);
      }
    }

    rc_close_archive(rca);
    return 0;
  }

  return badarchive(archive);
}


static int
rcextract(const char *archive, char **members)
{ RcArchive rca = rc_open_archive(archive, RC_RDONLY);
  RcMember m;

  if ( rca )
  { for(m=rca->members; m; m = m->next)
    { if ( memberOfList(m->name, members) )
      { FILE *fd = fopen(m->name, "wb");

	if ( fd )
	{ RcObject o = rc_open(rca, m->name, m->rc_class, RC_RDONLY);
	  char buf[8192];
	  int size = m->size;

	  while( size > 0 )
	  { int n = rc_read(o, buf, sizeof(buf));

	    if ( n > 0 )
	    { if ( (int)fwrite(buf, sizeof(char), n, fd) != n )
	      { fclose(fd);
		error("Failure writing %s: %s", m->name, strerror(errno));
	      }
	      size -= n;
	    } else if ( n == 0 )
	    { error("Premature EOF on archive %s", m->name);
	      break;
	    } else
	    { error("Read error on archive %s", m->name);
	      break;
	    }
	  }

	  if ( size == 0 )
	    verbose("x %s\n", m->name);

	  fclose(fd);
	  rc_close(o);
	} else
	  error("Could not open %s: %s", m->name, strerror(errno));
      }
    }

    rc_close_archive(rca);
    return 0;
  }

  return badarchive(archive);
}


static int
rcadd(const char *archive, char **members)
{ RcArchive rca = rc_open_archive(archive, RC_RDWR|RC_CREATE);
  char *rcclass = "data";
  char *enc     = "none";
  int clen = strlen("--class=");
  int elen = strlen("--encoding=");

  if ( !rca )
    return badarchive(archive);

  if ( members )
  { for( ; *members; members++ ) 
    { char *m = *members;

      if ( strncmp(m, "--class=", clen) == 0 )
      { rcclass = m+clen;

	continue;
      }
      if ( strncmp(m, "--encoding=", elen) == 0 )
      { enc = m+elen;

	continue;
      }

      if ( !rc_append_file(rca, basename(m), rcclass, enc, m) )
	error("Could not add \"%s\": %s", m, rc_strerror(rc_errno));
      else
	verbose("a %s\n", m);
    }
  }

  if ( !rc_close_archive(rca) )
  { error("Failed to create \"%s\": %s", archive, rc_strerror(rc_errno));
    return 1;
  }

  return 0;
}


static int
rcdel(const char *archive, char **members)
{ RcArchive rca = rc_open_archive(archive, RC_RDWR|RC_CREATE);
  char *rcclass = "data";
  int clen = strlen("--class=");

  if ( !rca )
    return badarchive(archive);

  if ( members )
  { for( ; *members; members++ ) 
    { char *m = *members;

      if ( strncmp(m, "--class=", clen) == 0 )
      { rcclass = m+clen;

	continue;
      }

      if ( !rc_delete(rca, m, rcclass) )
	error("Could not delete \"%s\": %s", m, rc_strerror(rc_errno));
      else
	verbose("d %s\n", m);
    }
  }

  if ( !rc_close_archive(rca) )
  { error("Failed to create \"%s\": %s", archive, rc_strerror(rc_errno));
    return 1;
  }

  return 0;
}


int
main(int argc, char **argv)
{ char *cmd;
  char *archive;
  char **members;

  program = argv[0];

  if ( argc >= 3 )
  { cmd     = argv[1];
    archive = argv[2];
    members = (argc == 3 ? NULL : argv+3);
    
    if ( strcmp(cmd, "l") == 0 )
      return rcls(archive, members);
    if ( strcmp(cmd, "x") == 0 )
      return rcextract(archive, members);
    if ( strcmp(cmd, "a") == 0 )
      return rcadd(archive, members);
    if ( strcmp(cmd, "d") == 0 )
      return rcdel(archive, members);
  }

  usage();
  return 1;
}
