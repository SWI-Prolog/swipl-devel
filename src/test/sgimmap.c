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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /*HAVE_SYS_TYPES_H*/
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /*HAVE_SYS_STAT_H*/
#include <signal.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif
#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

#ifdef NEED_DECL_ERRNO
extern int errno;
#endif

int	mapfd;				/* map this one */
int	pagsiz;				/* pagesize */
void *  wraddr;				/* current address */
char *  top;				/* currently assigned top */

#define ulong unsigned long		/* avoid redefinition */

#define KB * 1024
#define MB KB KB

#define RoundDown(p, n)	((p) & ~((n)-1))

		 /*******************************
		 *	   OBJECT TO MAP	*
		 *******************************/

#ifdef MAP_ANON
#define get_map_fd() (-1)
#define MAP_FLAGS MAP_ANON|MAP_PRIVATE|MAP_AUTOGROW|MAP_AUTORESRV
#else
#define MAP_FLAGS MAP_PRIVATE|MAP_AUTOGROW|MAP_AUTORESRV

static int
get_map_fd()
{ int fd;
  static char *map = "/tmp/pl-map";

  if ( (fd = open("/dev/zero", O_RDONLY)) >= 0 )
    return fd;

  if ( (fd = open(map, O_RDONLY)) < 0 )
  { if ( errno == ENOENT )
    { char buf[1024];
      char *s;
      int n;
      int oldmask = umask(0);

      if ( (fd = open(map, O_RDWR|O_CREAT, 0666)) < 0 )
      { perror(map);
        exit(1);
      }
      umask(oldmask);
      for(n=1024, s = buf; n > 0; n--)
        *s++ = '\0';
      for(n=pagsiz/1024; n > 0; n--)
      { if ( write(fd, buf, 1024) != 1024 )
	{ perror(map);
	  exit(1);
	}
      }

      return fd;
    }
    
    perror(map);
    exit(1);
    return -1;				/* make compiler happy */
  }

  return fd;
}
#endif /*MAP_ANON*/


static int
test_map(void *base, ulong size)
{ int times = size / sizeof(int);
  int *p = base;
  int n;

  top = base;

#ifdef VERBOSE
  printf("write-test from %p\n", p);
#endif
  for(n=0; n<times; n++)
  { p[n] = n;
  }
#ifdef VERBOSE
  printf("\nread-test ... "); fflush(stdout);
#endif
  for(n=0; n<times; n++)
  { if ( p[n] != n )
    { fprintf(stderr, "Read bad value at %d: %d\n", n, p[n]);
      return FALSE;
    }
  }
#ifdef VERBOSE
  printf("ok\n");
#endif
  return TRUE;
}


#ifndef HAVE_GETPAGESIZE
#ifdef _SC_PAGESIZE
int
getpagesize()
{ return sysconf(_SC_PAGESIZE);
}
#else /*_SC_PAGESIZE*/

#if hpux
#include <a.out.h>
int
getpagesize()
{  
#ifdef EXEC_PAGESIZE
  return EXEC_PAGESIZE;
#else
  return 4096;				/* not that important */
#endif
}
#endif /*hpux*/
#endif /*_SC_PAGESIZE*/
#endif /*HAVE_GETPAGESIZE*/


		 /*******************************
		 *	       MAIN		*
		 *******************************/

static void
ok()
{ printf("MMAP_STACK=1;\n");

  if ( mapfd == -1 )
    printf("HAVE_MAP_ANON=1;\n");

  exit(0);
}



int
main(int argc, char **argv)
{ char *base;
  ulong size;
  ulong truncto;

  if ( argc == 2 )
    size = atol(argv[1]) MB;
  else
    size = 4 MB;

  truncto = size/2;

  pagsiz = getpagesize();
  mapfd  = get_map_fd();

  base = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_FLAGS, mapfd, 0L);
  if ( base == MAP_FAILED )
    perror("mmap");
    
  if ( !test_map(base, size) )
    exit(1);

#ifdef MAP_FIXED
#ifdef VERBOSE
  printf("Truncating area to %ld ...\n", truncto);
#endif
  if ( munmap(base+truncto, size-truncto) < 0 )
  { perror("munmap()");
    exit(1);
  }
  if ( mmap(base+truncto, size-truncto,
	    PROT_READ|PROT_WRITE, MAP_FLAGS|MAP_FIXED,
	    mapfd, 0L) != base+truncto )
  { perror("remap()");
    exit(1);
  }
#ifdef VERBOSE
  printf("Testing again ...\n");
#endif
#endif /*MAP_FIXED*/

  if ( test_map(base, size) )
    ok();

  exit(1);
}
