/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <signal.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif

#ifndef SIGRETTYPE
#define SIGRETTYPE void
#endif

#ifdef NEED_DECL_ERRNO
extern int errno;
#endif

int	mapfd;				/* map this one */
int	pagsiz;				/* pagesize */
void *  wraddr;				/* current address */
int	provides_address = 1;		/* assume */

#define ulong unsigned long		/* avoid redefinition */

#define K * 1024
#define MB K K

#define RoundUp(x, y)	((x)%(y) == 0 ? (x) : ((x)|((y)-1))+1)
#define RoundDown(p, n)	((p) & ~((n)-1))
#define min(x, y)	((x) < (y) ? (x) : (y))

typedef SIGRETTYPE (*handler_t)(int signal);

#ifdef MAP_ANON
#define get_map_fd() (-1)
#define STACK_MAP_TYPE MAP_ANON|MAP_PRIVATE|MAP_FIXED
#else
#define STACK_MAP_TYPE MAP_PRIVATE|MAP_FIXED

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
  }
}
#endif /*MAP_ANON*/


SIGRETTYPE
segv_handler(int s, int type, void *scp, char *sigaddr)
{ ulong addr = RoundDown((ulong)wraddr, pagsiz);

  if ( sigaddr != wraddr )
    provides_address = 0;

  if ( mmap((void *) addr, pagsiz,
	    PROT_READ|PROT_WRITE,
	    STACK_MAP_TYPE|MAP_FIXED,
	    mapfd, 0L) != (void *)addr )
  { perror("mmap");
    exit(1);
  }
#ifdef VERBOSE
  printf("+"); fflush(stdout);
#endif

#ifndef BSD_SIGNALS
  signal(SIGSEGV, (handler_t) segv_handler);
#endif
}


static int
test_map(int *low)
{ int size = 40 K;
  int n;

#ifdef VERBOSE
  printf("write-test from %p\n", low);
#endif
  for(n=0; n<size; n++)
  { wraddr = &low[n];
    low[n] = n;
  }
#ifdef VERBOSE
  printf("\nread-test ... "); fflush(stdout);
#endif
  for(n=0; n<size; n++)
  { if ( low[n] != n )
    { fprintf(stderr, "Read bad value at %d: %d\n", n, low[n]);
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
		 *	  TOP OF THE HEAP	*
		 *******************************/

#ifdef HAVE_GETRLIMIT
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#ifdef RLIMIT_DATA
ulong
topOfHeap()
{ struct rlimit limit;
  ulong heap_base = (ulong)sbrk(0);

  heap_base = RoundDown(heap_base, 8 MB);

  if ( getrlimit(RLIMIT_DATA, &limit) == 0 )
  { ulong top = limit.rlim_cur + heap_base;

#ifdef VERBOSE
    printf("Heap: %p ... %p\n", (void *)heap_base, (void *)top);
#endif
    return top;
  }

  return 0L;
}
#else
#define topOfHeap() (0L)
#endif /*RLIMIT_DATA*/
#else
#define topOfHeap() (0L)
#endif /*HAVE_GETRLIMIT*/


		 /*******************************
		 *	       MAIN		*
		 *******************************/

static int
testarea(ulong base, ulong top)
{ ulong step = 8 * pagsiz;
  ulong addr;

  for(addr=base; addr<top; addr += step)
  { if ( (ulong) mmap((void *) addr, pagsiz,
		      PROT_READ|PROT_WRITE, STACK_MAP_TYPE,
		      mapfd, 0L) == addr )
    {
#ifdef VERBOSE
      printf("."); fflush(stdout);
#endif
      if ( munmap((void *) addr, pagsiz) != 0 )
      { perror("munmap");
	return FALSE;
      }
    } else
    {
#ifdef VERBOSE
      char msg[1024];
      sprintf(msg, "Failed to map at %p", addr);
      perror(msg);
#endif
      return FALSE;
    }
  }

  signal(SIGSEGV, (handler_t) segv_handler);

  return test_map((int *)base);
}


static void
ok()
{ printf("MMAP_STACK=1;\n");

  if ( provides_address )
    printf("SIGNAL_HANDLER_PROVIDES_ADDRESS=1;\n");
  if ( mapfd == -1 )
    printf("HAVE_MAP_ANON=1;\n");
}


int
main(int argc, char **argv)
{ ulong top, htop = topOfHeap();
  ulong base;

  pagsiz = getpagesize();
  mapfd  = get_map_fd();

  if ( htop )
  { top  = RoundDown(htop, pagsiz);
    base = top - (64 MB * 3 + 8 MB);

    if ( testarea(base, top) )
    { ok();
      exit(0);
    }
  }

  base = RoundDown((ulong)sbrk(0) + 64 MB, pagsiz);
  top = base + (64 MB * 3 + 8 MB);

  if ( testarea(base, top) )
  { if ( htop )
      printf("TOPOFHEAP=0;\n");
    ok();
    exit(0);
  }

  exit(1);
}
