/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <signal.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

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
#define M K K

#define RoundUp(x, y)	((x)%(y) == 0 ? (x) : ((x)|((y)-1))+1)
#define RoundDown(p, n)	((p) & ~((n)-1))
#define min(x, y)	((x) < (y) ? (x) : (y))

typedef SIGRETTYPE (*handler_t)(int signal);

SIGRETTYPE
segv_handler(int s, int type, void *scp, char *sigaddr)
{ ulong addr = RoundDown((ulong)wraddr, pagsiz);

  if ( sigaddr != wraddr )
    provides_address = 0;

  if ( mmap((void *) addr, pagsiz, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED,
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


void
test_map(int *low)
{ int size = 40 K;
  int n;

#ifdef VERBOSE
  printf("write-test from 0x%08xL\n", (ulong)low);
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
      exit(1);
    }
  }
#ifdef VERBOSE
  printf("ok\n");
#endif
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


int
main(int argc, char **argv)
{ ulong thelow  = 0xffffffff;
  ulong thehigh = 0;
  ulong step    = 1 M;
  ulong low, high;
  ulong addr;
  ulong seghigh;

  pagsiz = getpagesize();
  mapfd  = get_map_fd();
  low    = RoundUp((ulong)malloc(1) + 100 K, step);
  high   = min(RoundUp((ulong)&argc - 10 M, step), 0x80000000L);
  
  if ( low & 0xf0000000L )
  { if ( (low & 0xf0000000L) == 0x10000000L )
      seghigh = 0x20000000L;
    else if ( (low & 0xf0000000L) == 0x20000000L )
      seghigh = 0x40000000L;
    else if ( (low & 0xf0000000L) == 0x40000000L )
      seghigh = 0x80000000L;
  } else
    seghigh = 0x20000000L;

  if ( high > seghigh )
    high = seghigh;

  for(addr=low; addr<=high; addr += step)
  { if ( (ulong) mmap((void *) addr, pagsiz,
		      PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED,
		      mapfd, 0L) == addr )
    { if ( addr < thelow )
	thelow = addr;
      if ( addr > thehigh )
	thehigh = addr;
      if ( munmap((void *) addr, pagsiz) != 0 )
      { perror("munmap");
	exit(1);
      }
    } else
    { if ( thelow < thehigh )
	break;
    }
  }

  if ( thelow < thehigh )
  { signal(SIGSEGV, (handler_t) segv_handler);
    test_map((void *)thelow);

    printf("MMAP_STACK=1;\n");
    if ( thelow > low )
      printf("MMAP_MIN_ADDRESS=0x%08xL;\n", thelow);
    printf("MMAP_MAX_ADDRESS=0x%08xL;\n", thehigh);
    if ( provides_address )
      printf("SIGNAL_HANDLER_PROVIDES_ADDRESS=1;\n");
#ifdef VERBOSE
    printf("Space is %d MB\n", (thehigh-thelow)/(1 M));
#endif
    exit(0);
  } else
    exit(1);
}
