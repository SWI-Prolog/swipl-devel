/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef NO_SEGV_HANDLING
#define SEGV_HANDLING 1
#endif

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif HAVE_SYS_TYPES_H
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif HAVE_SYS_STAT_H
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
char *  top;				/* currently assigned top */

#define ulong unsigned long		/* avoid redefinition */

#define KB * 1024
#define MB KB KB

#define RoundDown(p, n)	((p) & ~((n)-1))

typedef SIGRETTYPE (*handler_t)(int signal);

#ifdef MAP_ANON
#define get_map_fd() (-1)
#define MAP_FLAGS MAP_ANON|MAP_PRIVATE|MAP_NORESERVE
#else
#define MAP_FLAGS MAP_PRIVATE|MAP_NORESERVE

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


		 /*******************************
		 *	REGISTER HANDLER	*
		 *******************************/

#ifdef HAVE_SIGINFO_H
#include <siginfo.h>
#endif


static handler_t
set_stack_guard_handler(int sig, void *func)
{
#ifdef HAVE_SIGACTION
  struct sigaction old;
  struct sigaction new;

  memset(&new, 0, sizeof(new));	/* deal with other fields */
#ifdef HAVE_SIGINFO
  new.sa_sigaction = func;
  new.sa_flags     = SA_RESTART|SA_SIGINFO;
#else
  new.sa_handler   = func;
  new.sa_flags     = SA_RESTART;
#endif

  if ( sigaction(sig, &new, &old) == 0 )
    return old.sa_handler;
  else
    return SIG_DFL;
#else
  return signal(sig, func);
#endif
}

		 /*******************************
		 *	      HANDLER		*
		 *******************************/


#ifdef SEGV_HANDLING
SIGRETTYPE
#ifdef HAVE_SIGINFO
segv_handler(int s, siginfo_t *info, void *extra)
#else
segv_handler(int s)
#endif
{ ulong addr = RoundDown((ulong)wraddr, pagsiz);

#ifdef HAVE_SIGINFO
  if ( !info || info->si_addr != wraddr )
  { if ( provides_address  )
    { provides_address = 0;
#ifdef VERBOSE
      if ( info )
	printf("\rExpected %p, got addr=%p\n", wraddr, info->si_addr);
      else
	printf("\nGot no siginfo\n");
#endif
    }
  }
#endif

  if ( mprotect((void *) addr, pagsiz, PROT_READ|PROT_WRITE) < 0 )
  { fprintf(stderr,
	    "Failed to change protection from %p for %d bytes\n",
	    (void *)addr, pagsiz);
    perror("mprotect");
    exit(1);
  }

#ifdef VERBOSE
  printf("\rSegv added page from %p", (void *)addr); fflush(stdout);
#endif

#ifndef BSD_SIGNALS
  signal(SIGSEGV, (handler_t) segv_handler);
#endif
}

void
expand_to(void *addr)
{ wraddr = addr;
}

#else /*SEGV_HANDLING*/

static void
expand_to(void *addr)
{ if ( (char *)addr >= top )
  { int incr;

    addr = (void *)RoundDown((ulong)addr, pagsiz);
    incr = (char *)addr-top + pagsiz;

    if ( mprotect((void *) top, incr, PROT_READ|PROT_WRITE) < 0 )
    { perror("mprotect");
      exit(1);
    }
    top += incr;

#ifdef VERBOSE
    printf("\rAdded page from %p", addr); fflush(stdout);
#endif
  }
}

#endif /*SEGV_HANDLING*/

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
  { expand_to(&p[n]);
    p[n] = n;
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

#if defined(SEGV_HANDLING) && defined(HAVE_SIGINFO)
  if ( provides_address )
    printf("SIGNAL_HANDLER_PROVIDES_ADDRESS=1;\n");
#endif
  if ( mapfd == -1 )
    printf("HAVE_MAP_ANON=1;\n");
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

  base = mmap(NULL, size, PROT_NONE, MAP_FLAGS, mapfd, 0L);
  if ( base == MAP_FAILED )
    perror("mmap");
    
#ifdef SEGV_HANDLING
  set_stack_guard_handler(SIGSEGV, segv_handler);
#endif

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
	    PROT_NONE, MAP_FLAGS|MAP_FIXED, mapfd, 0L) != base+truncto )
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
