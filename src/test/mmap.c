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

#define KB * 1024
#define MB KB KB

#define RoundUp(x, y)	((x)%(y) == 0 ? (x) : ((x)|((y)-1))+1)
#define RoundDown(p, n)	((p) & ~((n)-1))
#define min(x, y)	((x) < (y) ? (x) : (y))
#define max(x, y)	((x) > (y) ? (x) : (y))

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
{ int size = 40 KB;
  int n;

#ifdef VERBOSE
  printf("\nwrite-test from %p\n", low);
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
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

					/* __linux__ TASK_SIZE stuff */
					/* contributed by Roman Hodek */
#ifdef __linux__
#include <asm/page.h>
#include <asm/system.h>
#include <asm/ptrace.h>
#include <asm/segment.h>
#include <asm/processor.h>
#endif

#ifdef RLIMIT_DATA
ulong
topOfHeap(ulong heap_base)
{ struct rlimit limit;

  if ( getrlimit(RLIMIT_DATA, &limit) == 0 )
  { ulong top = limit.rlim_cur + heap_base;

#ifdef TASK_SIZE
    if ( top < heap_base || top > TASK_SIZE )
      return TASK_SIZE;
#else
    if ( top < heap_base )
      return 0L;
#endif

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

#ifdef VERBOSE
  printf("Testing area %p ... %p\n", base, top);
#endif

  for(addr=base; addr<top; addr += step)
  {
#if VERBOSE >= 2
    printf("%p ... ", addr); fflush(stdout);
#else
#ifdef VERBOSE
    if ( ((addr-base)/step) % 64 == 0 )
    { printf("\n%p ", addr);
      fflush(stdout);
    }
#endif
#endif

    if ( (ulong) mmap((void *) addr, pagsiz,
		      PROT_READ|PROT_WRITE, STACK_MAP_TYPE,
		      mapfd, 0L) == addr )
    {
#if VERBOSE >= 2
      printf("ok\n");
#else
#ifdef VERBOSE
      printf("."); fflush(stdout);
#endif
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
run_testarea() runs testarea as a child, 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(HAVE_SYS_RESOURCE_H)
#include <sys/resource.h>
#endif
#if defined(HAVE_SYS_WAIT_H) || defined(UNION_WAIT)
#include <sys/wait.h>
#endif

#ifdef UNION_WAIT

#define wait_t union wait

#ifndef WEXITSTATUS
#define WEXITSTATUS(s) ((s).w_status)
#endif
#ifndef WTERMSIG
#define WTERMSIG(s) ((s).w_status)
#endif

#else /*UNION_WAIT*/

#define wait_t int

#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#endif /*UNION_WAIT*/


static int
run_testarea(ulong base, ulong top)
{ pid_t pid;

  if ( (pid = fork()) == 0 )
  { if ( testarea(base, top) )		/* the child */
    { ok();
      exit(0);
    }
  } else if ( pid < 0 )
  { perror("fork");
    exit(1);
  } else				/* wait for it */
  { wait_t status;
    int n;

    while((n = wait(&status)) != -1 && n != pid);
    if ( n == -1 )
    { perror("wait");
      exit(1);
    }
    if ( WIFEXITED(status) )
      return WEXITSTATUS(status) == 0 ? TRUE : FALSE;

    return FALSE;			/* signalled */
  }
}

#define DEFSTACKSIZE (64 MB * 3 + 8 MB)

int
main(int argc, char **argv)
{ ulong top, htop;
  ulong base;
  ulong hbase;
  ulong stack;
  ulong defsize = DEFSTACKSIZE;		/* Thats what we want */
  ulong size;			
  int tried = 0;

  pagsiz = getpagesize();
  mapfd  = get_map_fd();
  hbase  = RoundDown((ulong)sbrk(0) + 8 MB, pagsiz);
  htop   = topOfHeap(hbase);

  if ( htop )
    defsize = size = min(htop-(hbase+1 MB), DEFSTACKSIZE);
  else
    defsize = size = DEFSTACKSIZE;

again:
  if ( htop )				/* try from the top ... */
  { top  = RoundDown(htop, pagsiz);
    base = top - size;

    base = max(base, hbase);
    if ( run_testarea(base, top) )
    { if ( size != DEFSTACKSIZE )
	printf("MMAP_STACKSIZE=%d;\n", size/(1 MB));
      exit(0);				/* done */
    }
  }

  base = hbase;				/* failed, lets try from the bottom */
  top = base + size;
  if ( htop )
    top = min(top, htop);
  stack = (ulong)&top;			/* do not overwrite the stack */
  if ( stack > base )
  { ulong stack_base;

    if ( STACK_DIRECTION < 0 )
      stack_base = RoundDown(stack - 8 MB, pagsiz);
    else
      stack_base = RoundDown(stack, 64 KB);

    top = min(top, stack_base);
  }
  size = top-base;

  if ( run_testarea(base, top) )
  { if ( size != defsize )
      printf("MMAP_STACKSIZE=%d;\n", size/(1 MB));
    if ( htop )
      printf("TOPOFHEAP=0;\n");
    exit(0);
  }

  if ( !tried++ )
  {
#if defined(__NetBSD__) && _MACHINE == amiga
    size = 120 MB;
    goto again;
#endif
  }


  exit(1);
}
