/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Initialise the system
*/

#define GLOBAL				/* allocate global variables here */
#include "pl-incl.h"
#include <sys/stat.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module initialises the system and defines the global variables.  It
also holds the code  for  dynamically  expanding  stacks  based  on  MMU
access.   Finally  it holds the code to handle signals transparently for
foreign language code or packages with which Prolog was linked together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards void initStacks P((long, long, long, long, long));

void
setupProlog()
{ DEBUG(1, printf("Starting Heap Initialisation\n"));

  critical = 0;
  aborted = FALSE;

  startCritical;
#if unix
  DEBUG(1, printf("Prolog Signal Handling ...\n"));
  initSignals();
#endif
  DEBUG(1, printf("OS ...\n"));
  initOs();
  DEBUG(1, printf("Stacks ...\n"));
  initStacks( options.localSize, 
	      options.globalSize, 
	      options.trailSize, 
	      options.argumentSize,
	      options.lockSize);

  if ( status.dumped == FALSE )
  { DEBUG(1, printf("Atoms ...\n"));
    initAtoms();
    DEBUG(1, printf("Functors ...\n"));
    initFunctors();
    DEBUG(1, printf("Modules ...\n"));
    initModules();
    DEBUG(1, printf("Records ...\n"));
    initRecords();
    DEBUG(1, printf("Flags ...\n"));
    initFlags();
    DEBUG(1, printf("Foreign Predicates ...\n"));
    initBuildIns();
    DEBUG(1, printf("Operators ...\n"));
    initOperators();
    DEBUG(1, printf("Arithmetic ...\n"));
    initArith();
    DEBUG(1, printf("Tracer ...\n"));
    initTracer();
    debugstatus.styleCheck = LONGATOM_CHECK |
			     SINGLETON_CHECK |
			     DOLLAR_STYLE |
			     DISCONTIGUOUS_STYLE;
    DEBUG(1, printf("wam_table ...\n"));
    initWamTable();
  } else
  { resetReferences();
    resetGC();			/* reset garbage collector */
    stateList = (State) NULL;	/* all states are already in core */
  }
  DEBUG(1, printf("IO ...\n"));
  initIO();
  resetLoader();
  getSymbols();
  resetTerm();
  status.io_initialised = TRUE;

  endCritical;

  environment_frame = (LocalFrame) NULL;
  statistics.inferences = 0;
#if O_STORE_PROGRAM || O_SAVE
  cannot_save_program = NULL;
#else
  cannot_save_program = "Not supported on this machine";
#endif

#if O_XWINDOWS
  DEBUG(1, printf("XWindows ...\n");
  initXWindows();
#endif

  DEBUG(1, printf("Heap Initialised\n"));
}

#if unix

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			   SIGNAL HANDLING

SWI-Prolog catches a number of signals.  Interrupt is catched  to  allow
the  user  to interrupt normal execution.  Floating point exceptions are
trapped  to  generate  a  normal   error   or   arithmetic   exceptions.
Segmentation  violations  are  trapped  on  machines  using  the  MMU to
implement stack overflow  checks  and  stack  expansion.   These  signal
handlers  needs  to be preserved over saved states and the system should
allow foreign language code to handle signals without  interfering  with
Prologs signal handlers.  For this reason a layer is wired around the OS
signal handling.

Code in SWI-Prolog should  call  pl_signal()  rather  than  signal()  to
install  signal  handlers.  SWI-Prolog assumes the handler function is a
void function.  On some systems this gives  some  compiler  warnigns  as
they  define  signal handlers to be int functions.  This should be fixed
some day.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
fatal_signal_handler(sig, type, scp, addr)
int sig, type;
struct sigcontext *scp;
char *addr;
{ DEBUG(1, printf("Fatal signal %d\n", sig));

  deliverSignal(sig, type, scp, addr);
}


void
initSignals()
{ int n;

  if ( status.dumped == FALSE )
  { for( n = 0; n < MAXSIGNAL; n++ )
    { signalHandlers[n].os = signalHandlers[n].user = SIG_DFL;
      signalHandlers[n].catched = FALSE;
    }
    
    pl_signal(SIGTTOU, SIG_IGN);
    pl_signal(SIGSEGV, fatal_signal_handler);
    pl_signal(SIGBUS,  fatal_signal_handler);
    pl_signal(SIGILL,  fatal_signal_handler);
  } else
  { for( n = 0; n < MAXSIGNAL; n++ )
      if ( signalHandlers[n].os != SIG_DFL )
        signal(n, signalHandlers[n].os);
  }
}

handler_t
pl_signal(sig, func)
int sig;
handler_t func;
{ handler_t old = signal(sig, func);

  signalHandlers[sig].os = func;
  signalHandlers[sig].catched = (func == SIG_DFL ? FALSE : TRUE);

  return old;
}

void
deliverSignal(sig, type, scp, addr)
int sig, type;
struct sigcontext *scp;
char *addr;
{ if ( signalHandlers[sig].user != SIG_DFL )
  { (*signalHandlers[sig].user)(sig, type, scp, addr);
    return;
  }

  sysError("Unexpected signal: %d\n", sig);
}

#endif unix

#if O_DYNAMIC_STACKS

#define STACK_SEPARATION size_alignment
#define STACK_MINIMUM    (32 * 1024)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			STACK MEMORY MANAGEMENT

In these days some operating systems allows the  user  to  map  physical
memory  anywhere  in  the  virtual  address  space.  For multiple stacks
machines such as Prolog, this is ideal.  The  stacks  can  be  allocated
very  far  appart  with  large  gaps  between  them.   Stack overflow is
detected by hardware and results (in  Unix)  in  a  segmentation  fault.
This fault is trapped and the stack is automatically expanded by mapping
more  memory.

In theory the stacks can be deallocated dynamically as  well,  returning
the  resources to the system.  Currently this can be done explicitely by
calling  trim_stacks/0  and  the  garbage  collector.    It   might   be
interesting  to  do  this  automatically  at  certain points to minimise
memory requirements.  How?

Currently this mechanism can use mmap() and munmap() of SunOs 4.0 or the
system-V shared memory primitives (if they meet certain criteria.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <errno.h>
extern int errno;
extern int getpagesize();
extern char *sbrk();

static int size_alignment;	/* Stack sizes must be aligned to this */
static int base_alignment;	/* Stack bases must be aligned to this */

#define MB (1024L * 1024L)	/* megabytes */

static long
align_size(x)
long x;
{ return x % size_alignment ? (x / size_alignment + 1) * size_alignment : x;
}

static long
align_base(x)
long x;
{ return x % base_alignment ? (x / base_alignment + 1) * base_alignment : x;
}

#if O_CAN_MAP
#include <sys/mman.h>
#include <fcntl.h>

extern int munmap();
static int mapfd = -1;			/* File descriptor used for mapping */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return a file descriptor to a file, open  for  reading  and  holding  at
least  one  page of 0's. On some systems /dev/zero is available for this
trick.  If not, a file of one page is created under the name /tmp/pl-map
if it does not already exists and this file is opened for  reading.   It
can  be  shared  by  many  SWI-Prolog  processes  and (therefore) is not
removed on exit.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
swap_fd()
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
      { fatalError("Can't create map file %s: %s", map, OsError());
        return -1;
      }
      umask(oldmask);
      for(n=1024, s = buf; n > 0; n--)
        *s++ = EOS;
      for(n=size_alignment/1024; n > 0; n--)
        if ( write(fd, buf, 1024) != 1024 )
          fatalError("Failed to create map file %s: %s\n", map, OsError());

      return fd;
    }
    fatalError("Can't open map file %s: %s", map, OsError());
    return -1;
  }

  return fd;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expand stack `s' by one page.  This might not be  enough,  but  in  this
(very  rare) case another segmentation fault will follow to get the next
page.  The memory is expanded by mapping the map-fd file onto  the  page
using  a  private  map.  This way the contents of the map-file is copied
into the page but all changes to the page are  kept  local.   Note  that
SunOs  4.0.0  on SUN-3 has a bug that causes the various mapped pages to
point to the same physical memory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
map(s)
Stack s;
{ if ( mmap(s->max, size_alignment,
	    PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED,
	    mapfd, 0L) != s->max )
    fatalError("Failed to map memory at 0x%x for %d bytes on fd=%d: %s\n",
	       s->max, size_alignment, mapfd, OsError());

  DEBUG(2, printf("mapped %d bytes from 0x%x\n",
		  size_alignment, (unsigned) s->max));
  s->max += size_alignment;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unmap() returns all memory resources of a stack that are  no  longer  in
use to the OS.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
unmap(s)
Stack s;
{ caddress addr = (caddress) align_size(s->top + 1);

  if ( addr < s->max )
  { if ( munmap(addr, s->max - addr) != 0 )
      fatalError("Failed to unmap memory: %s", OsError());
    s->max = addr;
  }
}


static void
deallocateStack(s)
Stack s;
{ long len = (ulong)s->max - (ulong)s->base;

  if ( len > 0 && munmap(s->base, len) != 0 )
    fatalError("Failed to unmap memory: %s", OsError());
}


void
deallocateStacks()
{ deallocateStack(&stacks.local);
  deallocateStack(&stacks.global);
  deallocateStack(&stacks.trail);
  deallocateStack(&stacks.argument);
  deallocateStack(&stacks.lock);
}


bool
restoreStack(s)
Stack s;
{ caddress max;
  long len;
  struct stat statbuf;

  if ( mapfd < 0 || fstat(mapfd, &statbuf) == -1 )
  { mapfd = swap_fd();
    base_alignment = size_alignment = getpagesize();
  }

  max = (caddress) align_size(s->top + 1);
  len = max - (caddress) s->base;

  if ( mmap(s->base, len,
	    PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED,
	    mapfd, 0L) != s->base )
    fatalError("Failed to map memory at 0x%x for %d bytes on fd=%d: %s\n",
	       s->base, len, mapfd, OsError());

  s->max = max;
  DEBUG(0, printf("mapped %d bytes from 0x%x\n", len, (unsigned) s->base));
  succeed;
}


#endif O_CAN_MAP

#if O_SHARED_MEMORY
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>
extern int shmget();
extern char *shmat();
extern int shmdt();
extern int shmctl();
#if gould
#define S_IRUSR SHM_R
#define S_IWUSR SHM_W
#endif
#if mips
struct pte { long pad };		/* where is the real one? */
#include <sys/param.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Shared memory based MMU controlled stacks are a bit  more  tricky.   The
main  problem is that shared memory segments are scares resources.  Upto
a certain limit, each time the size of the stack is doubled.  Afterwards
the stack grows in fixed segments  of  size  s->segment_initial  *  2  ^
s->segment_double.   These  parameters  may  vary  from  stack to stack,
suiting the caracteristics of the stack and of the OS limits on  virtual
address space and number of shared memory segnments.  See pl-incl.h

The  shared  memory  segments  are  created,  mapped   and   immediately
afterwards  freed.   According  to  the documentation they actually will
live untill they are unmapped by the last process.  Immediately  freeing
them  avoids the burden to do this on exit() and ensures these resources
are freed, also if SWI-Prolog crashes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_SHM_ALIGN_FAR_APART

#define min(a, b) ((a) < (b) ? (a) : (b))

static long
new_stack_size(s)
Stack s;
{ long size = s->top - s->base;
  long free = size / s->segment_initial;

  if ( free > s->segment_double ) free = s->segment_double;
  else if ( free < 1 )            free = 1;
  
  size = align_size(size + free * s->segment_initial);  

  if ( size > s->limit )
    size = s->limit;

  return size;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resize_segment(s, n, size)
  Resize segment n of stack s to get size size.  The base address of the
  segement is assumed to be correct.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resize_segment(s, n, size)
Stack s;
int n;
long size;
{ if ( s->segments[n].size != size )
  { int id = -1;
    char *addr;

    if ( size > 0 )
    { if ( (id=shmget(IPC_PRIVATE, size, S_IRUSR|S_IWUSR)) < 0 )
	fatalError("Failed to create shared memory object: %s", OsError());
      if ( (addr = shmat(id, 0, 0)) < 0 )
	fatalError("Failed to attach shared memory segment: %s", OsError());
      bcopy(s->segments[n].base, addr, min(size, s->segments[n].size));
      if ( shmdt(addr) < 0 )
      	fatalError("Failed to detach shared memory segment: %s", OsError());
    }    

    if ( s->segments[n].size > 0 )
      if ( shmdt(s->segments[n].base) < 0 )
      	fatalError("Failed to detach shared memory segment: %s", OsError());
    
    if ( id >= 0 )
    { DEBUG(0, printf("Attach segment of size %ld at 0x%x\n",
		      size, s->segments[n].base));
      if ( shmat(id, s->segments[n].base, 0) != s->segments[n].base )
      	fatalError("Failed to attach shared memory segment at 0x%x: %s",
		   s->segments[n].base, OsError());
      
      if ( shmctl(id, IPC_RMID, NULL) < 0 )
	fatalError("Failed to release shared memory object: %s", OsError());
    }

    s->segments[n].size = 0;
  }
}


static void
map(s)
Stack s;
{ long new_size = new_stack_size(s);
  int  top_segment = new_size / base_alignment;
  int  n;

  DEBUG(1, printf("Expanding %s stack to %ld\n", s->name, new_size));

  for(n=0; n < top_segment; n++)
    resize_segment(s, n, base_alignment);

  resize_segment(s, n, new_size % base_alignment);

  for(n++; s->segments[n].size > 0; n++ )
    resize_segment(s, n, 0L);

  s->max = s->base + new_size;
}


static void
unmap(s)
Stack s;
{ if ( new_stack_size(s) < s->max - s->base )
    map(s);
}

#else O_SHM_ALIGN_FAR_APART

static void
map(s)
Stack s;
{ int id;
  char *rval;
  long len;
  caddress addr;

  len  = (s->segment_top <= s->segment_double
	  		? s->segment_initial << (s->segment_top)
	  		: s->segment_initial << s->segment_double);
  addr = s->segments[s->segment_top].base;

  if ( (id=shmget(IPC_PRIVATE, len, S_IRUSR|S_IWUSR)) < 0 )
  { if ( errno == EINVAL )
      fatalError("Kernel is not configured with option IPCSHMEM (contact a guru)");
    fatalError("Failed to create shared memory object: %s", OsError());
  }

  if ( (rval = shmat(id, addr, 0)) != (char *) addr )
    fatalError("Failed to map memory at %ld: %s\n", addr, OsError());

  if ( shmctl(id, IPC_RMID, NULL) < 0 )
    fatalError("Failed to release shared memory object: %s", OsError());

  s->segment_top++;
  s->max = s->segments[s->segment_top].base = addr+len;
}


static void
unmap(s)
Stack s;
{ while( s->segment_top > 0 && s->segments[s->segment_top-1].base > s->top )
  { s->segment_top--;
    if ( shmdt(s->segments[s->segment_top].base) < 0 )
      fatalError("Failed to unmap: %s\n", OsError());
    s->max = s->segments[s->segment_top].base;
  }
}

#endif O_SHM_ALIGN_FAR_APART
#endif O_SHARED_MEMORY

static bool
expandStack(s, addr)
Stack s;
caddress addr;
{
#if O_NO_SEGV_ADDRESS
  addr = s->top + STACK_SEPARATION;
#endif
  if ( addr < s->max || addr >= s->base + s->maxlimit + STACK_SEPARATION )
    fail;				/* outside this area */

  if ( addr <= s->max + STACK_SEPARATION*2 )
  { if ( addr < s->base + s->limit )
    { DEBUG(1, printf("Expanding %s stack\n", s->name));
      map(s);
      considerGarbageCollect(s);

      succeed;
    }

    outOf(s);   
  }

  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This the the signal handler for segmentation faults if we are using  MMU
controlled  stacks.   The  only  argument  we  are  interested in is the
address of the segmentation fault.  SUN provides this via  an  argument.
If   your   system   does   not   provide   this  information,  set  the
O_NO_SEGV_ADDRESS flag.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
segv_handler(sig, type, scp, addr)
int sig, type;
struct sigcontext *scp;
char *addr;
{ DEBUG(1, printf("Page fault at %ld (0x%x)\n", (long) addr, (unsigned) addr));

  if ( expandStack(&stacks.global, addr) ||
       expandStack(&stacks.local, addr) ||
       expandStack(&stacks.trail, addr) ||
       expandStack(&stacks.argument, addr) ||
       expandStack(&stacks.lock, addr) )
    return;

  deliverSignal(sig, type, scp, addr);
}

static bool
limit_stack(s, limit)
Stack s;
long limit;
{ if ( limit > s->maxlimit || limit <= 0 )
    limit = s->maxlimit;
  if ( limit <= s->top - s->base )
    limit = s->top - s->base;

  limit = align_size(limit);
  s->limit = limit;

  succeed;
}

static void
init_stack(s, name, base, limit)
Stack s;
char *name;
caddress base;
long limit;
{ s->maxlimit = limit;		/* deleted this notion */
  s->name     = name;
  s->base     = s->max = s->top = base;
  limit_stack(s, limit);
#if O_SHARED_MEMORY
#if O_SHM_ALIGN_FAR_APART
{ int n;

  s->segment_initial = 32 * 1024;
  s->segment_double  = 20;
  for(n=0; n < MAX_STACK_SEGMENTS; n++)
  { s->segments[n].size = 0;
    s->segments[n].base = s->base + base_alignment * n;
  }
}
#else O_SHM_ALIGN_FAR_APART
  s->segment_top     = 0;
  s->segment_initial = 32 * 1024;
  s->segment_double  = 5;
  s->segments[0].base = base;
#endif O_SHM_ALIGN_FAR_APART
#endif O_SHARED_MEMORY
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
initStacks() initialises the stacks structure,  thus  assigning  a  base
address,  a limit and a name to each of the stacks.  Finally it installs
a signal handler for handling  segmentation  faults.   The  segmentation
fault handler will actually create and expand the stacks on segmentation
faults.   Currently,  it is assumed memory can be mapped from sbrk(0) to
MAX_VIRTUAL_ADDRESS and the stack is outside this area.  This is true on
SUN  and  GOULD.   On  other  machines  the  C-stack  might   start   at
MAX_VIRTUAL_ADDRESS   and   grow   downwards.    In   this   case  lower
MAX_VIRTUAL_ADDRESS a bit (if you have 100 MB virtual address  space  or
more,  I  would  suggest  16 MB), so space is allocated for the C-stack.
The stacks are allocated right below MAX_VIRTUAL_ADDRESS, at a  distance
STACK_SEPARATION  from  each other.  STACK_SEPARATION must be a multiple
of the page size and must be at least  MAXVARIABLES  *  sizeof(word)  as
this  is the maximum discontinuity in writing the stacks.  On almost any
machine size_alignment will do.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
initStacks(local, global, trail, argument, lock)
long local, global, trail, argument, lock;
{ long heap = 0;			/* malloc() heap */
  int large = 1;
  ulong base, top, space, large_size;

  if ( status.dumped == FALSE )
  { hBase = (char *)0x20000000L;
    hTop  = (char *)NULL;
  }

  size_alignment = getpagesize();
#if O_CAN_MAP
  base_alignment = size_alignment;
  mapfd  = swap_fd();
#endif
#if O_SHARED_MEMORY
  base_alignment = SHMLBA;
  DEBUG(0, printf("Shared memory must be aligned to %d (0x%x) bytes\n",
		  base_alignment, base_alignment));
#endif

  assert(MAXVARIABLES*sizeof(word) < STACK_SEPARATION);

  local    = (long) align_size(local);	/* Round up to page boundary */
  global   = (long) align_size(global);
  trail    = (long) align_size(trail);
  argument = (long) align_size(argument);
  lock     = (long) align_size(lock);

  if ( local    == 0 ) large++;		/* find dynamic ones */
  if ( global   == 0 ) large++;
  if ( trail    == 0 ) large++;
  if ( argument == 0 ) large++;
  if ( lock     == 0 ) large++;

  base  = (long) align_base(sbrk(0));
  top   = (long) MAX_VIRTUAL_ADDRESS;
  DEBUG(1, printf("top = 0x%x, stack at 0x%x\n", top, (unsigned) &top));
  space = top - base;
  space -= align_base(heap) +
           align_base(local + STACK_SEPARATION) +
	   align_base(global + STACK_SEPARATION) +
	   align_base(trail + STACK_SEPARATION) +
	   align_base(lock + STACK_SEPARATION) +
	   align_base(argument);
  
  large_size = ((space / large) / base_alignment) * base_alignment;
  if ( large_size < STACK_MINIMUM )
    fatalError("Can't fit requested stack sizes in address space");
  DEBUG(1, printf("Large stacks are %ld\n", large_size));

  heap                          = large_size;
  if ( local    == 0 ) local    = large_size;
  if ( global   == 0 ) global   = large_size;
  if ( trail    == 0 ) trail    = large_size;
  if ( argument == 0 ) argument = large_size;
  if ( lock     == 0 ) lock     = large_size;

  base += heap;

#define INIT_STACK(name, print) \
  DEBUG(1, printf("%s stack at 0x%x; size = %ld\n", print, base, name)); \
  init_stack(&stacks.name, print, base, name); \
  base += name + STACK_SEPARATION; \
  base = align_base(base);

  INIT_STACK(global,   "global");
  INIT_STACK(local,    "local");
  INIT_STACK(trail,    "trail");
  INIT_STACK(lock,     "lock");
  INIT_STACK(argument, "argument");

  pl_signal(SIGSEGV, segv_handler);
}

		/********************************
		*     STACK TRIMMING & LIMITS   *
		*********************************/

word
pl_trim_stacks()
{ unmap(&stacks.local);
  unmap(&stacks.global);
  unmap(&stacks.trail);
  unmap(&stacks.argument);
  unmap(&stacks.lock);

  succeed;
}

word
pl_limit_stack(s, l)
Word s, l;
{ Atom k;
  long limit;

  if ( !isAtom(*s) || !(isInteger(*l) || *l == (word) ATOM_unlimited) )
    return warning("limit_stack/2: instantiation fault");
  k = (Atom)*s;
  limit = (*l == (word) ATOM_unlimited ? 0L : valNum(*l) * 1024L);

  if ( k == ATOM_local )
    return limit_stack(&stacks.local, limit);
  else if ( k == ATOM_global )
    return limit_stack(&stacks.global, limit);
  else if ( k == ATOM_trail )
    return limit_stack(&stacks.trail, limit);
  else if ( k == ATOM_argument )
    return limit_stack(&stacks.argument, limit);
  else
    return warning("limit_stack/2: unknown stack: %s", stringAtom(k));
}

#else O_DYNAMIC_STACKS

		/********************************
		*    SIMPLE STACK ALLOCATION    *
		*********************************/

forwards void init_stack P((Stack, char *, long));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On systems that do not allow us to get access to the MMU (or that do not
have an MMU)  the  stacks  have  fixed  size  and  overflow  checks  are
implemented  in  software.   The stacks are allocated using malloc(). If
you malloc() does not allow you to get more than 64K bytes in one go you
better start looking for another Prolog system (IBM-PC  is  an  example:
why does IBM bring computers on the marked that are 10 years out-of-date
at the moment of announcement?).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_limit_stack(s, l)		/* does not work on these systems */
Word s, l;
{ succeed;
}

word
pl_trim_stacks()
{ succeed;
}

static void
init_stack(s, name, size)
Stack s;
char *name;
long size;
{ if ( s->base == NULL )
  { fatalError("Not enough core to allocate stacks");
    return;
  }

  s->name 	= name;
  s->top	= s->base;
  s->limit	= size;
  s->max	= s->base + size;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On tos, malloc() returns a 2 byte  aligned  pointer.   We  need  4  byte
aligned  pointers.   Allocate() is patched for that and dumped states do
not exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if tos
#define MALLOC(p, n)    Allocate(n)
#else
#define MALLOC(p, n)	(status.dumped == FALSE ? Malloc(n) : Realloc(p, n))
#endif

static void
initStacks(local, global, trail, argument, lock)
long local, global, trail, argument, lock;
{ long old_heap = statistics.heap;

  if ( status.dumped == FALSE )
  { hBase = (char *)0x20000000L;
    hTop  = (char *)NULL;
  }

  gBase = (Word) MALLOC(gBase, global + sizeof(word) +
			       local + sizeof(struct localFrame) +
			       MAXARITY * sizeof(word));
  lBase = (LocalFrame)	addPointer(gBase, global+sizeof(word));
  tBase = (TrailEntry)	MALLOC(tBase, trail);
  aBase = (Word *)	MALLOC(aBase, argument);
  pBase = (Lock)	MALLOC(pBase, lock);

  init_stack((Stack)&stacks.global,	"global",	global);
  init_stack((Stack)&stacks.local,	"local",	local);
  init_stack((Stack)&stacks.trail,	"trail",	trail);
  init_stack((Stack)&stacks.argument,	"argumet",	argument);
  init_stack((Stack)&stacks.lock,	"lock",		lock);

  statistics.heap = old_heap;
}

#endif O_DYNAMIC_STACKS
