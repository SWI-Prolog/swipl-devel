/*  pl-os.c,v 1.45 1994/04/11 08:37:37 jan Exp

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Operating System Dependencies
*/

/*  Modified (M) 1993 Dave Sherratt  */

/*#define O_DEBUG 1*/

#if __TOS__
#include <tos.h>		/* before pl-os.h due to Fopen, ... */
#endif
#if OS2 && EMX
#include <os2.h>                /* this has to appear before pl-incl.h */
#endif

#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-itf.h"

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_PWD_H
#include <pwd.h>
#endif
#if HAVE_VFORK_H
#include <vfork.h>
#endif
#ifdef HAVE_UNISTD_H
#define lock lock_function		/* WATCOM problem */
#include <unistd.h>
#undef lock
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

#if O_RLC
#include <console.h>
#endif

#include <fcntl.h>
#ifndef __WATCOMC__			/* appears a conflict */
#include <errno.h>
#endif

#if defined(__WATCOMC__)
#define lock lock_function
#include <io.h>
#undef lock
#include <dos.h>
#endif

#if OS2 && EMX
static real initial_time;
#endif /* OS2 */

forwards void	initExpand(void);
forwards void	initRandom(void);
forwards void	initEnviron(void);
forwards char *	okToExec(char *);
forwards char *	Which(char *);

#ifndef DEFAULT_PATH
#define DEFAULT_PATH "/bin:/usr/bin"
#endif

		 /*******************************
		 *	       GLOBALS		*
		 *******************************/
#ifdef HAVE_CLOCK
static long clock_wait_ticks;
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is a contraction of functions that used to be all  over  the
place.   together  with  pl-os.h  (included  by  pl-incl.h) this file
should define a basic  layer  around  the  OS,  on  which  the  rest  of
SWI-Prolog  is  based.   SWI-Prolog  has  been developed on SUN, running
SunOs 3.4 and later 4.0.

Unfortunately some OS's simply do not offer  an  equivalent  to  SUN  os
features.   In  most  cases part of the functionality of the system will
have to be dropped. See the header of pl-incl.h for details.
- - - - - - - - - - -  - - - - - */

		/********************************
		*         INITIALISATION        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool initOs()

    Initialise the OS dependant functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
initOs(void)
{ DEBUG(1, Sdprintf("OS:initExpand() ...\n"));
  initExpand();
  DEBUG(1, Sdprintf("OS:initRandom() ...\n"));
  initRandom();
  DEBUG(1, Sdprintf("OS:initEnviron() ...\n"));
  initEnviron();

#ifdef HAVE_CLOCK
  clock_wait_ticks = 0L;
#endif

#if OS2
  { DATETIME i;
    DosGetDateTime((PDATETIME)&i);
    initial_time = (i.hours * 3600.0) 
                   + (i.minutes * 60.0) 
		   + i.seconds
		   + (i.hundredths / 100.0);
  }
#endif /* OS2 */

  DEBUG(1, Sdprintf("OS:done\n"));

  succeed;
}


typedef void (*halt_function)(int, Void);
typedef struct on_halt *OnHalt;

struct on_halt
{ halt_function	function;
  Void		argument;
  OnHalt	next;
};

static OnHalt on_halt_list;
static int halting;

void
PL_on_halt(halt_function f, Void arg)
{ if ( !halting )
  { OnHalt h = allocHeap(sizeof(struct on_halt));

    h->function = f;
    h->argument = arg;
    h->next = on_halt_list;
    on_halt_list = h;
  }
}


volatile void
Halt(int rval)
{ OnHalt h;

  pl_notrace();				/* avoid recursive tracing */

  if ( !halting )
  { for(h = on_halt_list; h; h = h->next)
      (*h->function)(rval, h->argument);

    if ( status.initialised )
      callGoal(MODULE_system, (word) lookupAtom("$run_at_halt"), FALSE);

#if defined(__WINDOWS__) || defined(__WIN32__)
    if ( rval != 0 )
      PlMessage("Exit status is %d", rval);
#endif

    dieIO();
    RemoveTemporaryFiles();
  }

  exit(rval);
  /*NOTREACHED*/
}

		/********************************
		*            OS ERRORS          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    char *OsError()
	Return a char *, holding a description of the last OS call error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
OsError(void)
{
#ifdef HAVE_STRERROR
#ifdef __WIN32__
  return strerror(_xos_errno());
#else
  return strerror(errno);
#endif
#else /*HAVE_STRERROR*/
static char errmsg[64];

#if unix
  extern int sys_nerr;
#if !EMX
  extern char *sys_errlist[];
#endif
  extern int errno;

  if ( errno < sys_nerr )
    return sys_errlist[errno];
#endif

  Ssprintf(errmsg, "Unknown Error (%d)", errno);
  return errmsg;
#endif /*HAVE_STRERROR*/
}

		/********************************
		*    PROCESS CHARACTERISTICS    *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    real CpuTime()

    Returns a floating point number, representing the amount  of  (user)
    CPU-seconds  used  by the process Prolog is in.  For systems that do
    not allow you to obtain this information  you  may  wish  to  return
    elapsed  time  since Prolog was started, as this function is used to
    by consult/1 and time/1 to determine the amount of CPU time used  to
    consult a file or to execute a query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_TIMES
#include <sys/times.h>

#if defined(_SC_CLK_TCK)
#define Hz ((int)sysconf(_SC_CLK_TCK))
#else
#ifdef HZ
#  define Hz HZ
#else
#  define Hz 60				/* if nothing better: guess */
#endif
#endif /*_SC_CLK_TCK*/
#endif /*HAVE_TIMES*/


real
CpuTime(void)
{
#ifdef HAVE_TIMES
  struct tms t;

  times(&t);

  return t.tms_utime / (real)(Hz) ;
#endif

#if OS2 && EMX
  DATETIME i;

  DosGetDateTime((PDATETIME)&i);
  return (((i.hours * 3600) 
                 + (i.minutes * 60) 
		 + i.seconds
	         + (i.hundredths / 100.0)) - initial_time);
#endif

#ifdef HAVE_CLOCK
  return (real) (clock() - clock_wait_ticks) / (real) CLOCKS_PER_SEC;
#endif
}

		/********************************
		*       MEMORY MANAGEMENT       *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    long *Allocate(n)
	  long n;

    Allocate a memory area of `n' bytes from the operating system.   `n'
    is a long as we need to allocate one uniform array of longs for both
    the  local  stack  and  global  stack,  which  implies  it should be
    possible to allocate at least a few hundred Kbytes.  If  you  cannot
    implement  this  function  you  are in deep trouble.  You either can
    decide to redesign large part of the data representation, or  forget
    about  SWI-Prolog.   Memory  is never returned to the system.  As it
    would only concern small areas,  all  over  SWI-Prolog's  memory  no
    currently  available operating system (I'm aware of) will be able to
    handle it anyway.  THE RETURN VALUE SHOULD BE ROUNDED TO BE A  VALID
    POINTER FOR LONGS AND STRUCTURES AND AT LEAST A MULTIPLE OF 4.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Void
Allocate(long int n)
{ Void mem = Malloc(n);

  return (Void) mem;
}


		/********************************
		*     STRING MANIPULATION	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The builtin strcmp() of SunOs is broken on some machines ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if sun
int
strcmp(s1, s2)
unsigned char *s1, *s2;
{ while(*s1 && *s1 == *s2)
    s1++, s2++;

  return *s1 - *s2;
}
#endif


		/********************************
		*           ARITHMETIC          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    long Random()

    Return a random number. Used for arithmetic only.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
initRandom(void)
{ long init = Time();
#ifdef HAVE_SRANDOM
  srandom(init);
#else
#ifdef HAVE_SRAND
  srand(init);
#endif
#endif
}

long
Random(void)
{ 
#ifdef HAVE_RANDOM
  return random();
#else
  return rand();
#endif
}

		/********************************
		*             FILES             *
		*********************************/

      /* (Everything you always wanted to know about files ...) */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generation and administration of temporary files.  Currently  only  used
by  the foreign language linker.  It might be useful to make a predicate
available to the Prolog user based on these functions.  These  functions
are  in  this  module as non-UNIX OS probably don't have getpid() or put
temporaries on /tmp.

    Atom TemporaryFile(id)
	 char *id;

    The return value of this call is an atom,  whose  string  represents
    the  path  name of a unique file that can be used as temporary file.
    `id' is a char * that can be used to make it easier to identify  the
    file as a specific kind of SWI-Prolog intermediate file.

    void RemoveTemporaryFiles()

    Remove all temporary files.  This function should be  aware  of  the
    fact  that some of the file names generated by TemporaryFile() might
    not be created at all, or might already have been deleted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct tempfile * TempFile;

static struct tempfile
{ Atom 		name;
  TempFile	next;
} *tempfiles, *temptail;		/* chain of temporary files */

Atom
TemporaryFile(char *id)
{ static char temp[MAXPATHLEN];
  TempFile tf = (TempFile) allocHeap(sizeof(struct tempfile));

#if unix
  static int temp_counter = 0;
  Ssprintf(temp, "/tmp/pl_%s_%d_%d", id, (int) getpid(), temp_counter++);
#endif

#if EMX
  static int temp_counter = 0;
  char *foo;

  if ( (foo = tempnam(".", (const char *)id)) )
  { strcpy(temp, foo);
    free(foo);
  } else
    Ssprintf(temp, "pl_%s_%d_%d", id, getpid(), temp_counter++);
#endif

#if tos
  tmpnam(temp);
#endif

  tf->name = lookupAtom(temp);
  tf->next = (TempFile) NULL;
  
  if ( temptail == (TempFile) NULL )
  { tempfiles = temptail = tf;
  } else
  { temptail->next = tf;
    temptail = tf;
  }

  return tf->name;
}

void
RemoveTemporaryFiles(void)
{ TempFile tf, tf2;  

  for(tf = tempfiles; tf; tf = tf2)
  { RemoveFile(stringAtom(tf->name));
    tf2 = tf->next;
    freeHeap(tf, sizeof(struct tempfile));
  }

  tempfiles = temptail = (TempFile) NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fortunately most C-compilers  are  sold  with  a  library  that  defines
Unix-style  access  to  the  file system.  The standard functions go via
macros to deal with 16-bit machines, but are not  defined  as  functions
here.   Some  more  specific things SWI-Prolog wants to know about files
are defined here:

    int  getdtablesize()

    SWI-Prolog assumes it can refer to open i/o streams via  read()  and
    write() by small integers, returned by open(). These integers should
    be  in  the range [0, ..., getdtablesize()). If your system does not
    do this you better redefine the Open(), Read() and Write() macros so
    they  do  meet  this  requirement.   Prolog  allocates  a  table  of
    structures with getdtablesize() entries.

    long LastModifiedFile(path)
	 char *path;

    Returns the last time `path' has been modified.  Used by the  source
    file administration to implement make/0.

    bool ExistsFile(path)
	 char *path;

    Succeeds if `path' refers to the pathname of a regular file  (not  a
    directory).

    bool AccessFile(path, mode)
	 char *path;
	 int mode;

    Succeeds if `path' is the pathname of an existing file and it can
    be accessed in any of the inclusive or constructed argument `mode'.

    bool ExistsDirectory(path)
	 char *path;

    Succeeds if `path' refers to the pathname  of  a  directory.

    bool RemoveFile(path)
	 char *path;

    Removes a (regular) file from the  file  system.   Returns  TRUE  if
    succesful FALSE otherwise.

    bool RenameFile(old, new)
	 char *old, *new;

    Rename file from name `old' to name `new'. If new already exists, it is
    deleted. Returns TRUE if succesful, FALSE otherwise.

    bool OpenStream(stream)
	 int stream;

    Succeeds if `stream' refers to an open i/o stream.

    bool MarkExecutable(path)
	 char *path;

    Mark `path' as an executable program.  Used by the intermediate code
    compiler and the creation of stand-alone executables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef HAVE_GETDTABLESIZE
int
getdtablesize()
{
#ifdef OPEN_MAX
  return OPEN_MAX;
#else
#ifdef _SC_OPEN_MAX			/* POSIX, USG */
  return sysconf(_SC_OPEN_MAX);
#else
#ifdef HAVE_GETRLIMIT
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef RLIMIT_NOFILE
  { struct rlimit rlp;
    getrlimit(RLIMIT_NOFILE,&rlp);
    return (rlp.rlim_cur);
  }
#endif /*RLIMIT_NOFILE*/
#endif /*HAVE_GETRLIMIT*/
#endif /*_SC_OPEN_MAX*/
#endif /*OPEN_MAX*/
}
#endif /*HAVE_GETDTABLESIZE*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Size of a VM page of memory.  Most BSD machines have this function.  If not,
here are several alternatives ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

/* ********************************************************************
   Design Note -- atoenne@mpi-sb.mpg.de --

   Beware! OsPath() and PrologPath() are insecure functions.
   Make sure that you copy the result of these functions to a proper location
   before you call the functions again. Otherwise you will write over the
   former result.
   ******************************************************************** */

#if O_HPFS

/* 
   Conversion rules Prolog <-> OS/2 (using HPFS)
   / <-> \
   /x:/ <-> x:\  (embedded drive letter)
   No length restrictions up to MAXPATHLEN, no case conversions.
*/

char *
PrologPath(char *ospath, char *path)
{ char *s = ospath, *p = path;
  int limit = MAXPATHLEN-1;

  if (isLetter(s[0]) && s[1] == ':')
  { *p++ = '/';
    *p++ = *s++;
    *p++ = *s++;
    limit -= 3;
  }
  for(; *s && limit; s++, p++, limit--)
    *p = (*s == '\\' ? '/' : makeLower(*s));
  *p = EOS;

  return path;
}


char *
OsPath(const char *unixpath)
{ static char path[MAXPATHLEN];
  register char *s = unixpath, *p = path;
  register int limit = MAXPATHLEN-1;

  if ( s[0] == '/' && isLetter(s[1]) && s[2] == ':') /* embedded drive letter*/
  { s++;
    *p++ = *s++;
    *p++ = *s++;
    if ( *s != '/' )
      *p++ = '\\';
    limit -= 2;
  }

  for(; *s && limit; s++, p++, limit--)
    *p = (*s == '/' ? '\\' : *s);
  if ( p[-1] == '\\' && p > path )
    p--;
  *p = EOS;

  return path;
} 
#endif /* O_HPFS */

#if unix
char *
PrologPath(char *p, char *buf)
{ strcpy(buf, p);

  return buf;
}

char *
OsPath(const char *p)
{ return (char *) p;
}
#endif

#if O_XOS
char *
PrologPath(char *p, char *buf)
{ _xos_canonical_filename(p, buf);
  return buf;
}

char *
OsPath(const char *p)
{ return (char *) p;
}
#endif /* O_XOS */

long
LastModifiedFile(char *f)
{
#if HAVE_STAT
  struct stat buf;

  if ( stat(OsPath(f), &buf) < 0 )
    return -1;

  return (long)buf.st_mtime;
#endif

#if tos
#define DAY	(24*60*60L)
  static int msize[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  long t;
  int n;
  struct ffblk buf;
  struct dz
  { unsigned int hour : 5;	/* hour (0-23) */
    unsigned int min  : 6;	/* minute (0-59) */
    unsigned int sec  : 5;	/* seconds in steps of 2 */
    unsigned int year : 7;	/* year (0=1980) */
    unsigned int mon  : 4;	/* month (1-12) */
    unsigned int day  : 5;	/* day (1-31) */
  } *dz;

  if ( findfirst(OsPath(f), &buf, FA_HIDDEN) != 0 )
    return -1;
  dz = (struct dz *) &buf.ff_ftime;
  DEBUG(2, Sdprintf("%d/%d/%d %d:%d:%d\n",
	   dz->day, dz->mon, dz->year+1980, dz->hour, dz->min, dz->sec));

  t = (10*365+2) * DAY;		/* Start of 1980 */
  for(n=0; n < dz->year; n++)
    t += ((n % 4) == 0 ? 366 : 365) * DAY;
  for(n=1; n < dz->mon; n++)
    t += msize[n+1] * DAY;
  t += (dz->sec * 2) + (dz->min * 60) + (dz->hour *60*60L);

  return t;
#endif
}  


#ifndef F_OK
#define F_OK 0
#endif

bool
ExistsFile(char *path)
{
#ifdef HAVE_ACCESS
  if ( access(OsPath(path), F_OK) == 0 )
    succeed;
  fail;
#else
#ifdef HAVE_STAT
  struct stat buf;

  if ( stat(OsPath(path), &buf) == -1 || (buf.st_mode & S_IFMT) != S_IFREG )
    fail;
  succeed;
#endif

#if tos
  struct ffblk buf;

  if ( findfirst(OsPath(path), &buf, FA_HIDDEN) == 0 )
  { DEBUG(2, Sdprintf("%s (%s) exists\n", path, OsPath(path)));
    succeed;
  }
  DEBUG(2, Sdprintf("%s (%s) does not exist\n", path, OsPath(path)));
  fail;
#endif
#endif
}

bool
AccessFile(char *path, int mode)
{
#ifdef HAVE_ACCESS
  int m = 0;

  if ( mode == ACCESS_EXIST ) 
    m = F_OK;
  else
  { if ( mode & ACCESS_READ    ) m |= R_OK;
    if ( mode & ACCESS_WRITE   ) m |= W_OK;
#ifdef X_OK
    if ( mode & ACCESS_EXECUTE ) m |= X_OK;
#endif
  }

  return access(OsPath(path), m) == 0 ? TRUE : FALSE;
#endif

#ifdef tos
  struct ffblk buf;

  if ( findfirst(OsPath(path), &buf, FA_DIREC|FA_HIDDEN) != 0 )
    fail;			/* does not exists */
  if ( (mode & ACCESS_WRITE) && (buf.ff_attrib & FA_RDONLY) )
    fail;			/* readonly file */

  succeed;
#endif
}

bool
ExistsDirectory(char *path)
{ char *ospath = OsPath(path);
#ifdef HAVE_STAT
  struct stat buf;

  if ( stat(ospath, &buf) < 0 )
    fail;

  if ( (buf.st_mode & S_IFMT) == S_IFDIR )
    succeed;

  fail;
#endif

#ifdef tos
  struct ffblk buf;

  if ( findfirst(ospath, &buf, FA_DIREC|FA_HIDDEN) == 0 &&
       buf.ff_attrib & FA_DIREC )
    succeed;
  if ( streq(ospath, ".") || streq(ospath, "..") )	/* hack */
    succeed;
  fail;
#endif
}


long
SizeFile(char *path)
{ struct stat buf;
#ifdef HAVE_STAT
  if ( stat(OsPath(path), &buf) == -1 )
    return -1;
#endif

  return buf.st_size;
}


int
RemoveFile(const char *path)
{
#ifdef HAVE_REMOVE
  return remove(OsPath(path)) == 0 ? TRUE : FALSE;
#else
  return unlink(OsPath(path)) == 0 ? TRUE : FALSE;
#endif
}


bool
RenameFile(char *old, char *new)
{
#ifdef HAVE_RENAME
  return rename(old, new) == 0 ? TRUE : FALSE;
#else
  char os_old[MAXPATHLEN];
  char os_new[MAXPATHLEN];
  int rval;

  strcpy(os_old, OsPath(old));
  strcpy(os_new, OsPath(new));

  unlink(new);
  if ((rval = link(os_old, os_new)) == 0 
              && (rval = unlink(os_old)) != 0)
    unlink(new);

  if (rval == 0)
    succeed;

  fail;
#endif /*HAVE_RENAME*/
}

bool
SameFile(char *f1, char *f2)
{ if ( !streq(f1, f2) )
  { 
#ifdef unix				/* doesn't work on most not Unix's */
    struct stat buf1;
    struct stat buf2;

    if ( stat(OsPath(f1), &buf1) != 0 || stat(OsPath(f2), &buf2) != 0 )
      fail;
    if ( buf1.st_ino == buf2.st_ino && buf1.st_dev == buf2.st_dev )
      succeed;
#endif
#ifdef O_XOS
    char p1[MAXPATHLEN];
    char p2[MAXPATHLEN];

    _xos_limited_os_filename(f1, p1);
    _xos_limited_os_filename(f2, p2);
    if ( streq(p1, p2) )
      succeed;
#endif
    /* Amazing! There is no simple way to check two files for identity. */
    /* stat() and fstat() both return dummy values for inode and device. */
    /* this is fine as OS'es not supporting symbolic links don't need this */

    fail;
  }

  succeed;
}


bool
OpenStream(int fd)
{
#ifdef HAVE_FSTAT
  struct stat buf;

  return fstat(fd, &buf) == 0 ? TRUE : FALSE;
#else
  return fd < 3 ? TRUE : FALSE;	/* Sinput, Soutput and Serror are open */
#endif
}


bool
MarkExecutable(char *name)
{
#if defined(HAVE_STAT) && defined(HAVE_CHMOD)
  struct stat buf;
  int um;

  um = umask(0777);
  umask(um);
  if ( stat(name, &buf) == -1 )
    return warning("Can't stat(2) `%s': %s", name, OsError());

  if ( (buf.st_mode & 0111) == (~um & 0111) )
    succeed;

  buf.st_mode |= 0111 & ~um;
  if ( chmod(name, buf.st_mode) == -1 )
    return warning("Couldn't turn %s into an executable: %s", name, OsError());
#endif /* defined(HAVE_STAT) && defined(HAVE_CHMOD) */

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    char *AbsoluteFile(file)
  	 char *file;

    Expand a file specification to a system-wide unique  description  of
    the  file  that can be passed to the file functions that take a path
    as argument.  Path should refer to the same file, regardless of  the
    current  working  directory.   On  Unix absolute file names are used
    for this purpose.

    This  function  is  based  on  a  similar  (primitive)  function  in
    Edinburgh C-Prolog.

    char *BaseName(path)
	 char *path;

    Return the basic file name for a file having path `path'.

    char *DirName(path)
	 char *path;
    
    Return the directory name for a file having path `path'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(HAVE_SYMLINKS) && defined(HAVE_STAT)
#define O_CANONISE_DIRS

typedef struct canonical_dir *CanonicalDir;

static struct canonical_dir
{ char *	name;			/* name of directory */
  char *	canonical;		/* canonical name of directory */
  dev_t		device;			/* device number */
  ino_t		inode;			/* inode number */
  CanonicalDir  next;			/* next in chain */
} *canonical_dirlist = NULL;            /* initialization -- atoenne -- */

forwards char   *canoniseDir(char *);
#endif /*O_CANONISE_DIRS*/

static  char    CWDdir[MAXPATHLEN];	/* current directory */
static  int     CWDlen;			/* Length of CWDdir */

static void
initExpand(void)
{ 
#ifdef O_CANONISE_DIRS
  char *dir;
  char *cpaths;
#endif

  CWDdir[0] = EOS;
  CWDlen = 0;

#ifdef O_CANONISE_DIRS
  if ( (cpaths = getenv("CANONICAL_PATHS")) )
  { char buf[MAXPATHLEN];

    while(*cpaths)
    { char *e;

      if ( (e = strchr(cpaths, ':')) )
      { int l = e-cpaths;

	strncpy(buf, cpaths, l);
	buf[l] = EOS;
	cpaths += l+1;
	canoniseDir(buf);
      } else
      { canoniseDir(cpaths);
	break;
      }
    }
  }

  if ( (dir = getenv("HOME")) ) canoniseDir(dir);
  if ( (dir = getenv("PWD"))  ) canoniseDir(dir);
#endif
}

#ifdef O_CANONISE_DIRS

static char *
canoniseDir(char *path)
{ CanonicalDir d;
  struct stat buf;

  DEBUG(1, Sdprintf("canoniseDir(%s) --> ", path));

  for(d = canonical_dirlist; d; d = d->next)
  { if ( streq(d->name, path) )
    { if ( d->name != d->canonical )
	strcpy(path, d->canonical);

      DEBUG(1, Sdprintf("(lookup) %s\n", path));
      return path;
    }
  }

  if ( stat(OsPath(path), &buf) == 0 )
  { CanonicalDir dn = allocHeap(sizeof(struct canonical_dir));
    char dirname[MAXPATHLEN];
    char *e = path + strlen(path);

    dn->next   = canonical_dirlist;
    dn->name   = store_string(path);
    dn->inode  = buf.st_ino;
    dn->device = buf.st_dev;

    do
    { strncpy(dirname, path, e-path);
      dirname[e-path] = EOS;
      if ( stat(OsPath(dirname), &buf) < 0 )
	break;

      DEBUG(2, Sdprintf("Checking %s (dev=%d,ino=%d)\n",
			dirname, buf.st_dev, buf.st_ino));

      for(d = canonical_dirlist; d; d = d->next)
      { if ( d->inode == buf.st_ino && d->device == buf.st_dev )
	{ canonical_dirlist = dn;

	  DEBUG(2, Sdprintf("Hit with %s (dev=%d,ino=%d)\n",
			    d->canonical, d->device, d->inode));

	  strcpy(dirname, d->canonical);
	  strcat(dirname, e);
	  strcpy(path, dirname);
	  dn->canonical = store_string(path);
	  DEBUG(1, Sdprintf("(replace) %s\n", path));
	  return path;
	}
      }

      for(e--; *e != '/' && e > path + 1; e-- )
	;

    } while( e > path );

    dn->canonical = dn->name;
    canonical_dirlist = dn;

    DEBUG(1, Sdprintf("(new, existing) %s\n", path));
    return path;
  }

  DEBUG(1, Sdprintf("(nonexisting) %s\n", path));
  return path;
}

#else

#define canoniseDir(d)

#endif /*O_CANONISE_DIRS*/


static char *
canoniseFileName(char *path)
{ char *out = path, *in = path;
  char *osave[100];
  int  osavep = 0;

  while( in[0] == '/' && in[1] == '.' && in[2] == '.' && in[3] == '/' )
    in += 3;
  if ( in[0] == '/' )
    *out++ = '/';
  osave[osavep++] = out;

  while(*in)
  { if (*in == '/')
    {
    again:
      if ( *in )
      { while( in[1] == '/' )
	  in++;
	while( in[1] == '.' && (in[2] == '/' || in[2] == EOS) )
	{ in += 2;
	  goto again;
	}
	while (in[1] == '.' && in[2] == '.' &&
	       (in[3] == '/' || in[3] == EOS) && osavep > 0 )
	{ out = osave[--osavep];
	  in += 3;
	  goto again;
	}
      }
      if ( *in )
	in++;
      if ( out > path && out[-1] != '/' )
	*out++ = '/';
      osave[osavep++] = out;
    } else
      *out++ = *in++;
  }
  *out++ = *in++;

  return path;
}


char *
canonisePath(char *path)
{ canoniseFileName(path);

#ifdef O_CANONISE_DIRS
{ char *e;
  char dirname[MAXPATHLEN];

  e = path + strlen(path) - 1;
  for( ; *e != '/' && e > path; e-- )
    ;
  strncpy(dirname, path, e-path);
  dirname[e-path] = EOS;
  canoniseDir(dirname);
  strcat(dirname, e);
  strcpy(path, dirname);
}
#endif

  return path;
}


#include <ctype.h>

forwards char	*takeWord(char **);
forwards int	ExpandFile(char *, char **);

static char *
takeWord(char **string)
{ static char wrd[MAXPATHLEN];
  register char *s = *string;
  register char *q = wrd;
  register int left = MAXPATHLEN-1;

  while( isalnum(*s) || *s == '_' )
  { if ( --left < 0 )
    { warning("Variable or user name too long");
      return (char *) NULL;
    }
    *q++ = *s++;
  }
  *q = EOS;
  
  *string = s;
  return wrd;
}


bool
expandVars(char *pattern, char *expanded)
{ int size = 0;
  char c;

  if ( *pattern == '~' )
  { 
#ifdef HAVE_GETPWNAM
    static char fred[20];
    static char fredLogin[MAXPATHLEN];
#else
    char plp[MAXPATHLEN];
#endif
    char *user;
    char *value;
    int l;

    pattern++;
    user = takeWord(&pattern);

#ifdef HAVE_GETPWNAM
    if ( user[0] != EOS || (value = getenv("HOME")) == (char *) NULL )
    { struct passwd *pwent;

      if ( !streq(fred, user) )
      { if ( (pwent = getpwnam(user)) == (struct passwd *) NULL )
	{ if ( fileerrors )
	    warning("%s: Unknown user");
	  fail;
	}
	strcpy(fred, user);
	strcpy(fredLogin, pwent->pw_dir);
      }
      value = fredLogin;
    }	  
#else
    if ( user[0] != EOS || (value = getenv("HOME")) == (char *) NULL )
    { value = "/";	/* top directory of current drive */
    } else
    { value = PrologPath(value, plp);
    }
#endif /*HAVE_GETPWNAM*/
    size += (l = (int) strlen(value));
    if ( size >= MAXPATHLEN )
      return warning("Path name too long");
    strcpy(expanded, value);
    expanded += l;
  }

  for( ;; )
  { switch( c = *pattern++ )
    { case EOS:
	break;
      case '$':
	{ char *var = takeWord(&pattern);
	  char *value = getenv(var);
	  int l;

	  if ( value == (char *) NULL )
	  { if ( fileerrors )
	      warning("%s: Undefined variable", var);
	    fail;
	  }
	  size += (l = (int)strlen(value));
	  if ( size >= MAXPATHLEN )
	    return warning("Path name too long");
	  strcpy(expanded, value);
	  expanded += l;

	  continue;
	}
      default:
	if ( ++size >= MAXPATHLEN )
	  return warning("Path name too long");
	*expanded++ = c;

	continue;
    }
    break;
  }

  if ( ++size >= MAXPATHLEN )
    return warning("Path name too long");
  *expanded = EOS;

  succeed;
}


static int
ExpandFile(char *pattern, char **vector)
{ static char expanded[MAXPATHLEN];
  int matches = 0;

  if ( !expandVars(pattern, expanded) )
    return -1;
  
  vector[matches++] = expanded;

  return matches;
}


char *
ExpandOneFile(char *spec)
{ static char file[MAXPATHLEN];
  char *vector[256];
  
  switch( ExpandFile(spec, vector) )
  { case -1:
	return (char *) NULL;
    case 0:
	warning("%s: No match", spec);
	return (char *) NULL;
    case 1:
	strcpy(file, vector[0]);
	return file;
    default:
	warning("%s: Ambiguous", spec);
	return (char *) NULL;
  }
}


#ifdef O_HASDRIVES

int
IsAbsolutePath(const char *p)		/* /d:/ or d:/ */
{ if ( p[0] == '/' && p[2] == ':' && isLetter(p[1]) &&
       (p[3] == '/' || p[3] == '\0') )
    succeed;

  if ( p[1] == ':' && isLetter(p[0]) && (p[2] == '/' || p[2] == '\0') )
    succeed;

  fail;
}


static inline int
isDriveRelativePath(const char *p)	/* '/...' */
{ return p[0] == '/' && !IsAbsolutePath(p);
}

#ifdef __WIN32__
#undef mkdir
#include <direct.h>
#define mkdir _xos_mkdir
#endif

static int
GetCurrentDriveLetter()
{
#ifdef OS2
  return _getdrive();
#endif
#ifdef __WIN32__
  return _getdrive() + 'a' - 1;
#endif
#ifdef __WATCOMC__
  { unsigned drive;
    _dos_getdrive(&drive);
    return = 'a' + drive - 1;
  }
#endif
}

#else /*O_HASDRIVES*/

int
IsAbsolutePath(const char *p)
{ return p[0] == '/';
}

#endif /*O_HASDRIVES*/

#define isRelativePath(p) ( p[0] == '.' )


char *
AbsoluteFile(char *spec)
{ static char path[MAXPATHLEN];
  char buf[MAXPATHLEN];
  char *file;  

  PrologPath(spec, buf);
  if ( (file = ExpandOneFile(buf)) == (char *) NULL )
    return (char *) NULL;

  if ( IsAbsolutePath(file) )
  { strcpy(path, file);

    return canonisePath(path);
  }

#ifdef O_HASDRIVES
  if ( isDriveRelativePath(file) )	/* /something  --> d:/something */
  { if ((strlen(file) + 3) > MAXPATHLEN)
    { warning("path name too long");
      return (char *) NULL;
    }
    path[0] = GetCurrentDriveLetter();
    path[1] = ':';
    strcpy(&path[2], file);
    return canonisePath(path);
  }
#endif /*O_HASDRIVES*/

  if ( CWDlen == 0 )
  { char buf[MAXPATHLEN];

    getcwd(buf, MAXPATHLEN);
    strcpy(CWDdir, canonisePath(buf));
    CWDlen = strlen(CWDdir);
    CWDdir[CWDlen++] = '/';
    CWDdir[CWDlen] = EOS;
  }

  if ( (CWDlen + strlen(file) + 1) >= MAXPATHLEN )
  { warning("path name too long");
    return (char *) NULL;
  }
  
  strcpy(path, CWDdir);
  if ( file[0] != EOS )
    strcpy(&path[CWDlen], file);
  if ( strchr(file, '.') || strchr(file, '/') )
    return canonisePath(path);
  else
    return path;
}


char *
BaseName(register char *f)
{ register char *base;

  for(base = f; *f; f++)
    if (*f == '/')
      base = f+1;

  return base;
}

char *
DirName(char *f)
{ static char dir[MAXPATHLEN];
  char *base, *p;

  for(base = p = f; *p; p++)
    if (*p == '/' && p[1] != EOS )
      base = p+1;
  strncpy(dir, f, base-f);
  dir[base-f] = EOS;
  
  return dir;
}

char *
ReadLink(char *f)
{
#ifdef HAVE_READLINK
  static char buf[MAXPATHLEN];
  int n;

  if ( (n=readlink(f, buf, sizeof(buf)-1)) > 0 )
  { buf[n] = EOS;
    return buf;
  }
#endif

  return NULL;
}


static char *
DeRefLink1(char *f)
{ char *l;

  if ( (l=ReadLink(f)) )
  { if ( l[0] == '/' )
    { return l;				/* absolute path */
    } else
    { static char buf[MAXPATHLEN];
      char *q;

      strcpy(buf, f);
      q = &buf[strlen(buf)];
      while(q>buf && q[-1] != '/')
	q--;
      strcpy(q, l);

      canoniseFileName(buf);

      return buf;
    }
  }

  return NULL;
}


char *
DeRefLink(char *link)
{ char *f;
  int n = 20;				/* avoid loop! */

  while((f=DeRefLink1(link)) && n-- > 0)
    link = f;

  return n > 0 ? link : NULL;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool ChDir(path)
	 char *path;

    Change the current working directory to `path'.  File names may depend
    on `path'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
ChDir(char *path)
{ char *ospath = OsPath(path);

  if ( path[0] == EOS ||
       streq(path, CWDdir) ||
       streq(path, ".") )		/* Same directory */
    succeed;

  if ( chdir(ospath) == 0 )
  { strcpy(CWDdir, AbsoluteFile(path));
    CWDlen = strlen(CWDdir);
    if ( CWDlen == 0 || CWDdir[CWDlen-1] != '/' )
    { CWDdir[CWDlen++] = '/';
      CWDdir[CWDlen] = EOS;
    }
    succeed;
  }

  fail;
}


		/********************************
		*        TIME CONVERSION        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    struct tm *LocalTime(time)
	      long *time;

    Convert time in Unix internal form (seconds since Jan 1 1970) into a
    structure providing easier access to the time.

    For non-Unix systems: struct time is supposed  to  look  like  this.
    Move  This  definition to pl-os.h and write the conversion functions
    here.

    struct tm {
	int	tm_sec;		/ * second in the minute (0-59)* /
	int	tm_min;		/ * minute in the hour (0-59) * /
	int	tm_hour;	/ * hour of the day (0-23) * /
	int	tm_mday;	/ * day of the month (1-31) * /
	int	tm_mon;		/ * month of the year (1-12) * /
	int	tm_year;	/ * year (0 = 1900) * /
	int	tm_wday;	/ * day in the week (1-7, 1 = sunday) * /
	int	tm_yday;	/ * day in the year (0-365) * /
	int	tm_isdst;	/ * daylight saving time info * /
    };

    long Time()

    Return time in seconds after Jan 1 1970 (Unix' time notion).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct tm *
LocalTime(long int *t)
{ return localtime((const time_t *) t);
}


long
Time(void)
{ return (long)time((time_t *) NULL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			TERMINAL IO MANIPULATION

ResetStdin()
    Clear the Sinput buffer after a saved state.  Only necessary
    if O_SAVE is defined.

PushTty()
    Push the tty to the specified state.

PopTty()
    Restore the tty state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int prompt_next = TRUE;		/* prompt on next GetChar() */
static IOFUNCTIONS org_terminal;	/* original stdio functions */

static void
ResetStdin()
{ Sinput->limitp = Sinput->bufp = Sinput->buffer;
  if ( !org_terminal.read )
    org_terminal = *Sinput->functions;
}

static PL_dispatch_hook_t PL_dispatch_events = NULL;

PL_dispatch_hook_t
PL_dispatch_hook(PL_dispatch_hook_t hook)
{ PL_dispatch_hook_t old = PL_dispatch_events;

  PL_dispatch_events = hook;
  return old;
}


static int
Swrite_protocol(void *handle, char *buf, int size)
{ int n, rval;
#ifdef HAVE_CLOCK
  long oldclock = clock();
#endif

  for(n=0; n<size; n++)
    protocol(buf[n]);

  rval = (*org_terminal.write)(handle, buf, size);

#ifdef HAVE_CLOCK
  clock_wait_ticks += clock() - oldclock;
#endif

  return rval;
}

#ifdef HAVE_LIBREADLINE

#ifdef HAVE_RL_INSERT_CLOSE
#define PAREN_MATCHING 1
extern rl_delete_text(int from, int to);
#endif

#undef ESC				/* will be redefined ... */
#include <stdio.h>			/* readline needs it */
#define savestring(x)			/* avoid definition there */
#include <readline/readline.h>
extern void add_history(char *);	/* should be in readline.h */
extern rl_begin_undo_group(void);	/* delete when conflict arrises! */
extern rl_end_undo_group(void);
extern Function *rl_event_hook;
extern char *filename_completion_function(char *, int);


static int
event_hook(void)
{ ttybuf tab;
  int rval;

  PushTty(&tab, TTY_OUTPUT);
  rval = (*PL_dispatch_events)();
  PopTty(&tab);

  return rval;
}


static Char
GetRawChar(void)
{ if ( PL_dispatch_events )
  { while((*PL_dispatch_events)() != PL_DISPATCH_INPUT)
      ;
  }

#ifdef O_RLC				/* Windows console version */
  { Char chr = getkey();

    if ( chr == 04 )
      chr = EOF;
    return chr;
  }
#else /*O_RLC*/
  { unsigned char chr;

  if (read(0, &chr, 1) == 0)
    return EOF;
  else
    return chr;
  }
#endif /*O_RLC*/
}


static int
Sread_readline(void *handle, char *buf, int size)
{ Atom sfn = source_file_name;		/* save over call-back */
  int  sln = source_line_no;
  int rval;
#ifdef HAVE_CLOCK
  long oldclock = clock();
#endif

  pl_ttyflush();

  if ( ttymode == TTY_RAW )
  { int c = GetRawChar();

    if ( c == '\n' )
      prompt_next = TRUE;

    buf[0] = c & 0xff;
    rval = 1;
  } else if ( status.notty )		/* do not use readline */
  { int n;

#ifndef __unix__
#define isatty(x) 1
#endif
    if ( prompt_next && isatty(1) )
    { extern int Output;
      int old = Output;
      Output = 1;
      Putf("%s", PrologPrompt());
      pl_flush();
      Output = old;

      prompt_next = FALSE;
    }

    n = read((int)handle, buf, size);
    if ( n > 0 && buf[n-1] == '\n' )
      prompt_next = TRUE;

    rval = n;
  } else				/* use readline */
  { ttybuf tbuf;
    char *line;

    rl_event_hook = (PL_dispatch_events ? (Function *) event_hook
					: (Function *) NULL);

    PushTty(&tbuf, TTY_SAVE);
    line = readline(PrologPrompt());
    PopTty(&tbuf);

    if ( line )
    { char *s;
      int l = strlen(line);
	  
      if ( l > size )
      { warning("Input line too long");	/* must be tested! */
	l = size-1;
      }
      memcpy(buf, line, l);
      buf[l++] = '\n';
      rval = l;

      for(s = line; *s; s++)
      { if ( !isBlank(*s) )
	{ add_history(line);
	  break;
	}
      }

      if ( !*s )			/* blanks only! */
	free(line);
    } else
      rval = 0;
  }

  if ( ttymode != TTY_RAW )
  { int n;

    for(n=0; n<rval; n++)
      protocol(buf[n]);
  }

#ifdef HAVE_CLOCK
  clock_wait_ticks += clock() - oldclock;
#endif

  source_line_no   = sln;
  source_file_name = sfn;

  return rval;
}


static void
prolog_complete(int ignore, int key)
{ if ( rl_point > 0 && rl_line_buffer[rl_point-1] != ' ' )
  { rl_begin_undo_group();
    rl_complete(ignore, key);
    if ( rl_point > 0 && rl_line_buffer[rl_point-1] == ' ' )
    {
#ifdef HAVE_RL_INSERT_CLOSE		/* actually version >= 1.2 */
      rl_delete_text(rl_point-1, rl_point);
      rl_point -= 1;
#else
      rl_delete(-1);
#endif
    }
    rl_end_undo_group();
  } else
    rl_complete(ignore, key);
}


static char **
prolog_completion(char *text, int start, int end)
{ char **matches = NULL;

  if ( (start == 1 && rl_line_buffer[0] == '[') )	/* [file */
    matches = completion_matches(text,
				 (Function *) filename_completion_function);
  else if (start == 2 && strncmp(rl_line_buffer, "['", 2))
    matches = completion_matches(text,
				 (Function *) filename_completion_function);
  else
    matches = completion_matches(text, atom_generator);

  return matches;
}


void
ResetTty(void)				/* used to initialise readline */
{ static IOFUNCTIONS funcs;

  ResetStdin();

  rl_readline_name = "Prolog";
  rl_attempted_completion_function = prolog_completion;
  rl_basic_word_break_characters = ":\t\n\"\\'`@$><= [](){}+*!";
  rl_add_defun("prolog-complete", (Function *) prolog_complete, '\t');
#if HAVE_RL_INSERT_CLOSE
  rl_add_defun("insert-close", rl_insert_close, ')');
#endif

  funcs = *Sinput->functions;		/* structure copy */
  funcs.read = Sread_readline;		/* read through readline */
  funcs.write = Swrite_protocol;	/* protocol output */

  Sinput->functions  = &funcs;
  Soutput->functions = &funcs;
  Serror->functions  = &funcs;
}

#else /*!HAVE_LIBREADLINE*/

int
Sread_terminal(void *handle, char *buf, int size)
{ Atom sfn = source_file_name;		/* save over call-back */
  int  sln = source_line_no;

  if ( prompt_next && ttymode != TTY_RAW )
  { Putf("%s", PrologPrompt());
    
    prompt_next = FALSE;
  }

  pl_ttyflush();

  if ( PL_dispatch_events )
  { for(;;)
    { if ( (*PL_dispatch_events)() == PL_DISPATCH_INPUT )
#ifdef __WATCOMC__
      { int c = (ttymode == TTY_RAW) ? getch() : getche();

	if ( c == EOF || c == '\04' )
	  size = 0;
	else
	{ buf[0] = c & 0xff;
	  size = 1;
	}
	break;
      }
#else	
      { size = (*org_terminal.read)(handle, buf, size);
	break;
      }
#endif /*__WATCOMC__*/
    }
  } else
  {
#if defined(__WATCOMC__)
    int c = (ttymode == TTY_RAW) ? getch() : getche();

    if ( c == EOF || c == '\04' )
      size = 0;
    else
    { buf[0] = c & 0xff;
      size = 1;
    }
#else
    size = (*org_terminal.read)(handle, buf, size);
#endif
  }

  if ( size == 0 )
  { if ( (int) handle == 0 )
    { Sclearerr(Sinput);
      prompt_next = TRUE;
    }
  } else if ( size > 0 && buf[size-1] == '\n' )
    prompt_next = TRUE;

  source_line_no   = sln;
  source_file_name = sfn;

  return size;
}

void
ResetTty()
{ static IOFUNCTIONS funcs;

  ResetStdin();

  funcs = *Sinput->functions;
  funcs.read = Sread_terminal;
  funcs.write = Swrite_protocol;

  Sinput->functions  = &funcs;
  Soutput->functions = &funcs;
  Serror->functions  = &funcs;

  prompt_next = TRUE;
}

#endif /*HAVE_LIBREADLINE*/

#ifdef O_TERMIO				/* sys/termios.h or sys/termio.h */

#ifndef NO_SYS_IOCTL_H_WITH_SYS_TERMIOS_H
#include <sys/ioctl.h>
#endif
#ifndef TIOCGETA
#define TIOCGETA TCGETA
#endif

bool
PushTty(ttybuf *buf, int mode)
{ struct termios tio;

  buf->mode = ttymode;
  ttymode = mode;

  if ( status.notty )
    succeed;

  if ( ioctl(0, TIOCGETA, &buf->tab) )	/* save the old one */
    fail;
  tio = buf->tab;

  switch( mode )
  { case TTY_RAW:
	tio.c_lflag &= ~(ECHO|ICANON);
	tio.c_cc[VTIME] = 0, tio.c_cc[VMIN] = 1;
	break;
    case TTY_OUTPUT:
	tio.c_oflag |= (OPOST|ONLCR);
        break;
    case TTY_SAVE:
        succeed;
    default:
	sysError("Unknown PushTty() mode: %d", mode);
	/*NOTREACHED*/
  }

#ifdef TIOCSETAW
  ioctl(0, TIOCSETAW, &tio);
#else
  ioctl(0, TCSETAW, &tio);
  ioctl(0, TCXONC, (void *)1);
#endif

  succeed;
}


bool
PopTty(ttybuf *buf)
{ ttymode = buf->mode;

  if ( status.notty )
    succeed;

#ifdef TIOCSETA
  ioctl(0, TIOCSETA, &buf->tab);
#else
  ioctl(0, TCSETA, &buf->tab);
  ioctl(0, TCXONC, (void *)1);
#endif

  succeed;
}

#else /* O_TERMIO */

bool
PushTty(buf, mode)
ttybuf *buf;
int mode;
{ buf->mode = ttymode;
  ttymode = mode;

  succeed;
}


bool
PopTty(buf)
ttybuf *buf;
{ ttymode = buf->mode;
  if ( ttymode != TTY_RAW )
    prompt_next = TRUE;

  succeed;
}

#endif /* O_TERMIO */


		/********************************
		*      ENVIRONMENT CONTROL      *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple  library  to  manipulate  the  Unix  environment.   The  modified
environment  will  be  passed  to  child  processes  and the can also be
requested via getenv/2 from Prolog.  Functions

    char *Setenv(name, value)
         char *name, *value;
	
    Set the Unix environment variable with name `name'.   If  it  exists
    its  value  is  changed, otherwise a new entry in the environment is
    created.  The return value is a pointer to the old value, or NULL if
    the variable is new.

    char *Unsetenv(name)
         char *name;

    Delete a variable from the environment.  Return  value  is  the  old
    value, or NULL if the variable did not exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if HAVE_PUTENV

char *
Setenv(char *name, char *value)
{ char *rval;
  char buf[256];

  if ( (rval = getenv(name)) )
    rval = store_string(rval);
  Ssprintf(buf, "%s=%s", name, value);
  if ( putenv(store_string(buf)) < 0 )
    warning("setenv/2: %s", OsError());

  return rval;
}

char *
Unsetenv(char *name)
{ return Setenv(name, "");
}

static void
initEnviron()
{
}

#else /*HAVE_PUTENV*/

#ifdef tos
char **environ;
#else
extern char **environ;		/* Unix predefined environment */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Grow the environment array by one and return the (possibly  moved)  base
pointer to the new environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards char	**growEnviron(char**, int);
forwards char	*matchName(char *, char *);
forwards void	setEntry(char **, char *, char *);

static char **
growEnviron(char **e, int amount)
{ static int filled;
  static int size = -1;

  if ( amount == 0 )			/* reset after a dump */
  { size = -1;
    return e;
  }

  if ( size < 0 )
  { register char **env, **e1, **e2;

    for(e1=e, filled=0; *e1; e1++, filled++)
      ;
    size = ROUND(filled+10+amount, 32);
    env = (char **)malloc(size * sizeof(char *));
    for ( e1=e, e2=env; *e1; *e2++ = *e1++ )
      ;
    *e2 = (char *) NULL;
    filled += amount;

    return env;
  }

  filled += amount;
  if ( filled + 1 > size )
  { register char **env, **e1, **e2;
  
    size += 32;
    env = (char **)realloc(e, size * sizeof(char *));
    for ( e1=e, e2=env; *e1; *e2++ = *e1++ )
      ;
    *e2 = (char *) NULL;
    
    return env;
  }

  return e;
}

static void
initEnviron(void)
{
#ifdef tos
  environ = mainEnv;
#endif
  growEnviron(environ, 0);
}


static char *
matchName(register char *e, register char *name)
{ while( *name && *e == *name )
    e++, name++;

  if ( (*e == '=' || *e == EOS) && *name == EOS )
    return (*e == '=' ? e+1 : e);

  return (char *) NULL;
}


static void
setEntry(char **e, char *name, char *value)
{ int l = (int)strlen(name);

  *e = (char *) malloc(l + strlen(value) + 2);
  strcpy(*e, name);
  e[0][l++] = '=';
  strcpy(&e[0][l], value);
}

  
char *
Setenv(char *name, char *value)
{ char **e;
  char *v;
  int n;

  for(n=0, e=environ; *e; e++, n++)
  { if ( (v=matchName(*e, name)) != NULL )
    { if ( !streq(v, value) )
        setEntry(e, name, value);
      return v;
    }
  }
  environ = growEnviron(environ, 1);
  setEntry(&environ[n], name, value);
  environ[n+1] = (char *) NULL;

  return (char *) NULL;
}


char *
Unsetenv(char *name)
{ char **e;
  char *v;
  int n;

  for(n=0, e=environ; *e; e++, n++)
  { if ( (v=matchName(*e, name)) != NULL )
    { environ = growEnviron(environ, -1);
      e = &environ[n];
      do
      { e[0] = e[1];
        e++;
      } while(*e);

      return v;
    }
  }

  return (char *) NULL;
}

#endif /*HAVE_PUTENV*/

		/********************************
		*       SYSTEM PROCESSES        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Invokation of system commands.  We could have used Unix system() library
call.  The reason I implemented  it  using  lower  level  primitives  is
twofold.   First I want to set the environment PROLOGCHILD, which allows
us to block novice users from invoking ?-  shell.   %  pl,  ....  (which
happens  often  with new students).  Second I want to close non-terminal
related I/O in the child process.

    int System(command)
	char *command;

    Invoke a command on the operating system.  The return value  is  the
    exit  status  of  the  command.   Return  value  0 implies succesful
    completion. If you are not running Unix your C-library might provide
    an alternative.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef unix
#define SPECIFIC_SYSTEM 1
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifdef UNION_WAIT
#define wait_t union wait
#else
#define wait_t int
#endif

int
System(char *cmd)
{ int pid;
  char *shell = "/bin/sh";
  int rval;
  void (*old_int)();
  void (*old_stop)();
#ifndef HAVE_LIBREADLINE
  ttybuf buf;
#endif

  Setenv("PROLOGCHILD", "yes");

#ifndef HAVE_LIBREADLINE
  PushTty(&buf, TTY_SAVE);
  PopTty(&ttytab);			/* restore cooked mode */
#endif

  if ( (pid = vfork()) == -1 )
  { return warning("Fork failed: %s\n", OsError());
  } else if ( pid == 0 )		/* The child */
  { int i;

    for(i = 3; i < getdtablesize(); i++)
      close(i);
    stopItimer();

    execl(shell, BaseName(shell), "-c", cmd, (char *)0);
    fatalError("Failed to execute %s: %s", shell, OsError());
    fail;
    /*NOTREACHED*/
  } else
  { wait_t status;			/* the parent */
    int n;

    old_int  = signal(SIGINT,  SIG_IGN);
#ifdef SIGTSTP
    old_stop = signal(SIGTSTP, SIG_DFL);
#endif /* SIGTSTP */

    while((n = Wait(&status)) != -1 && n != pid);
    if (n == -1)
    { warning("Failed to execute %s", cmd);
      rval = 1;
    } else if (WIFEXITED(status))
    { rval = WEXITSTATUS(status);
#ifdef WIFSIGNALED
    } else if (WIFSIGNALED(status))
    { warning("Child %s catched signal %d\n", cmd, WTERMSIG(status));
      rval = 1;
#endif
    } else
    { rval = 1;				/* make gcc happy */
      fatalError("Unknown return code from wait(3)");
      /*NOTREACHED*/
    }
  }

  signal(SIGINT,  old_int);		/* restore signal handlers */
#ifdef SIGTSTP
  signal(SIGTSTP, old_stop);
#endif /* SIGTSTP */
#ifndef HAVE_LIBREADLINE
  PopTty(&buf);
#endif

  return rval;
}
#endif /* unix */

#ifdef tos
#define SPECIFIC_SYSTEM 1
#include <aes.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The routine system_via_shell() has been written by Tom Demeijer.  Thanks!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define _SHELL_P ((long *)0x4f6L)
#define SHELL_OK (do_sys != 0)

int cdecl (*do_sys)(const char *cmd); /* Parameter on stack ! */

static int
system_via_shell(const char *cmd)
{ long oldssp;

  oldssp = Super((void *)0L);
  do_sys = (void (*))*_SHELL_P;
  Super((void *)oldssp);

  if(cmd==NULL && SHELL_OK)
    return 0;

  if (SHELL_OK)
    return do_sys(cmd);

  return -1;
}

int
System(command)
char *command;
{ char	   path[MAXPATHLEN];
  char	   *cmd_path;
  COMMAND  commandline;
  char	   *s, *q;
  int	   status, l;
  char	   *cmd = command;

  if ( (status = system_via_shell(command)) != -1 )
  { Sprintf("\033e");		/* get cursor back */

    return status;
  }

	/* get the name of the executable and store in path */
  for(s=path; *cmd != EOS && !isBlank(*cmd); *s++ = *cmd++)
    ;
  *s = EOS;
  if ( (cmd_path = Which(path)) == NULL )
  { warning("%s: command not found", path);
    return 1;
  }

	/* copy the command in commandline */
  while( isBlank(*cmd) )
    cmd++;

  for(l = 0, s = cmd, q = commandline.command_tail; *s && l <= 126; s++ )
  { if ( *s != '\'' )
    { *q++ = (*s == '/' ? '\\' : *s);
      l++;
    }
  }
  commandline.length = l;
  *q = EOS;
  
	/* execute the command */
  if ( (status = (int) Pexec(0, OsPath(cmd_path), &commandline, NULL)) < 0 )
  { warning("Failed to execute %s: %s", command, OsError());
    return 1;
  }

	/* clean up after a graphics application */
  if ( strpostfix(cmd_path, ".prg") || strpostfix(cmd_path, ".tos") )
  { graf_mouse(M_OFF, NULL);		/* get rid of the mouse */
    Sprintf("\033e\033E");		/* clear screen and get cursor */
  }  

  return status;
}
#endif

#ifdef HAVE_WINEXEC			/* Windows 3.1 */
#define SPECIFIC_SYSTEM 1

int
System(char *command)
{ char *msg;
  int rval = WinExec(command, SW_SHOWNORMAL);

  if ( rval < 32 )
  { switch( rval )
    { case 0:	msg = "Not enough memory"; break;
      case 2:	msg = "File not found"; break;
      case 3:	msg = "No path"; break;
      case 5:	msg = "Unknown error"; break;
      case 6:	msg = "Lib requires separate data segment"; break;
      case 8:	msg = "Not enough memory"; break;
      case 10:	msg = "Incompatible Windows version"; break;
      case 11:	msg = "Bad executable file"; break;
      case 12:	msg = "Incompatible operating system"; break;
      case 13:	msg = "MS-DOS 4.0 executable"; break;
      case 14:	msg = "Unknown executable file type"; break;
      case 15:	msg = "Real-mode application"; break;
      case 16:	msg = "Cannot start multiple copies"; break;
      case 19:	msg = "Executable is compressed"; break;
      case 20:	msg = "Invalid DLL"; break;
      case 21:	msg = "Application is 32-bits"; break;
      default:	msg = "Unknown error";
    }

    warning("Could not start %s: error %d (%s)",
	    command, rval, msg);
    return 1;
  }

  return 0;
}
#endif


#ifdef __WIN32__
#define SPECIFIC_SYSTEM 1

					/* definition in pl-nt.c */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Nothing special is needed.  Just hope the C-library defines system().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef SPECIFIC_SYSTEM

int
System(command)
char *command;
{ return system(command);
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    char *Symbols()

    Return the path name of the executable of SWI-Prolog. Used by the -c
    compiler to generate the #!<path> header line and by the incremental
    loader, who gives this path to ld, using ld -A <path>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
Symbols(void)
{ char *file;
  char buf[MAXPATHLEN];

  PrologPath(mainArgv[0], buf);
  file = Which(buf);

#if __unix__				/* argv[0] can be an #! script! */
  if ( file )
  { int n, fd;
    char buf[MAXPATHLEN];

    if ( (fd = open(file, O_RDONLY)) < 0 )
    { warning("Cannot open %s: %s", file, OsError());
      return file;
    }

    if ( (n=read(fd, buf, sizeof(buf)-1)) > 0 )
    { close(fd);

      buf[n] = EOS;
      if ( strncmp(buf, "#!", 2) == 0 )
      { char *s = &buf[2], *q;
	while(*s && isBlank(*s))
	  s++;
	for(q=s; *q && !isBlank(*q); q++)
	  ;
	*q = EOS;

	return store_string(s);
      }
    }

    close(fd);
  }
#endif /*__unix__*/

  return file;
}


#if unix
static char *
okToExec(char *s)
{ struct stat stbuff;

  if (stat(s, &stbuff) == 0 &&			/* stat it */
     (stbuff.st_mode & S_IFMT) == S_IFREG &&	/* check for file */
     access(s, X_OK) == 0)			/* can be executed? */
    return s;
  else
    return (char *) NULL;
}
#define PATHSEP	':'
#endif /* unix */

#ifdef tos
#define EXEC_EXTENSIONS { ".ttp", ".prg", NULL }
#define PATHSEP ','
#endif

#if defined(OS2) || defined(__DOS__) || defined(__WINDOWS__) || defined(__WIN32__)
#define EXEC_EXTENSIONS { ".exe", ".com", ".bat", ".cmd", NULL }
#define PATHSEP ';'
#endif

#ifdef EXEC_EXTENSIONS

static char *
okToExec(s)
char *s;
{ static char *extensions[] = EXEC_EXTENSIONS;
  static char **ext;

  DEBUG(2, Sdprintf("Checking %s\n", s));
  for(ext = extensions; *ext; ext++)
    if ( stripostfix(s, *ext) )
      return ExistsFile(s) ? s : (char *) NULL;

  for(ext = extensions; *ext; ext++)
  { static char path[MAXPATHLEN];

    strcpy(path, s);
    strcat(path, *ext);
    if ( ExistsFile(path) )
      return path;
  }

  return (char *) NULL;
}
#endif /*EXEC_EXTENSIONS*/

static char *
Which(char *program)
{ static char fullname[MAXPATHLEN];
  char *path, *dir;
  char *e;

  if ( IsAbsolutePath(program) ||
#if OS2 && EMX
       isDriveRelativePath(program) ||
#endif /* OS2 */
       isRelativePath(program) ||
       strchr(program, '/') )
  { if ( (e = okToExec(program)) != NULL )
    { strcpy(fullname, e);
      
      return fullname;
    }

    return NULL;
  }

#if OS2 && EMX
  if ((e = okToExec(program)) != NULL)
  {
    getcwd(fullname, MAXPATHLEN);
    strcat(fullname, "/");
    strcat(fullname, e);
    return fullname;
  }
#endif /* OS2 */
  if  ((path = getenv("PATH") ) == 0)
    path = DEFAULT_PATH;

  while(*path)
  { if ( *path == PATHSEP )
    { if ( (e = okToExec(program)) != NULL)
      { strcpy(fullname, e);

        return fullname;
      } else
        path++;				/* fix by Ron Hess (hess@sco.com) */
    } else
    { for(dir = fullname; *path && *path != PATHSEP; *dir++ = *path++)
	;
      if (*path)
	path++;				/* skip : */
      if (strlen(fullname) + strlen(program)+2 > MAXPATHLEN)
        continue;
      *dir++ = '/';
      *dir = EOS;
      strcpy(dir, program);
      if ( (e = okToExec(OsPath(fullname))) != NULL )
	return e;
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void Pause(time)
	 real time;

    Suspend execution `time' seconds.   Time  is  given  as  a  floating
    point,  expressing  the  time  to sleep in seconds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_SELECT

void
Pause(real time)
{ struct timeval timeout;

  if ( time <= 0.0 )
    return;

  if ( time < 60.0 )		/* select() is expensive. Does it make sense */
  { timeout.tv_sec = (int) time;
    timeout.tv_usec = (int)(time * 1000000) % 1000000;
    select(32, NULL, NULL, NULL, &timeout);
  } else
    sleep( (int)(time+0.5) );
}

#else /*!HAVE_SELECT*/
#ifdef HAVE_DOSSLEEP

void                            /* a millisecond granualrity. */
Pause(time)                     /* the EMX function sleep uses a seconds */
real time;                      /* granularity only. */
{                               /* the select() trick does not work at all. */
  if ( time <= 0.0 )
    return;

  DosSleep((ULONG)(time * 1000));
}

#else /*HAVE_DOSSLEEP*/
#ifdef HAVE_SLEEP

void
Pause(real t)
{ if ( t <= 0.5 )
    return;

  sleep((int)(t + 0.5));
}
#else /*HAVE_SLEEP*/
#ifdef HAVE_DELAY

void
Pause(real t)
{ delay((int)(t * 1000));
}

#endif /*HAVE_DELAY*/
#endif /*HAVE_SLEEP*/
#endif /*HAVE_DOSSLEEP*/
#endif /*HAVE_SELECT*/

#if tos
void
Pause(t)
real t;
{ long wait = (long)(t * 200.0);
  long start_tick = clock();
  long end_tick = wait + start_tick;

  while( clock() < end_tick )
  { if ( kbhit() )
    { wait_ticks += clock() - start_tick;
      start_tick = clock();
      TtyAddChar(getch());
    }
  }

  wait_ticks += end_tick - start_tick;
}
#endif
