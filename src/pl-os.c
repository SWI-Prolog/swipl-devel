/*  pl-os.c,v 1.23 1993/05/06 13:57:47 jan Exp

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Operating System Dependencies
*/

/*  Modified (M) 1993 Dave Sherratt  */

#if __TOS__
#include <tos.h>		/* before pl-os.h due to Fopen, ... */
static long	wait_ticks;	/* clock ticks not CPU time */
#endif
#if OS2 && EMX
#include <os2.h>                /* this has to appear before pl-incl.h */
#endif
#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-itf.h"

#if unix || EMX
#include <sys/param.h>
#include <sys/stat.h>
#include <pwd.h>
#include <sys/file.h>
#include <unistd.h>
#endif

#if sun
extern int fstat(/*int, struct stat **/);
extern int stat(/*char *, struct stat**/);
extern int unlink(/*char **/);
extern int link(/*char **/);
extern int select(/*int *, int*, int*, struct timeval **/);
extern int ioctl(/*int, int, Void*/);
extern int execl(/*char *, ... */);
extern int srandom P((long));
extern int random P((void));
#endif

#if OS2 && EMX
static real initial_time;
#endif /* OS2 */

forwards void	initExpand P((void));
forwards void	initRandom P((void));
forwards void	initEnviron P((void));
forwards char *	okToExec P((char *));
forwards char *	Which P((char *));
forwards Char	do_get_char P((void));

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
initOs()
{ DEBUG(1, printf("OS:initExpand() ...\n"));
  initExpand();
  DEBUG(1, printf("OS:initRandom() ...\n"));
  initRandom();
  DEBUG(1, printf("OS:initEnviron() ...\n"));
  initEnviron();

#if tos
  wait_ticks = clock();
#endif
#if OS2
  {
    DATETIME i;
    DosGetDateTime((PDATETIME)&i);
    initial_time = (i.hours * 3600.0) 
                   + (i.minutes * 60.0) 
		   + i.seconds
		   + (i.hundredths / 100.0);
  }
#endif /* OS2 */
  DEBUG(1, printf("OS:done\n"));

  succeed;
}


typedef void (*halt_function) P((int, Void));
typedef struct on_halt *OnHalt;

struct on_halt
{ halt_function	function;
  Void		argument;
  OnHalt	next;
};

static OnHalt on_halt_list;

void
PL_on_halt(f, arg)
halt_function f;
Void arg;
{ OnHalt h = allocHeap(sizeof(struct on_halt));

  h->function = f;
  h->argument = arg;
  h->next = on_halt_list;
  on_halt_list = h;
}


volatile void
Halt(status)
int status;
{ OnHalt h;

  for(h = on_halt_list; h; h = h->next)
    (*h->function)(status, h->argument);

  dieIO();
  RemoveTemporaryFiles();

  exit(status);
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
OsError()
{ static char errmsg[64];

#if unix
  extern int sys_nerr;
#if !EMX
  extern char *sys_errlist[];
#endif
  extern int errno;

  if ( errno < sys_nerr )
    return sys_errlist[errno];
#endif

#if tos
  if ( errno < sys_nerr )
    return strerror(errno);
#endif

  sprintf(errmsg, "Unknown Error (%d)", errno);
  return errmsg;
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

#if unix
#ifdef HZ
#  define Hz HZ
#else
#  define Hz        60
#endif
#include <sys/times.h>
#endif


real
CpuTime()
{
#if unix
  struct tms t;

  times(&t);

  return t.tms_utime / ( real )( Hz ) ;
#endif

#if OS2 && EMX
  DATETIME i;

  DosGetDateTime((PDATETIME)&i);
  return (((i.hours * 3600) 
                 + (i.minutes * 60) 
		 + i.seconds
	         + (i.hundredths / 100.0)) - initial_time);
#endif

#if tos
  return (real) (clock() - wait_ticks) / 200.0;
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
Allocate(n)
long n;
{ Void mem = Malloc(n);

  return (Void) mem;
}

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
#endif

#if hpux || tos
void
bzero(p, n)
Void p;
register size_t n;
{ char *s = p;

  while( n-- > 0 )
    *s++ = '\0';
}
#endif


		/********************************
		*             PRINT             *
		*********************************/

#if gould
char *
vsprintf(buf, fm, args)
char *buf, *fm;
va_list args;
{ FILE f;
#define BIGBUF 10000000

  f._cnt    = BIGBUF;		/* Hack oh dear hack!!! */
  f._ptr    = f._base = buf;	/* down with gould (they are in military */
  f._bufsiz = BIGBUF;		/* bussiness anyway!) */
  f._flag   = 0; 		/* was _IOLBF; */
  f._file   = '\0';
  
  DEBUG(9, printf("calling _doprnt(%s, ...)\n", fm));
  _doprnt(fm, args, &f);
  *f._ptr++ = '\0';

  return buf;
}

int
vfprintf(fd, fm, args)
FILE *fd;
char *fm;
va_list args;
{ return _doprnt(fm, args, fd);
}
#endif

		/********************************
		*     STRING MANIPULATION	*
		********************************/

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
initRandom()
{
#ifdef SRANDOM
  SRANDOM(Time());
#else
  srand((unsigned)Time());
#endif
}

long
Random()
{ 
#ifdef RANDOM
  return RANDOM();
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
TemporaryFile(id)
char *id;
{ static char temp[MAXPATHLEN];
  TempFile tf = (TempFile) allocHeap(sizeof(struct tempfile));

#if unix
  static int temp_counter = 0;
  sprintf(temp, "/tmp/pl_%s_%d_%d", id, getpid(), temp_counter++);
#endif

#if EMX
  static int temp_counter = 0;
  char *foo;

  if ( (foo = tempnam(".", (const char *)id)) )
  { strcpy(temp, foo);
    free(foo);
  } else
    sprintf(temp, "pl_%s_%d_%d", id, getpid(), temp_counter++);
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
RemoveTemporaryFiles()
{ TempFile tf, tf2;  

  for(tf = tempfiles; tf; tf = tf2)
  { DeleteFile(stringAtom(tf->name));
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

    int  GetDTableSize()

    SWI-Prolog assumes it can refer to open i/o streams via  read()  and
    write() by small integers, returned by open(). These integers should
    be  in  the range [0, ..., GetDTableSize()). If your system does not
    do this you better redefine the Open(), Read() and Write() macros so
    they  do  meet  this  requirement.   Prolog  allocates  a  table  of
    structures with GetDTableSize entries.

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

    bool DeleteFile(path)
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

int
GetDTableSize()
{
#ifdef DESCRIPTOR_TABLE_SIZE
  return DESCRIPTOR_TABLE_SIZE;
#else
#  if hpux || SGU
#    include <sys/resource.h>
     struct rlimit rlp;
     (void) getrlimit(RLIMIT_NOFILE,&rlp);
     return (rlp.rlim_cur);
#  else
     extern int getdtablesize P((void));

     return getdtablesize();
#  endif
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Size of a VM page of memory.  Most BSD machines have this function.  If not,
here are several alternatives ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if solaris
int
getpagesize()
{ return sysconf(_SC_PAGESIZE);
}
#endif


/* ********************************************************************
   Design Note -- atoenne@mpi-sb.mpg.de --

   Beware! OsPath() and PrologPath() are insecure functions.
   Make sure that you copy the result of these functions to a proper location
   before you call the functions again. Otherwise you will write over the
   former result.
   ******************************************************************** */

#if tos
char *
PrologPath(char *tospath)
{ static char path[MAXPATHLEN];
  register char *s = tospath, *p = path;

  for(; *s; s++, p++)
    *p = (*s == '\\' ? '/' : makeLower(*s));
  *p = EOS;

  return path;
}


char *
OsPath(char *unixpath)
{ static char path[MAXPATHLEN];
  register char *s = unixpath, *p = path;

  if ( isLetter(s[0]) && s[1] == ':' )		/* drive indicator */
  { *p++ = *s++;
    *p++ = *s++;
  }

  while(*s)
  { int i, dotseen;

    for(i=0, dotseen=0; *s && !IS_DIR_SEPARATOR(*s); s++)
    { if ( dotseen > 0 )		/* copy after dot */
      { if ( dotseen++ <= 3 )
          p[i++] = *s;
      } else
      { if ( *s == '.' )		/* dot; possibly backup */
	{ dotseen = 1;
	  if ( i > 8 )
	    i = 8;
	  p[i++] = '.';
	} else if ( i < 12 )
	{ p[i++] = *s;
	  if ( i == 8 )
	    p[i++] = '.';
	}
      }
    }

    p += i;
    if ( IS_DIR_SEPARATOR(*s) )
    { s++;
      *p++ = '\\';
    }
  }

  *p = EOS;

  return path;
} 
#endif

#if OS2 && EMX

/* 
   Conversion rules Prolog <-> OS/2 (using HPFS)
   / <-> \
   /x:/ <-> x:\  (embedded drive letter)
   No length restrictions up to MAXPATHLEN, no case conversions.
*/

char *
PrologPath(char *ospath)
{ static char path[MAXPATHLEN];
  register char *s = ospath, *p = path;
  register int limit = MAXPATHLEN-1;

  if (isLetter(s[0]) && s[1] == ':')
  { *p++ = '/';
    *p++ = *s++;
    *p++ = *s++;
    limit -= 3;
  }
  for(; *s && limit; s++, p++, limit--)
    *p = (*s == '\\' ? '/' : *s);
  *p = EOS;

  return path;
}


char *
OsPath(char *unixpath)
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
  *p = EOS;

  return path;
} 
#endif /* OS2 */

#if unix
char *PrologPath(p)
char *p;
{ return p;
}

char *
OsPath(p)
char *p;
{ return p;
}
#endif

long
LastModifiedFile(f)
char *f;
{
#if unix || EMX
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
  DEBUG(2, printf("%d/%d/%d %d:%d:%d\n",
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


bool
ExistsFile(path)
char *path;
{
#if unix || EMX
  struct stat buf;

  if ( stat(OsPath(path), &buf) == -1 || (buf.st_mode & S_IFMT) != S_IFREG )
    fail;
  succeed;
#endif

#if tos
  struct ffblk buf;

  if ( findfirst(OsPath(path), &buf, FA_HIDDEN) == 0 )
  { DEBUG(2, printf("%s (%s) exists\n", path, OsPath(path)));
    succeed;
  }
  DEBUG(2, printf("%s (%s) does not exist\n", path, OsPath(path)));
  fail;
#endif
}

bool
AccessFile(path, mode)
char *path;
int mode;
{
#if unix || EMX
  int m = 0;

  if ( mode & ACCESS_READ    ) m |= R_OK;
  if ( mode & ACCESS_WRITE   ) m |= W_OK;
  if ( mode & ACCESS_EXECUTE ) m |= X_OK;

  return access(OsPath(path), m) == 0 ? TRUE : FALSE;
#endif

#if tos
  struct ffblk buf;

  if ( findfirst(OsPath(path), &buf, FA_DIREC|FA_HIDDEN) != 0 )
    fail;			/* does not exists */
  if ( (mode & ACCESS_WRITE) && (buf.ff_attrib & FA_RDONLY) )
    fail;			/* readonly file */

  succeed;
#endif
}

bool
ExistsDirectory(path)
char *path;
{
#if unix || EMX
  struct stat buf;

  if ( stat(OsPath(path), &buf) == -1 || (buf.st_mode & S_IFMT) != S_IFDIR )
    fail;
  succeed;
#endif

#if tos
  struct ffblk buf;

  if ( findfirst(OsPath(path), &buf, FA_DIREC|FA_HIDDEN) == 0 &&
       buf.ff_attrib & FA_DIREC )
    succeed;
  if ( streq(path, ".") || streq(path, "..") )	/* hack */
    succeed;
  fail;
#endif
}


long
SizeFile(path)
char *path;
{ struct stat buf;
  if ( stat(OsPath(path), &buf) == -1 )
    return -1;

  return buf.st_size;
}


bool
DeleteFile(path)
char *path;
{
#if unix || EMX
  return unlink(OsPath(path)) == 0 ? TRUE : FALSE;
#endif

#if tos
  return remove(OsPath(path)) == 0 ? TRUE : FALSE;
#endif
}


bool
RenameFile(old, new)
char *old, *new;
{
  char os_old[MAXPATHLEN];
  char os_new[MAXPATHLEN];
#if unix
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
#endif

#if tos || EMX
  return rename(os_old, os_new) == 0 ? TRUE : FALSE;
#endif
}


bool
SameFile(f1, f2)
char *f1, *f2;
{ if ( streq(f1, f2) == FALSE )
  { 
#if unix
    struct stat buf1;
    struct stat buf2;

    if ( stat(OsPath(f1), &buf1) != 0 || stat(OsPath(f2), &buf2) != 0 )
      fail;
    if ( buf1.st_ino == buf2.st_ino && buf1.st_dev == buf2.st_dev )
      succeed;
#endif
#if OS2 && EMX
    /* Amazing! There is no simple way to check two files for identity. */
    /* stat() and fstat() both return dummy values for inode and device. */
#endif /* OS2 */

    fail;
  }

  succeed;
}


bool
OpenStream(fd)
int fd;
{
#if unix || EMX
  struct stat buf;

  return fstat(fd, &buf) == 0 ? TRUE : FALSE;
#endif

#if tos
  return fd < 3 ? TRUE : FALSE;	/* stdin, stdout and stderr are open */
#endif
}


bool
MarkExecutable(name)
char *name;
{
#if unix
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

  succeed;
#endif

#if tos || OS2
  succeed;		/* determined by extension */
#endif
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

#if unix
typedef struct canonical_dir *CanonicalDir;

static struct canonical_dir
{ char *	name;			/* name of directory */
  char *	canonical;		/* canonical name of directory */
  dev_t		device;			/* device number */
  ino_t		inode;			/* inode number */
  CanonicalDir  next;			/* next in chain */
} *canonical_dirlist = NULL;            /* initialization -- atoenne -- */

forwards char	*canonisePath P((char *)); /* canonise a path-name */
forwards char   *canoniseDir P((char *));
#endif

static  char    CWDdir[MAXPATHLEN];	   /* current directory */


static void
initExpand()
{ 
#if unix
  char *dir;
  char *cpaths;
#endif

  CWDdir[0] = EOS;

#if unix
  if ( (cpaths = getenv("CANONICAL_PATHS")) )
  { char buf[MAXPATHLEN];

    while(*cpaths)
    { char *e;

      if ( (e = index(cpaths, ':')) )
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

#if unix

static char *
canoniseDir(path)
char *path;
{ CanonicalDir d;
  struct stat buf;

  DEBUG(1, printf("canoniseDir(%s) --> ", path); fflush(stdout));

  for(d = canonical_dirlist; d; d = d->next)
  { if ( streq(d->name, path) )
    { if ( d->name != d->canonical )
	strcpy(path, d->canonical);

      DEBUG(1, printf("(lookup) %s\n", path));
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

      for(d = canonical_dirlist; d; d = d->next)
      { if ( d->inode == buf.st_ino && d->device == buf.st_dev )
	{ canonical_dirlist = dn;

	  strcpy(dirname, d->canonical);
	  strcat(dirname, e);
	  strcpy(path, dirname);
	  dn->canonical = store_string(path);
	  DEBUG(1, printf("(replace) %s\n", path));
	  return path;
	}
      }

      for(e--; *e != '/' && e > path + 1; e-- )
	;

    } while( e > path );

    dn->canonical = dn->name;
    canonical_dirlist = dn;

    DEBUG(1, printf("(new, existing) %s\n", path));
    return path;
  }

  DEBUG(1, printf("(nonexisting) %s\n", path));
  return path;
}

#else

#define canoniseDir(d)

#endif


static char *
canonisePath(path)
register char *path;
{ register char *out = path;
  char *osave[100];
  int  osavep = 0;
  char *bsave = out;

  while( path[0] == '/' && path[1] == '.' &&
	 path[2] == '.' && path[3] == '/')
    path += 3;

  while(*path)
  { if (*path == '/')
    { while(path[1] == '/')
	path++;
      while (path[1] == '.' && path[2] == '/')
	path += 2;
      while (path[1] == '.' && path[2] == '.' && path[3] == '/')
      { out = osave[--osavep];
	path += 3;
      }
      osave[osavep++] = out;
      *out++ = *path++;
    } else
      *out++ = *path++;
  }
  *out++ = *path++;

#if unix
{ char *e;
  char dirname[MAXPATHLEN];

  e = bsave + strlen(bsave) - 1;
  for( ; *e != '/' && e > bsave; e-- )
    ;
  strncpy(dirname, bsave, e-bsave);
  dirname[e-bsave] = EOS;
  canoniseDir(dirname);
  strcat(dirname, e);
  strcpy(bsave, dirname);
}
#endif

  return bsave;
}

#include <ctype.h>

forwards char	*takeWord P((char **));
forwards int	ExpandFile P((char *, char **));

static char *
takeWord(string)
char **string;
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
expandVars(pattern, expanded)
char *pattern, *expanded;
{ int size = 0;
  char c;

  if ( *pattern == '~' )
  {
#if unix
    static char fred[20];
    static char fredLogin[MAXPATHLEN];
    extern struct passwd *getpwnam();
#endif
    char *user;
    char *value;
    int l;

    pattern++;
    user = takeWord(&pattern);

#if unix
    if ( user[0] != EOS || (value = getenv("HOME")) == (char *) NULL )
    { struct passwd *pwent;

      if ( !streq(fred, user) )
      { if ( (pwent = getpwnam(user)) == (struct passwd *) NULL )
	  return warning("%s: Unknown user");
	strcpy(fred, user);
	strcpy(fredLogin, pwent->pw_dir);
      }
      value = fredLogin;
    }	  
#endif
#if tos || OS2
    if ( user[0] != EOS || (value = getenv("HOME")) == (char *) NULL )
    { value = "/";	/* top directory of current drive */
    } else
    { value = PrologPath(value);
    }
#endif
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
	    return warning("%s: Undefined variable", var);
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
  *expanded++ = EOS;

  succeed;
}


static int
ExpandFile(pattern, vector)
char *pattern;
char **vector;
{ static char expanded[MAXPATHLEN];
  int matches = 0;

  if ( expandVars(pattern, expanded) == FALSE )
    return -1;
  
  vector[matches++] = expanded;

  return matches;
}


char *
ExpandOneFile(spec)
char *spec;
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


#if unix
#if O_GETCWD || SGU
char	*getcwd P((char *, size_t));

char *
getwd(buf)
char *buf;
{ return getcwd(buf, MAXPATHLEN);
}
#else
extern char *getwd P((char *));
#endif
#endif

#if OS2 && EMX
char *getwd P((char *));

char *getwd(buf)
char *buf;
{ strcpy(buf, PrologPath(_getcwd2(buf, MAXPATHLEN)));
  return buf;
}
#endif /* OS2 */


#if tos
char	*getwd P((char *));

char *
getwd(buf)
char *buf;
{ char path[MAXPATHLEN];

  if ( Dgetpath(path, 0) != 0 )
  { warning("Can't get current directory: %s", OsError());
    strcpy(path, "");
  }
  sprintf(buf, "%c:%s", Dgetdrv()+'a', PrologPath(path));

  return buf;
}
#endif

#if unix
#define isAbsolutePath(p) ( p[0] == '/' )
#define isRelativePath(p) ( p[0] == '.' )
#endif
#if tos
#define isAbsolutePath(p) ( isLetter(p[0]) && p[1] == ':' )
#define isRelativePath(p) ( p[0] == '.' || p[0] == '/' || p[0] == '\\' )
#endif
#if OS2
#define isAbsolutePath(p) (p[0] == '/' && isLetter(p[1]) && \
			   p[2] == ':' && p[3] == '/' )
#define isDriveRelativePath(p) (p[0] == '/' && p[2] != ':')
#define isRootlessPath(p) (p[0] == '/' && isLetter(p[1]) && \
			   p[2] == ':' && p[3] != '/')
#define isRelativePath(p) (p[0] == '.')
#endif /* OS2 */

/*
  Design Note -- atoenne --
  AbsoluteFile may only be called with a proper PrologPath. Otherwise the
  canonisePath will not work.
*/

char *
AbsoluteFile(spec)
char *spec;
{ static char path[MAXPATHLEN];
  char *file;  

  if ( (file = ExpandOneFile(spec)) == (char *) NULL )
    return (char *) NULL;

  if ( isAbsolutePath(file) )
  { strcpy(path, file);

    return canonisePath(path);
  }
#if OS2 && EMX
  if (isDriveRelativePath(file))
  {
    if ((strlen(file) + 4) > MAXPATHLEN)
    {
      warning("path name too long");
      return (char *) NULL;
    }
    path[0] = '/';
    path[1] = (char) _getdrive();
    path[2] = ':';
    strcpy(&path[3], file);
    return canonisePath(path);
  }
#endif /* OS2 */
  if ( CWDdir[0] == EOS )
  {
    getwd(CWDdir);
  }

#if OS2 && EMX
  if (isRootlessPath(file))
  {
    if ((strlen(CWDdir) + strlen(file) - 2) >= MAXPATHLEN)
    {
      warning("path name too long");
      return (char *) NULL;
    }
    strcpy(path, CWDdir);
    strcat(path, "/");
    strcat(path, &file[3]);
    return canonisePath(path);
  }
#endif /* OS2 */
  if ( (strlen(CWDdir) + strlen(file) + 2) >= MAXPATHLEN )
  { warning("path name too long");
    return (char *) NULL;
  }
  
  strcpy(path, CWDdir);
  strcat(path, "/");
  strcat(path, file);

  return canonisePath(path);
}


char *
BaseName(f)
register char *f;
{ register char *base;

  for(base = f; *f; f++)
    if (*f == '/')
      base = f+1;

  return base;
}

char *
DirName(f)
char *f;
{ static char dir[MAXPATHLEN];
  char *base, *p;

  for(base = p = f; *p; p++)
    if (*p == '/' && p[1] != EOS )
      base = p;
  strncpy(dir, f, base-f);
  dir[base-f] = EOS;
  
  return dir;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool ChDir(path)
	 char *path;

    Change the current working directory to `path'.  File names may depend
    on `path'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
ChDir(path)
char *path;
{ extern int chdir(/*char**/);
  char *ospath = OsPath(path);

  if ( ospath[0] == EOS ||
       streq(ospath, CWDdir) ||
       streq(ospath, ".") )		/* Same directory */
    succeed;

  if ( chdir(ospath) == 0 )
  { CWDdir[0] = EOS;
    succeed;
  }

  fail;
}

#if minix || tos
long
getw(fd)
register FILE *fd;
{ register ulong r;

  r = getc(fd);
  r = (r << 8) | getc(fd);
  r = (r << 8) | getc(fd);
  r = (r << 8) | getc(fd);

  return r;
}

long
putw(l, fd)
long l;
FILE *fd;
{ putc((char)((l >> 24) & 0xff), fd);
  putc((char)((l >> 16) & 0xff), fd);
  putc((char)((l >>  8) & 0xff), fd);
  putc((char)((l)       & 0xff), fd);

  return l;
}
#endif /* minix || tos */

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
LocalTime(t)
long *t;
{ extern struct tm *localtime();

  return localtime(t);
}

long
Time()
{
#if !EMX
extern long time();
#endif

  return (long)time((time_t *) NULL);
}

#if tos
void
gettimeofday(tz, p)
struct timeval *tz;
void *p;
{ tz->tv_usec = 0;
  tz->tv_sec  = Time();
}
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			TERMINAL IO MANIPULATION

ResetStdin()
    Clear the stdin buffer after a saved state.  Only necessary
    if O_SAVE is defined.

GetChar()
    Read a character from the terminal.

PushTty()
    Push the tty to the specified state.

PopTty()
    Restore the tty state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_SAVE && !O_SAVE_STDIO
static void
ResetStdin()
{
#ifdef RESET_STDIN
  RESET_STDIN;
#else
#if unix
#if linux				/* actually gcc libc (4.3.3) */
  stdin->_gptr = stdin->_egptr;
#else
  stdin->_ptr = stdin->_base;
  stdin->_cnt = 0;
#endif
#endif
#if EMX
  stdin->ptr = stdin->buffer;
  stdin->rcount = 0;
  stdin->wcount = 0;
#endif
  clearerr(stdin);
#endif
}
#else
#define ResetStdin()
#endif /*O_SAVE*/

int (*PL_dispatch_events)() = NULL;	/* event-dIspatching */

#if O_READLINE

#undef ESC				/* will be redefined ... */
#include <readline/readline.h>
#undef savestring

static int
event_hook()
{ ttybuf tab;
  int rval;

  PushTty(&tab, TTY_OUTPUT);
  rval = (*PL_dispatch_events)();
  PopTty(&tab);

  return rval;
}


static char *
xmalloc(size)
int size;
{ char *result = malloc(size);

  if ( !result )
    fatalError("Not enough core");

  return result;
}

#define savestring(x) strcpy(xmalloc(1 + strlen(x)), (x))

Char
GetChar()
{ static char *line;			/* read line */
  static char *line_p;			/* pointer in it */
  Atom sfn = source_file_name;		/* save over call-back */
  int  sln = source_line_no;
  Char c;

  if ( ttymode == TTY_RAW )
  { if ( PL_dispatch_events )
    { for(;;)
      { if ( (*PL_dispatch_events)() == PL_DISPATCH_INPUT )
	{ char chr;

	  if (read(0, &chr, 1) == 0)
	    c = EOF;
	  else
	    c = (Char) chr;
	  break;
	}
      }
    } else
    { char chr;				/* don't use getchar(); I/O buffer */
					/* might not be empty after save() */
      c = (read(0, &chr, 1) == 0 ? EOF : chr);
    }
  } else
  { if ( !line )
    { ttybuf buf;
      rl_event_hook = (PL_dispatch_events ? (Function *) event_hook
					  : (Function *) NULL);

      PushTty(&buf, TTY_SAVE);
      line = readline(PrologPrompt());
      PopTty(&buf);

      if ( !line )
      { return EOF;
      } else
      { char *s;
	  
	for(s = line; *s; s++)
	{ if ( !isBlank(*s) )
	  { add_history(savestring(line));
	    break;
	  }
	}

	line_p = line;
      }
    }

    if ( *line_p )
    { c = *line_p++;
    } else
    { if ( line )
	free(line);
      line = NULL;
      c = '\n';
    }
  }

  source_line_no   = sln;
  source_file_name = sfn;
  return c;
}


static void
prolog_complete(ignore, key)
int ignore;
int key;
{ if ( rl_point > 0 && rl_line_buffer[rl_point-1] != ' ' )
  { rl_begin_undo_group();
    rl_complete(ignore, key);
    if ( rl_point > 0 && rl_line_buffer[rl_point-1] == ' ' )
      rl_delete(-1);
    rl_end_undo_group();
  } else
    rl_complete(ignore, key);
}


static char **
prolog_completion(text, start, end)
char *text;
int start, end;
{ extern char *filename_completion_function P((char *, int));
  char **matches = NULL;

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
ResetTty()				/* used to initialise readline */
{ ResetStdin();

  rl_readline_name = "Prolog";
  rl_attempted_completion_function = (Function *)prolog_completion;
  rl_basic_word_break_characters = "\t\n\"\\'`@$><= [](){}+*!";
  rl_add_defun("prolog-complete", prolog_complete, '\t');
}

#else /*!O_READLINE*/

static int prompt_next;

Char
GetChar()
{ Char c;
  Atom sfn = source_file_name;		/* save over call-back */
  int  sln = source_line_no;

  if ( prompt_next )
  { Putf("%s", PrologPrompt());
    pl_ttyflush();
    
    prompt_next = FALSE;
  }

  if ( PL_dispatch_events )
  { for(;;)
    { if ( (*PL_dispatch_events)() == PL_DISPATCH_INPUT )
      { char chr;
	
	if (read(0, &chr, 1) == 0)
	  c = EOF;
	else
	  c = (Char) chr;
	break;
      }
    }
  } else
    c = getchar();

  if ( c == '\n' )
    prompt_next = TRUE;

  source_line_no   = sln;
  source_file_name = sfn;

  return c;
}

void
ResetTty()
{ ResetStdin();

  prompt_next = TRUE;
}

#endif /*O_READLINE*/

#if O_TERMIOS				/* System V/POSIX termio system */

bool
PushTty(buf, mode)
ttybuf *buf;
int mode;
{ struct termio tio;

  if ( status.notty )
    succeed;

  if ( ioctl(0, TCGETA, &buf->tab) )	/* save the old one */
    fail;
  tio = buf->tab;
  buf->mode = ttymode;
  ttymode = mode;

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

  ioctl(0, TCSETAW, &tio);
  ioctl(0, TCXONC, 1);

  succeed;
}


bool
PopTty(buf)
ttybuf *buf;
{
  if ( status.notty )
    succeed;

  ioctl(0, TCSETA, &buf->tab);
  ioctl(0, TCXONC, 1);
  ttymode = buf->mode;

  succeed;
}

#else					/* Use Readline's functions */

bool
PushTty(buf, mode)
ttybuf *buf;
int mode;
{ fail;
}


bool
PopTty(buf)
ttybuf *buf;
{ fail;
}

#endif /* O_TERMIOS */


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

#if tos
char **environ;
#else
extern char **environ;		/* Unix predefined environment */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Grow the environment array by one and return the (possibly  moved)  base
pointer to the new environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards char	**growEnviron P((char**, int));
forwards char	*matchName P((char *, char *));
forwards void	setEntry P((char **, char *, char *));

static char **
growEnviron(e, amount)
char **e;
int amount;
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
initEnviron()
{
#if tos
  environ = mainEnv;
#endif
  growEnviron(environ, 0);
}


static char *
matchName(e, name)
register char *e, *name;
{ while( *name && *e == *name )
    e++, name++;

  if ( (*e == '=' || *e == EOS) && *name == EOS )
    return (*e == '=' ? e+1 : e);

  return (char *) NULL;
}


static void
setEntry(e, name, value)      
char **e;
char *name, *value;
{ int l = (int)strlen(name);

  *e = (char *) malloc(l + strlen(value) + 2);
  strcpy(*e, name);
  e[0][l++] = '=';
  strcpy(&e[0][l], value);
}

  
char *
Setenv(name, value)
char *name, *value;
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
Unsetenv(name)
char *name;
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

#if unix
#if !v7
#include <sys/wait.h>
#endif

int
System(cmd)
char *cmd;
{ int pid;
  char *shell;
  ttybuf buf;
  int rval;
  void (*old_int)();
  void (*old_stop)();

  Setenv("PROLOGCHILD", "yes");

/*if ((shell = getenv("SHELL")) == (char *)NULL) bourne shell for speed now */
    shell = "/bin/sh";

#if !O_READLINE
  PushTty(&buf, TTY_SAVE);
  PopTty(&ttytab);			/* restore cooked mode */
#endif

  if ( (pid = vfork()) == -1 )
  { return warning("Fork failed: %s\n", OsError());
  } else if ( pid == 0 )		/* The child */
  { int i;

    for(i = 3; i < GetDTableSize(); i++)
      close(i);
    stopItimer();

    execl(shell, BaseName(shell), "-c", cmd, (char *)0);
    fatalError("Failed to execute %s: %s", shell, OsError());
    fail;
    /*NOTREACHED*/
  } else
#if v7 || hpux || SGU
  { int waitstat, retstat;		/* the parent */

    old_int  = signal(SIGINT,  SIG_IGN);
#ifdef SIGTSTP
    old_stop = signal(SIGTSTP, SIG_DFL);
#endif /* SIGTSTP */

    while( (waitstat = Wait(&retstat)) != pid && waitstat != -1 )
      ;
    if ( waitstat == -1 )
    {  warning("Failed to execute %s", cmd);
       rval = 1;
    }

    rval = retstat;
  }
#else /* v7 */
  { union wait status;			/* the parent */
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
    { rval = status.w_retcode;
    } else if (WIFSIGNALED(status))
    { warning("Child %s catched signal %d\n", cmd, status.w_termsig);
      rval = 1;
    } else
    { rval = 1;				/* make gcc happy */
      fatalError("Unknown return code from wait(3)");
      /*NOTREACHED*/
    }
  }
#endif /* v7 */

  signal(SIGINT,  old_int);		/* restore signal handlers */
#ifdef SIGTSTP
  signal(SIGTSTP, old_stop);
#endif /* SIGTSTP */
#if !O_READLINE
  PopTty(&buf);
#endif

  return rval;
}
#endif /* unix */

#if tos
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
  { printf("\033e");		/* get cursor back */

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
    printf("\033e\033E");		/* clear screen and get cursor */
  }  

  return status;
}
#endif

/* OS/2 does not have the same problems as Unix(tm). We simply fire up a
   shell that does the job.
*/

#if OS2 && EMX
int
System(command)
char *command;
{
  return system(command);
}
#endif /* OS2 */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    char *Symbols()

    Return the path name of the executable of SWI-Prolog. Used by the -c
    compiler to generate the #!<path> header line and by the incremental
    loader, who gives this path to ld, using ld -A <path>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if unix || EMX
char *
Symbols()
{ return Which(PrologPath(mainArgv[0]));
}
#endif

#if tos
char *
Symbols()
{ return "pl1.5";
}
#endif

#if unix
static char *
okToExec(s)
char *s;
{ struct stat stbuff;
#if sun
  extern int access(/*char *, int*/);
#endif

  if (stat(s, &stbuff) == 0 &&			/* stat it */
     (stbuff.st_mode & S_IFMT) == S_IFREG &&	/* check for file */
     access(s, X_OK) == 0)			/* can be executed? */
    return s;
  else
    return (char *) NULL;
}

#define PATHSEP	':'
#endif /* unix */

#if tos
static char *
okToExec(s)
char *s;
{ static char path[MAXPATHLEN];

  DEBUG(2, printf("Checking %s\n", s));
  if ( strpostfix(s, ".ttp" ) || strpostfix(s, ".prg") )
    return ExistsFile(s) ? s : (char *) NULL;

  strcpy(path, s);
  strcat(path, ".ttp");
  DEBUG(2, printf("Checking %s\n", path));
  if ( ExistsFile(path) == TRUE )
    return path;
  strcpy(path, s);
  strcat(path, ".prg");
  DEBUG(2, printf("Checking %s\n", path));
  if ( ExistsFile(path) == TRUE )
    return path;
  return (char *) NULL;
}

#define PATHSEP ','
#endif /* tos */

#if OS2 && EMX
static char *
okToExec(s)
char *s;
{ static char path[MAXPATHLEN];

  DEBUG(2, printf("Checking %s\n", s));
  if (strpostfix(s, ".exe") ||
      strpostfix(s, ".com") ||
      strpostfix(s, ".bat") ||
      strpostfix(s, ".cmd"))
    return ExistsFile(s) ? s : (char *) NULL;

  strcpy(path, s);
  strcat(path, ".exe");
  DEBUG(2, printf("Checking %s\n", path));
  if ( ExistsFile(path) == TRUE )
    return path;
  strcpy(path, s);
  strcat(path, ".com");
  DEBUG(2, printf("Checking %s\n", path));
  if ( ExistsFile(path) == TRUE )
    return path;
  strcpy(path, s);
  strcat(path, ".cmd");
  DEBUG(2, printf("Checking %s\n", path));
  if ( ExistsFile(path) == TRUE )
    return path;
  strcpy(path, s);
  strcat(path, ".bat");
  DEBUG(2, printf("Checking %s\n", path));
  if ( ExistsFile(path) == TRUE )
    return path;
  return (char *) NULL;
}

#define PATHSEP ';'
#endif /* OS2 */

static char *
Which(program)
char *program;
{ static char fullname[MAXPATHLEN];
  char *path, *dir;
  char *e;

  if ( isAbsolutePath(program) ||
#if OS2 && EMX
       isDriveRelativePath(program) ||
#endif /* OS2 */
       isRelativePath(program) ||
       index(program, '/') )
  { if ( (e = okToExec(program)) != NULL )
    { strcpy(fullname, e);
      
      return fullname;
    }

    return NULL;
  }

#if OS2 && EMX
  if ((e = okToExec(program)) != NULL)
  {
    getwd(fullname);
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
    void Sleep(time)
	 real time;

    Suspend execution `time' seconds.   Time  is  given  as  a  floating
    point,  expressing  the  time  to sleep in seconds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if unix
#if O_NOSELECT
void
Sleep(time)
real time;
{ if ( time < 0.5 )
    return;
  sleep( (int)(time+0.5) );
}

#else

void
Sleep(time)			/* system has select() */
real time;
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
#endif /* has select() */
#endif /* unix */

#if OS2 && EMX                  /* the OS/2 API call for DosSleep allows */
void                            /* a millisecond granualrity. */
Sleep(time)                     /* the EMX function sleep uses a seconds */
real time;                      /* granularity only. */
{                               /* the select() trick does not work at all. */
  if ( time <= 0.0 )
    return;

  DosSleep((ULONG)(time * 1000));
}
#endif /* OS2 */

#if tos
void
Sleep(t)
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
