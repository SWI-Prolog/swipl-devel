/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Operating System Dependencies
*/

#if __TOS__
#include <tos.h>		/* before pl-os.h due to Fopen, ... */
static long	wait_ticks;	/* clock ticks not CPU time */
#endif
#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-itf.h"

#if unix
#include <sys/param.h>
#include <sys/stat.h>
#include <pwd.h>
#include <sys/file.h>
#include <unistd.h>

#if sun
extern int fstat(/*int, struct stat **/);
extern int stat(/*char *, struct stat**/);
extern int unlink(/*char **/);
extern int link(/*char **/);
extern int select(/*int *, int*, int*, struct timeval **/);
extern int ioctl(/*int, int, Void*/);
extern int execl(/*char *, ... */);
#endif
#endif unix

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
  extern char *sys_errlist[];
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
#define Hz	(60.0)
#include <sys/times.h>
#endif

real
CpuTime()
{
#if unix
  struct tms t;

  times(&t);

  return t.tms_utime / Hz;
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

  SECURE(assert(malloc_verify() == 1));

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
#if hpux || minix || tos || LINUX
#if sun
  extern int srand();
#endif
  srand((unsigned)Time());
#else
  extern int srandom();
  long seed = Time();
  srandom(seed);
#endif
}

long
Random()
{ 
#if hpux || minix || tos || LINUX
  extern int rand();
  return rand();
#else
  extern int random();
  return random();
#endif hpux
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
    they  do  meat  this  requirement.   Prolog  allocates  a  table  of
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
#  if hpux
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

#define isDirSep(c) ( (c) == '/' || (c) == '\\' )

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

    for(i=0, dotseen=0; *s && !isDirSep(*s); s++)
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
    if ( isDirSep(*s) )
    { s++;
      *p++ = '\\';
    }
  }

  *p = EOS;

  return path;
} 
#endif

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
#if unix
  struct stat buf;

  if ( stat(f, &buf) < 0 )
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
#if unix
  struct stat buf;

  if ( stat(path, &buf) == -1 || (buf.st_mode & S_IFMT) != S_IFREG )
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
#if unix
  int m = 0;

  if ( mode & ACCESS_READ    ) m |= R_OK;
  if ( mode & ACCESS_WRITE   ) m |= W_OK;
  if ( mode & ACCESS_EXECUTE ) m |= X_OK;

  return access(path, m) == 0 ? TRUE : FALSE;
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
#if unix
  struct stat buf;

  if ( stat(path, &buf) == -1 || (buf.st_mode & S_IFMT) != S_IFDIR )
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
  if ( stat(path, &buf) == -1 )
    return -1;

  return buf.st_size;
}


bool
DeleteFile(path)
char *path;
{
#if unix
  return unlink(path) == 0 ? TRUE : FALSE;
#endif
#if tos
  return remove(OsPath(path)) == 0 ? TRUE : FALSE;
#endif
}


bool
RenameFile(old, new)
char *old, *new;
{
#if unix
  int rval;

  unlink(new);
  if ((rval = link(old, new)) == 0 && (rval = unlink(old)) != 0)
    unlink(new);

  if (rval == 0)
    succeed;

  fail;
#endif
#if tos
  return rename(OsPath(old), OsPath(new)) == 0 ? TRUE : FALSE;
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

    if ( stat(f1, &buf1) != 0 || stat(f2, &buf2) != 0 )
      fail;
    if ( buf1.st_ino == buf2.st_ino && buf1.st_dev == buf2.st_dev )
      succeed;
#endif

    fail;
  }

  succeed;
}


bool
OpenStream(fd)
int fd;
{
#if unix
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
#if tos
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
} *canonical_dirlist;

forwards char	*canonisePath P((char *)); /* canonise a path-name */
forwards char   *canoniseDir P((char *));
#endif

static  char    CWDdir[MAXPATHLEN];	   /* current directory */


static void
initExpand()
{ char *dir;
  char *cpaths;

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

  if ( stat(path, &buf) == 0 )
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
      if ( stat(dirname, &buf) < 0 )
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
#if tos
    if ( user[0] != EOS || (value = getenv("HOME")) == (char *) NULL )
    { value = "/";	/* top directory of current drive */
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
#if hpux || LINUX
char	*getwd P((char *));

char *
getwd(buf)
char *buf;
{ extern char *getcwd();

  return getcwd(buf, MAXPATHLEN);
}
#else
extern char *getwd P((char *));
#endif hpux
#endif

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

  if ( CWDdir[0] == EOS )
    getwd(CWDdir);

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

  if ( streq(ospath, CWDdir) ||
       streq(ospath, ".") )		/* Same directory */
    succeed;

  if ( chdir(OsPath(path)) == 0 )
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
#endif minix || tos

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
{ extern long time();

  return time((time_t *) NULL);
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

		/********************************
		*        TERMINAL CONTROL       *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Terminal control probably is the biggest mess.  One day (v7) things used
to be simple.  New features made a mess of it.  System V then defined  a
much  more powerful terminal interface using a single control structure.
Unfortunately at this moment every Unix system seems  to  have  its  own
driver:  v7  drivers, various extended versions of this and the System V
drivers.

SWI-Prolog uses the following terminal modes:

  - If it wants to know what action to perform when in trace mode or has
    trapped a signal (^C). In this  case  it  wants  the  input  without
    waiting for a return and without echoing.

  - If it is reading a Prolog term from the terminal.  In this  case  it
    wants  to  trap ESC and EOF without waiting for a return in order to
    do `atom-completion': finishing atoms after a unique prefix has been
    typed by the user.  This mode askes for some sub-modes to let Prolog
    act as if the characters are typed by the user.

  - Prolog assumes the terminal is initialy in  `COOKED'  mode:  the  OS
    reads  the characters from the terminal and allows for line editing.
    The line is passed as a whole if the user hits return.

As we usualy want to go back to the previous mode two  functions  should
be provided by this layer of the terminal driver:

    void PushTty(buf, mode)
	 ttybuf *buf;
	 int mode;

    Save the current settings in `buf' and switch to mode  `mode'.   The
    type ttybuf should be defined in pl-os.h. Modes:

	 TTY_SAVE		Only save current setting: do not change
	 TTY_RAW		Non-Echoing, not waiting for return
	 TTY_COOKED		Initial mode (asumes echo)
	 TTY_EXTEND_ATOMS	Tty flushes on ESC, EOF and NL. Used by
				read/1.
	 TTY_RETYPE		Push back characters without showing them
				on the terminal.
	 TTY_APPEND		Push back characters, showing them on the
				terminal.

    The  last  three  serve  for  the  atom-completion.   This  is  only
    interesting  if  you  can  provide a function to fake input from the
    user by the program.  if this cannot be done you may wish to include
    the O_LINE_EDIT option, which  makes  Prolog  defines  it's  own  line
    editing cababilities.

    void PopTty(buf)
	 ttybuf *buf;

    Restore the terminal into the mode saved in buf by a PushTty() call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_LINE_EDIT
#define CRLF		(0x0001)	/* map CR -> LF */
#define QSIZE		(256)
#define Empty(d)	( (d)->in == (d)->out )
#define Advance(p)	{ if ( ++(p) == QSIZE ) (p) = 0; }
#define Retreat(p)	{ if ( --(p) < 0 ) (p) = QSIZE-1; }

struct tty_driver
{ char	werase;		/* word erase character */
  char  kill;		/* kill character */
  char	erase;		/* erase character */
  char	erase2;		/* alternative erase character */
  char	eol;		/* alternative end-of-line */
  char	eol2;		/* 2nd alternative end-of-line */
  char	reprint;	/* reprint input */
  char	intr;		/* interrupt */
  int	mode;		/* mode of the driver */
  int	emitting;	/* Lines available */
  bool	isatty;		/* stdin actually is a terminal? */
  int	column;		/* current cursor column */
  int	in;		/* in-pointer in queue */
  int	out;		/* out-pointer in queue */
  short	flags;		/* FLAGS */
  char	queue[QSIZE];	/* character queue */
} stdin_driver =
{ Control('W'), Control('U'), Control('?'), Control('H'),
  Control('D'), Control('['), Control('R'), Control('C'),
  TTY_EXTEND_ATOMS,		/* mode */
  0,			/* no lines yet */
  TRUE,			/* line 0 is a tty */
  0, 0, 0,		/* colunm, in, out */
  CRLF			/* flags */
};
#endif O_LINE_EDIT

#if unix && O_TERMIOS				/* System V termio system */

bool
PushTty(buf, mode)
ttybuf *buf;
int mode;
{ struct termio tio;

  if ( status.notty )
    succeed;

  if ( ioctl(0, TCGETA, &buf->tab) )		/* save the old one */
    fail;
  tio = buf->tab;
  buf->mode = ttymode;

  if ( mode != TTY_SAVE )
    ttymode = mode;

#if O_LINE_EDIT
  stdin_driver.mode = mode;
#endif

  switch( mode )
  { case TTY_SAVE:
	succeed;
#if O_EXTEND_ATOMS
#if O_LINE_EDIT
    case TTY_COOKED:
    case TTY_RAW:
	tio.c_lflag &= ~(ECHO|ICANON);
	tio.c_cc[VTIME] = 0, tio.c_cc[VMIN] = 1;
	break;
    case TTY_EXTEND_ATOMS:
	tio.c_lflag &= ~(ICANON|ECHO|ECHOE);
	tio.c_cc[VTIME] = 0, tio.c_cc[VMIN] = 1;
	stdin_driver.erase  = tio.c_cc[VERASE];
	stdin_driver.kill   = tio.c_cc[VKILL];
	break;
    case TTY_RETYPE:
	break;
    case TTY_APPEND:
	break;
#else O_LINE_EDIT
    case TTY_RAW:
	tio.c_lflag &= ~(ECHO|ICANON);
	tio.c_cc[VTIME] = 0, tio.c_cc[VMIN] = 1;
	break;
    case TTY_EXTEND_ATOMS:
	tio.c_cc[VEOF]	= 0;	/* disable EOF */
	tio.c_cc[VEOL]	= 04;	/* ^D: give alternatives */
	tio.c_cc[VEOL2]	= ESC;	/* ESC: complete */
	break;
    case TTY_RETYPE:
	tio.c_lflag &= ~ECHO;
	break;
    case TTY_APPEND:
	tio.c_lflag |= ECHO;
	break;
#endif O_LINE_EDIT
#endif O_EXTEND_ATOMS
    default:
	sysError("Unknown PushTty() mode: %d", mode);
	/*NOTREACHED*/
  }

  if ( ioctl(0, TCSETA, &tio) )
    fail;

  succeed;
}


bool
PopTty(buf)
ttybuf *buf;
{
  if ( status.notty )
    succeed;
#if O_LINE_EDIT
  stdin_driver.mode = buf->mode;
#endif
  if ( ioctl(0, TCSETA, &buf->tab) )
    fail;
  ttymode = buf->mode;

  succeed;
}

#endif
#if unix && !O_TERMIOS			/* Unix with (old) sgtty() driver */

bool
PushTty(buf, mode)
ttybuf *buf;
int mode;
{ struct tchars chrs;
  struct sgttyb flgs;
  bool flags_set = FALSE;
  bool chars_set = FALSE;

  if ( status.notty )
    succeed;

  DEBUG(1, printf("PushTty(0x%x, %d)\n", buf, mode));

  if ( ioctl(0, TIOCGETP, &buf->tab) ||
       ioctl(0, TIOCGETC, &buf->chars) )
  { DEBUG(1, printf("Failed to get terminal parameters: %s\n", OsError()));
    fail;
  }

  flgs = buf->tab;
  chrs = buf->chars;
  buf->mode = ttymode;

  if ( mode != TTY_SAVE )
    ttymode = mode;

#if O_LINE_EDIT
  stdin_driver.mode = mode;
#endif

  switch( mode )
  { case TTY_SAVE:
#if !O_LINE_EDIT
    case TTY_COOKED:
#endif
	succeed;
#if O_LINE_EDIT
    case TTY_COOKED:
#endif
    case TTY_RAW:
	flgs.sg_flags &= ~(ECHO);
	flgs.sg_flags |= CBREAK;
	flags_set = TRUE;
	break;
#if O_LINE_EDIT
    case TTY_EXTEND_ATOMS:
	flgs.sg_flags &= ~(ECHO);
	flgs.sg_flags |= CBREAK;
	stdin_driver.erase = flgs.sg_erase;
	stdin_driver.kill  = flgs.sg_kill;
	flags_set = TRUE;
	break;
    case TTY_RETYPE:
    case TTY_APPEND:
	break;
#else O_LINE_EDIT
    case TTY_EXTEND_ATOMS:
	chrs.t_brkc = ESC;		/* ESC, EOF already on 04 */
	chars_set = TRUE;
	break;
    case TTY_RETYPE:
	flgs.sg_flags &= ~ECHO;
	flags_set = TRUE;
	break;
    case TTY_APPEND:
	flgs.sg_flags |= ECHO;
	flags_set = TRUE;
	break;
#endif O_LINE_EDIT
    default:
	sysError("Unknown PushTty() mode: %d", mode);
	/*NOTREACHED*/
  }

  if ( flags_set )
    if ( ioctl(0, TIOCSETN, &flgs) != 0 )
      return warning("Failed to set terminal flags: %s", OsError());
  if ( chars_set )
    if ( ioctl(0, TIOCSETC, &chrs) != 0 )
      return warning("Failed to set terminal characters: %s", OsError());

  succeed;
}


bool
PopTty(buf)
ttybuf *buf;
{ if ( status.notty )
    succeed;

  if ( ioctl(0, TIOCSETN, &buf->tab) ||
       ioctl(0, TIOCSETC, &buf->chars) )
    fail;
  ttymode = buf->mode;

  succeed;
}
#endif unix && !O_TERMIOS

#if tos					/* ATARI_ST, running TOS */
bool
PushTty(buf, mode)
ttybuf *buf;
int mode;
{ if ( mode != TTY_SAVE )
    ttymode = mode;

  switch( mode )
  { case TTY_SAVE:
	buf->mode = stdin_driver.mode;
	succeed;
    case TTY_RAW:
    case TTY_EXTEND_ATOMS:
    case TTY_RETYPE:
    case TTY_APPEND:
	stdin_driver.mode = mode;
	succeed;			/* to be implemented later */
    default:
	return sysError("Unknown PushTty() mode: %d", mode);
	/*NOTREACHED*/
  }
}

bool
PopTty(buf)
ttybuf *buf;
{ stdin_driver.mode = buf->mode;
  ttymode = buf->mode;
  succeed;
}
#endif tos

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void ResetTty()

    Reset terminal to a sensible state after an abort ore restore()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ResetTty()
{
#ifdef RESET_STDIN
  RESET_STDIN;
#else
#if unix && !LINUX
  stdin->_ptr = stdin->_base;
  stdin->_cnt = 0;
#endif
  clearerr(stdin);
#endif
#if O_LINE_EDIT
  stdin_driver.in = stdin_driver.out = 0;
  stdin_driver.emitting = 0;
#if unix
  stdin_driver.isatty = isatty(fileno(stdin));
#endif
#if tos
  stdin_driver.isatty = TRUE;		/* how to find out? */
#endif
#endif O_LINE_EDIT
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void PretendTyped(c)
         char c;

    Pretend the user typed character `c'. If the tty mode is  TTY_RETYPE
    the  character  should  just  be  pushed  in  the  input buffer.  If
    TTY_APPEND it should also be echoed to the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_EXTEND_ATOMS && !O_LINE_EDIT
void
PretendTyped(c)
char c;
{ ioctl(0, TIOCSTI, &c);
}
#endif O_EXTEND_ATOMS

#if O_LINE_EDIT
		/********************************
		*         LINE EDITING          *
		*********************************/

forwards bool	tty_erase P((struct tty_driver *));
forwards void	tty_werase P((struct tty_driver *));
forwards void	tty_kill P((struct tty_driver *));
forwards void	tty_reprint P((struct tty_driver *));
forwards bool	tty_insert P((struct tty_driver *, Char));
forwards void	add_char P((struct tty_driver *, Char));
forwards Char	get_edit_char P((struct tty_driver *));
forwards bool	tty_endsline P((struct tty_driver *, Char));
forwards void	tty_putc P((Char));
forwards void	tty_putstr P((char *));

#define isPrint(c) ((c >= ' ' && c < 127) || c == '\t' || c == '\n')

static void
tty_putc(c)
Char c;
{
#if unix
  putchar(c);
#endif
#if tos
  if ( c == '\n' )
    putch('\r');
  putch(c);
#endif
}

static void
tty_putstr(s)
char *s;
{ for( ; *s; s++ )
    tty_putc(*s);
}

static bool
tty_erase(d)
register struct tty_driver *d;
{ Char c;

  if ( Empty(d) )
    fail;

  Retreat(d->in);
  switch( c = d->queue[d->in] )
  { case '\t':		do
			{ tty_putc('\b');
			  d->column--;
			} while( d->column % 8 );
			break;
    default:		if ( c < ' ' || c == 127 )
			{ tty_putstr("\b\b  \b\b");
			  d->column -= 2;
			} else if ( c < 127 )
			{ d->column--;
			  tty_putstr("\b \b");
			  break;
			}
  }  

  succeed;
}

static void
tty_werase(d)
register struct tty_driver *d;
{ int last;

  do
  { last = d->in;
    Retreat(last);
  } while( isBlank(d->queue[last]) && tty_erase(d) );

  do
  { last = d->in;
    Retreat(last);
  } while( !isBlank(d->queue[last]) && tty_erase(d) );
}

static void
tty_kill(d)
register struct tty_driver *d;
{ while( tty_erase(d) )
   ;
}

static bool
tty_endsline(d, c)
register struct tty_driver *d;
Char c;
{ return c == '\n' || c == d->eol || c == d->eol2;
}

static bool
tty_insert(d, c)
register struct tty_driver *d;
Char c;
{ d->queue[d->in] = c;
  Advance(d->in);

  if ( tty_endsline(d, c) )
    d->emitting++;

  if ( d->mode != TTY_RETYPE )
  { switch(c)
    { case '\t':
	  tty_putc(c);
	  d->column = ((d->column + 8)/8)*8;
	  break;
      case '\n':
	  tty_putc(c);
	  d->column = 0;
	  break;
      default:
	  if ( c < ' ' || c == 127 )
	  { tty_putc('^');
	    tty_putc(c < ' ' ? c + '@' : '?');
	    d->column += 2;
	  } else if ( c < 127 )
	  { tty_putc(c);
	    d->column++;
	  }
    }
  }

  succeed;
}

static void
tty_reprint(d)
register struct tty_driver *d;
{ int n;

  tty_putc('\n');
  for(n = d->out; n != d->in; )
  { tty_putc(d->queue[n]);
    Advance(n)
  }
}

void
TtyAddChar(c)
Char c;
{ add_char(&stdin_driver, c);
}

static void
add_char(d, c)
register struct tty_driver *d;
Char c;
{ if ( d->flags & CRLF && c == '\r' )
    c = '\n';

  if ( c == d->werase )
    tty_werase(d);
  else if ( c == d->erase || c == d->erase2 )
    tty_erase(d);
  else if ( c == d->kill )
    tty_kill(d);
  else if ( c == d->reprint )
    tty_reprint(d);
  else if ( c == d->intr )
  { d->in = d->out = 0;		/* empty queue */
    d->emitting = 0;
    interruptHandler();
  }
  else
    tty_insert(d, c);

  fflush(stdout);		/* needed to get unbuffered output after a */
			      	/* dump on hpux */
}

#if unix
#define GETC()	do_get_char()
#endif
#if tos
#define GETC()	tos_getch()

static Char
tos_getch(void)
{ long t = clock();
  Char c = getch();
  wait_ticks += clock() - t;

  return c == '\r' ? '\n' : c;
}
#endif

static Char
GetCMap()
{ Char c = GETC();

#if O_MAP_TAB_ON_ESC
  if ( c == '\t' )
    c = ESC;
#endif

  return c;
}


static Char
get_edit_char(d)
register struct tty_driver *d;
{ Char c;
  
  if ( status.notty || !d->isatty )
    return do_get_char();

  DEBUG(3, printf("entering get_edit_char(); d->in = %d, d->out = %d\n",
		  d->in, d->out));
  if ( d->mode == TTY_RAW )
  { if ( Empty(d) )
      return GetCMap();
  } else
  { while( d->emitting == 0 )
      add_char(d, GetCMap());
  }
  
  c = d->queue[d->out];
  DEBUG(3, printf("Returning %d (%c) from %d\n", c, c, d->out));
  Advance(d->out);
  if ( tty_endsline(d, c) )
    d->emitting--;    

  return c;
}

#if PROTO
void
PretendTyped(char c)
#else
void
PretendTyped(c)
char c;
#endif
{ struct tty_driver *d = &stdin_driver;

  tty_insert(d, c);  
  d->emitting = FALSE;
}

#endif O_LINE_EDIT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read a character.   When using PCE  we  should be  prepared to  handle
notification correctly here.  Otherwise live is simple.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int (*PL_dispatch_events)() = NULL;

static Char
do_get_char()
{ Char c;

  if ( PL_dispatch_events != NULL )
  { DEBUG(3, printf("do_get_char() --> "));
    for(;;)
    { if ( (*PL_dispatch_events)() == PL_DISPATCH_INPUT )
      { char chr;

	if (read(0, &chr, 1) == 0)
	  c = EOF;
	else
	  c = (Char) chr;
	break;
      }
    }

    DEBUG(3, printf("%d (%c) --> ", c, c));
  } else
    c = (Char) getchar();

  return c;
}


Char
GetChar()
{
#if O_LINE_EDIT
  return (Char) get_edit_char(&stdin_driver);
#else
  return do_get_char();
#endif
}

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

  PushTty(&buf, TTY_SAVE);
  PopTty(&ttytab);			/* restore cooked mode */

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
#if v7 || hpux
  { int waitstat, retstat;		/* the parent */

    old_int  = signal(SIGINT,  SIG_IGN);
    old_stop = signal(SIGTSTP, SIG_DFL);

    while( (waitstat = Wait(&retstat)) != pid && waitstat != -1 )
      ;
    if ( waitstat == -1 )
    {  warning("Failed to execute %s", cmd);
       rval = 1;
    }

    rval = retstat;
  }
#else v7
  { union wait status;			/* the parent */
    int n;

    old_int  = signal(SIGINT,  SIG_IGN);
    old_stop = signal(SIGTSTP, SIG_DFL);

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
#endif v7

  signal(SIGINT,  old_int);		/* restore signal handlers */
  signal(SIGTSTP, old_stop);
  PopTty(&buf);

  return rval;
}
#endif unix

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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    char *Symbols()

    Return the path name of the executable of SWI-Prolog. Used by the -c
    compiler to generate the #!<path> header line and by the incremental
    loader, who gives this path to ld, using ld -A <path>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if unix
char *
Symbols()
{ return Which(mainArgv[0]);
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
#if !LINUX
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
#endif unix

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
#endif tos

static char *
Which(program)
char *program;
{ static char fullname[MAXPATHLEN];
  char *path, *dir;
  char *e;

  if ( isAbsolutePath(program) ||
       isRelativePath(program) ||
       index(program, '/') )
  { if ( (e = okToExec(program)) != NULL )
    { strcpy(fullname, e);
      
      return fullname;
    }

    return NULL;
  }

  if  ((path = getenv("PATH") ) == 0)
    path = DEFAULT_PATH;

  while(*path)
  { if ( *path == PATHSEP )
    { if ( (e = okToExec(program)) != NULL)
      { strcpy(fullname, e);

        return fullname;
      } else
        return NULL;
    } else
    { for(dir = fullname; *path && *path != PATHSEP; *dir++ = *path++)
	;
      if (*path)
	path++;						/* skip : */
      if (strlen(fullname) + strlen(program)+2 > MAXPATHLEN)
        continue;
      *dir++ = '/';
      *dir = EOS;
      strcpy(dir, program);
      if ( (e = okToExec(fullname)) != NULL )
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
#if minix			/* v7 unix does not have fine granularity */
void				/* timer. Just use sleep() */
Sleep(time)
real time;
{ if ( time < 0.5 )
    return;
  sleep( (int)(time+0.5) );
}

#else /* has select() */

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
#endif unix

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
