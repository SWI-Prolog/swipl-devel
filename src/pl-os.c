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

/*  Modified (M) 1993 Dave Sherratt  */

/*#define O_DEBUG 1*/

#if __TOS__
#include <tos.h>		/* before pl-os.h due to Fopen, ... */
#endif
#if OS2 && EMX
#include <os2.h>                /* this has to appear before pl-incl.h */
#endif

#include <math.h>		/* avoid abs() problem with msvc++ */
#include <stdio.h>		/* rename() and remove() prototypes */
#include "pl-incl.h"
#include "pl-ctype.h"

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if !O_XOS
#define statfunc stat
#endif
#if HAVE_PWD_H
#include <pwd.h>
#endif
#if HAVE_VFORK_H
#include <vfork.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#if defined(HAVE_SYS_RESOURCE_H)
#include <sys/resource.h>
#endif
#ifdef HAVE_FTIME
#include <sys/timeb.h>
#endif

#include <fcntl.h>
#ifndef __WATCOMC__			/* appears a conflict */
#include <errno.h>
#endif

#if defined(__WATCOMC__)
#include <io.h>
#include <dos.h>
#endif

#if OS2 && EMX
static real initial_time;
#endif /* OS2 */

static void	initExpand(void);
static void	cleanupExpand(void);
static void	initRandom(void);
static void	initEnviron(void);
static long	Time(void);
static char *	Which(const char *program, char *fullname);

#ifndef DEFAULT_PATH
#define DEFAULT_PATH "/bin:/usr/bin"
#endif

		 /*******************************
		 *	       GLOBALS		*
		 *******************************/
#ifdef HAVE_CLOCK
long clock_wait_ticks;
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

#ifdef __WIN32__
  if ( iswin32s() )
    set(&features, DOS_FILE_NAMES_FEATURE);
  else
    set(&features, FILE_CASE_PRESERVING_FEATURE);
#else
  set(&features, FILE_CASE_FEATURE);
  set(&features, FILE_CASE_PRESERVING_FEATURE);
#endif

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


void
cleanupOs(void)
{ cleanupExpand();
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

#ifdef __unix__
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
    double CpuTime(cputime_kind)

    Returns a floating point number, representing the amount  of  (user)
    CPU-seconds  used  by the process Prolog is in.  For systems that do
    not allow you to obtain this information  you  may  wish  to  return
    elapsed  time  since Prolog was started, as this function is used to
    by consult/1 and time/1 to determine the amount of CPU time used  to
    consult a file or to execute a query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __WIN32__			/* defined in pl-nt.c */

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


double
CpuTime(cputime_kind which)
{
#ifdef HAVE_TIMES
  struct tms t;
  double used;
  static int MTOK_got_hz = FALSE;
  static double MTOK_hz;

  if ( !MTOK_got_hz )
  { MTOK_hz = (double) Hz;
    MTOK_got_hz++;
  }
  times(&t);

  switch( which )
  { case CPU_USER:
      used = (double) t.tms_utime / MTOK_hz;
      break;
    case CPU_SYSTEM:
    default:				/* make compiler happy */
      used = (double) t.tms_stime / MTOK_hz;
  }

  if ( isnan(used) )			/* very dubious, but this */
    used = 0.0;				/* happens when running under GDB */

  return used;
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

  return 0.0;
}

#endif /*__WIN32__*/

void
PL_clock_wait_ticks(long waited)
{
#ifdef HAVE_CLOCK
  clock_wait_ticks += waited;
#endif
}


double
WallTime(void)
{ double stime;

#ifdef HAVE_GETTIMEOFDAY
  struct timeval tp;

  gettimeofday(&tp, NULL);
  stime = (double)tp.tv_sec + (double)tp.tv_usec/1000000.0;
#else
#ifdef HAVE_FTIME
  struct timeb tb;

  ftime(&tb);
  stime = (double)tb.time + (double)tb.millitm/1000.0;
#else
  stime = (double)time((time_t *)NULL);
#endif
#endif

  return stime;
}

		 /*******************************
		 *	       MEMORY		*
		 *******************************/

ulong
UsedMemory(void)
{
#if defined(HAVE_GETRUSAGE) && defined(HAVE_RU_IDRSS)
  struct rusage usage;

  if ( getrusage(RUSAGE_SELF, &usage) == 0 &&
       usage.ru_idrss )
  { return usage.ru_idrss;		/* total unshared data */
  }
#endif

  return (GD->statistics.heap +
	  usedStack(global) +
	  usedStack(local) +
	  usedStack(trail));
}


ulong
FreeMemory(void)
{ ulong used = UsedMemory();

#if defined(HAVE_GETRLIMIT) && defined(RLIMIT_DATA)
  struct rlimit limit;

  if ( getrlimit(RLIMIT_DATA, &limit) == 0 )
    return limit.rlim_cur - used;
#endif

  return 0L;
}


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
  long l = rand();			/* 0<n<2^15-1 */
  
  l ^= rand()<<10;
  l ^= rand()<<20;

  return l & (~PLMININT);
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

    atom_t TemporaryFile(const char *id)

    The return value of this call is an atom,  whose  string  represents
    the  path  name of a unique file that can be used as temporary file.
    `id' is a char * that can be used to make it easier to identify  the
    file as a specific kind of SWI-Prolog intermediate file.

    void RemoveTemporaryFiles()

    Remove all temporary files.  This function should be  aware  of  the
    fact  that some of the file names generated by TemporaryFile() might
    not be created at all, or might already have been deleted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct tempfile
{ atom_t	name;
  TempFile	next;
};					/* chain of temporary files */

#define tmpfile_head (GD->os._tmpfile_head)
#define tmpfile_tail (GD->os._tmpfile_tail)

#ifndef DEFTMPDIR
#ifdef __WIN32__
#define DEFTMPDIR "c:/tmp"
#else
#define DEFTMPDIR "/tmp"
#endif
#endif

atom_t
TemporaryFile(const char *id)
{ char temp[MAXPATHLEN];
  TempFile tf = allocHeap(sizeof(struct tempfile));
  char envbuf[MAXPATHLEN];
  char *tmpdir;

  if ( !((tmpdir = getenv3("TEMP", envbuf, sizeof(envbuf))) ||
	 (tmpdir = getenv3("TMP",  envbuf, sizeof(envbuf)))) )
    tmpdir = DEFTMPDIR;

#ifdef __unix__
{ static int MTOK_temp_counter = 0;

  Ssprintf(temp, "%s/pl_%s_%d_%d",
	   tmpdir, id, (int) getpid(), MTOK_temp_counter++);
}
#endif

#ifdef __WIN32__
{ char *tmp;
  static int temp_counter = 0;

#ifdef __LCC__
  if ( (tmp = tmpnam(NULL)) )
#else
  if ( (tmp = _tempnam(tmpdir, id)) )
#endif
  { PrologPath(tmp, temp);
  } else
    Ssprintf(temp, "%s/pl_%s_%d", tmpdir, id, temp_counter++);
}
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

  tf->name = PL_new_atom(temp);		/* locked: ok! */
  tf->next = NULL;
  
  startCritical;
  if ( !tmpfile_tail )
  { tmpfile_head = tmpfile_tail = tf;
  } else
  { tmpfile_tail->next = tf;
    tmpfile_tail = tf;
  }
  endCritical;

  return tf->name;
}

void
RemoveTemporaryFiles(void)
{ TempFile tf, tf2;  

  startCritical;
  for(tf = tmpfile_head; tf; tf = tf2)
  { RemoveFile(stringAtom(tf->name));
    tf2 = tf->next;
    freeHeap(tf, sizeof(struct tempfile));
  }

  tmpfile_head = tmpfile_tail = NULL;
  endCritical;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fortunately most C-compilers  are  sold  with  a  library  that  defines
Unix-style  access  to  the  file system.  The standard functions go via
macros to deal with 16-bit machines, but are not  defined  as  functions
here.   Some  more  specific things SWI-Prolog wants to know about files
are defined here:

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

#if O_HPFS

/*  Conversion rules Prolog <-> OS/2 (using HPFS)
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
OsPath(const char *plpath, char *path)
{ const char *s = plpath, *p = path;
  int limit = MAXPATHLEN-1;

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

#ifdef __unix__
char *
PrologPath(const char *p, char *buf)
{ strcpy(buf, p);

  return buf;
}

char *
OsPath(const char *p, char *buf)
{ strcpy(buf, p);

  return buf;
}
#endif /*__unix__*/

#if O_XOS
char *
PrologPath(const char *p, char *buf)
{ _xos_canonical_filename(p, buf);
  if ( !trueFeature(FILE_CASE_FEATURE) )
    strlwr(buf);

  return buf;
}

char *
OsPath(const char *p, char *buf)
{ strcpy(buf, p);

  return buf;
}
#endif /* O_XOS */

long
LastModifiedFile(char *f)
{ char tmp[MAXPATHLEN];

#if defined(HAVE_STAT) || defined(__unix__)
  struct stat buf;

  if ( statfunc(OsPath(f, tmp), &buf) < 0 )
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

  if ( findfirst(OsPath(f, tmp), &buf, FA_HIDDEN) != 0 )
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
ExistsFile(const char *path)
{ 
#ifdef O_XOS
  return _xos_exists(path, _XOS_FILE);
#else
  char tmp[MAXPATHLEN];

#if defined(HAVE_STAT) || defined(__unix__)
  struct stat buf;

  if ( statfunc(OsPath(path, tmp), &buf) == -1 ||
       (buf.st_mode & S_IFMT) != S_IFREG )
    fail;
  succeed;
#endif

#if tos
  struct ffblk buf;

  if ( findfirst(OsPath(path, tmp), &buf, FA_HIDDEN) == 0 )
  { DEBUG(2, Sdprintf("%s (%s) exists\n", path, OsPath(path)));
    succeed;
  }
  DEBUG(2, Sdprintf("%s (%s) does not exist\n", path, OsPath(path)));
  fail;
#endif
#endif
}


bool
AccessFile(const char *path, int mode)
{ char tmp[MAXPATHLEN];
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

  return access(OsPath(path, tmp), m) == 0 ? TRUE : FALSE;
#endif

#ifdef tos
  struct ffblk buf;

  if ( findfirst(OsPath(path, tmp), &buf, FA_DIREC|FA_HIDDEN) != 0 )
    fail;			/* does not exists */
  if ( (mode & ACCESS_WRITE) && (buf.ff_attrib & FA_RDONLY) )
    fail;			/* readonly file */

  succeed;
#endif
}


bool
ExistsDirectory(const char *path)
{
#ifdef O_XOS
  return _xos_exists(path, _XOS_DIR);
#else
  char tmp[MAXPATHLEN];
  char *ospath = OsPath(path, tmp);

#if defined(HAVE_STAT) || defined(__unix__)
  struct stat buf;

  if ( statfunc(ospath, &buf) < 0 )
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
#endif /*O_XOS*/
}


long
SizeFile(const char *path)
{ char tmp[MAXPATHLEN];
  struct stat buf;

#if defined(HAVE_STAT) || defined(__unix__)
  if ( statfunc(OsPath(path, tmp), &buf) == -1 )
    return -1;
#endif

  return buf.st_size;
}


int
RemoveFile(const char *path)
{ char tmp[MAXPATHLEN];

#ifdef HAVE_REMOVE
  return remove(OsPath(path, tmp)) == 0 ? TRUE : FALSE;
#else
  return unlink(OsPath(path, tmp)) == 0 ? TRUE : FALSE;
#endif
}


bool
RenameFile(const char *old, const char *new)
{ char oldbuf[MAXPATHLEN];
  char newbuf[MAXPATHLEN];
  char *osold, *osnew;

  osold = OsPath(old, oldbuf);
  osnew = OsPath(new, newbuf);

#ifdef HAVE_RENAME
  remove(osnew);			/* assume we have this too */
  return rename(osold, osnew) == 0 ? TRUE : FALSE;
#else
{ int rval;

  unlink(osnew);
  if ( (rval = link(osold, osnew)) == 0 
       && (rval = unlink(osold)) != 0)
    unlink(osnew);

  if ( rval == 0 )
    succeed;

  fail;
}
#endif /*HAVE_RENAME*/
}

bool
SameFile(const char *f1, const char *f2)
{ if ( trueFeature(FILE_CASE_FEATURE) )
  { if ( streq(f1, f2) )
      succeed;
  } else
  { if ( stricmp(f1, f2) == 0 )
      succeed;
  }

#ifdef __unix__				/* doesn't work on most not Unix's */
  { struct stat buf1;
    struct stat buf2;
    char tmp[MAXPATHLEN];

    if ( statfunc(OsPath(f1, tmp), &buf1) != 0 ||
	 statfunc(OsPath(f2, tmp), &buf2) != 0 )
      fail;
    if ( buf1.st_ino == buf2.st_ino && buf1.st_dev == buf2.st_dev )
      succeed;
  }
#endif
#ifdef O_XOS
  { char p1[MAXPATHLEN];
    char p2[MAXPATHLEN];

    _xos_limited_os_filename(f1, p1);
    _xos_limited_os_filename(f2, p2);
    if ( trueFeature(FILE_CASE_FEATURE) )
    { if ( streq(p1, p2) )
	succeed;
    } else
    { if ( stricmp(p1, p2) == 0 )
	succeed;
    }
  }
#endif /*O_XOS*/
    /* Amazing! There is no simple way to check two files for identity. */
    /* stat() and fstat() both return dummy values for inode and device. */
    /* this is fine as OS'es not supporting symbolic links don't need this */

  fail;
}


bool
MarkExecutable(const char *name)
{
#if (defined(HAVE_STAT) && defined(HAVE_CHMOD)) || defined(__unix__)
  struct stat buf;
  int um;

  um = umask(0777);
  umask(um);
  if ( statfunc(name, &buf) == -1 )
  { term_t file = PL_new_term_ref();

    PL_put_atom_chars(file, name);
    return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		    ATOM_stat, ATOM_file, file);
  }

  if ( (buf.st_mode & 0111) == (~um & 0111) )
    succeed;

  buf.st_mode |= 0111 & ~um;
  if ( chmod(name, buf.st_mode) == -1 )
  { term_t file = PL_new_term_ref();

    PL_put_atom_chars(file, name);
    return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		    ATOM_chmod, ATOM_file, file);
  }
#endif /* defined(HAVE_STAT) && defined(HAVE_CHMOD) */

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    char *AbsoluteFile(const char *file, char *path)

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

    char *DirName(const char *path, char *dir)
    
    Return the directory name for a file having path `path'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(HAVE_SYMLINKS) && (defined(HAVE_STAT) || defined(__unix__))
#define O_CANONISE_DIRS

struct canonical_dir
{ char *	name;			/* name of directory */
  char *	canonical;		/* canonical name of directory */
  dev_t		device;			/* device number */
  ino_t		inode;			/* inode number */
  CanonicalDir  next;			/* next in chain */
};

#define canonical_dirlist (GD->os._canonical_dirlist)

forwards char   *canoniseDir(char *);
#endif /*O_CANONISE_DIRS*/

#define CWDdir	(LD->os._CWDdir)	/* current directory */
#define CWDlen	(LD->os._CWDlen)	/* strlen(CWDdir) */

static void
initExpand(void)
{ 
#ifdef O_CANONISE_DIRS
  char *dir;
  char *cpaths;
#endif

  CWDdir = NULL;
  CWDlen = 0;

#ifdef O_CANONISE_DIRS
{ char envbuf[MAXPATHLEN];

  if ( (cpaths = getenv3("CANONICAL_PATHS", envbuf, sizeof(envbuf))) )
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

  if ( (dir = getenv3("HOME", envbuf, sizeof(envbuf))) ) canoniseDir(dir);
  if ( (dir = getenv3("PWD",  envbuf, sizeof(envbuf))) ) canoniseDir(dir);
  if ( (dir = getenv3("CWD",  envbuf, sizeof(envbuf))) ) canoniseDir(dir);
}
#endif
}

#ifdef O_CANONISE_DIRS

static void
cleanupExpand(void)
{ CanonicalDir dn = canonical_dirlist, next;

  canonical_dirlist = NULL;
  for( ; dn; dn = next )
  { next = dn->next;
    free(dn);
  }
}


static void
registerParentDirs(const char *path)
{ const char *e = path + strlen(path);

  while(e>path)
  { char dirname[MAXPATHLEN];
    char tmp[MAXPATHLEN];
    CanonicalDir d;
    struct stat buf;

    for(e--; *e != '/' && e > path + 1; e-- )
      ;

    strncpy(dirname, path, e-path);
    dirname[e-path] = EOS;

    for(d = canonical_dirlist; d; d = d->next)
    { if ( streq(d->name, dirname) )
	return;
    }
	
    if ( statfunc(OsPath(dirname, tmp), &buf) == 0 )
    { CanonicalDir dn   = malloc(sizeof(*dn));

      dn->next		= canonical_dirlist;
      dn->name		= store_string(dirname);
      dn->inode		= buf.st_ino;
      dn->device	= buf.st_dev;
      dn->canonical	= dn->name;
      canonical_dirlist	= dn;

      DEBUG(1, Sdprintf("Registered canonical dir %s\n", dirname));
    } else
      return;
  }
}


static char *
canoniseDir(char *path)
{ CanonicalDir d;
  struct stat buf;
  char tmp[MAXPATHLEN];

  DEBUG(1, Sdprintf("canoniseDir(%s) --> ", path));

  for(d = canonical_dirlist; d; d = d->next)
  { if ( streq(d->name, path) )
    { if ( d->name != d->canonical )
	strcpy(path, d->canonical);

      DEBUG(1, Sdprintf("(lookup) %s\n", path));
      return path;
    }
  }

					/* we need to use malloc() here */
					/* because allocHeap() only ensures */
					/* alignment for `word', and inode_t */
					/* is sometimes bigger! */

  if ( statfunc(OsPath(path, tmp), &buf) == 0 )
  { CanonicalDir dn = malloc(sizeof(*dn));
    char dirname[MAXPATHLEN];
    char *e = path + strlen(path);

    dn->next   = canonical_dirlist;
    dn->name   = store_string(path);
    dn->inode  = buf.st_ino;
    dn->device = buf.st_dev;

    do
    { strncpy(dirname, path, e-path);
      dirname[e-path] = EOS;
      if ( statfunc(OsPath(dirname, tmp), &buf) < 0 )
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
	  canonical_dirlist = dn;
	  DEBUG(1, Sdprintf("(replace) %s\n", path));
	  registerParentDirs(path);
	  return path;
	}
      }

      for(e--; *e != '/' && e > path + 1; e-- )
	;
    } while( e > path );

    dn->canonical = dn->name;
    canonical_dirlist = dn;

    DEBUG(1, Sdprintf("(new, existing) %s\n", path));
    registerParentDirs(path);
    return path;
  }

  DEBUG(1, Sdprintf("(nonexisting) %s\n", path));
  return path;
}

#else

#define canoniseDir(d)

static void
cleanupExpand(void)
{
}

#endif /*O_CANONISE_DIRS*/


static char *
canoniseFileName(char *path)
{ char *out = path, *in = path;
  char *osave[100];
  int  osavep = 0;

  while( in[0] == '/' && in[1] == '.' && in[2] == '.' && in[3] == '/' )
    in += 3;
  while( in[0] == '.' && in[1] == '/' )
    in += 2;
  if ( in[0] == '/' )
    *out++ = '/';
#ifdef O_HASSHARES
  if ( in[1] == '/' )
  { in++;
    *out++ = '/';
  }
#endif
  osave[osavep++] = out;

  while(*in)
  { if (*in == '/')
    {
    again:
      if ( *in )
      { while( in[1] == '/' )		/* delete multiple / */
	  in++;
	if ( in[1] == '.' )
	{ if ( in[2] == '/' )		/* delete /./ */
	  { in += 2;
	    goto again;
	  }
	  if ( in[2] == EOS )		/* delete trailing /. */
	  { *out = EOS;
	    return path;
	  }
	  if ( in[2] == '.' &&		/* delete /foo/../ */
	       (in[3] == '/' || in[3] == EOS) && osavep > 0 )
	  { out = osave[--osavep];
	    in += 3;
	    if ( in[0] == EOS && out > path )
	    { out[-1] = EOS;		/* delete trailing / */
	      return path;
	    }
	    goto again;
	  }
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
{ if ( !trueFeature(FILE_CASE_FEATURE) )
    strlwr(path);

  canoniseFileName(path);

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


static char *
takeWord(const char **string, char *wrd)
{ const char *s = *string;
  char *q = wrd;
  int left = MAXPATHLEN-1;

  while( isAlpha(*s) || *s == '_' )
  { if ( --left < 0 )
    { PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
	       ATOM_max_variable_length);
      return NULL;
    }
    *q++ = *s++;
  }
  *q = EOS;
  
  *string = s;
  return wrd;
}


bool
expandVars(const char *pattern, char *expanded)
{ int size = 0;
  char c;
  char word[MAXPATHLEN];

  if ( *pattern == '~' )
  { char *user;
    char *value;
    int l;

    pattern++;
    user = takeWord(&pattern, word);
    if ( user[0] == EOS )		/* ~/bla */
    {
#ifdef O_XOS
      value = _xos_home();
#else /*O_XOS*/
      if ( !(value = GD->os.myhome) )
      { char envbuf[MAXPATHLEN];

	if ( (value = getenv3("HOME", envbuf, sizeof(envbuf))) )
	{ value = GD->os.myhome = store_string(PrologPath(value, word));
	} else
	{ value = GD->os.myhome = "/";
	}
      }
#endif /*O_XOS*/
    } else				/* ~fred */
#ifdef HAVE_GETPWNAM
    { struct passwd *pwent;

      if ( GD->os.fred && streq(GD->os.fred, user) )
      { value = GD->os.fredshome;
      } else
      { if ( !(pwent = getpwnam(user)) )
	{ if ( fileerrors )
	  { term_t name = PL_new_term_ref();

	    PL_put_atom_chars(name, user);
	    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_user, name);
	  }
	  fail;
	}
	if ( GD->os.fred )
	  remove_string(GD->os.fred);
	if ( GD->os.fredshome )
	  remove_string(GD->os.fredshome);
	
	GD->os.fred = store_string(user);
	value = GD->os.fredshome = store_string(pwent->pw_dir);
      }
    }	  
#else
    { if ( fileerrors )
	PL_error(NULL, 0, NULL, ERR_NOTIMPLEMENTED, PL_new_atom("user_info"));

      fail;
    }
#endif
    size += (l = (int) strlen(value));
    if ( size >= MAXPATHLEN )
      return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_path_length);
    strcpy(expanded, value);
    expanded += l;

					/* ~/ should not become // */
    if ( expanded[-1] == '/' && pattern[0] == '/' )
      pattern++;
  }

  for( ;; )
  { switch( c = *pattern++ )
    { case EOS:
	break;
      case '$':
	{ char envbuf[MAXPATHLEN];
	  char *var = takeWord(&pattern, word);
	  char *value = getenv3(var, envbuf, sizeof(envbuf));
	  int l;

	  if ( value == (char *) NULL )
	  { if ( fileerrors )
	    { term_t name = PL_new_term_ref();

	      PL_put_atom_chars(name, var);
	      PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_variable, name);
	    }

	    fail;
	  }
	  size += (l = (int)strlen(value));
	  if ( size >= MAXPATHLEN )
	    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
			    ATOM_max_path_length);
	  strcpy(expanded, value);
	  expanded += l;

	  continue;
	}
      default:
	if ( ++size >= MAXPATHLEN )
	  return PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
			  ATOM_max_path_length);
	*expanded++ = c;

	continue;
    }
    break;
  }

  if ( ++size >= MAXPATHLEN )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
		    ATOM_max_path_length);
  *expanded = EOS;

  succeed;
}


static int
ExpandFile(const char *pattern, char **vector)
{ char expanded[MAXPATHLEN];
  int matches = 0;

  if ( !expandVars(pattern, expanded) )
    return -1;
  
  vector[matches++] = store_string(expanded);

  return matches;
}


char *
ExpandOneFile(const char *spec, char *file)
{ char *vector[256];
  int size;

  switch( (size=ExpandFile(spec, vector)) )
  { case -1:
      return NULL;
    case 0:
    { term_t tmp = PL_new_term_ref();
      
      PL_put_atom_chars(tmp, spec);
      PL_error(NULL, 0, "no match", ERR_EXISTENCE, ATOM_file, tmp);

      return NULL;
    }
    case 1:
      strcpy(file, vector[0]);
      remove_string(vector[0]);
      return file;
    default:
    { term_t tmp = PL_new_term_ref();
      int n;
      
      for(n=0; n<size; n++)
	remove_string(vector[n]);
      PL_put_atom_chars(tmp, spec);
      PL_error(NULL, 0, "ambiguous", ERR_EXISTENCE, ATOM_file, tmp);

      return NULL;
    }
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

#ifdef O_HASSHARES
  if ( p[0] == '/' && p[1] == '/' )	/* //host/share */
    succeed;
#endif

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
AbsoluteFile(const char *spec, char *path)
{ char tmp[MAXPATHLEN];
  char buf[MAXPATHLEN];
  char *file = PrologPath(spec, buf);
  
  if ( trueFeature(FILEVARS_FEATURE) )
  { if ( !(file = ExpandOneFile(buf, tmp)) )
      return (char *) NULL;
  }

  if ( IsAbsolutePath(file) )
  { strcpy(path, file);

    return canonisePath(path);
  }

#ifdef O_HASDRIVES
  if ( isDriveRelativePath(file) )	/* /something  --> d:/something */
  { if ((strlen(file) + 3) > MAXPATHLEN)
    { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_path_length);
      return (char *) NULL;
    }
    path[0] = GetCurrentDriveLetter();
    path[1] = ':';
    strcpy(&path[2], file);
    return canonisePath(path);
  }
#endif /*O_HASDRIVES*/

  if ( !PL_cwd() )
    return NULL;

  if ( (CWDlen + strlen(file) + 1) >= MAXPATHLEN )
  { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_path_length);
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


void
PL_changed_cwd(void)
{ if ( CWDdir )
    remove_string(CWDdir);
  CWDdir = NULL;
  CWDlen = 0;
}


const char *
PL_cwd(void)
{ if ( CWDlen == 0 )
  { char buf[MAXPATHLEN];
    char *rval;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On SunOs, getcwd() is using popen() to read the output of /bin/pwd.  This
is slow and appears not to cooperate with profile/3.  getwd() is supposed
to be implemented directly.  What about other Unixes?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(HAVE_GETWD) && (defined(__sun__) || !defined(HAVE_GETCWD))
    rval = getwd(buf);
#else
    rval = getcwd(buf, MAXPATHLEN);
#endif
    if ( !rval )
    { term_t tmp = PL_new_term_ref();

      PL_put_atom(tmp, ATOM_dot);
      PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
	       ATOM_getcwd, ATOM_directory, tmp);

      return NULL;
    }

    canonisePath(buf);
    CWDlen = strlen(buf);
    buf[CWDlen++] = '/';
    buf[CWDlen] = EOS;
    
    if ( CWDdir )
      remove_string(CWDdir);
    CWDdir = store_string(buf);
  }

  return (const char *)CWDdir;
}


char *
BaseName(const char *f)
{ const char *base;

  for(base = f; *f; f++)
  { if (*f == '/')
      base = f+1;
  }

  return (char *)base;
}


char *
DirName(const char *f, char *dir)
{ const char *base, *p;

  for(base = p = f; *p; p++)
  { if (*p == '/' && p[1] != EOS )
      base = p;
  }
  if ( base == f )
  { if ( *f == '/' )
      strcpy(dir, "/");
    else
      strcpy(dir, ".");
  } else
  { strncpy(dir, f, base-f);
    dir[base-f] = EOS;
  }
  
  return dir;
}


char *
ReadLink(const char *f, char *buf)
{
#ifdef HAVE_READLINK
  int n;

  if ( (n=readlink(f, buf, MAXPATHLEN-1)) > 0 )
  { buf[n] = EOS;
    return buf;
  }
#endif

  return NULL;
}


static char *
DeRefLink1(const char *f, char *lbuf)
{ char buf[MAXPATHLEN];
  char *l;

  if ( (l=ReadLink(f, buf)) )
  { if ( l[0] == '/' )			/* absolute path */
    { strcpy(lbuf, buf);
      return lbuf;
    } else
    { char *q;

      strcpy(lbuf, f);
      q = &lbuf[strlen(lbuf)];
      while(q>lbuf && q[-1] != '/')
	q--;
      strcpy(q, l);

      canoniseFileName(lbuf);

      return lbuf;
    }
  }

  return NULL;
}


char *
DeRefLink(const	char *link, char *buf)
{ char tmp[MAXPATHLEN];
  char *f;
  int n = 20;				/* avoid loop! */

  while((f=DeRefLink1(link, tmp)) && n-- > 0)
    link = f;

  if ( n > 0 )
  { strcpy(buf, link);
    return buf;
  } else
    return NULL;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool ChDir(path)
	 char *path;

    Change the current working directory to `path'.  File names may depend
    on `path'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
ChDir(const char *path)
{ char ospath[MAXPATHLEN];
  char tmp[MAXPATHLEN];

  OsPath(path, ospath);

  if ( path[0] == EOS || streq(path, ".") ||
       (CWDdir && streq(path, CWDdir)) )
    succeed;

  AbsoluteFile(path, tmp);

  if ( chdir(ospath) == 0 )
  { int len;

    len = strlen(tmp);
    if ( len == 0 || tmp[len-1] != '/' )
    { tmp[len++] = '/';
      tmp[len] = EOS;
    }
    CWDlen = len;
    if ( CWDdir )
      remove_string(CWDdir);
    CWDdir = store_string(tmp);

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


static long
Time(void)
{ return (long)time((time_t *) NULL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			TERMINAL IO MANIPULATION

ResetStdin()
    Clear the Sinput buffer after a saved state.  Only necessary
    if O_SAVE is defined.

PushTty(IOSTREAM *s, ttybuf *buf, int state)
    Push the tty to the specified state and save the old state in
    buf.

PopTty(IOSTREAM *s, ttybuf *buf)
    Restore the tty state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
ResetStdin()
{ Sinput->limitp = Sinput->bufp = Sinput->buffer;
  if ( !GD->os.org_terminal.read )
    GD->os.org_terminal = *Sinput->functions;
}

static int
Swrite_protocol(void *handle, char *buf, int size)
{ int rval;
#ifdef HAVE_CLOCK
  long oldclock = clock();
#endif

  protocol(buf, size);

  rval = (*GD->os.org_terminal.write)(handle, buf, size);

#ifdef HAVE_CLOCK
  clock_wait_ticks += clock() - oldclock;
#endif

  return rval;
}

int
Sread_terminal(void *handle, char *buf, int size)
{ long h = (long)handle;
  int fd = (int)h;
  source_location oldsrc = LD->read_source;

  if ( LD->prompt.next && ttymode != TTY_RAW )
  { Sfputs(PrologPrompt(), Suser_output);
    
    LD->prompt.next = FALSE;
  }

  Sflush(Suser_output);
  PL_dispatch(fd, PL_DISPATCH_WAIT);
  size = (*GD->os.org_terminal.read)(handle, buf, size);

  if ( size == 0 )			/* end-of-file */
  { if ( fd == 0 )
    { Sclearerr(Suser_input);
      LD->prompt.next = TRUE;
    }
  } else if ( size > 0 && buf[size-1] == '\n' )
    LD->prompt.next = TRUE;

  LD->read_source = oldsrc;

#ifdef WIN32
  if ( size > 0 )			/* Why needed only here? */
    protocol(buf, size);		/* and what is the correct condition */
#endif

  return size;
}

void
ResetTty()
{ startCritical;
  ResetStdin();

  if ( !GD->os.iofunctions.read )
  { GD->os.iofunctions       = *Sinput->functions;
    GD->os.iofunctions.read  = Sread_terminal;
    GD->os.iofunctions.write = Swrite_protocol;

    Sinput->functions  = 
    Soutput->functions = 
    Serror->functions  = &GD->os.iofunctions;
  }
  LD->prompt.next = TRUE;
  endCritical;
}

#ifdef O_HAVE_TERMIO			/* sys/termios.h or sys/termio.h */

#ifndef HAVE_TCSETATTR
#ifndef NO_SYS_IOCTL_H_WITH_SYS_TERMIOS_H
#include <sys/ioctl.h>
#endif
#ifndef TIOCGETA
#define TIOCGETA TCGETA
#endif
#endif

bool
PushTty(IOSTREAM *s, ttybuf *buf, int mode)
{ struct termios tio;
  int fd;

  buf->mode = ttymode;
  ttymode = mode;

  if ( (fd = Sfileno(s)) < 0 || !isatty(fd) )
    succeed;				/* not a terminal */
  if ( !trueFeature(TTY_CONTROL_FEATURE) )
    succeed;

#ifdef HAVE_TCSETATTR 
  if ( tcgetattr(fd, &buf->tab) )	/* save the old one */
    fail;
#else
  if ( ioctl(fd, TIOCGETA, &buf->tab) )	/* save the old one */
    fail;
#endif

  tio = buf->tab;

  switch( mode )
  { case TTY_RAW:
#if defined(HAVE_TCSETATTR) && defined(HAVE_CFMAKERAW)
	cfmakeraw(&tio);
	tio.c_oflag = buf->tab.c_oflag;	/* donot change output modes */
	tio.c_lflag |= ISIG;
#else
	tio.c_lflag &= ~(ECHO|ICANON);
#endif
					/* OpenBSD requires this anyhow!? */
					/* Bug in OpenBSD or must we? */
					/* Could this do any harm? */
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

#ifdef HAVE_TCSETATTR
  if ( tcsetattr(fd, TCSANOW, &tio) != 0 )
  { static int MTOK_warned;			/* MT-OK */

    if ( !MTOK_warned++ )
      warning("Failed to set terminal: %s", OsError());
  }
#else
#ifdef TIOCSETAW
  ioctl(fd, TIOCSETAW, &tio);
#else
  ioctl(fd, TCSETAW, &tio);
  ioctl(fd, TCXONC, (void *)1);
#endif
#endif

  succeed;
}


bool
PopTty(IOSTREAM *s, ttybuf *buf)
{ int fd;
  ttymode = buf->mode;

  if ( (fd = Sfileno(s)) < 0 || !isatty(fd) )
    succeed;				/* not a terminal */
  if ( !trueFeature(TTY_CONTROL_FEATURE) )
    succeed;

#ifdef HAVE_TCSETATTR
  tcsetattr(fd, TCSANOW, &buf->tab);
#else
#ifdef TIOCSETA
  ioctl(fd, TIOCSETA, &buf->tab);
#else
  ioctl(fd, TCSETA, &buf->tab);
  ioctl(fd, TCXONC, (void *)1);
#endif
#endif

  succeed;
}

#else /* O_HAVE_TERMIO */

#ifdef HAVE_SGTTYB

bool
PushTty(IOSTREAM *s, ttybuf *buf, int mode)
{ struct sgttyb tio;
  int fd;

  buf->mode = ttymode;
  ttymode = mode;

  if ( (fd = Sfileno(s)) < 0 || !isatty(fd) )
    succeed;				/* not a terminal */
  if ( !trueFeature(TTY_CONTROL_FEATURE) )
    succeed;

  if ( ioctl(fd, TIOCGETP, &buf->tab) )  /* save the old one */
    fail;
  tio = buf->tab;

  switch( mode )
    { case TTY_RAW:
	tio.sg_flags |= CBREAK;
	tio.sg_flags &= ~ECHO;
	break;
      case TTY_OUTPUT:
	tio.sg_flags |= (CRMOD);
	break;
      case TTY_SAVE:
	succeed;
      default:
	sysError("Unknown PushTty() mode: %d", mode);
	/*NOTREACHED*/
      }
  
  
  ioctl(fd, TIOCSETP,  &tio);
  ioctl(fd, TIOCSTART, NULL);

  succeed;
}


bool
PopTty(IOSTREAM *s, ttybuf *buf)
{ ttymode = buf->mode;
  int fd;

  if ( (fd = Sfileno(s)) < 0 || !isatty(fd) )
    succeed;				/* not a terminal */
  if ( !trueFeature(TTY_CONTROL_FEATURE) )
    succeed;

  ioctl(fd, TIOCSETP,  &buf->tab);
  ioctl(fd, TIOCSTART, NULL);

  succeed;
}

#else /*HAVE_SGTTYB*/

bool
PushTty(IOSTREAM *s, ttybuf *buf, int mode)
{ buf->mode = ttymode;
  ttymode = mode;

  succeed;
}


bool
PopTty(IOSTREAM *s, ttybuf *buf)
{ ttymode = buf->mode;
  if ( ttymode != TTY_RAW )
    LD->prompt.next = TRUE;

  succeed;
}

#endif /*HAVE_SGTTYB*/
#endif /*O_HAVE_TERMIO*/


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

#ifndef __WIN32__
char *
getenv3(const char *name, char *buf, int len)
{ char *s = getenv(name);

  if ( s && strlen(s) < len )
  { strcpy(buf, s);

    return buf;
  }

  return NULL;
}

int
getenvl(const char *name)
{ char *s;

  if ( (s = getenv(name)) )
    return strlen(s);

  return -1;
}
#endif

#if HAVE_PUTENV

int
Setenv(char *name, char *value)
{ char *buf = alloca(strlen(name) + strlen(value) + 2);

  if ( buf )
  { Ssprintf(buf, "%s=%s", name, value);

    if ( putenv(store_string(buf)) < 0 )
      return PL_error("setenv", 2, NULL, ERR_NOMEM);
  } else
    return PL_error("setenv", 2, NULL, ERR_NOMEM);

  succeed;
}

int
Unsetenv(char *name)
{
#ifdef HAVE_UNSETENV
  unsetenv(name);

  succeed;
#else
  return Setenv(name, "");
#endif
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
{ growEnviron(environ, 0);
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
    int System(command)
	char *command;

    Invoke a command on the operating system.  The return value  is  the
    exit  status  of  the  command.   Return  value  0 implies succesful
    completion. If you are not running Unix your C-library might provide
    an alternative.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __unix__
#define SPECIFIC_SYSTEM 1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
According to the autoconf docs HAVE_SYS_WAIT_H   is set if sys/wait.h is
defined *and* is POSIX.1 compliant,  which   implies  it uses int status
argument to wait() 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_SYS_WAIT_H
#undef UNION_WAIT
#include <sys/wait.h>
#define wait_t int

#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#else /*HAVE_SYS_WAIT_H*/

#ifdef UNION_WAIT			/* Old BSD style wait */
#include <sys/wait.h>
#define wait_t union wait

#ifndef WEXITSTATUS
#define WEXITSTATUS(s) ((s).w_status)
#endif
#ifndef WTERMSIG
#define WTERMSIG(s) ((s).w_status)
#endif
#endif /*UNION_WAIT*/

#endif /*HAVE_SYS_WAIT_H*/


int
System(char *cmd)
{ int pid;
  char *shell = "/bin/sh";
  int rval;
  void (*old_int)();
  void (*old_stop)();
  unsigned char fds[256];
  int nfds = openFileDescriptors(fds, sizeof(fds));

  Setenv("PROLOGCHILD", "yes");

  if ( (pid = vfork()) == -1 )
  { return PL_error("shell", 2, OsError(), ERR_SYSCALL, "fork");
  } else if ( pid == 0 )		/* The child */
  { int i;

    for(i = 0; i < nfds; i++)
    { int fd = fds[i];

      if ( fd >= 3 )
	close(fd);
    }
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
    { term_t tmp = PL_new_term_ref();
      
      PL_put_atom_chars(tmp, cmd);
      PL_error("shell", 2, NULL, ERR_SHELL_FAILED, tmp);

      rval = 1;
    } else if (WIFEXITED(status))
    { rval = WEXITSTATUS(status);
#ifdef WIFSIGNALED
    } else if (WIFSIGNALED(status))
    { term_t tmp = PL_new_term_ref();
      int sig = WTERMSIG(status);
      
      PL_put_atom_chars(tmp, cmd);
      PL_error("shell", 2, NULL, ERR_SHELL_SIGNALLED, tmp, sig);
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

  return rval;
}
#endif /* __unix__ */

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
{ char     tmp[MANIPULATION];
  char	   path[MAXPATHLEN];
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
  if ( !(cmd_path = Which(path, tmp)) )
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
    char *Symbols(char *buf)

    Return the path name of the executable of SWI-Prolog. Used by the -c
    compiler to generate the #!<path> header line and by the incremental
    loader, who gives this path to ld, using ld -A <path>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __WIN32__			/* Win32 version in pl-nt.c */

char *
findExecutable(const char *av0, char *buffer)
{ char *file;
  char buf[MAXPATHLEN];
  char tmp[MAXPATHLEN];

  PrologPath(av0, buf);
  file = Which(buf, tmp);

#if __unix__				/* argv[0] can be an #! script! */
  if ( file )
  { int n, fd;
    char buf[MAXPATHLEN];

					/* Fails if mode is x-only, but */
					/* then it can't be a script! */
    if ( (fd = open(file, O_RDONLY)) < 0 )
      return strcpy(buffer, file);

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

	return strcpy(buffer, s);
      }
    }

    close(fd);
  }
#endif /*__unix__*/

  return strcpy(buffer, file ? file : buf);
}
#endif /*__WIN32__*/


#ifdef __unix__
static char *
okToExec(const char *s)
{ struct stat stbuff;

  if (statfunc(s, &stbuff) == 0 &&			/* stat it */
     (stbuff.st_mode & S_IFMT) == S_IFREG &&	/* check for file */
     access(s, X_OK) == 0)			/* can be executed? */
    return (char *)s;
  else
    return (char *) NULL;
}
#define PATHSEP	':'
#endif /* __unix__ */

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
okToExec(const char *s)
{ static char *extensions[] = EXEC_EXTENSIONS;
  static char **ext;

  DEBUG(2, Sdprintf("Checking %s\n", s));
  for(ext = extensions; *ext; ext++)
    if ( stripostfix(s, *ext) )
      return ExistsFile(s) ? (char *)s : (char *) NULL;

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
Which(const char *program, char *fullname)
{ char *path, *dir;
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
    { if ( (e = okToExec(program)) )
	return strcpy(fullname, e);
      else
        path++;				/* fix by Ron Hess (hess@sco.com) */
    } else
    { char tmp[MAXPATHLEN];

      for(dir = fullname; *path && *path != PATHSEP; *dir++ = *path++)
	;
      if (*path)
	path++;				/* skip : */
      if (strlen(fullname) + strlen(program)+2 > MAXPATHLEN)
        continue;
      *dir++ = '/';
      strcpy(dir, program);
      if ( (e = okToExec(OsPath(fullname, tmp))) )
	return strcpy(fullname, e);
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

#ifdef WIN32
#define PAUSE_DONE 1			/* see pl-nt.c */
#endif

#if !defined(PAUSE_DONE) && defined(HAVE_USLEEP)
#define PAUSE_DONE 1

void
Pause(real t)
{ usleep((unsigned long)(t * 1000000.0));
}

#endif /*HAVE_USLEEP*/


#if !defined(PAUSE_DONE) && defined(HAVE_SELECT)
#define PAUSE_DONE 1

void
Pause(real time)
{ struct timeval timeout;

  if ( time <= 0.0 )
    return;

  if ( time < 60.0 )		/* select() is expensive. Does it make sense */
  { timeout.tv_sec = (long) time;
    timeout.tv_usec = (long)(time * 1000000) % 1000000;
    select(32, NULL, NULL, NULL, &timeout);
  } else
    sleep( (int)(time+0.5) );
}

#endif /*HAVE_SELECT*/

#if !defined(PAUSE_DONE) && defined(HAVE_DOSSLEEP)
#define PAUSE_DONE 1

void                            /* a millisecond granualrity. */
Pause(time)                     /* the EMX function sleep uses a seconds */
real time;                      /* granularity only. */
{                               /* the select() trick does not work at all. */
  if ( time <= 0.0 )
    return;

  DosSleep((ULONG)(time * 1000));
}

#endif /*HAVE_DOSSLEEP*/

#if !defined(PAUSE_DONE) && defined(HAVE_SLEEP)
#define PAUSE_DONE 1

void
Pause(real t)
{ if ( t <= 0.5 )
    return;

  sleep((int)(t + 0.5));
}

#endif /*HAVE_SLEEP*/

#if !defined(PAUSE_DONE) && defined(HAVE_DELAY)
#define PAUSE_DONE 1

void
Pause(real t)
{ delay((int)(t * 1000));
}

#endif /*HAVE_DELAY*/

#if !defined(PAUSE_DONE) && defined(tos)
#define PAUSE_DONE 1

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

#endif /*tos*/

#ifndef PAUSE_DONE
void
Pause(real t)
{ Sdprintf("Pause(): not implemented\n");
}
#endif
