/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*  Modified (M) 1993 Dave Sherratt  */

/*#define O_DEBUG 1*/

#if OS2 && EMX
#include <os2.h>                /* this has to appear before pl-incl.h */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Solaris has asctime_r() with 3 arguments. Using _POSIX_PTHREAD_SEMANTICS
is supposed to give the POSIX standard one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(__sun__) || defined(__sun)
#define _POSIX_PTHREAD_SEMANTICS 1
#endif

#define __MINGW_USE_VC2005_COMPAT		/* Get Windows time_t as 64-bit */

#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include <math.h>
#include <stdio.h>		/* rename() and remove() prototypes */

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef O_XOS
#define statstruct struct _stati64
#else
#define statstruct struct stat
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
#include <time.h>
#include <fcntl.h>
#ifndef __WATCOMC__			/* appears a conflict */
#include <errno.h>
#endif

#if defined(__WATCOMC__)
#include <io.h>
#include <dos.h>
#endif

#if OS2 && EMX
static double initial_time;
#endif /* OS2 */

#define LOCK()   PL_LOCK(L_OS)
#define UNLOCK() PL_UNLOCK(L_OS)

static void	initExpand(void);
static void	cleanupExpand(void);
static void	initEnviron(void);

#ifndef DEFAULT_PATH
#define DEFAULT_PATH "/bin:/usr/bin"
#endif

		/********************************
		*         INITIALISATION        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool initOs()

    Initialise the OS dependant functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
initOs(void)
{ GET_LD

  GD->statistics.start_time = WallTime();

  DEBUG(1, Sdprintf("OS:initExpand() ...\n"));
  initExpand();
  DEBUG(1, Sdprintf("OS:initEnviron() ...\n"));
  initEnviron();

#ifdef __WINDOWS__
  setPrologFlagMask(PLFLAG_FILE_CASE_PRESERVING);
#else
  setPrologFlagMask(PLFLAG_FILE_CASE);
  setPrologFlagMask(PLFLAG_FILE_CASE_PRESERVING);
#endif

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
#ifdef __WINDOWS__
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

#ifndef __WINDOWS__			/* defined in pl-nt.c */

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

#ifdef HAVE_CLOCK_GETTIME
#define timespec_to_double(ts) \
	((double)(ts).tv_sec + (double)(ts).tv_nsec/(double)1000000000.0)
#endif

double
CpuTime(cputime_kind which)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_PROCESS_CPUTIME_ID)
#define CPU_TIME_DONE
  struct timespec ts;
  (void)which;

  if ( clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts) == 0 )
    return timespec_to_double(ts);
  return 0.0;
#endif

#if !defined(CPU_TIME_DONE) && defined(HAVE_TIMES)
#define CPU_TIME_DONE
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

#if !defined(CPU_TIME_DONE)
  (void)which;

  return 0.0;
#endif
}

#endif /*__WINDOWS__*/


double
WallTime(void)
{ double stime;

#if HAVE_CLOCK_GETTIME
  struct timespec tp;

  clock_gettime(CLOCK_REALTIME, &tp);
  stime = timespec_to_double(tp);
#else
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
#endif

  return stime;
}

		 /*******************************
		 *	      FEATURES		*
		 *******************************/

#ifndef __WINDOWS__			/* Windows version in pl-nt.c */

#ifdef HAVE_SC_NPROCESSORS_CONF
static int
CpuCount()
{
  return sysconf(_SC_NPROCESSORS_CONF);
}
#else

#ifdef PROCFS_CPUINFO
static int
CpuCount()
{ FILE *fd = fopen("/proc/cpuinfo", "r");

  if ( fd )
  { char buf[256];
    int count = 0;

    while(fgets(buf, sizeof(buf)-1, fd))
    { char *vp;

      if ( (vp = strchr(buf, ':')) )
      { char *en;

	for(en=vp; en > buf && en[-1] <= ' '; en--)
	  ;
	*en = EOS;
	DEBUG(2, Sdprintf("Got %s = %s\n", buf, vp+2));
	if ( streq("processor", buf) && isDigit(vp[2]) )
	{ int cpu = atoi(vp+2);

	  if ( cpu+1 > count )
	    count = cpu+1;
	}
      }
    }

    fclose(fd);
    return count;
  }

  return 0;
}

#else /*PROCFS_CPUINFO*/

#ifdef HAVE_SYSCTLBYNAME	/* MacOS X */

#include <sys/param.h>
#include <sys/sysctl.h>

int
CpuCount(void)
{ int     count ;
  size_t  size=sizeof(count) ;

  if ( sysctlbyname("hw.ncpu", &count, &size, NULL, 0) )
    return 0;

  return count;
}

#else

#define CpuCount() 0

#endif /*sysctlbyname*/

#endif /*PROCFS_CPUINFO*/

#endif /*HAVE_SC_NPROCESSORS_CONF*/


void
setOSPrologFlags(void)
{ int cpu_count = CpuCount();

  if ( cpu_count > 0 )
    PL_set_prolog_flag("cpu_count", PL_INTEGER, cpu_count);
}
#endif

		 /*******************************
		 *	       MEMORY		*
		 *******************************/

uintptr_t
UsedMemory(void)
{ GET_LD

#if defined(HAVE_GETRUSAGE) && defined(HAVE_RU_IDRSS)
  struct rusage usage;

  if ( getrusage(RUSAGE_SELF, &usage) == 0 &&
       usage.ru_idrss )
  { return usage.ru_idrss;		/* total unshared data */
  }
#endif

  return (usedStack(global) +
	  usedStack(local) +
	  usedStack(trail));
}


uintptr_t
FreeMemory(void)
{
#if defined(HAVE_GETRLIMIT) && defined(RLIMIT_DATA)
  uintptr_t used = UsedMemory();
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
    uint64_t _PL_Random()

    Return a random number. Used for arithmetic only. More trouble. On
    some systems (__WINDOWS__) the seed of rand() is thread-local, while on
    others it is global.  We appear to have the choice between

	# srand()/rand()
	Differ in MT handling, often bad distribution

	# srandom()/random()
	Not portable, not MT-Safe but much better distribution

	# drand48() and friends
	Depreciated according to Linux manpage, suggested by Solaris
	manpage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
setRandom(unsigned int *seedp)
{ unsigned int seed;

  if ( seedp )
  { seed = *seedp;
  } else
  {
#ifdef __WINDOWS__
     seed = (unsigned int)GetTickCount();
#else
#ifdef HAVE_GETTIMEOFDAY
     struct timeval tp;

     gettimeofday(&tp, NULL);
     seed = (unsigned int)(tp.tv_sec + tp.tv_usec);
#else
     seed = (unsigned int)time((time_t *) NULL);
#endif
#endif
  }

#ifdef HAVE_SRANDOM
  srandom(seed);
#else
#ifdef HAVE_SRAND
  srand(seed);
#endif
#endif
}

uint64_t
_PL_Random(void)
{ GET_LD

  if ( !LD->os.rand_initialised )
  { setRandom(NULL);
    LD->os.rand_initialised = TRUE;
  }

#ifdef HAVE_RANDOM
  { uint64_t l = random();

    l ^= (uint64_t)random()<<15;
    l ^= (uint64_t)random()<<30;
    l ^= (uint64_t)random()<<45;

    return l;
  }
#else
  { uint64_t l = rand();			/* 0<n<2^15-1 */

    l ^= (uint64_t)rand()<<15;
    l ^= (uint64_t)rand()<<30;
    l ^= (uint64_t)rand()<<45;

    return l;
  }
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

    atom_t TemporaryFile(const char *id, int *fdp)

    The return value of this call is an atom,  whose  string  represents
    the  path  name of a unique file that can be used as temporary file.
    `id' is a char * that can be used to make it easier to identify  the
    file as a specific kind of SWI-Prolog intermediate file.

    void RemoveTemporaryFiles()

    Remove all temporary files.  This function should be  aware  of  the
    fact  that some of the file names generated by TemporaryFile() might
    not be created at all, or might already have been deleted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef DEFTMPDIR
#ifdef __WINDOWS__
#define DEFTMPDIR "c:/tmp"
#else
#define DEFTMPDIR "/tmp"
#endif
#endif

static int
free_tmp_symbol(Symbol s)
{ int rc;
  atom_t tname = (atom_t)s->name;
  PL_chars_t txt;

  get_atom_text(tname, &txt);
  PL_mb_text(&txt, REP_FN);
  rc = RemoveFile(txt.text.t);
  PL_free_text(&txt);

  PL_unregister_atom(tname);
  return rc;
}


static void
void_free_tmp_symbol(Symbol s)
{ (void)free_tmp_symbol(s);
}


#ifndef O_EXCL
#define O_EXCL 0
#endif
#ifndef O_BINARY
#define O_BINARY 0
#endif

atom_t
TemporaryFile(const char *id, int *fdp)
{ char temp[MAXPATHLEN];
  static char *tmpdir = NULL;
  atom_t tname;
  int retries = 0;

  if ( !tmpdir )
  { LOCK();
    if ( !tmpdir )
    { char envbuf[MAXPATHLEN];
      char *td;

      if ( (td = Getenv("TEMP", envbuf, sizeof(envbuf))) ||
	   (td = Getenv("TMP",  envbuf, sizeof(envbuf))) )
	tmpdir = strdup(td);
      else
	tmpdir = DEFTMPDIR;
    }
    UNLOCK();
  }

retry:
#ifdef __unix__
{ static int MTOK_temp_counter = 0;
  const char *sep = id[0] ? "_" : "";

  Ssprintf(temp, "%s/pl_%s%s%d_%d",
	   tmpdir, id, sep, (int) getpid(), MTOK_temp_counter++);
}
#endif

#ifdef __WINDOWS__
{ char *tmp;
  static int temp_counter = 0;

#ifdef __LCC__
  if ( (tmp = tmpnam(NULL)) )
#else
  if ( (tmp = _tempnam(tmpdir, id)) )
#endif
  { PrologPath(tmp, temp, sizeof(temp));
  } else
  { const char *sep = id[0] ? "_" : "";

    Ssprintf(temp, "%s/pl_%s%s%d", tmpdir, id, sep, temp_counter++);
  }
}
#endif

  if ( fdp )
  { int fd;

    if ( (fd=open(temp, O_CREAT|O_EXCL|O_WRONLY|O_BINARY, 0600)) < 0 )
    { if ( ++retries < 10000 )
	goto retry;
      else
	return NULL_ATOM;
    }

    *fdp = fd;
  }

  tname = PL_new_atom(temp);		/* locked: ok! */

  LOCK();
  if ( !GD->os.tmp_files )
  { GD->os.tmp_files = newHTable(4);
    GD->os.tmp_files->free_symbol = void_free_tmp_symbol;
  }
  UNLOCK();

  addHTable(GD->os.tmp_files, (void*)tname, (void*)TRUE);

  return tname;
}


int
DeleteTemporaryFile(atom_t name)
{ int rc = FALSE;

  if ( GD->os.tmp_files )
  { LOCK();
    if ( GD->os.tmp_files && GD->os.tmp_files->size > 0 )
    { Symbol s = lookupHTable(GD->os.tmp_files, (void*)name);

      if ( s )
      { rc = free_tmp_symbol(s);
	deleteSymbolHTable(GD->os.tmp_files, s);
      }
    }
    UNLOCK();
  }

  return rc;
}


void
RemoveTemporaryFiles(void)
{ LOCK();
  if ( GD->os.tmp_files )
  { Table t = GD->os.tmp_files;

    GD->os.tmp_files = NULL;
    UNLOCK();
    destroyHTable(t);
  } else
  { UNLOCK();
  }
}


#if O_HPFS

/*  Conversion rules Prolog <-> OS/2 (using HPFS)
    / <-> \
    /x:/ <-> x:\  (embedded drive letter)
    No length restrictions up to MAXPATHLEN, no case conversions.
*/

char *
PrologPath(char *ospath, char *path, size_t len)
{ char *s = ospath, *p = path;
  int limit = len-1;

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
PrologPath(const char *p, char *buf, size_t len)
{ strncpy(buf, p, len);

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
PrologPath(const char *p, char *buf, size_t len)
{ GET_LD
  int flags = (truePrologFlag(PLFLAG_FILE_CASE) ? 0 : XOS_DOWNCASE);

  return _xos_canonical_filename(p, buf, len, flags);
}

char *
OsPath(const char *p, char *buf)
{ strcpy(buf, p);

  return buf;
}
#endif /* O_XOS */


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
#define O_CANONICALISE_DIRS

struct canonical_dir
{ char *	name;			/* name of directory */
  char *	canonical;		/* canonical name of directory */
  dev_t		device;			/* device number */
  ino_t		inode;			/* inode number */
  CanonicalDir  next;			/* next in chain */
};

#define canonical_dirlist (GD->os._canonical_dirlist)

forwards char   *canonicaliseDir(char *);
#endif /*O_CANONICALISE_DIRS*/

static void
initExpand(void)
{
#ifdef O_CANONICALISE_DIRS
  char *dir;
  char *cpaths;
#endif

  GD->paths.CWDdir = NULL;
  GD->paths.CWDlen = 0;

#ifdef O_CANONICALISE_DIRS
{ char envbuf[MAXPATHLEN];

  if ( (cpaths = Getenv("CANONICAL_PATHS", envbuf, sizeof(envbuf))) )
  { char buf[MAXPATHLEN];

    while(*cpaths)
    { char *e;

      if ( (e = strchr(cpaths, ':')) )
      { int l = e-cpaths;

	strncpy(buf, cpaths, l);
	buf[l] = EOS;
	cpaths += l+1;
	canonicaliseDir(buf);
      } else
      { canonicaliseDir(cpaths);
	break;
      }
    }
  }

  if ( (dir = Getenv("HOME", envbuf, sizeof(envbuf))) ) canonicaliseDir(dir);
  if ( (dir = Getenv("PWD",  envbuf, sizeof(envbuf))) ) canonicaliseDir(dir);
  if ( (dir = Getenv("CWD",  envbuf, sizeof(envbuf))) ) canonicaliseDir(dir);
}
#endif
}

#ifdef O_CANONICALISE_DIRS

static void
cleanupExpand(void)
{ CanonicalDir dn = canonical_dirlist, next;

  canonical_dirlist = NULL;
  for( ; dn; dn = next )
  { next = dn->next;
    if ( dn->canonical && dn->canonical != dn->name )
      remove_string(dn->canonical);
    remove_string(dn->name);
    PL_free(dn);
  }
  if ( GD->paths.CWDdir )
  { remove_string(GD->paths.CWDdir);
    GD->paths.CWDdir = NULL;
    GD->paths.CWDlen = 0;
  }
}


static void
registerParentDirs(const char *path)
{ const char *e = path + strlen(path);

  while(e>path)
  { char dirname[MAXPATHLEN];
    char tmp[MAXPATHLEN];
    CanonicalDir d;
    statstruct buf;

    for(e--; *e != '/' && e > path + 1; e-- )
      ;

    strncpy(dirname, path, e-path);
    dirname[e-path] = EOS;

    for(d = canonical_dirlist; d; d = d->next)
    { if ( streq(d->name, dirname) )
	return;
    }

    if ( statfunc(OsPath(dirname, tmp), &buf) == 0 )
    { CanonicalDir dn   = PL_malloc(sizeof(*dn));

      dn->name		= store_string(dirname);
      dn->inode		= buf.st_ino;
      dn->device	= buf.st_dev;
      dn->canonical	= dn->name;
      dn->next		= canonical_dirlist;
      canonical_dirlist	= dn;

      DEBUG(1, Sdprintf("Registered canonical dir %s\n", dirname));
    } else
      return;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verify_entry() verifies the path cache for this   path is still safe. If
not it updates the cache and returns FALSE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
verify_entry(CanonicalDir d)
{ char tmp[MAXPATHLEN];
  statstruct buf;

  if ( statfunc(OsPath(d->canonical, tmp), &buf) == 0 )
  { if ( d->inode  == buf.st_ino &&
	 d->device == buf.st_dev )
      return TRUE;

    DEBUG(1, Sdprintf("%s: inode/device changed\n", d->canonical));

    d->inode  = buf.st_ino;
    d->device = buf.st_dev;
    return TRUE;
  } else
  { DEBUG(1, Sdprintf("%s: no longer exists\n", d->canonical));

    if ( d == canonical_dirlist )
    { canonical_dirlist = d->next;
    } else
    { CanonicalDir cd;

      for(cd=canonical_dirlist; cd; cd=cd->next)
      { if ( cd->next == d )
	{ cd->next = d->next;
	  break;
	}
      }
    }

    remove_string(d->name);
    if ( d->canonical != d->name )
      remove_string(d->canonical);
    PL_free(d);
  }

  return FALSE;
}


static char *
canonicaliseDir(char *path)
{ CanonicalDir d, next;
  statstruct buf;
  char tmp[MAXPATHLEN];

  DEBUG(1, Sdprintf("canonicaliseDir(%s) --> ", path));

  for(d = canonical_dirlist; d; d = next)
  { next = d->next;

    if ( streq(d->name, path) && verify_entry(d) )
    { if ( d->name != d->canonical )
	strcpy(path, d->canonical);

      DEBUG(1, Sdprintf("(lookup) %s\n", path));
      return path;
    }
  }

					/* we need to use malloc() here */
					/* because allocHeapOrHalt() only ensures */
					/* alignment for `word', and inode_t */
					/* is sometimes bigger! */

  if ( statfunc(OsPath(path, tmp), &buf) == 0 )
  { CanonicalDir dn = PL_malloc(sizeof(*dn));
    char dirname[MAXPATHLEN];
    char *e = path + strlen(path);

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

      for(d = canonical_dirlist; d; d = next)
      { next = d->next;

	if ( d->inode == buf.st_ino && d->device == buf.st_dev &&
	     verify_entry(d) )
	{ DEBUG(2, Sdprintf("Hit with %s (dev=%d,ino=%d)\n",
			    d->canonical, d->device, d->inode));

	  strcpy(dirname, d->canonical);
	  strcat(dirname, e);
	  strcpy(path, dirname);
	  dn->canonical = store_string(path);
	  dn->next = canonical_dirlist;
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
    dn->next = canonical_dirlist;
    canonical_dirlist = dn;

    DEBUG(1, Sdprintf("(new, existing) %s\n", path));
    registerParentDirs(path);
    return path;
  }

  DEBUG(1, Sdprintf("(nonexisting) %s\n", path));
  return path;
}

#else

#define canonicaliseDir(d)

static void
cleanupExpand(void)
{
}

#endif /*O_CANONICALISE_DIRS*/


char *
canonicaliseFileName(char *path)
{ char *out = path, *in = path, *start = path;
  tmp_buffer saveb;

#ifdef O_HASDRIVES			/* C: */
  if ( in[1] == ':' && isLetter(in[0]) )
  { in += 2;

    out = start = in;
  }
#ifdef __MINGW32__ /* /c/ in MINGW is the same as c: */
  else if ( in[0] == '/' && isLetter(in[1]) &&
	    in[2] == '/' )
  {
    out[0] = in[1];
    out[1] = ':';
    in += 3;
    out = start = in;
  }
#endif
#endif

#ifdef O_HASSHARES			/* //host/ */
  if ( in[0] == '/' && in[1] == '/' && isAlpha(in[2]) )
  { char *s;

    for(s = in+3; *s && (isAlpha(*s) || *s == '-' || *s == '.'); s++)
      ;
    if ( *s == '/' )
    { in = out = s+1;
      start = in-1;
    }
  }
#endif

  while( in[0] == '/' && in[1] == '.' && in[2] == '.' && in[3] == '/' )
    in += 3;
  while( in[0] == '.' && in[1] == '/' )
    in += 2;
  if ( in[0] == '/' )
    *out++ = '/';
  initBuffer(&saveb);
  addBuffer(&saveb, out, char*);

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
	    goto out;
	  }
	  if ( in[2] == '.' && (in[3] == '/' || in[3] == EOS) )
	  { if ( !isEmptyBuffer(&saveb) )		/* delete /foo/../ */
	    { out = popBuffer(&saveb, char*);
	      in += 3;
	      if ( in[0] == EOS && out > start+1 )
	      { out[-1] = EOS;		/* delete trailing / */
		goto out;
	      }
	      goto again;
	    } else if (	start[0] == '/' && out == start+1 )
	    { in += 3;
	      goto again;
	    }
	  }
	}
      }
      if ( *in )
	in++;
      if ( out > path && out[-1] != '/' )
	*out++ = '/';
      addBuffer(&saveb, out, char*);
    } else
      *out++ = *in++;
  }
  *out++ = *in++;

out:
  discardBuffer(&saveb);

  return path;
}


static char *
utf8_strlwr(char *s)
{ char tmp[MAXPATHLEN];
  char *o, *i;

  strcpy(tmp, s);
  for(i=tmp, o=s; *i; )
  { int c;

    i = utf8_get_char(i, &c);
    c = towlower((wint_t)c);
    o = utf8_put_char(o, c);
  }
  *o = EOS;

  return s;
}


char *
canonicalisePath(char *path)
{ GET_LD

  if ( !truePrologFlag(PLFLAG_FILE_CASE) )
    utf8_strlwr(path);

  canonicaliseFileName(path);

#ifdef O_CANONICALISE_DIRS
{ char *e;
  char dirname[MAXPATHLEN];
  size_t plen = strlen(path);

  if ( plen > 0 )
  { e = path + plen - 1;
    for( ; *e != '/' && e > path; e-- )
      ;
    strncpy(dirname, path, e-path);
    dirname[e-path] = EOS;
    canonicaliseDir(dirname);
    strcat(dirname, e);
    strcpy(path, dirname);
  }
}
#endif

  return path;
}


static char *
takeWord(const char **string, char *wrd, int maxlen)
{ const char *s = *string;
  char *q = wrd;
  int left = maxlen-1;

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


char *
expandVars(const char *pattern, char *expanded, int maxlen)
{ GET_LD
  int size = 0;
  char wordbuf[MAXPATHLEN];
  char *rc = expanded;

  if ( *pattern == '~' )
  { char *user;
    char *value;
    int l;

    pattern++;
    user = takeWord(&pattern, wordbuf, sizeof(wordbuf));
    LOCK();

    if ( user[0] == EOS )		/* ~/bla */
    {
#ifdef O_XOS
      value = _xos_home();
#else /*O_XOS*/
      if ( !(value = GD->os.myhome) )
      { char envbuf[MAXPATHLEN];

	if ( (value = Getenv("HOME", envbuf, sizeof(envbuf))) &&
	     (value = PrologPath(value, wordbuf, sizeof(wordbuf))) )
	{ GD->os.myhome = store_string(value);
	} else
	{ value = GD->os.myhome = store_string("/");
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
	{ if ( truePrologFlag(PLFLAG_FILEERRORS) )
	  { term_t name = PL_new_term_ref();

	    PL_put_atom_chars(name, user);
	    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_user, name);
	  }
	  UNLOCK();
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
    { if ( truePrologFlag(PLFLAG_FILEERRORS) )
	PL_error(NULL, 0, NULL, ERR_NOT_IMPLEMENTED, "user_info");

      UNLOCK();
      fail;
    }
#endif
    size += (l = (int) strlen(value));
    if ( size+1 >= maxlen )
    { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_path_length);
      return NULL;
    }
    strcpy(expanded, value);
    expanded += l;
    UNLOCK();

					/* ~/ should not become // */
    if ( expanded[-1] == '/' && pattern[0] == '/' )
      pattern++;
  }

  for( ;; )
  { int c = *pattern++;

    switch( c )
    { case EOS:
	break;
      case '$':
	{ char envbuf[MAXPATHLEN];
	  char *var = takeWord(&pattern, wordbuf, sizeof(wordbuf));
	  char *value;
	  int l;

	  if ( var[0] == EOS )
	    goto def;
	  LOCK();
	  value = Getenv(var, envbuf, sizeof(envbuf));
	  if ( value == (char *) NULL )
	  { if ( truePrologFlag(PLFLAG_FILEERRORS) )
	    { term_t name = PL_new_term_ref();

	      PL_put_atom_chars(name, var);
	      PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_variable, name);
	    }

	    UNLOCK();
	    fail;
	  }
	  size += (l = (int)strlen(value));
	  if ( size+1 >= maxlen )
	  { UNLOCK();
	    PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
		     ATOM_max_path_length);
	    return NULL;
	  }
	  strcpy(expanded, value);
	  UNLOCK();

	  expanded += l;

	  continue;
	}
      default:
      def:
	size++;
	if ( size+1 >= maxlen )
	{ PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
		   ATOM_max_path_length);
	  return NULL;
	}
	*expanded++ = c;

	continue;
    }
    break;
  }

  if ( ++size >= maxlen )
  { PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
	     ATOM_max_path_length);
    return NULL;
  }

  *expanded = EOS;

  return rc;
}


#ifdef O_HASDRIVES

#define IS_DIR_SEPARATOR(c) ((c) == '/' || (c) == '\\')

int
IsAbsolutePath(const char *p)				/* /d:/ */
{ if ( p[0] == '/' && p[2] == ':' && isLetter(p[1]) &&
       (p[3] == '/' || p[3] == '\0') )
    succeed;

#ifdef __MINGW32__ /* /c/ in MINGW is the same as c: */
  if ( p[0] == '/' && isLetter(p[1]) &&
       (p[2] == '/' || p[2] == '\0') )
    succeed;
#endif

  if ( p[1] == ':' && isLetter(p[0]) &&			/* d:/ or d:\ */
       (IS_DIR_SEPARATOR(p[2]) || p[2] == '\0') )
    succeed;

#ifdef O_HASSHARES
  if ( (p[0] == '/' && p[1] == '/') ||	/* //host/share */
       (p[0] == '\\' && p[1] == '\\') )	/* \\host\share */
    succeed;
#endif

  fail;
}


static inline int
isDriveRelativePath(const char *p)	/* '/...' */
{ return IS_DIR_SEPARATOR(p[0]) && !IsAbsolutePath(p);
}

#ifdef __WINDOWS__
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
#ifdef __WINDOWS__
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
{ GET_LD
  char tmp[MAXPATHLEN];
  char buf[MAXPATHLEN];
  char *file = PrologPath(spec, buf, sizeof(buf));

  if ( !file )
     return (char *) NULL;
  if ( truePrologFlag(PLFLAG_FILEVARS) )
  { if ( !(file = expandVars(buf, tmp, sizeof(tmp))) )
      return (char *) NULL;
  }

  if ( IsAbsolutePath(file) )
  { strcpy(path, file);

    return canonicalisePath(path);
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
    return canonicalisePath(path);
  }
#endif /*O_HASDRIVES*/

  if ( !PL_cwd(path, MAXPATHLEN) )
    return NULL;

  if ( (GD->paths.CWDlen + strlen(file) + 1) >= MAXPATHLEN )
  { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_path_length);
    return (char *) NULL;
  }

  strcpy(path, GD->paths.CWDdir);
  strcpy(&path[GD->paths.CWDlen], file);

  return canonicalisePath(path);
}


void
PL_changed_cwd(void)
{ LOCK();
  if ( GD->paths.CWDdir )
    remove_string(GD->paths.CWDdir);
  GD->paths.CWDdir = NULL;
  GD->paths.CWDlen = 0;
  UNLOCK();
}


static char *
cwd_unlocked(char *cwd, size_t cwdlen)
{ GET_LD

  if ( GD->paths.CWDlen == 0 )
  { char buf[MAXPATHLEN];
    char *rval;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On SunOs, getcwd() is using popen() to read the output of /bin/pwd.  This
is slow and appears not to cooperate with profile/3.  getwd() is supposed
to be implemented directly.  What about other Unixes?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(HAVE_GETWD) && (defined(__sun__) || defined(__sun))
#undef HAVE_GETCWD
#endif

#if defined(HAVE_GETWD) && !defined(HAVE_GETCWD)
    rval = getwd(buf);
#else
    rval = getcwd(buf, sizeof(buf));
#endif
    if ( !rval )
    { term_t tmp = PL_new_term_ref();

      PL_put_atom(tmp, ATOM_dot);
      PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
	       ATOM_getcwd, ATOM_directory, tmp);

      return NULL;
    }

    canonicalisePath(buf);
    GD->paths.CWDlen = strlen(buf);
    buf[GD->paths.CWDlen++] = '/';
    buf[GD->paths.CWDlen] = EOS;

    if ( GD->paths.CWDdir )
      remove_string(GD->paths.CWDdir);
    GD->paths.CWDdir = store_string(buf);
  }

  if ( GD->paths.CWDlen < cwdlen )
  { memcpy(cwd, GD->paths.CWDdir, GD->paths.CWDlen+1);
    return cwd;
  } else
  { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_path_length);
    return NULL;
  }
}


char *
PL_cwd(char *cwd, size_t cwdlen)
{ char *rc;

  LOCK();
  rc = cwd_unlocked(cwd, cwdlen);
  UNLOCK();

  return rc;
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
  { if ( dir != f )			/* otherwise it is in-place */
      strncpy(dir, f, base-f);
    dir[base-f] = EOS;
  }

  return dir;
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
       (GD->paths.CWDdir && streq(path, GD->paths.CWDdir)) )
    succeed;

  AbsoluteFile(path, tmp);

  if ( chdir(ospath) == 0 )
  { size_t len;

    len = strlen(tmp);
    if ( len == 0 || tmp[len-1] != '/' )
    { tmp[len++] = '/';
      tmp[len] = EOS;
    }
    LOCK();					/* Lock with PL_changed_cwd() */
    GD->paths.CWDlen = len;			/* and PL_cwd() */
    if ( GD->paths.CWDdir )
      remove_string(GD->paths.CWDdir);
    GD->paths.CWDdir = store_string(tmp);
    UNLOCK();

    succeed;
  }

  fail;
}


		/********************************
		*        TIME CONVERSION        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    struct tm *PL_localtime_r(time_t time, struct tm *r)

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

    time_t Time()

    Return time in seconds after Jan 1 1970 (Unix' time notion).

Note: MinGW has localtime_r(),  but  it  is   not  locked  and  thus not
thread-safe. MinGW does not have localtime_s(), but   we  test for it in
configure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct tm *
PL_localtime_r(const time_t *t, struct tm *r)
{
#ifdef HAVE_LOCALTIME_R
  return localtime_r(t, r);
#else
#ifdef HAVE_LOCALTIME_S
  return localtime_s(r, t) == EINVAL ? NULL : t;
#else
  struct tm *rc;

  LOCK();
  if ( (rc = localtime(t)) )
    *r = *rc;
  else
    r = NULL;
  UNLOCK();

  return r;
#endif
#endif
}

char *
PL_asctime_r(const struct tm *tm, char *buf)
{
#ifdef HAVE_ASCTIME_R
  return asctime_r(tm, buf);
#else
  char *rc;

  LOCK();
  if ( (rc = asctime(tm)) )
    strcpy(buf, rc);
  else
    buf = NULL;
  UNLOCK();

  return buf;
#endif
}


		 /*******************************
		 *	      TERMINAL		*
		 *******************************/

#ifdef HAVE_TCSETATTR
#include <termios.h>
#include <unistd.h>
#define O_HAVE_TERMIO 1
#else /*HAVE_TCSETATTR*/
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#define termios termio
#define O_HAVE_TERMIO 1
#else
#ifdef HAVE_SYS_TERMIOS_H
#include <sys/termios.h>
#define O_HAVE_TERMIO 1
#endif
#endif
#endif /*HAVE_TCSETATTR*/

typedef struct tty_state
{
#if defined(O_HAVE_TERMIO)
  struct termios tab;
#elif defined(HAVE_SGTTYB)
  struct sgttyb tab;
#else
  int tab;				/* empty is not allowed */
#endif
} tty_state;

#define TTY_STATE(buf) (((tty_state*)(buf->state))->tab)

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
ResetStdin(void)
{ Sinput->limitp = Sinput->bufp = Sinput->buffer;
  if ( !GD->os.org_terminal.read )
    GD->os.org_terminal = *Sinput->functions;
}

static ssize_t
Sread_terminal(void *handle, char *buf, size_t size)
{ GET_LD
  intptr_t h = (intptr_t)handle;
  int fd = (int)h;
  source_location oldsrc = LD->read_source;

  if ( Soutput && true(Soutput, SIO_ISATTY) )
  { if ( LD->prompt.next && ttymode != TTY_RAW )
      PL_write_prompt(TRUE);
    else
      Sflush(Suser_output);
  }

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

  return size;
}

void
ResetTty(void)
{ GET_LD
  startCritical;
  ResetStdin();

  if ( !GD->os.iofunctions.read )
  { GD->os.iofunctions       = *Sinput->functions;
    GD->os.iofunctions.read  = Sread_terminal;

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
{ GET_LD
  struct termios tio;
  int fd;

  buf->mode  = ttymode;
  buf->state = NULL;
  ttymode    = mode;

  if ( (fd = Sfileno(s)) < 0 || !isatty(fd) )
    succeed;				/* not a terminal */
  if ( !truePrologFlag(PLFLAG_TTY_CONTROL) )
    succeed;

  buf->state = allocHeapOrHalt(sizeof(tty_state));

#ifdef HAVE_TCSETATTR
  if ( tcgetattr(fd, &TTY_STATE(buf)) )	/* save the old one */
    fail;
#else
  if ( ioctl(fd, TIOCGETA, &TTY_STATE(buf)) )	/* save the old one */
    fail;
#endif

  tio = TTY_STATE(buf);

  switch( mode )
  { case TTY_RAW:
#if defined(HAVE_TCSETATTR) && defined(HAVE_CFMAKERAW)
	cfmakeraw(&tio);
	tio.c_oflag = TTY_STATE(buf).c_oflag;	/* donot change output modes */
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
PopTty(IOSTREAM *s, ttybuf *buf, int do_free)
{ ttymode = buf->mode;

  if ( buf->state )
  { int fd = Sfileno(s);

    if ( fd >= 0 )
    {
#ifdef HAVE_TCSETATTR
      tcsetattr(fd, TCSANOW, &TTY_STATE(buf));
#else
#ifdef TIOCSETA
      ioctl(fd, TIOCSETA, &TTY_STATE(buf));
#else
      ioctl(fd, TCSETA, &TTY_STATE(buf));
      ioctl(fd, TCXONC, (void *)1);
#endif
#endif
    }

    if ( do_free )
    { freeHeap(buf->state, sizeof(tty_state));
      buf->state = NULL;
    }
  }

  succeed;
}

#else /* O_HAVE_TERMIO */

#ifdef HAVE_SGTTYB

bool
PushTty(IOSTREAM *s, ttybuf *buf, int mode)
{ struct sgttyb tio;
  int fd;

  buf->mode = ttymode;
  buf->state = NULL;
  ttymode = mode;

  if ( (fd = Sfileno(s)) < 0 || !isatty(fd) )
    succeed;				/* not a terminal */
  if ( !truePrologFlag(PLFLAG_TTY_CONTROL) )
    succeed;

  buf->state = allocHeapOrHalt(sizeof(tty_state));

  if ( ioctl(fd, TIOCGETP, &TTY_STATE(buf)) )  /* save the old one */
    fail;
  tio = TTY_STATE(buf);

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
PopTty(IOSTREAM *s, ttybuf *buf, int do_free)
{ ttymode = buf->mode;

  if ( buf->state )
  { int fd = Sfileno(s);

    if ( fd >= 0 )
    { ioctl(fd, TIOCSETP,  &buf->tab);
      ioctl(fd, TIOCSTART, NULL);
    }

    if ( do_free )
    { freeHeap(buf->state, sizeof(tty_state));
      buf->state = NULL;
    }
  }

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
PopTty(IOSTREAM *s, ttybuf *buf, int do_free)
{ GET_LD
  ttymode = buf->mode;
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
Simple  library  to  manipulate  the    OS   environment.  The  modified
environment will be passed to  child  processes   and  the  can  also be
requested via getenv/2 from Prolog. Functions

    int Setenv(name, value)
         char *name, *value;

    Set the OS environment variable with name `name'.   If  it  exists
    its  value  is  changed, otherwise a new entry in the environment is
    created.  The return value is a pointer to the old value, or NULL if
    the variable is new.

    int Unsetenv(name)
         char *name;

    Delete a variable from the environment.  Return  value  is  the  old
    value, or NULL if the variable did not exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

size_t
getenv3(const char *name, char *buf, size_t len)
{
#if O_XOS
  return _xos_getenv(name, buf, len);
#else
  char *s = getenv(name);
  size_t l;

  if ( s )
  { if ( (l=strlen(s)) < len )
      memcpy(buf, s, l+1);
    else if ( len > 0 )
      buf[0] = EOS;                     /* empty string if not fit */

    return l;
  }

  return (size_t)-1;
#endif
}


char *
Getenv(const char *name, char *buf, size_t len)
{ size_t l = getenv3(name, buf, len);

  if ( l != (size_t)-1 && l < len )
    return buf;

  return NULL;
}


#if defined(HAVE_PUTENV) || defined(HAVE_SETENV)

int
Setenv(char *name, char *value)
{
#ifdef HAVE_SETENV
  if ( setenv(name, value, TRUE) != 0 )
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "setenv");
#else
  char *buf;

  if ( *name == '\0' || strchr(name, '=') != NULL )
  { errno = EINVAL;
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "setenv");
  }

  buf = alloca(strlen(name) + strlen(value) + 2);

  if ( buf )
  { Ssprintf(buf, "%s=%s", name, value);

    if ( putenv(store_string(buf)) < 0 )
      return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "setenv");
  } else
    return PL_error(NULL, 0, NULL, ERR_NOMEM);
#endif
  succeed;
}

int
Unsetenv(char *name)
{
#ifdef HAVE_UNSETENV
#ifdef VOID_UNSETENV
  unsetenv(name);
#else
  if ( unsetenv(name) < 0 )
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "unsetenv");
#endif

  succeed;
#else
  if ( !getenv(name) )
    succeed;

  return Setenv(name, "");
#endif
}

static void
initEnviron()
{
}

#else /*HAVE_PUTENV*/

extern char **environ;		/* Unix predefined environment */

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
  { char **env, **e1, **e2;

    for(e1=e, filled=0; *e1; e1++, filled++)
      ;
    size = ROUND(filled+10+amount, 32);
    env = (char **)PL_malloc(size * sizeof(char *));
    for ( e1=e, e2=env; *e1; *e2++ = *e1++ )
      ;
    *e2 = (char *) NULL;
    filled += amount;

    return env;
  }

  filled += amount;
  if ( filled + 1 > size )
  { char **env, **e1, **e2;

    size += 32;
    env = (char **)PL_realloc(e, size * sizeof(char *));
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
matchName(const char *e, const char *name)
{ while( *name && *e == *name )
    e++, name++;

  if ( (*e == '=' || *e == EOS) && *name == EOS )
    return (*e == '=' ? e+1 : e);

  return (char *) NULL;
}


static void
setEntry(char **e, char *name, char *value)
{ size_t l = strlen(name);

  *e = PL_malloc_atomic(l + strlen(value) + 2);
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
{ GET_LD
  int pid;
  char *shell = "/bin/sh";
  int rval;
  void (*old_int)();
  void (*old_stop)();

  if ( (pid = fork()) == -1 )
  { return PL_error("shell", 2, OsError(), ERR_SYSCALL, "fork");
  } else if ( pid == 0 )		/* The child */
  { Setenv("PROLOGCHILD", "yes");
    PL_cleanup_fork();
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

    for(;;)
    {
#ifdef HAVE_WAITPID
      n = waitpid(pid, &status, 0);
#else
      n = wait(&status);
#endif
      if ( n == -1 && errno == EINTR )
	continue;
      if ( n != pid )
	continue;
      break;
    }

    if ( n == -1 )
    { term_t tmp = PL_new_term_ref();

      PL_put_atom_chars(tmp, cmd);
      PL_error("shell", 2, MSG_ERRNO, ERR_SHELL_FAILED, tmp);

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


#ifdef __WINDOWS__
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
    char *findExecutable(char *buf)

    Return the path name of the executable of SWI-Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __WINDOWS__			/* Win32 version in pl-nt.c */
static char *	Which(const char *program, char *fullname);

char *
findExecutable(const char *av0, char *buffer)
{ char *file;
  char buf[MAXPATHLEN];
  char tmp[MAXPATHLEN];

  if ( !av0 || !PrologPath(av0, buf, sizeof(buf)) )
    return NULL;
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

#ifdef __unix__
static char *
okToExec(const char *s)
{ statstruct stbuff;

  if (statfunc(s, &stbuff) == 0 &&	/* stat it */
     S_ISREG(stbuff.st_mode) &&		/* check for file */
     access(s, X_OK) == 0)		/* can be executed? */
    return (char *)s;
  else
    return (char *) NULL;
}
#define PATHSEP	':'
#endif /* __unix__ */

#if defined(OS2) || defined(__DOS__) || defined(__WINDOWS__)
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
      if ((dir-fullname) + strlen(program)+2 > MAXPATHLEN)
        continue;
      *dir++ = '/';
      strcpy(dir, program);
      if ( (e = okToExec(OsPath(fullname, tmp))) )
	return strcpy(fullname, e);
    }
  }

  return NULL;
}

#endif /*__WINDOWS__*/

/** int Pause(double time)

Suspend execution `time' seconds. Time  is   given  as  a floating point
number, expressing the time  to  sleep   in  seconds.  Just  about every
platform requires it own implementation. We provide them in the order of
preference. The implementations differ on  their granularity and whether
or not they can  be  interrupted   savely  restarted.  The  recent POSIX
nanosleep() is just about the  only   function  that  really works well:
accurate, interruptable and restartable.
*/

#ifdef __WINDOWS__
#define PAUSE_DONE 1			/* see pl-nt.c */
#endif

#if !defined(PAUSE_DONE) && defined(HAVE_NANOSLEEP)
#define PAUSE_DONE 1

int
Pause(double t)
{ struct timespec req;
  int rc;

  if ( t < 0.0 )
    succeed;

  req.tv_sec = (time_t) t;
  req.tv_nsec = (long)((t - floor(t)) * 1000000000);

  for(;;)
  { rc = nanosleep(&req, &req);
    if ( rc == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    } else
      return TRUE;
  }
}

#endif /*HAVE_NANOSLEEP*/


#if !defined(PAUSE_DONE) && defined(HAVE_USLEEP)
#define PAUSE_DONE 1

int
Pause(double t)
{ if ( t <= 0.0 )
    return TRUE;

  usleep((unsigned long)(t * 1000000.0));

  return TRUE;
}

#endif /*HAVE_USLEEP*/


#if !defined(PAUSE_DONE) && defined(HAVE_SELECT)
#define PAUSE_DONE 1

int
Pause(double time)
{ struct timeval timeout;

  if ( time <= 0.0 )
    return;

  if ( time < 60.0 )		/* select() is expensive. Does it make sense */
  { timeout.tv_sec = (long)time;
    timeout.tv_usec = (long)(time * 1000000) % 1000000;
    select(32, NULL, NULL, NULL, &timeout);

    return TRUE;
  } else
  { int rc;
    int left = (int)(time+0.5);

    do
    { rc = sleep(left);
      if ( rc == -1 && errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	  return FALSE;

	return TRUE;
      }
      left -= rc;
    } while ( rc != 0 );
  }
}

#endif /*HAVE_SELECT*/

#if !defined(PAUSE_DONE) && defined(HAVE_DOSSLEEP)
#define PAUSE_DONE 1

int					/* a millisecond granualrity. */
Pause(double time)			/* the EMX function sleep uses seconds */
{ if ( time <= 0.0 )			/* the select() trick does not work at all. */
    return TRUE;

  DosSleep((ULONG)(time * 1000));

  return TRUE;
}

#endif /*HAVE_DOSSLEEP*/

#if !defined(PAUSE_DONE) && defined(HAVE_SLEEP)
#define PAUSE_DONE 1

int
Pause(double t)
{ if ( t <= 0.5 )
    succeed;

  sleep((int)(t + 0.5));

  succeed;
}

#endif /*HAVE_SLEEP*/

#if !defined(PAUSE_DONE) && defined(HAVE_DELAY)
#define PAUSE_DONE 1

int
Pause(double t)
{ delay((int)(t * 1000));

  return TRUE;
}

#endif /*HAVE_DELAY*/

#ifndef PAUSE_DONE
int
Pause(double t)
{ return notImplemented("sleep", 1);
}
#endif

