/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      Vu University Amsterdam

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

#define __WIN32__ 1
#if !defined(VC8) && (_MSC_VER >= 1400)	/* Visual studio 8 */
#define VC8 1				/* (aka Microsoft 2005 VC++ */
#endif

#define NOTTYCONTROL		TRUE	/* default -tty */
#define O_GMP			1

#if defined(O_PLMT) && !defined(_REENTRANT)
#define _REENTRANT 1
#endif

#define OPEN_MAX 32

#define HAVE_UXNT_H 1

#define HAVE_CLOCK 1			/* clock() timing function */

/* Define for emulating dlopen(), etc. using LoadLibrary */
#define EMULATE_DLOPEN 1

/* Define for the _xos_... functions */
#define O_XOS 1

/* Define O_RLC for the ../readline library */
#define O_RLC 1

/* Define for Windows DDE support */
#define O_DDE 1

/* Define for Windows DLL support */
#define O_DLL 1

/* Define if you disk-drives are special to you (DOS, Windows, OS/2) */
#define O_HASDRIVES 1

/* Define if you have shares using the notation //host/share */
#define O_HASSHARES 1

/* Maximum length of a path-name.  Note XOS! */
#define MAXPATHLEN 512

/* Format for int64_t */
#ifndef INT64_FORMAT
#define INT64_FORMAT "%I64d"
#endif

/* Define if floats are IEEE754 */
#define IEEE754 1

/* setenv comes from uxnt.c */
#define HAVE_SETENV 1


#ifdef __MINGW32__
#include "config.h"
#endif

/* FIXME: this is overriding what is in config.h. */
/* Define to make use of standard (UNIX98) pthread recursive mutexes */
#define RECURSIVE_MUTEXES 1

/* FIXME: this is overriding what is in config.h. */
/* Define if pthread has pthread_mutexattr_settype() */
#define HAVE_PTHREAD_MUTEXATTR_SETTYPE 1

#ifndef __MINGW32__
typedef unsigned long sigset_t;		/* we don't have this */
typedef int mode_t;

/* Define to 1 if you have the <shlobj.h> header file. */
#define HAVE_SHLOBJ_H 1

/* Define to 1 if you have the <dbghelp.h> header file. */
#define HAVE_DBGHELP_H 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

#define inline __inline

#define snprintf ms_snprintf		/* defined in pl-nt.c */

#ifdef O_GMP
#define HAVE_GMP_H 1
#endif

#ifdef __LCC__
#define NO_MS_EXTENSIONS 1
#endif

/* Define to extension used for shared objects if not "so" */
#define SO_EXT "dll"

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define if SIGPROF and setitimer() are available */
#define O_PROFILE 1

/* "Define if Prolog kernel is in shared object" */
#define O_SHARED_KERNEL 1

/* The number of bytes in a int.  */
#define SIZEOF_INT 4

/* The number of bytes in a long.  */
#define SIZEOF_LONG 4

/* The number of bytes in a wchar_t.  */
#define SIZEOF_WCHAR_T 2

/* Define if you have the access function.  */
#define HAVE_ACCESS 1

/* Define if you have the chmod function.  */
#define HAVE_CHMOD 1

/* Define if you have the fstat function.  */
#define HAVE_FSTAT 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getpid function.  */
#define HAVE_GETPID 1

/* Define if you have the ftime function.  */
#define HAVE_FTIME 1

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the opendir function.  */
#define HAVE_OPENDIR 1

/* Define if you have the popen function.  */
#define HAVE_POPEN 1

/* Define if you have the putenv function.  */
#define HAVE_PUTENV 1

/* Define if you have the remove function.  */
#define HAVE_REMOVE 1

/* Define if you have the rename function.  */
#define HAVE_RENAME 1

/* Define if you have the stricmp() function. */
#define HAVE_STRICMP 1

/* Define if you have the mbscasecoll() function. */
#define mbscasecoll mbsicoll
#define HAVE_MBSCASECOLL 1

/* Define if you have the strlwr() function */
#define HAVE_STRLWR 1

/* Define if you have the rl_insert_close function.  */
#define HAVE_RL_INSERT_CLOSE 1

/* Define if you have the select function.  */
#define HAVE_SELECT 1

/* Define if you have the signal function.  */
#define HAVE_SIGNAL 1

/* Define if you have the srand function.  */
#define HAVE_SRAND 1

/* Define if you have the stat function.  */
#define HAVE_STAT 1

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

/* Define to 1 if you have the `ceil' function. */
#define HAVE_CEIL  1

/* Define to 1 if you have the `floor' function. */
#define HAVE_FLOOR 1

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/stat.h> header file.  */
#define HAVE_SYS_STAT_H 1

/* Define if you have the m library (-lm).  */
#define HAVE_LIBM 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have `isnan' function */
#define HAVE_ISNAN 1

/* Define to 1 if you have `_fpclass' function */
#define HAVE__FPCLASS 1

/* Define to 1 if you have `signbit' function */
/* #undef HAVE_SIGNBIT 1 */

/* Define to 1 if you have <float.h> header */
#define HAVE_FLOAT_H 1

/* Define to 1 if you have the 'wcsxfrm' function. */
#define HAVE_WCSXFRM 1

#endif
