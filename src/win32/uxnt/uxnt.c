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

#define _UXNT_KERNEL 1
#include "uxnt.h"			/* my prototypes */

#include <windows.h>
#include "dirent.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#undef mkdir				/* avoid conflict */
#include <direct.h>
#ifndef __LCC__				/* not delete altogether? */
#define mkdir _xos_mkdir
#endif
#include <errno.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif


#ifdef __LCC__
#define _close close
#define _read read
#define _write write
#define _lseek lseek
#define _tell tell
#define _chdir chdir
#define _mkdir mkdir
#define _rmdir rmdir
#define _getcwd getcwd
#endif


typedef struct dir_name_map_entry *DirNameMapEntry;
typedef struct dir_name_map	  *DirNameMap;

static DirNameMap	load_dirmap(const char *dir);
static void		free_dirmap(DirNameMap map);
static const char *	map_fat_name(DirNameMap map, const char *fatname);

static char _xos_namebuf[PATH_MAX];
static int  xerrno;

#define XENOMAP 1
#define XENOMEM 2


		 /*******************************
		 *	       ERRNO		*
		 *******************************/

int
_xos_errno()
{ return errno;
}

		 /*******************************
		 *	       HOME		*
		 *******************************/

static int
existsAndWriteableDir(const char *name)
{ DWORD a;

  if ( (a=GetFileAttributes(name)) != 0xFFFFFFFF )
  { if ( a & FILE_ATTRIBUTE_DIRECTORY )
    { if ( !(a & FILE_ATTRIBUTE_READONLY) )
	return TRUE;
    }
  }

  return FALSE;
}


char *
_xos_home()				/* expansion of ~ */
{ static char home[MAXPATHLEN];
  static int done = FALSE;

  if ( !done )
  { char h[MAXPATHLEN];

					/* Unix, set by user */
    if ( GetEnvironmentVariable("HOME", h, sizeof(h)) &&
	 existsAndWriteableDir(h) )
    { _xos_canonical_filename(h, home);
    } else if ( GetEnvironmentVariable("USERPROFILE", h, sizeof(h)) &&
		existsAndWriteableDir(h) )
    { _xos_canonical_filename(h, home);
    } else
    { char d[100];
      char p[MAXPATHLEN];
      char tmp[MAXPATHLEN];
      int haved, havep;

      haved = GetEnvironmentVariable("HOMEDRIVE", d, sizeof(d));
      havep = GetEnvironmentVariable("HOMEPATH",  p, sizeof(p));

      tmp[0] = '\0';
      if ( haved && havep )		/* Windows-NT */
      { strcpy(tmp, d);
	strcat(tmp, p);
      } else if ( haved )
      { strcpy(tmp, d);
	strcat(tmp, "\\");
      } else if ( havep )
      { strcpy(tmp, p);
      } else if ( GetWindowsDirectory(tmp, sizeof(tmp)) == 0 )
      { int drv = _getdrive();		/* A=1 */

	home[0] = drv-1+'a';
	strcpy(home+1, ":\\");
      }

      _xos_canonical_filename(tmp, home);
#if 0
      if ( !existsAndWriteableDir(tmp) )
      { MessageBox(NULL,
		   "Could not find suitable folder for storing profile information\n"
		   "Tried the following paths:\n\n"
		   "\t%HOME%\n"
		   "\t%USERPROFILE%\n"
		   "\t%HOMEDRIVE%\\%HOMEPATH%\n"
		   "\tThe top of the current drive",
		   "SWI-Prolog: no home (~)",
		   MB_ICONWARNING);
      }
#endif
    }

    done = TRUE;
  }

  return home;
}


		 /*******************************
		 *	  NAME CONVERSION	*
		 *******************************/

char *
_xos_os_filename(const char *cname, char *osname)
{ char *s = osname;
  const char *q = cname;

  if ( !osname )
    osname = _xos_namebuf;
  s = osname;
					/* /c:/ --> c:/ */
  if ( q[0] == '/' && isalpha(q[1]) && q[2] == ':' &&
       (q[3] == '/' || q[3] == '\0') )
  { *s++ = q[1];
    *s++ = ':';
    q += 3;
  }

  if ( q[0] == '/' || q[0] == '\\' )	/* deal with //host/share */
    *s++ = '\\';

  while( *q )				/* map / --> \, delete multiple '\' */
  { if ( *q == '/' || *q == '\\' )
    { *s++ = '\\';
      q++;
      while(*q == '/' || *q == '\\')
	q++;
    } else
      *s++ = *q++;
  }

  while(s > osname+1 && s[-1] == '\\' )	/* delete trailing '\' */
    s--;
					/* d: --> d:\ */
  if ( s == &osname[2] && osname[1] == ':' && isalpha(osname[0]) )
    *s++ = '\\';
  *s = '\0';

  return osname;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE:  for  getcwd(),  _xos_canonical_filename(buf,  buf)  should  work.
Change _xos_getcwd() if this assumption is violated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_xos_canonical_filename(const char *spec, char *xname)
{ const char *s = spec;
  char *p = xname;

  if ( isupper(s[0]) && s[1] == ':' )
  { *p++ = tolower(s[0]);
    *p++ = s[1];
    s += 2;
  }

  for(; *s; s++, p++)
    *p = (*s == '\\' ? '/' : *s);
  *p = '\0';

  return xname;
}


int
_xos_is_absolute_filename(const char *spec)
{ char buf[PATH_MAX];

  _xos_os_filename(spec, buf);
  if ( buf[1] == ':' && isalpha(buf[0]) )
    return TRUE;
  if ( buf[0] == '\\' && buf[1] == '\\' )
    return TRUE;

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get rid of possible  8+3  characters   in  the  path.  The documentation
suggests  you  can  do   that   using    a   single   FindFirstFile   or
GetFullPathName, but it appears you canot.  If you like, here is the code
that doesn't work:

char *
_xos_long_file_name(const char *file, char *longname)
{ DWORD len;
  LPTSTR fp;

  if ( !(len=GetFullPathName(file, PATH_MAX, longname, &fp)) ||
       len >= PATH_MAX )
    strcpy(longname, file);

  return longname;
}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_xos_long_file_name(const char *file, char *longname)
{ const char *i = file;
  char *o = longname;
  char *ok = longname;
  int changed = 0;

  while(*i)
  { int dirty = FALSE;

    while(*i && *i != '\\' && *i != '/' )
    { if ( *i == '~' )
	dirty++;
      *o++ = *i++;
    }
    if ( dirty )
    { WIN32_FIND_DATA data;
      HANDLE h;

      *o = '\0';
      if ( (h=FindFirstFile(longname, &data)) != INVALID_HANDLE_VALUE )
      { strcpy(ok, data.cFileName);
	FindClose(h);
	o = ok + strlen(ok);
	changed++;
      }
    }
    if ( *i )
      *o++ = *i++;
    ok = o;
  }

  *o = '\0';

  return longname;
}


char *
_xos_absolute_filename(const char *local, char *absolute)
{ char *filepart;

  if ( GetFullPathName(local, PATH_MAX, absolute, &filepart) )
    return absolute;

  return NULL;
}


int
_xos_same_file(const char *p1, const char *p2)
{ if ( strcmp(p1, p2) == 0 )
    return TRUE;
  else
  { char osp1[PATH_MAX], osp2[PATH_MAX];

    if ( _xos_absolute_filename(p1, osp1) &&
	 _xos_absolute_filename(p2, osp2) )
    { strlwr(osp1);
      strlwr(osp2);
      if ( strcmp(osp1, osp2) == 0 )
	return TRUE;
    }
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Apply file-name limitations to a   path-name.  For DOS-type filesystems,
this implies limitation to  the  8+3   convention  and  omitting illegal
characters.  NT doesn't have all this,   but  filenames are matches case
insensitive, so we map everything to one case.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_xos_limited_os_filename(const char *spec, char *limited)
{ strcpy(limited, spec);
  strlwr(limited);

  return limited;
}


		 /*******************************
		 *	  FILE READ/WRITE	*
		 *******************************/

int
_xos_open(const char *path, int access, ...)
{ va_list args;
  char buf[PATH_MAX];
  int mode;
  
  va_start(args, access);
  mode = va_arg(args, int);
  va_end(args);

  return _open(_xos_os_existing_filename(path, buf), access, mode);
}


int
_xos_close(int handle)
{ return _close(handle);
}


int
_xos_read(int handle, void *buf, unsigned int size)
{ return _read(handle, buf, size);
}


int
_xos_write(int handle, const void *buf, unsigned int size)
{ return _write(handle, buf, size);
}


long
_xos_lseek(int handle, long offset, int whence)
{ return _lseek(handle, offset, whence);
}


long
_xos_tell(int handle)
{ return _tell(handle);
}


		 /*******************************
		 *      FILE MANIPULATIONS	*
		 *******************************/

int
_xos_access(const char *path, int mode)
{ char buf[PATH_MAX];

  return _access(_xos_os_existing_filename(path, buf), mode);
}


int
_xos_chmod(const char *path, int mode)
{ char buf[PATH_MAX];

  return _chmod(_xos_os_existing_filename(path, buf), mode);
}


int
_xos_remove(const char *path)
{ char buf[PATH_MAX];

  return remove(_xos_os_existing_filename(path, buf));
}


int
_xos_rename(const char *old, const char *new)
{ char osold[PATH_MAX];
  char osnew[PATH_MAX];

  return rename(_xos_os_existing_filename(old, osold),
		_xos_os_existing_filename(new, osnew));
}


int
_xos_stat(const char *path, struct stat *sbuf)
{ char buf[PATH_MAX];
  
  _xos_os_existing_filename(path, buf);

  return stat(buf, (struct stat *) sbuf);
}


int
_xos_exists(const char *path, int flags)
{ char buf[PATH_MAX];
  DWORD a;

  _xos_os_existing_filename(path, buf);

  if ( (a=GetFileAttributes(buf)) != 0xFFFFFFFF )
  { if ( flags & _XOS_DIR )
    { if ( a & FILE_ATTRIBUTE_DIRECTORY )
	return TRUE;
      else
	return FALSE;
    }
    if ( flags & _XOS_FILE )
    { if ( a & FILE_ATTRIBUTE_DIRECTORY )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *	    DIRECTORIES		*
		 *******************************/

DIR *
opendir(const char *path)
{ char buf[PATH_MAX];
  DIR *dp = malloc(sizeof(DIR));

  _xos_os_existing_filename(path, buf);
  strcat(buf, "\\*.*");
  
  if ( !(dp->data = malloc(sizeof(WIN32_FIND_DATA))) )
  { errno = ENOMEM;
    return NULL;
  }
  dp->first = 1;
  dp->handle = FindFirstFile(buf, dp->data);

  if ( dp->handle == INVALID_HANDLE_VALUE )
  { if ( _access(path, 04) )		/* does not exist */
    { free(dp->data);
      return NULL;
    }
  }

  return dp;
}


int
closedir(DIR *dp)
{ if ( dp )
  { if ( dp->handle )
      FindClose(dp->handle);
    free(dp->data);
    free(dp);

    return 0;
  }

  return -1;
}


static struct dirent *
translate_data(DIR *dp)
{ WIN32_FIND_DATA *data;

  if ( !dp->handle )
    return NULL;

  data = dp->data;
  strcpy(dp->d_name, data->cFileName);

  return dp;
}


struct dirent *
readdir(DIR *dp)
{ if ( dp->first )
  { dp->first = 0;
  } else
  { if ( dp->handle )
    { if ( !FindNextFile(dp->handle, dp->data) )
	return NULL;
    }
  }

  return translate_data(dp);
}


int
_xos_chdir(const char *path)
{ char buf[PATH_MAX];

  _xos_os_existing_filename(path, buf);
  if ( isalpha(buf[0]) && buf[1] == ':' )
  { int drv = tolower(buf[0]) - 'a' + 1;

    if ( _getdrive() != drv )
    { if ( _chdrive(drv) < 0 )
	return -1;
    }
  }

  return _chdir(buf);
}


int
_xos_mkdir(const char *path, int mode)
{ char buf[PATH_MAX];

  return _mkdir(_xos_os_existing_filename(path, buf));
}


int
_xos_rmdir(const char *path)
{ char buf[PATH_MAX];

  return _rmdir(_xos_os_existing_filename(path, buf));
}


char *
_xos_getcwd(char *buf, int len)
{ char buf0[PATH_MAX];
  char buf1[PATH_MAX];

  if ( _getcwd(buf0, PATH_MAX) )
  { _xos_long_file_name(buf0, buf1);
    if ( strlen(buf1) < (unsigned) len )
    { _xos_canonical_filename(buf1, buf);

      return buf;
    }
  }

  return NULL;
}


		 /*******************************
		 *	  WIN32S NAME MAP	*
		 *******************************/

static int
containsUpperCase(const char *s)
{ for( ; *s; s++)
  { if ( isupper(*s) )
      return TRUE;
  }

  return FALSE;
}


FILE *
open_dirmap(const char *dir, const char *how)
{ char mapname[PATH_MAX];
  char *s;

  _xos_os_filename(dir, mapname);
  s = &mapname[strlen(mapname)];
  if ( s > mapname && s[-1] != '\\' )
  { *s++ = '\\';
    *s =  '\0';
  }
  strcpy(s, "dir.map");

  return fopen(mapname, how);
} 


int
_xos_make_filemap(const char *dir)
{ char pattern[PATH_MAX];
  char *s;
  HANDLE handle;
  WIN32_FIND_DATA data;
  FILE *mapfd;

  _xos_os_filename(dir, pattern);
  s = &pattern[strlen(pattern)];
  if ( s > pattern && s[-1] != '\\' )
  { *s++ = '\\';
    *s =  '\0';
  }
  strcpy(s, "*.*");

  if ( !(mapfd = open_dirmap(dir, "w")) )
    return -1;

  if ( (handle = FindFirstFile(pattern, &data)) )
  { do
    { if ( data.cAlternateFileName[0] ||
	   containsUpperCase(data.cFileName) ||
	   (data.dwFileAttributes & FILE_ATTRIBUTE_READONLY) )
      { if ( !data.cAlternateFileName[0] )
	  strcpy(data.cAlternateFileName, data.cFileName);

	fprintf(mapfd,
		"%s:%s:%06o:%02o\n",
		data.cFileName,
		strlwr(data.cAlternateFileName),
		(data.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
			? 0444 : 0644,
		(data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
			? _XOS_DIR : _XOS_FILE);
      }

      if ( (data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) &&
	   data.cFileName[0] != '.' )
      { char subdir[PATH_MAX];

	strcpy(subdir, dir);
	strcat(subdir, "\\");
	strcat(subdir, data.cFileName);

	_xos_make_filemap(subdir);
      }
    } while( FindNextFile(handle, &data) );

    FindClose(handle);
  }

  fclose(mapfd);
  return 0;
}

char *
_xos_os_existing_filename(const char *cname, char *osname)
{ return _xos_os_filename(cname, osname);
}
