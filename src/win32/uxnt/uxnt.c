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

#define UNICODE 1
#define _UNICODE 1

#define _UXNT_KERNEL 1
#include "uxnt.h"			/* my prototypes */
#include "utf8.c"

#include <windows.h>
#include <tchar.h>
#include <wchar.h>
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
		 *		UTF-8		*
		 *******************************/

static char *
wcstoutf8(char *dest, const wchar_t *src, size_t len)
{ char *o = dest;
  char *e = &o[len];

  for(; *src; src++)
  { if ( o+6 > e )
    { errno = ENAMETOOLONG;
      return NULL;
    }
    o = utf8_put_char(o, *src);
  }
  *o = '\0';

  return dest;
}


static wchar_t *
utf8towcs(wchar_t *dest, const char *src, size_t len)
{ wchar_t *o = dest;
  wchar_t *e = &o[len-1];

  for( ; *src; )
  { int wc;

    src = utf8_get_char(src, &wc);
    if ( o >= e )
    { errno = ENAMETOOLONG;
      return NULL;
    }
    *o++ = wc;
  }
  *o = 0;

  return dest;
}


		 /*******************************
		 *	       HOME		*
		 *******************************/

static int
existsAndWriteableDir(const TCHAR *name)
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
  { TCHAR h[MAXPATHLEN];

					/* Unix, set by user */
    if ( GetEnvironmentVariable(_T("HOME"), h, sizeof(h)) &&
	 existsAndWriteableDir(h) )
    { _xos_canonical_filenameW(h, home, sizeof(home), 0);
    } else if ( GetEnvironmentVariable(_T("USERPROFILE"), h, sizeof(h)) &&
		existsAndWriteableDir(h) )
    { _xos_canonical_filenameW(h, home, sizeof(home), 0);
    } else
    { TCHAR d[100];
      TCHAR p[MAXPATHLEN];
      TCHAR tmp[MAXPATHLEN];
      int haved, havep;

      haved = GetEnvironmentVariable(_T("HOMEDRIVE"), d, sizeof(d));
      havep = GetEnvironmentVariable(_T("HOMEPATH"),  p, sizeof(p));

      tmp[0] = '\0';
      if ( haved && havep )		/* Windows-NT */
      { _tcscpy(tmp, d);
	_tcscat(tmp, p);
      } else if ( haved )
      { _tcscpy(tmp, d);
	_tcscat(tmp, _T("\\"));
      } else if ( havep )
      { _tcscpy(tmp, p);
      } else if ( GetWindowsDirectory(tmp, sizeof(tmp)) == 0 )
      { int drv = _getdrive();		/* A=1 */

	tmp[0] = drv-1+'a';
	_tcscpy(&tmp[1], _T(":\\"));
      }

      _xos_canonical_filenameW(tmp, home, sizeof(home), 0);
    }

    done = TRUE;
  }

  return home;
}


		 /*******************************
		 *	  NAME CONVERSION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Map a UTF-8 string in Prolog internal representation to a UNICODE string
to be used with the Windows UNIODE access functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

wchar_t *
_xos_os_filenameW(const char *cname, wchar_t *osname, size_t len)
{ wchar_t *s = osname;
  wchar_t *e = &osname[len-1];
  const char *q = cname;

  s = osname;
					/* /c:/ --> c:/ */
  if ( q[0] == '/' && q[1] < 0x80 && isalpha(q[1]) && q[2] == ':' &&
       (q[3] == '/' || q[3] == '\0') )
  { if ( s+2 >= e )
    { errno = ENAMETOOLONG;
      return NULL;
    }
    *s++ = q[1];
    *s++ = ':';
    q += 3;
  }

  if ( q[0] == '/' || q[0] == '\\' )	/* deal with //host/share */
  { if ( s+1 >= e )
    { errno = ENAMETOOLONG;
      return NULL;
    }
    *s++ = '\\';
  }

  while( *q )				/* map / --> \, delete multiple '\' */
  { if ( *q == '/' || *q == '\\' )
    { if ( s+1 >= e )
      { errno = ENAMETOOLONG;
	return NULL;
      }
      *s++ = '\\';
      q++;
      while(*q == '/' || *q == '\\')
	q++;
    } else
    { int wc;

      q = utf8_get_char(q, &wc);
      if ( s+2 >= e )
      { errno = ENAMETOOLONG;
	return NULL;
      }
      *s++ = wc;
    }
  }

  while(s > osname+1 && s[-1] == '\\' )	/* delete trailing '\' */
    s--;
					/* d: --> d:\ */
  if ( s == &osname[2] && osname[1] == ':' &&
       osname[0] < 0x80 && isalpha(osname[0]) )
    *s++ = '\\';
  *s = '\0';

  return osname;
}


char *
_xos_os_filename(const char *cname, char *osname, size_t len)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(cname, buf, PATH_MAX) )
    return NULL;
  
  return wcstoutf8(osname, buf, len);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Transform a UNICODE Windows filename into a UTF-8 representation of the
filename in Prolog canonical representation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_xos_canonical_filenameW(const wchar_t *spec,
			 char *xname, size_t len,
			 int flags)
{ const wchar_t *s = spec;
  char *p = xname;
  char *e = &xname[len];

  if ( s[0] < 0x80 && isupper(s[0]) && s[1] == ':' )
  { *p++ = tolower(s[0]);
    *p++ = (char)s[1];
    s += 2;
  }

  for(; *s; s++)
  { int c = *s;
    
    if ( c == '\\' )
    { c = '/';
    } else if ( (flags&XOS_DOWNCASE) )
    { c = towlower((wchar_t)c);
    }

    if ( p+6 >= e )
    { errno = ENAMETOOLONG;
      return NULL;
    }
    p = utf8_put_char(p, c);
  }
  *p = '\0';

  return xname;
}


char *
_xos_canonical_filename(const char *spec, char *xname, size_t len, int flags)
{ TCHAR buf[PATH_MAX];

  if ( !utf8towcs(buf, spec, PATH_MAX) )
    return NULL;
  
  return _xos_canonical_filenameW(buf, xname, len, flags);
}


int
_xos_is_absolute_filename(const char *spec)
{ TCHAR buf[PATH_MAX];

  _xos_os_filenameW(spec, buf, PATH_MAX);
  if ( buf[1] == ':' && buf[0] < 0x80 && isalpha(buf[0]) )
    return TRUE;			/* drive */
  if ( buf[0] == '\\' && buf[1] == '\\' )
    return TRUE;			/* UNC */

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get rid of possible  8+3  characters   in  the  path.  The documentation
suggests  you  can  do   that   using    a   single   FindFirstFile   or
GetFullPathName, but it appears you cannot.  If   you  like, here is the
code that doesn't work:

char *
_xos_long_file_nameW(const char *file, char *longname)
{ DWORD len;
  LPTSTR fp;

  if ( !(len=GetFullPathName(file, PATH_MAX, longname, &fp)) ||
       len >= PATH_MAX )
    strcpy(longname, file);

  return longname;
}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

TCHAR *
_xos_long_file_nameW(const TCHAR *file, TCHAR *longname, size_t len)
{ const TCHAR *i = file;
  TCHAR *o = longname;
  TCHAR *ok = longname;
  TCHAR *e = &longname[len-1];
  int changed = 0;

  while(*i)
  { int dirty = FALSE;

    while(*i && *i != '\\' && *i != '/' )
    { if ( *i == '~' )
	dirty++;
      if ( o >= e )
      { errno = ENAMETOOLONG;
	return NULL;
      }
      *o++ = *i++;
    }
    if ( dirty )
    { WIN32_FIND_DATA data;
      HANDLE h;

      *o = '\0';
      if ( (h=FindFirstFile(longname, &data)) != INVALID_HANDLE_VALUE )
      { size_t l = _tcslen(data.cFileName);

	if ( ok+l >= e )
	{ errno = ENAMETOOLONG;
	  return NULL;
	}

	_tcscpy(ok, data.cFileName);
	FindClose(h);
	o = ok + l;
	changed++;
      }
    }
    if ( *i )
    { if ( o >= e )
      { errno = ENAMETOOLONG;
	return NULL;
      }
      *o++ = *i++;
    }
    ok = o;
  }

  *o = '\0';

  return longname;
}


char *
_xos_long_file_name_toA(const wchar_t *file, char *longname, size_t len)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_long_file_nameW(file, buf, PATH_MAX) )
    return NULL;

  return wcstoutf8(longname, buf, len);
}


char *
_xos_long_file_name(const char *file, char *longname, size_t len)
{ TCHAR in[PATH_MAX];
  TCHAR out[PATH_MAX];

  if ( !utf8towcs(in, file, PATH_MAX) )
    return NULL;

  if ( !_xos_long_file_nameW(in, out, PATH_MAX) )
    return NULL;

  return wcstoutf8(longname, out, len);
}


char *
_xos_absolute_filename(const char *local, char *absolute, size_t len)
{ TCHAR buf[PATH_MAX];
  TCHAR *filepart;
  TCHAR abs[PATH_MAX];

  if ( !_xos_os_filenameW(local, buf, PATH_MAX) )
    return NULL;

  if ( GetFullPathName(buf, PATH_MAX, abs, &filepart) )
    return _xos_canonical_filenameW(abs, absolute, len, 0);

  return NULL;
}


int
_xos_same_file(const char *p1, const char *p2)
{ if ( strcmp(p1, p2) == 0 )
  { return TRUE;
  } else
  { TCHAR osp1[PATH_MAX], osp2[PATH_MAX];
    TCHAR abs1[PATH_MAX], abs2[PATH_MAX];
    TCHAR *fp;

    if ( !_xos_os_filenameW(p1, osp1, PATH_MAX) ||
	 !_xos_os_filenameW(p2, osp2, PATH_MAX) )
      return -1;			/* error */

    if ( !GetFullPathName(osp1, PATH_MAX, abs1, &fp) ||
	 !GetFullPathName(osp2, PATH_MAX, abs2, &fp) )
      return -1;

    if ( _tcscmp(abs1, abs2) == 0 )
      return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Apply file-name limitations to a   path-name.  For DOS-type filesystems,
this implies limitation to  the  8+3   convention  and  omitting illegal
characters.  NT doesn't have all this,   but  filenames are matched case
insensitive, so we map everything to one case.  Note that both arguments
are in UTF-8 encoding!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_xos_limited_os_filename(const char *spec, char *limited)
{ const unsigned char *i = (const unsigned char*)spec;
  char *o = limited;

  while(*i)
  { int wc;
  
    i = utf8_get_char(i, &wc);
    wc = towlower((wchar_t)wc);
    o = utf8_put_char(o, wc);
  }
  *o = '\0';

  return limited;
}


		 /*******************************
		 *	  FILE READ/WRITE	*
		 *******************************/

int
_xos_open(const char *path, int access, ...)
{ va_list args;
  TCHAR buf[PATH_MAX];
  int mode;
  
  va_start(args, access);
  mode = va_arg(args, int);
  va_end(args);

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  return _wopen(buf, access, mode);
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


FILE *
_xos_fopen(const char *path, const char *mode)
{ TCHAR buf[PATH_MAX];
  TCHAR m[10];
  int i;

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return NULL;

  for(i=0; *mode && i < sizeof(m-1); )
    m[i++] = (*mode++)&0xff;
  m[i] = 0;

  return _wfopen(buf, m);
}



		 /*******************************
		 *      FILE MANIPULATIONS	*
		 *******************************/

int
_xos_access(const char *path, int mode)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  return _waccess(buf, mode);
}


int
_xos_chmod(const char *path, int mode)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  return _wchmod(buf, mode);
}


int
_xos_remove(const char *path)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  return _wremove(buf);
}


int
_xos_rename(const char *old, const char *new)
{ TCHAR osold[PATH_MAX];
  TCHAR osnew[PATH_MAX];

  if ( !_xos_os_filenameW(old, osold, PATH_MAX) ||
       !_xos_os_filenameW(new, osnew, PATH_MAX) )
    return -1;

  return _wrename(osold, osnew);
}


int
_xos_stat(const char *path, struct _stat *sbuf)
{ TCHAR buf[PATH_MAX];

   if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;
  
  return _wstat(buf, sbuf);
}


int
_xos_exists(const char *path, int flags)
{ TCHAR buf[PATH_MAX];
  DWORD a;

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

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
{ TCHAR buf[PATH_MAX];
  DIR *dp = malloc(sizeof(DIR));

  if ( !_xos_os_filenameW(path, buf, PATH_MAX-4) )
    return NULL;
  _tcscat(buf, _T("\\*.*"));
  
  if ( !(dp->data = malloc(sizeof(WIN32_FIND_DATA))) )
  { errno = ENOMEM;
    return NULL;
  }
  dp->first = 1;
  dp->handle = FindFirstFile(buf, dp->data);

  if ( dp->handle == INVALID_HANDLE_VALUE )
  { if ( _waccess(buf, 04) )		/* does not exist */
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
  if ( wcstoutf8(dp->d_name, data->cFileName, sizeof(dp->d_name)) )
    return dp;

  return NULL;
}


struct dirent *
readdir(DIR *dp)
{ for(;;)
  { struct dirent *de;

    if ( dp->first )
    { dp->first = 0;
    } else
    { if ( dp->handle )
      { if ( !FindNextFile(dp->handle, dp->data) )
	  return NULL;
      }
    }

    if ( (de = translate_data(dp)) )
      return de;
  }
}


int
_xos_chdir(const char *path)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  if ( buf[0] < 0x80 && isalpha(buf[0]) && buf[1] == ':' )
  { int drv = tolower(buf[0]) - 'a' + 1;

    if ( _getdrive() != drv )
    { if ( _chdrive(drv) < 0 )
	return -1;
    }
  }

  return _wchdir(buf);
}


int
_xos_mkdir(const char *path, int mode)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  return _wmkdir(buf);
}


int
_xos_rmdir(const char *path)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  return _wrmdir(buf);
}


char *
_xos_getcwd(char *buf, int len)
{ TCHAR buf0[PATH_MAX];
  TCHAR buf1[PATH_MAX];

  if ( _wgetcwd(buf0, sizeof(buf0)/sizeof(TCHAR)) &&
       _xos_long_file_nameW(buf0, buf1, sizeof(buf0)/sizeof(TCHAR)) )
  { return _xos_canonical_filenameW(buf1, buf, len, 0);
  }

  return NULL;
}

