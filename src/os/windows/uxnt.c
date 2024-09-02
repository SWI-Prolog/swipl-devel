/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2022, University of Amsterdam
                              VU University Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#define UNICODE 1
#define _UNICODE 1

#include "../../config/wincfg.h"
#define _UXNT_KERNEL 1
#include "uxnt.h"			/* my prototypes */
#include "utf8.c"

#ifdef _MSC_VER
#pragma warning(disable : 4996)	/* deprecate open() etc */
#endif

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
#include <fcntl.h>
#include <assert.h>

#ifndef true
#define true 1
#define false 0
#endif

#if WIN_PATH_MAX
#undef PATH_MAX
#define PATH_MAX WIN_PATH_MAX
#endif

#define WIN_PATH_PREFIX _T("\\\\?\\")
#define WIN_UNC_PREFIX  _T("\\\\?\\UNC")

static int exists_file_or_dir(const TCHAR *path, int flags);

extern int Sdprintf(const char *fmt, ...);

		 /*******************************
		 *	       ERRNO		*
		 *******************************/

int
_xos_errno(void)
{ return errno;
}

static void
set_posix_error(int win_error)
{ int error = 0;

  switch(win_error)
  { case ERROR_ACCESS_DENIED:	  error = EACCES; break;
    case ERROR_FILE_NOT_FOUND:    error = ENOENT; break;
    case ERROR_SHARING_VIOLATION: error = EAGAIN; break;
    case ERROR_ALREADY_EXISTS:    error = EEXIST; break;
  }

  errno = error;
}

		 /*******************************
		 *		UTF-8		*
		 *******************************/

#define FITS_UTF8(c, o, e) \
	((o)+6 < (e) || (o)+utf8_code_bytes(c) <= (e))

static char *
wcstoutf8(char *dest, const wchar_t *src, size_t len)
{ char *o = dest;
  char *e = &o[len-1];

  while( *src )
  { int c;

    src = get_wchar(src, &c);
    if ( !FITS_UTF8(c, o, e) )
    { errno = ENAMETOOLONG;
      return NULL;
    }
    o = utf8_put_char(o, c);
  }
  *o = '\0';

  return dest;
}


/* length of src in UTF-8, excluding terminating EOS */

static size_t
wcutf8len(const wchar_t *src)
{ size_t len = 0;

  while( *src )
  { int c;

    src = get_wchar(src, &c);

    if ( c < 0x80 )
    { len++;
    } else
    { char o[6];
      char *e;
      e = utf8_put_char(o, c);
      len += e-o;
    }
  }

  return len;
}


wchar_t *
_xos_utf8towcs(wchar_t *dest, const char *src, size_t len)
{ wchar_t *o = dest;
  wchar_t *e = &o[len];

  for( ; *src; )
  { int wc;
    int wl;

    src = utf8_get_char(src, &wc);
    wl = (wc <= 0xffff ? 1 : 2);
    if ( o+wl >= e )
    { errno = ENAMETOOLONG;
      return NULL;
    }
    o = put_wchar(o, wc);
  }
  *o = 0;

  return dest;
}


/* length of wide string needed to represent UTF-8 string */

static size_t
utf8_wcslen(const char *s, size_t len)
{ const char *e = &s[len];
  size_t l = 0;

  while(s<e)
  { int chr;

    s = utf8_get_char(s, &chr);
    if ( chr > 0xffff )
      l++;
    l++;
  }

  return l;
}


		 /*******************************
		 *	       HOME		*
		 *******************************/

static int
existsAndWriteableDir(const TCHAR *name)
{ DWORD a = GetFileAttributes(name);

  if ( a != INVALID_FILE_ATTRIBUTES )
  { if ( (a & FILE_ATTRIBUTE_DIRECTORY) )
    { if ( !(a & FILE_ATTRIBUTE_READONLY) )
	return true;
    }
  }

  return false;
}


char *
_xos_home(void)				/* expansion of ~ */
{ static char home[PATH_MAX];
  static int done = false;

  if ( !done )
  { TCHAR h[PATH_MAX];

					/* Unix, set by user */
    if ( GetEnvironmentVariable(_T("HOME"), h, sizeof(h)) &&
	 existsAndWriteableDir(h) )
    { _xos_canonical_filenameW(h, home, sizeof(home), 0);
    } else if ( GetEnvironmentVariable(_T("USERPROFILE"), h, sizeof(h)) &&
		existsAndWriteableDir(h) )
    { _xos_canonical_filenameW(h, home, sizeof(home), 0);
    } else
    { TCHAR d[100];
      TCHAR p[PATH_MAX];
      TCHAR tmp[PATH_MAX];
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

    done = true;
  }

  return home;
}


		 /*******************************
		 *	  NAME CONVERSION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Map a UTF-8 string in Prolog internal representation to a UNICODE string
to be used with the Windows UNICODE access functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ISSEP(c) ((c)=='/'||(c)=='\\')

static int
is_unc_path(const char *q)
{ if ( ISSEP(q[0]) && ISSEP(q[1]) )
  { const char *hp = q+2;

    for(q=hp; *q && *q < 0x80 && (isalnum(*q) || *q == '.' || *q == ':'); q++)
      ;
    if ( ISSEP(*q) )
      return true;
  }

  return false;
}

int
_xos_win_prefix_length(const wchar_t *s)
{ if ( wcsncmp(s, WIN_PATH_PREFIX, wcslen(WIN_PATH_PREFIX)) == 0 )
    return wcslen(WIN_PATH_PREFIX);
  if ( wcsncmp(s, WIN_UNC_PREFIX,  wcslen(WIN_UNC_PREFIX)) == 0 )
    return wcslen(WIN_UNC_PREFIX);

  return 0;
}


static const wchar_t* reserved_file_names[] =
{ _T("CON"),
  _T("PRN"),
  _T("AUX"),
  _T("NUL"),
  _T("COM1"), _T("COM2"), _T("COM3"), _T("COM4"), _T("COM5"),
  _T("COM6"), _T("COM7"), _T("COM8"), _T("COM9"),
  _T("LPT1"), _T("LPT2"), _T("LPT3"), _T("LPT4"), _T("LPT5"),
  _T("LPT6"), _T("LPT7"), _T("LPT8"), _T("LPT9"),
  (const wchar_t*)0
};

static int
is_reserved_name(const wchar_t *name)
{ for(const wchar_t**r = reserved_file_names; *r; r++)
  { if ( _tcsicmp(name, *r) == 0 )
      return true;
  }

  return false;
}

/* Work around a bug in GetFullPathNameW() which sometimes starts
   adding its own \\?\ prefix.  If so, we delete it again ...
*/

static void
remove_win_prefix(wchar_t *s)
{ int n;

  if ( (n=_xos_win_prefix_length(s)) )
  { const wchar_t *from = s+n;
    size_t bytes = (wcslen(from)+1)*sizeof(wchar_t);

    memmove(s, from, bytes);
  }
}


/* Get a canonical name for the file, as \\?\Drive:Path for normal
   names and \\?\UNC\server\Path for network paths.

   For normal drive paths we copy \\?\ to the output and call
   GetFullPathNameW() to place the translated version after it.
   For UNC paths that does not work as we must have only one \
   before the server.  So, we do the translation such that it
   starts at the C of \UNC, next we copy the prefix in front of
   it, after which we turn the 0 after the UNC back to a \ :(
*/

wchar_t *
_xos_os_filenameW(const char *cname, wchar_t *osname, size_t len)
{ TCHAR buf[PATH_MAX];
  wchar_t *s = osname;

  if ( !_xos_utf8towcs(buf, cname, PATH_MAX) )
    return NULL;

  if ( is_reserved_name(buf) )
  { wcscpy(osname, buf);
    return osname;
  }

  int unc;
  DWORD rc;

  if ( (unc=is_unc_path(cname)) )
  { size_t plen = wcslen(WIN_UNC_PREFIX);
    wchar_t *to = s+plen-1;

    len -= plen-1;
    rc = GetFullPathNameW(buf, len, to, NULL);
    if ( rc <= len )
    { remove_win_prefix(to);
      wcscpy(s, WIN_UNC_PREFIX);
      to[1] = '\\';
      return osname;
    }
  } else
  { wcscpy(s, WIN_PATH_PREFIX);
    s += wcslen(s);
    len -= (s-osname);
    rc = GetFullPathNameW(buf, len, s, NULL);
    if ( rc <= len )
    { remove_win_prefix(s);
      return osname;
    }
  }

  errno = ENAMETOOLONG;
  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple   Prolog   to   Windows   name     conversion    used   by   e.g.
prolog_to_os_filename/2. Here we want to   preserve relative file names,
drive root and UNC files. Removes   consequtive  slashes (except the UNC
case),  only  uses  backslash  in  the    output  and  removes  trailing
backslashes except for  after  the  drive   specification  or  UNC  host
specification.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_xos_os_filename(const char *cname, char *osname, size_t len)
{ char *o = osname;
  char *e = osname+len-1;
  const char *s = cname;
  int unc = false;
  char *eh = NULL;

  while( *s )
  { int c;

    s = utf8_get_char(s, &c);
    if ( ISSEP(c) )
    { int insert = false;

      if ( unc )
	eh = o;				/* end of host */

      if ( o == osname )
      { insert = true;
      } else if ( o == osname+1 && osname[0] == '\\' )
      { unc = true;
	insert = true;
      } else if ( o[-1] != '\\' )
      { insert = true;
      }

      if ( insert )
      { if ( !FITS_UTF8(c, o, e) )
	{ errno = ENAMETOOLONG;
	  return NULL;
	}
	*o++ = '\\';
      }
    } else
    { if ( !FITS_UTF8(c, o, e) )
      { errno = ENAMETOOLONG;
	return NULL;
      }
      o = utf8_put_char(o, c);
    }
  }
  while(o>osname+1 && o[-1] == '\\' && o[-2] != ':' && o-1 != eh)
    o--;
  *o = '\0';

  return osname;
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

  while(*s)
  { int c;

    s = get_wchar(s, &c);

    if ( c == '\\' )
    { c = '/';
    } else if ( (flags&XOS_DOWNCASE) )
    { if ( c <= 0xffff )
	c = towlower((wint_t)c);
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

  if ( !_xos_utf8towcs(buf, spec, PATH_MAX) )
    return NULL;

  return _xos_canonical_filenameW(buf, xname, len, flags);
}


int
_xos_is_absolute_filename(const char *spec)
{ if ( spec[1] == ':' && !(spec[0]&0x80) && iswalpha(spec[0]) )
    return true;			/* drive */
  if ( ISSEP(spec[0]) && ISSEP(spec[1]) )
    return true;			/* UNC */

  return false;
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
  { int dirty = false;

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
	  FindClose(h);
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

  if ( !_xos_utf8towcs(in, file, PATH_MAX) )
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


static void
delete_trailing_slash(wchar_t *s)
{ wchar_t *end = s + wcslen(s);

  while(end > s && end[-1] == '\\')
  { end[-1] = 0;
    end--;
  }
}


static int
compare_file_identifiers(const wchar_t *p1, const wchar_t *p2)
{ HANDLE h1 = INVALID_HANDLE_VALUE, h2 = INVALID_HANDLE_VALUE;
  int rc = false;

  if ( (h1=CreateFileW(p1, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE,
		       NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL))
       != INVALID_HANDLE_VALUE &&
       (h2=CreateFileW(p2, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE,
		       NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL))
       != INVALID_HANDLE_VALUE )
  { BY_HANDLE_FILE_INFORMATION info1, info2;

    if ( GetFileInformationByHandle(h1, &info1) &&
	 GetFileInformationByHandle(h2, &info2) )
      rc = ( info1.dwVolumeSerialNumber == info2.dwVolumeSerialNumber &&
	     info1.nFileIndexHigh       == info2.nFileIndexHigh &&
	     info1.nFileIndexLow	== info2.nFileIndexLow );
  }

  if ( h1 != INVALID_HANDLE_VALUE ) CloseHandle(h1);
  if ( h2 != INVALID_HANDLE_VALUE ) CloseHandle(h2);

  return rc;
}


int
_xos_same_file(const char *p1, const char *p2)
{ if ( strcmp(p1, p2) == 0 )
  { return true;
  } else
  { TCHAR osp1[PATH_MAX], osp2[PATH_MAX];

    if ( !_xos_os_filenameW(p1, osp1, PATH_MAX) ||
	 !_xos_os_filenameW(p2, osp2, PATH_MAX) )
      return -1;			/* error */

    delete_trailing_slash(osp1);
    delete_trailing_slash(osp2);

    if ( _tcsicmp(osp1, osp2) == 0 )
      return true;

    return compare_file_identifiers(osp1, osp2);
  }

  return false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Apply file-name limitations to a   path-name.  For DOS-type filesystems,
this implies limitation to  the  8+3   convention  and  omitting illegal
characters.  NT doesn't have all this,   but  filenames are matched case
insensitive, so we map everything to one case.  Note that both arguments
are in UTF-8 encoding!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_xos_limited_os_filename(const char *spec, char *limited, size_t len)
{ const char *i = spec;
  char *o = limited;
  char *e = &limited[len-1];

  while(*i)
  { int wc;

    i = utf8_get_char(i, &wc);
    wc = towlower((wchar_t)wc);
    if ( !FITS_UTF8(wc, o, e) )
    { errno = ENAMETOOLONG;
      return NULL;
    }

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


ssize_t
_xos_read(int handle, void *buf, size_t size)
{ return _read(handle, buf, (unsigned int)size);
}


ssize_t
_xos_write(int handle, const void *buf, size_t size)
{ return _write(handle, buf, (unsigned int)size);
}


long
_xos_lseek(int handle, long offset, int whence)
{ return _lseek(handle, offset, whence);
}


long
_xos_tell(int handle)
{ return _tell(handle);
}


#define MAX_FOPEN_FLAGS 10

FILE *
_xos_fopen(const char *path, const char *mode)
{ TCHAR buf[PATH_MAX];
  TCHAR m[MAX_FOPEN_FLAGS];
  int i;

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return NULL;

  for(i=0; *mode && i < MAX_FOPEN_FLAGS-1; )
    m[i++] = (*mode++)&0xff;
  m[i] = 0;

  return _wfopen(buf, m);
}


		 /*******************************
		 *      FILE MANIPULATIONS	*
		 *******************************/

static int win_file_access_check = XOS_ACCESS_OPENCLOSE;

int
_xos_set_win_file_access_check(int new)
{ int old = win_file_access_check;

  win_file_access_check = new;
  return old;
}

int
_xos_get_win_file_access_check(void)
{ return win_file_access_check;
}

int
_xos_access(const char *path, int mode)
{ TCHAR buf[PATH_MAX];
  char sd_buf[512];
  SECURITY_DESCRIPTOR *sd;
  BOOL access_status;
  DWORD desired_access = 0;
  DWORD sd_size, granted_access;
  HANDLE token = 0, imp_token = 0;
  GENERIC_MAPPING generic_mapping;
  PRIVILEGE_SET privelege_set;
  DWORD  priv_set_len = sizeof(PRIVILEGE_SET);
  int retval = -1;
  SECURITY_INFORMATION sec_info =
    DACL_SECURITY_INFORMATION |
    OWNER_SECURITY_INFORMATION |
    GROUP_SECURITY_INFORMATION;

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  if ( mode == F_OK || win_file_access_check == XOS_ACCESS_ACCESS )
    return _waccess(buf, mode);

  if ( win_file_access_check == XOS_ACCESS_OPENCLOSE )
  { int m = 0;
    int fd;

    if ( exists_file_or_dir(buf, _XOS_DIR) )
      return _waccess(buf, mode);

    if ( mode & X_OK )
      mode |= R_OK;

    if ( (mode&(R_OK|W_OK)) == (R_OK|W_OK) )
      m = _O_RDWR;
    else if ( mode&R_OK )
      m = _O_RDONLY;
    else
      m = _O_WRONLY;

    if ( (fd=_wopen(buf, m)) >= 0 )
    { _close(fd);
      return 0;
    }
    return -1;
  }

  sd = (SECURITY_DESCRIPTOR*)&sd_buf;
  if ( !GetFileSecurity(buf, sec_info, sd, sizeof(sd_buf), &sd_size) )
  { if ( GetLastError() == ERROR_INVALID_FUNCTION )
    { goto simple;
    } else if ( GetLastError() != ERROR_INSUFFICIENT_BUFFER )
    { errno = ENOENT;
      return -1;
    }

    if ( !(sd = malloc(sd_size)) )
    { errno = ENOMEM;
      return -1;
    }

    if ( !GetFileSecurity(buf, sec_info, sd, sd_size, &sd_size) )
      goto simple;
  }

  if ( mode & W_OK )
  { if ( _waccess(buf, W_OK ) < 0 )		/* read-only bit set */
      goto out;
  }

  if ( !OpenThreadToken(GetCurrentThread(),
			TOKEN_DUPLICATE | TOKEN_READ,
			true,
			&token) )
  { if ( GetLastError() != ERROR_NO_TOKEN )
      goto simple;

    if ( !OpenProcessToken(GetCurrentProcess(),
			   TOKEN_DUPLICATE | TOKEN_READ,
			   &token) )
      goto simple;
  }

  if ( !DuplicateToken(token,
		       SecurityImpersonation,
		       &imp_token) )
    goto simple;

  if (mode & R_OK) desired_access |= GENERIC_READ;
  if (mode & W_OK) desired_access |= GENERIC_WRITE;
  if (mode & X_OK) desired_access |= GENERIC_EXECUTE;

  generic_mapping.GenericRead    = FILE_GENERIC_READ;
  generic_mapping.GenericWrite   = FILE_GENERIC_WRITE;
  generic_mapping.GenericExecute = FILE_GENERIC_EXECUTE;
  generic_mapping.GenericAll     = FILE_ALL_ACCESS;
  MapGenericMask(&desired_access, &generic_mapping);

  if ( !AccessCheck(sd,
		    imp_token,
		    desired_access,
		    &generic_mapping,
		    &privelege_set,
		    &priv_set_len,
		    &granted_access,
		    &access_status) )
    goto simple;

  if ( access_status )
    retval  = 0;

out:
  if ( sd && (char*)sd != sd_buf ) free(sd);
  if (imp_token) CloseHandle(imp_token);
  if (token) CloseHandle(token);

  return retval;

simple:
  retval = _waccess(buf, mode);
  goto out;
}


int
_xos_access_dir(const char *path, int mode)
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

  if ( _wremove(buf) == 0 )
    return 0;

  if ( errno == EACCES )		/* try to remove read only file */
  { DWORD atts = GetFileAttributes(buf);

    if ( (atts & FILE_ATTRIBUTE_READONLY) )
    { DWORD rwatts = atts & ~FILE_ATTRIBUTE_READONLY;
      SetFileAttributes(buf, rwatts);
      if ( DeleteFile(buf) )
	return 0;
      SetFileAttributes(buf, atts);
    }
  }

  return -1;
}


int
_xos_rename(const char *old, const char *new)
{ TCHAR osold[PATH_MAX];
  TCHAR osnew[PATH_MAX];

  if ( !_xos_os_filenameW(old, osold, PATH_MAX) ||
       !_xos_os_filenameW(new, osnew, PATH_MAX) )
    return -1;

  if ( MoveFileEx(osold, osnew, MOVEFILE_REPLACE_EXISTING) )
    return 0;

  errno = EPERM;
  return -1;				/* TBD: map error codes */
}


int
_xos_file_size(const char *path, uint64_t *sizep)
{ TCHAR buf[PATH_MAX];
  WIN32_FILE_ATTRIBUTE_DATA info;

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  if ( GetFileAttributesExW(buf, GetFileExInfoStandard, &info) )
  { uint64_t size = info.nFileSizeHigh;

    size <<= sizeof(info.nFileSizeHigh)*8;
    size += info.nFileSizeLow;
    *sizep = size;
    return 0;
  }

  errno = ENOENT;
  return -1;
}

int
_xos_get_file_time(const char *name, int which, double *tp)
{ HANDLE hFile;
  wchar_t wfile[PATH_MAX];

#define nano * 0.000000001
#define ntick 100.0
#define SEC_TO_UNIX_EPOCH 11644473600.0

  if ( !_xos_os_filenameW(name, wfile, PATH_MAX) )
    return false;

  if ( (hFile=CreateFileW(wfile,
			  0,
			  FILE_SHARE_DELETE|FILE_SHARE_READ|FILE_SHARE_WRITE,
			  NULL,
			  OPEN_EXISTING,
			  FILE_FLAG_BACKUP_SEMANTICS,
			  NULL)) != INVALID_HANDLE_VALUE )
  { FILETIME wt;
    int rc;

    switch( which )
    { case XOS_TIME_CREATE:
	rc = GetFileTime(hFile, &wt, NULL, NULL);
        break;
      case XOS_TIME_ACCESS:
	rc = GetFileTime(hFile, NULL, &wt, NULL);
        break;
      case XOS_TIME_MODIFIED:
	rc = GetFileTime(hFile, NULL, NULL, &wt);
        break;
      default:
	assert(0);
        rc = false;
    }
    CloseHandle(hFile);

    if ( rc )
    { double t;

      t  = (double)wt.dwHighDateTime * (4294967296.0 * ntick nano);
      t += (double)wt.dwLowDateTime  * (ntick nano);
      t -= SEC_TO_UNIX_EPOCH;

      *tp = t;

      return 0;
    }
  }

  set_posix_error(GetLastError());

  return -1;
}


static int
exists_file_or_dir(const TCHAR *path, int flags)
{ DWORD a;

  if ( (a=GetFileAttributes(path)) != INVALID_FILE_ATTRIBUTES )
  { if ( (flags & _XOS_DIR) )
    { if ( (a & FILE_ATTRIBUTE_DIRECTORY) )
	return true;
      else
	return false;
    }
    if ( (flags & _XOS_FILE) )
    { if ( (a & FILE_ATTRIBUTE_DIRECTORY) )
	return false;
    }

    return true;
  }

  return false;
}


int
_xos_exists(const char *path, int flags)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  return exists_file_or_dir(buf, flags);
}


		 /*******************************
		 *	    DIRECTORIES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) According to the docs, FindFirstFileW()   does not _need_ the "\\?\"
prefix. It seems that on some specific   directories  this goes wrong in
Wine.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

DIR *
opendir(const char *path)
{ TCHAR buf[PATH_MAX];
  DIR *dp = malloc(sizeof(*dp));
  size_t len;

  if ( !dp )
  { errno = ENOMEM;
    return NULL;
  }

  /* -2: make sure there is space for the "\*" */
  if ( !_xos_os_filenameW(path, buf, PATH_MAX-2) )
  { free(dp);
    errno = ENAMETOOLONG;
    return NULL;
  }
  len = _tcslen(buf);
  if ( len > 0 && buf[len-1] == '\\' )
    len--;
  _tcscpy(buf+len, _T("\\*"));

  if ( !(dp->data = malloc(sizeof(WIN32_FIND_DATA))) )
  { free(dp);
    errno = ENOMEM;
    return NULL;
  }
  dp->first = true;
  dp->handle = FindFirstFileExW(buf,
				FindExInfoBasic, dp->data,
				FindExSearchNameMatch, NULL,
				FIND_FIRST_EX_LARGE_FETCH);

  if ( dp->handle == INVALID_HANDLE_VALUE )
  { buf[len] = 0;

    if ( _waccess(buf, R_OK) )		/* Dir does not exist or is unreadable */
    { closedir(dp);
      return NULL;
    }
  }

  return dp;
}


int
closedir(DIR *dp)
{ if ( dp )
  { if ( dp->handle != INVALID_HANDLE_VALUE )
      FindClose(dp->handle);
    free(dp->data);
    free(dp);

    return 0;
  }

  return -1;
}


struct dirent *
readdir(DIR *dp)
{ if ( dp->first )
  { dp->first = false;
    if ( dp->handle == INVALID_HANDLE_VALUE )
      return NULL;
  } else
  { if ( !FindNextFile(dp->handle, dp->data) )
      return NULL;
  }

  WIN32_FIND_DATA *data = dp->data;
  wcstoutf8(dp->d_name, data->cFileName, sizeof(dp->d_name));

  return dp;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We cannot pass the "\\?\" prefix  to SetCurrentDirectoryW() as that will
upset applications that inherit this directory. Still seems the argument
is limited to 260 characters.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
_xos_chdir(const char *path)
{ TCHAR buf[PATH_MAX];

  if ( !_xos_os_filenameW(path, buf, PATH_MAX) )
    return -1;

  if ( SetCurrentDirectoryW(buf+_xos_win_prefix_length(buf)) )
    return 0;

  errno = ENOENT;
  return -1;
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
_xos_getcwd(char *buf, size_t len)
{ TCHAR buf0[PATH_MAX];
  TCHAR buf1[PATH_MAX];

  if ( _wgetcwd(buf0, sizeof(buf0)/sizeof(TCHAR)) &&
       _xos_long_file_nameW(buf0, buf1, sizeof(buf0)/sizeof(TCHAR)) )
  { return _xos_canonical_filenameW(buf1, buf, len, 0);
  }

  return NULL;
}


		 /*******************************
		 *	    ENVIRONMENT		*
		 *******************************/

size_t
_xos_getenv(const char *name, char *buf, size_t buflen)
{ TCHAR nm[PATH_MAX];
  TCHAR val[PATH_MAX];
  TCHAR *valp = val;
  size_t size;

  if ( !_xos_utf8towcs(nm, name, PATH_MAX) )
    return -1;
  size = GetEnvironmentVariable(nm, valp, PATH_MAX);

  if ( size > 0 )
  { size_t rc;

    if ( size >= PATH_MAX )
    { if ( (valp = malloc((size+1)*sizeof(TCHAR))) == NULL )
	return -1;
      size = GetEnvironmentVariable(nm, valp, (DWORD)(size+1));
    }

    size = wcslen(valp);		/* return sometimes holds 0-bytes */

    if ( wcstoutf8(buf, valp, buflen) )
      rc = strlen(buf);
    else
      rc = wcutf8len(valp);

    if ( valp != val )
      free(valp);

    return rc;
  }

  return -1;
}


int
_xos_setenv(const char *name, char *value, int overwrite)
{ TCHAR nm[PATH_MAX];
  TCHAR buf[PATH_MAX];
  TCHAR *val = buf;
  int rc;

  if ( !_xos_utf8towcs(nm, name, PATH_MAX) )
    return -1;
  if ( !overwrite && GetEnvironmentVariable(nm, NULL, 0) > 0 )
    return 0;
  if ( !_xos_utf8towcs(val, value, PATH_MAX) )
  { size_t wlen = utf8_wcslen(value, strlen(value)) + 1;
    wchar_t *s;

    if ( (val = malloc(wlen*sizeof(TCHAR))) == NULL )
      return -1;
    s = _xos_utf8towcs(val, value, wlen);
    assert(s==val);
    (void)s;
  }

  rc = SetEnvironmentVariable(nm, val);
  if ( val != buf )
    free(val);

  if ( rc )
    return 0;

  return -1;				/* TBD: convert error */
}
