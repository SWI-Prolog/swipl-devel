/*  $Id$

    Part of SWI-Prolog and XPCE
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) University of Amsterdam. All rights reserved.

    Modified:

      * Fri Feb  6 1998
	Replaced calls to getenv() and _fullpath() by their native Win32
	counterparts.  Eventually, it might be better to remove the usage
	of the MSVC runtime library altogether for easier emulation and
	performance.
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
		 *	    VERSION ...		*
		 *******************************/

static int
iswin32s()
{ static int _iswin32s = -1;

  if ( _iswin32s < 0 )
  { if( GetVersion() & 0x80000000 && (GetVersion() & 0xFF) ==3)
      _iswin32s = TRUE;
    else
      _iswin32s = FALSE;
  }

  return _iswin32s;
}

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

char *
_xos_home()				/* expansion of ~ */
{ static char home[MAXPATHLEN];
  static int done = FALSE;

  if ( !done )
  { char h[MAXPATHLEN];

    if ( GetEnvironmentVariable("HOME", h, sizeof(h)) )
    { _xos_canonical_filename(h, home);
    } else
    { char d[100];
      char p[MAXPATHLEN];
      char tmp[MAXPATHLEN];
      int haved, havep;

      haved = GetEnvironmentVariable("HOMEDRIVE", d, sizeof(d));
      havep = GetEnvironmentVariable("HOMEPATH",  p, sizeof(p));

      if ( haved && havep )
      { strcpy(tmp, d);
	strcat(tmp, p);
	_xos_canonical_filename(tmp, home);
      } else if ( haved )
      { strcpy(tmp, d);
	strcat(tmp, "\\");
	_xos_canonical_filename(tmp, home);
      } else if ( havep )
      { _xos_canonical_filename(p, home);
      } else
      { strcpy(home, "/");
      }
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

  if ( iswin32s() )
    strlwr(xname);

  return xname;
}


int
_xos_is_absolute_filename(const char *spec)
{ char buf[PATH_MAX];

  _xos_os_filename(spec, buf);
  if ( buf[1] == ':' && islower(buf[0]) )
    return TRUE;

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get rid of possible 8+3 characters in the path
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
  if ( iswin32s() )
  { if ( !(dp->map = load_dirmap(path)) )
    { if ( xerrno == XENOMEM )
      { errno = ENOMEM;
	return NULL;
      }
    }
  } else
    dp->map = NULL;

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
    if ( dp->map )
      free_dirmap(dp->map);
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
  if ( dp->map )
    strcpy(dp->d_name, map_fat_name(dp->map, data->cFileName));
  else
    strcpy(dp->d_name, data->cFileName);

  if ( iswin32s() )
    strlwr(dp->d_name);

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

		 /*******************************
		 *	  READING THE MAP	*
		 *******************************/

typedef struct map_entry *MapEntry;

struct map_entry
{ char	       		fatname[14];	/* short-form name */
  char	       		fullname[256];	/* long-form name */
  unsigned short	mode;		/* file	mode */
  unsigned short	flags;		/* private flags */
};


static int
atooct(char *s)
{ int v = 0;

  for(v=0; *s; s++)
    v = (v * 8) + (*s - '0');

  return v;
}


static int
read_map_entry(FILE *fd, const char *fullname, MapEntry entry)
{ char linebuf[300];			/* 256 + 13 + some extra */

  while( fgets(linebuf, sizeof(linebuf), fd) != NULL )
  { char *fields[4];
    int nfields;
    char *here;

    for(nfields = 0, here = linebuf; nfields < 4; nfields++)
    { fields[nfields] = here;
      while(*here > ' ' && *here != ':')
	here++;
      *here++ = '\0';
      if ( nfields == 0 && fullname )
      { if ( stricmp(fields[0], fullname) != 0 )
	  goto next;
      }
    }

    if ( nfields >= 2 )
    { strcpy(entry->fullname, fields[0]);
      strcpy(entry->fatname, fields[1]);

      if ( nfields >= 3 )
	entry->mode = atooct(fields[2]);
      if ( nfields >= 4 )
	entry->flags = atooct(fields[3]);

      return TRUE;
    }
next:;
  }

  return FALSE;
}


static int
valid_fat_name(const char *name)
{ static char special[] = "$%-_@{}~`!#():";
  int beforedot=0;
  int afterdot=0;
  int dots=0;

  for( ; *name; name++ )
  { if ( !strchr(special, *name) )
      return FALSE;
    if ( *name == '.' )
    { if ( ++dots > 1 )
	return FALSE;
    } else
    { if ( dots )
      { if ( ++afterdot > 3 )
	  return FALSE;
      } else
      { if ( ++beforedot > 8 )
	  return FALSE;
      }
    }
  }

  return TRUE;
}


char *
_xos_fat_name(const char *fullname, char *fatname)
{ char *base;
  char *dir;
  FILE *mapfd;

  _xos_os_filename(fullname, fatname);
  base = &fatname[strlen(fatname)];
  while(base > fatname && base[0] != '\\')
    base--;
  if ( base[0] == '\\' )
  { *base++ = '\0';
    dir = fatname;
  } else
    dir = ".";
  
  if ( valid_fat_name(base) )
    return NULL;

  if ( (mapfd = open_dirmap(dir, "r")) )
  { struct map_entry entry;

    if ( read_map_entry(mapfd, base, &entry) )
    { if ( dir == fatname )
      { int dirl = strlen(dir);
	int basel = strlen(base);
	
	if ( dirl + basel + 1 > PATH_MAX )
	{ errno = ENAMETOOLONG;
	  return NULL;
	}

	base[-1] = '\\';
	strcpy(base, entry.fatname);
      } else
	strcpy(fatname, entry.fatname);

      fclose(mapfd);

      return fatname;
    }

    fclose(mapfd);
  }

  return NULL;
}


char *
_xos_os_existing_filename(const char *cname, char *osname)
{ if ( iswin32s() )
  { char *s;

    if ( (s = _xos_fat_name(cname, osname)) )
      return s;
  }

  return _xos_os_filename(cname, osname);
}


		 /*******************************
		 *	 OPENDIR() MAPPING	*
		 *******************************/

struct dir_name_map
{ DirNameMapEntry entries;
};


struct dir_name_map_entry
{ char            *fullname;
  char            *fatname;
  DirNameMapEntry  next;
};


static char *
store_string(const char *s)
{ int l = strlen(s);
  char *copy = malloc(l+1);

  if ( copy )
    strcpy(copy, s);

  return copy;
}


static DirNameMap
load_dirmap(const char *dir)
{ FILE *mapfd;
  DirNameMap map;
  struct map_entry entry;

  if ( !(mapfd = open_dirmap(dir, "r")) )
  { xerrno = XENOMAP;
    return NULL;
  }

  if ( !(map = malloc(sizeof(struct dir_name_map))) )
  { xerrno = XENOMEM;
    return NULL;
  }
  map->entries = NULL;

  while ( read_map_entry(mapfd, NULL, &entry) )
  { DirNameMapEntry e = malloc(sizeof(struct dir_name_map_entry));

    e->next = map->entries;
    map->entries = e;
    if ( !(e->fullname = store_string(entry.fullname)) ||
	 !(e->fatname  = store_string(entry.fatname)) )
    { fclose(mapfd);
      xerrno = XENOMEM;
      return NULL;
    }
  }

  fclose(mapfd);

  return map;
}


static void
free_dirmap(DirNameMap map)
{ DirNameMapEntry n, e = map->entries;

  for( ; e; e = n)
  { n = e->next;

    free(e->fullname);
    free(e->fatname);
    free(e);
  }
       
  free(map);
}


static const char *
map_fat_name(DirNameMap map, const char *fatname)
{ DirNameMapEntry e = map->entries;

  for( ; e; e = e->next )
  { if ( stricmp(e->fatname, fatname) == 0 )
      return e->fullname;
  }

  return fatname;
}

