/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <h/kernel.h>
#include <h/unix.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
          
#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_ACCESS_H
#  include <sys/access.h>
# endif
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#include <sys/stat.h>
#include <errno.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#else
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

static Chain DirectoryStack;
static Name  ExpandProblem;

static char *	canonisePath(char *);
static Name	getWorkingDirectoryPce(Pce pce);

#define MODIFIED_NOT_SET ((time_t) ~0L)


static const char *
nameToUTF8(Name n)
{ return charArrayToUTF8((CharArray)n);
}


static const char *
nameToFN(Name n)
{ return charArrayToFN((CharArray)n);
}


static status
initialiseDirectory(Directory d, Name name)
{ char path[MAXPATHLEN];
  Name expanded;
  const char *ufn;

  if ( !(expanded = expandFileName(name)) )
    fail;

  ufn = nameToUTF8(expanded);
  if ( absolutePath(ufn, path, sizeof(path)) < 0 )
    return errorPce(d, NAME_representation, NAME_nameTooLong);

  assign(d, path, UTF8ToName(path));
  assign(d, name, UTF8ToName(baseName(ufn)));
  d->modified = MODIFIED_NOT_SET;

  succeed;
}


static Directory
getConvertDirectory(Class class, Name name)
{ answer(answerObject(ClassDirectory, name, EAV));
}


static status
storeDirectory(Directory d, FileObj file)
{ return storeSlotsObject(d, file);
}


static status
loadDirectory(Directory d, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(d, fd, def));

  d->modified = MODIFIED_NOT_SET;
  succeed;
}


static status
existsDirectory(Directory d)
{ struct stat buf;

  if ( stat(nameToFN(d->path), &buf) == -1 ||
       (buf.st_mode & S_IFMT) != S_IFDIR )
    fail;

  succeed;
}


static status
makeDirectory(Directory d)
{ if ( !existsDirectory(d) )
  { if ( mkdir(nameToFN(d->path), 0777) != 0 )
      return errorPce(d, NAME_mkdir, getOsErrorPce(PCE));
  }

  succeed;
}


static status
removeDirectory(Directory d)
{ if ( rmdir(nameToFN(d->path)) != 0 )
  { if ( existsDirectory(d) )
      return errorPce(d, NAME_rmdir, getOsErrorPce(PCE));
  }

  succeed;
}


status
cdDirectory(Directory d)
{ if ( chdir(nameToFN(d->path)) )
    return errorPce(d, NAME_chdir, d->path, getOsErrorPce(PCE));

  succeed;
}


static status
pushDirectory(Directory d)
{ Name cwd;

  assert(DirectoryStack);
  TRY(cwd = getWorkingDirectoryPce(PCE));
  if ( cdDirectory(d) )
    return prependChain(DirectoryStack, cwd);

  fail;
}


static status
popDirectory(Directory d)
{ Name path;

  if ( emptyChain(DirectoryStack) )
    return errorPce(d, NAME_stackEmpty);

  path = getHeadChain(DirectoryStack);
  deleteHeadChain(DirectoryStack);

  if ( chdir(strName(path)) )
    return errorPce(d, NAME_chdir, path, getOsErrorPce(PCE));

  succeed;
}


static status
scanDirectory(Directory d, Chain files, Chain dirs, Regex pattern, Bool all)
{ DIR *dirp;
  struct dirent *dp;

  if ( notDefault(pattern) )
  { if ( getFeatureClass(ClassFile, NAME_caseSensitive) == OFF )
      ignoreCaseRegex(pattern, ON);
  } 

  if ( files != dirs )
  { TRY(pushDirectory(d));
    if ( !(dirp = opendir(".")) )
    { errorPce(d, NAME_readDirectory, getOsErrorPce(PCE));
      popDirectory(d);
      fail;
    }

    for (dp=readdir(dirp); dp!=NULL; dp=readdir(dirp))
    { char *name = dp->d_name;
      struct stat buf;

      if ( stat(name, &buf) != 0 )
	continue;

      if ( (notNil(files) && (buf.st_mode & S_IFMT) == S_IFREG) )
      { if ( notDefault(pattern) )
	{ CharArray ca = CtoScratchCharArray(name);
					/* TBD: UNICODE */
	  if ( !searchRegex(pattern, ca, DEFAULT, DEFAULT) )
	  { doneScratchCharArray(ca);
	    continue;
	  }
	  doneScratchCharArray(ca);
	}

	if ( all != ON && name[0] == '.' )
	  continue;

	appendChain(files, FNToName(name));
      } else if ( (notNil(dirs) && (buf.st_mode & S_IFMT) == S_IFDIR) )
      { if ( all != ON && name[0] == '.' )
	  continue;

	appendChain(dirs, FNToName(name));
      }
    }
    closedir(dirp);
    popDirectory(d);

    if ( notNil(dirs) )
      sortNamesChain(dirs, OFF);
    if ( notNil(files) )
      sortNamesChain(files, OFF);
  } else if ( notNil(files) )
  { if ( !(dirp = opendir(nameToFN(d->path))) )
      return errorPce(d, NAME_readDirectory, getOsErrorPce(PCE));
    for (dp=readdir(dirp); dp!=NULL; dp=readdir(dirp))
    { char *name = dp->d_name;
      
      if ( notDefault(pattern) )
      { CharArray ca = CtoScratchCharArray(name);
					/* TBD: UNICODE */
	if ( !searchRegex(pattern, ca, DEFAULT, DEFAULT) )
	{ doneScratchCharArray(ca);
	  continue;
	}
	doneScratchCharArray(ca);
      } else if ( all != ON && name[0] == '.' )
	continue;

      appendChain(files, FNToName(name));
    }
    closedir(dirp);

    sortNamesChain(files, OFF);
  }

  succeed;
}


static Chain
getDirectoriesDirectory(Directory d, Regex pattern, Bool all)
{ Chain dirs = answerObject(ClassChain, EAV);

  TRY(scanDirectory(d, NIL, dirs, pattern, all));

  answer(dirs);
}


static Chain
getFilesDirectory(Directory d, Regex pattern, Bool all)
{ Chain files = answerObject(ClassChain, EAV);

  TRY(scanDirectory(d, files, NIL, pattern, all));

  answer(files);
}



static Directory
getParentDirectory(Directory d)
{ char parent[MAXPATHLEN];
  const char *here = nameToFN(d->path);

  if ( IsDirSep(here[0]) && here[1] == EOS ) /* the root */
    fail;
#ifdef O_XOS
					/* DOS root: <Drive>:[\/] */
  if ( isalpha(here[0]) && here[1] == ':' &&
       (here[2] == EOS || (IsDirSep(here[2]) && here[3] == EOS)) )
    fail;
#endif

  if ( dirName(here, parent, sizeof(parent)) )
    answer(answerObject(ClassDirectory, FNToName(parent), EAV));

  fail;
}


static Chain
getRootsDirectory(Directory dir)
{ Chain ch = answerObject(ClassChain, EAV);
#ifdef WIN32
  char buf[MAXPATHLEN];
  extern int get_logical_drive_strings(int, char *);

  if ( get_logical_drive_strings(sizeof(buf)-1, buf) )
  { char *s = buf;

    while(*s)
    { char buf2[MAXPATHLEN];
      char *cnfn;

      if ( (cnfn=_xos_canonical_filename(s, buf2, sizeof(buf2), 0)) )
	appendChain(ch, cnfn);
      s += strlen(s)+1;
    }
  }
#else
  appendChain(ch, CtoName("/"));
#endif
  
  answer(ch);
}


static Name
getBaseNameDirectory(Directory d)
{ answer(d->name);
}


static Date
getTimeDirectory(Directory d, Name which)
{ struct stat buf;
  Name name = d->path;

  if ( isDefault(which) )
    which = NAME_modified;

  if ( stat(nameToFN(name), &buf) < 0 )
  { errorPce(d, NAME_cannotStat, getOsErrorPce(PCE));
    fail;
  }

  if ( which == NAME_modified )
    answer(CtoDate(buf.st_mtime));
  else
    answer(CtoDate(buf.st_atime));
}


static Name
getFileNameDirectory(Directory d, Name name)
{ const char *fn = nameToUTF8(name);

  if ( isAbsolutePath(fn) )
    answer(name);
  else
  { const char *dfn = nameToUTF8(d->path);
    size_t dfnl = strlen(dfn);
    int maxl = strlen(fn) + dfnl + 2;
    LocalArray(char, buf, maxl);

    memcpy(buf, dfn, dfnl);
    if ( dfnl > 0 && buf[dfnl-1] != '/' )
      buf[dfnl++] = '/';
    strcpy(&buf[dfnl], fn);

    answer(UTF8ToName(buf));
  }
}


static FileObj
getFileDirectory(Directory d, Name name)
{ return answerObject(ClassFile, getFileNameDirectory(d, name), EAV);
}


static Directory
getDirectoryDirectory(Directory d, Name name)
{ return answerObject(ClassDirectory, getFileNameDirectory(d, name), EAV);
}


static status
sameDirectory(Directory d1, Directory d2)
{ return sameOsPath(strName(d1->path), strName(d2->path));
}


static status
accessDirectory(Directory d, Name mode)
{ int m;

  if ( mode == NAME_read )
    m = R_OK;
  else /*if ( mode == NAME_write )*/
    m = W_OK;
  
  if ( access(nameToFN(d->path), m) == 0 )
    succeed;

  fail;
}


static status
changedDirectory(Directory d)
{ struct stat buf;

  if ( stat(nameToFN(d->path), &buf) < 0 )
    succeed;			/* we signal non-extistence as changed */

  if ( d->modified == MODIFIED_NOT_SET )
  { d->modified = buf.st_mtime;
    fail;
  } 
  if ( buf.st_mtime > d->modified )
  { d->modified = buf.st_mtime;
    succeed;
  }

  fail;
}


static Name
getPrintNameDirectory(Directory dir)
{ answer(isName(dir->path) ? dir->path : dir->name);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_scan[] =
        { "files=chain*", "directories=chain*", "pattern=[regex]", "hidden_too=[bool]" };
static char *T_patternADregexD_hidden_tooADboolD[] =
        { "pattern=[regex]", "hidden_too=[bool]" };

/* Instance Variables */

static vardecl var_directory[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Name of the directory"),
  IV(NAME_path, "name", IV_GET,
     NAME_name, "Full path name"),
  IV(NAME_modified, "alien:long", IV_NONE,
     NAME_time, "Time stamp for ->changed")
};

/* Send Methods */

static senddecl send_directory[] =
{ SM(NAME_initialise, 1, "path=name", initialiseDirectory,
     DEFAULT, "Create from name"),
  SM(NAME_scan, 4, T_scan, scanDirectory,
     NAME_contents, "Get member files and directories"),
  SM(NAME_make, 0, NULL, makeDirectory,
     NAME_edit, "Create the OS counterpart"),
  SM(NAME_remove, 0, NULL, removeDirectory,
     NAME_edit, "Delete the OS counterpart"),
  SM(NAME_access, 1, "{read,write}", accessDirectory,
     NAME_test, "Test if directory has access {read, write}"),
  SM(NAME_exists, 0, NULL, existsDirectory,
     NAME_test, "Test if directory exists"),
  SM(NAME_same, 1, "directory=directory", sameDirectory,
     NAME_test, "Test if two paths refer to the same physical directory"),
  SM(NAME_modified, 0, NULL, changedDirectory,
     NAME_time, "Succeed if directory has changed since last test"),
  SM(NAME_cd, 0, NULL, cdDirectory,
     NAME_workingDirectory, "Change to this directory"),
  SM(NAME_pop, 0, NULL, popDirectory,
     NAME_workingDirectory, "->cd back to old directory"),
  SM(NAME_push, 0, NULL, pushDirectory,
     NAME_workingDirectory, "->cd until ->pop")
};

/* Get Methods */

static getdecl get_directory[] =
{ GM(NAME_printName, 0, "text=char_array", NULL, getPrintNameDirectory,
     DEFAULT, "Equivalent to <-path"),
  GM(NAME_directories, 2, "names=chain", T_patternADregexD_hidden_tooADboolD, getDirectoriesDirectory,
     NAME_contents, "New chain with names of member directories"),
  GM(NAME_directory, 1, "directory", "name", getDirectoryDirectory,
     NAME_contents, "New directory object with name in directory"),
  GM(NAME_fileName, 1, "name", "name", getFileNameDirectory,
     NAME_contents, "Create path relative to directory"),
  GM(NAME_file, 1, "file", "name", getFileDirectory,
     NAME_contents, "New file object with name in directory"),
  GM(NAME_files, 2, "names=chain", T_patternADregexD_hidden_tooADboolD, getFilesDirectory,
     NAME_contents, "New chain with names of member files"),
  GM(NAME_convert, 1, "directory", "name", getConvertDirectory,
     NAME_conversion, "Convert directory name"),
  GM(NAME_parent, 0, "directory", NULL, getParentDirectory,
     NAME_hierarchy, "New directory for parent directory"),
  GM(NAME_baseName, 0, "name", NULL, getBaseNameDirectory,
     NAME_name, "Same as <-name, cf. `file <-base_name'"),
  GM(NAME_roots, 0, "chain", NULL, getRootsDirectory,
     NAME_name, "Unix: chain(/), Win32: GetLogicalDriveNames()"),
  GM(NAME_time, 1, "date=date", "which_time=[{modified,access}]",
     getTimeDirectory,
     NAME_time, "New date holding modification/access time")
};

/* Resources */

#define rc_directory NULL
/*
static classvardecl rc_directory[] =
{ 
};
*/

/* Class Declaration */

static Name directory_termnames[] = { NAME_name };

ClassDecl(directory_decls,
          var_directory, send_directory, get_directory, rc_directory,
          1, directory_termnames,
          "$Rev$");

status
makeClassDirectory(Class class)
{ declareClass(class, &directory_decls);
  setLoadStoreFunctionClass(class, loadDirectory, storeDirectory);

  DirectoryStack = globalObject(NAME_directoryStack, ClassChain, EAV);
  DEBUG(NAME_directory, Cprintf("DirectoryStack = %s\n", pp(DirectoryStack)));

  succeed;
}


		/********************************
		*           PRIMITIVES		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copied from SWI-Prolog pl-os.c. No poblem, as  SWI has copyright to both
systems.  If you spot a bug, please synchronise.

This routine works `in-place', using an   8-bit representation. It works
fine  on  encoded  8-bit  representations  as   long  as  the  important
characters . and / are unique. This is  true for UTF-8, but not for some
state-shifting multibyte encodings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
canonisePath(char *path)
{ char *out = path, *in = path;
  char *osave[100];
  int  osavep = 0;

  while( IsDirSep(in[0]) && in[1] == '.' && in[2] == '.' && IsDirSep(in[3]) )
    in += 3;
  while( in[0] == '.' && in[1] == '/' )
    in += 2;
  if ( IsDirSep(in[0]) )
    *out++ = '/';
#ifdef O_HASSHARES
  if ( in[1] == '/' )
  { in++;
    *out++ = '/';
  }
#endif
  osave[osavep++] = out;

  while(*in)
  { if ( IsDirSep(in[0]) )
    {
    again:
      if ( *in )
      { while( IsDirSep(in[1]) )		/* delete multiple / */
	  in++;
	if ( in[1] == '.' )
	{ if ( IsDirSep(in[2]) )		/* delete /./ */
	  { in += 2;
	    goto again;
	  }
	  if ( in[2] == EOS )		/* delete trailing /. */
	  { *out = EOS;
	    return path;
	  }
	  if ( in[2] == '.' &&		/* delete /foo/../ */
	       (IsDirSep(in[3]) || in[3] == EOS) && osavep > 0 )
	  { out = osave[--osavep];
	    in += 3;
	    goto again;
	  }
	}
      }
      if ( *in )
	in++;
      if ( out > path && !IsDirSep(out[-1]) )
	*out++ = '/';
      osave[osavep++] = out;
    } else
      *out++ = *in++;
  }
  *out++ = *in++;

  return path;
}


char *
dirName(const char *f, char *dir, size_t dirlen)
{ if ( f )
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
  
#ifdef O_XOS
    if ( isalpha(dir[0]) && dir[1] == ':' && dir[2] == EOS )
    { dir[2] = '/';
      dir[3] = EOS;
    }
#endif

    return dir;
  }
  
  return NULL;
}


char *
baseName(const char *f)
{ if ( f )
  { const char *base;
    static char buf[MAXPATHLEN];
    int len;

    for(base = f; *f; f++)
    { if ( IsDirSep(*f) && !IsDirSep(f[1]) && f[1] != EOS )
	base = f+1;
    }

    len = f - base;
    strcpy(buf, base);
    while ( len > 0 && buf[len-1] == '/' )
      len--;
    buf[len] = EOS;

    return buf;
  }

  return NULL;
}


static char   CWDdir[MAXPATHLEN];

static Name
getWorkingDirectoryPce(Pce pce)
{
#ifdef __unix__
  static dev_t device;
  static ino_t inode;
  struct stat buf;

  if ( stat(".", &buf) != 0 )
  { errorPce(CtoName("."), NAME_cannotStat);
    return NULL;
  }

  if ( CWDdir[0] == EOS || buf.st_ino != inode || buf.st_dev != device )
  {
#endif

#if HAVE_GETCWD
    if ( !getcwd(CWDdir, sizeof(CWDdir)) )
    { errorPce(CtoName("."), NAME_ioError, getOsErrorPce(PCE));
      return NULL;
    }
#else
    if ( getwd(CWDdir) == 0 )
    { errorPce(CtoName("."), NAME_ioError, getOsErrorPce(PCE));
      return NULL;
    }
#endif

#ifdef __unix__
    inode = buf.st_ino;
    device = buf.st_dev;
  }
#endif

  return FNToName(CWDdir);
} 



int
isAbsolutePath(const char *p)
{
#ifdef O_XOS
  return _xos_is_absolute_filename(p);
#else
  return IsDirSep(p[0]) || p[0] == '~';
#endif
}

#define isRelativePath(p) ( p[0] == '.' )


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
absolutePath(const char *file, char *path, size_t buflen)
	Convert a filename in UTF-8 to an absolute and canonical path in
	UTF-8 notation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
absolutePath(const char *file, char *path, size_t buflen)
{ if ( !file ) 
    return -1;				/* propagate error */
  
  if ( !isAbsolutePath(file) )
  { Name cwd;
    const char *ucwd;
    char *s;
    size_t ul;

    if ( !(cwd = getWorkingDirectoryPce(PCE)) )
      return -1;
    ucwd = charArrayToUTF8((CharArray)cwd);

    if ( (ul=strlen(ucwd)) + strlen(file) + 2 >= buflen )
    { errno = ENAMETOOLONG;
      return -1;
    }
    memcpy(path, ucwd, ul);
    s = path + ul;
    *s++ = '/';
    strcpy(s, file);
  } else if ( strlen(file)+1 > buflen )
  { errno = ENAMETOOLONG;
    return -1;
  } else
    strcpy(path, file);

  canonisePath(path);

  return strlen(path);
}


		/********************************
		*       ~ AND $ EXPANSION	*
		********************************/

#define USERNAME_MAX 20

static size_t
takeWord(const wchar_t *s)
{ size_t n = 0;

  while( *s && (iswalnum(*s) || *s == '_') )
  { n++, s++;
  }
  
  return n;
}


static inline wchar_t *
GETENV(const wchar_t *var, size_t len)
{ Name val;

  val = getEnvironmentVariablePce(PCE, WCToName(var, len));

  return val ? charArrayToWC((CharArray)val, NULL) : NULL;
}


int
expandFileNameW(const wchar_t *pattern, wchar_t *bin, size_t binlen)
{ wchar_t *expanded = bin;
  int size = 0;
  wint_t c;

  binlen--;				/* space for EOS */

  if ( *pattern == '~' )
  {
#ifdef HAVE_GETPWNAM
    static Name fred;
    static Name fredLogin;
#endif
    wchar_t *value;
    const wchar_t *s = ++pattern;	/* after ~ */
    size_t l;

    if ( (l = takeWord(s)) > USERNAME_MAX )
    { ExpandProblem = CtoName("User name too long");
      return -1;
    }
    if ( s[l] && !IsDirSep(s[l]) )	/* ~shhs[^/] */
      goto nouser;
    pattern = &s[l];

    if ( l == 0 )			/* ~/bla */
    { static Name myhome;

      if ( !myhome )
      {
#ifdef O_XOS
	myhome = UTF8ToName(_xos_home());
#else /*O_XOS*/
	myhome = getEnvironmentVariablePce(PCE, CtoName("HOME"));
#endif
	if ( !myhome)
	  myhome = CtoName("/");
      }
	
      value = charArrayToWC((CharArray)myhome, NULL);
    } else				/* ~fred */
#ifdef HAVE_GETPWNAM
    { struct passwd *pwent;
      Name user;
      
      user = WCToName(s, l);

      if ( fred != user )
      { if ( (pwent = getpwnam(charArrayToMB((CharArray)user))) == (struct passwd *) NULL )
	{ ExpandProblem = CtoName("Unknown user");
	  return -1;
	}
	fred      = user;
	fredLogin = MBToName(pwent->pw_dir);
      }
      value = charArrayToWC((CharArray)fredLogin, NULL);
    }	  
#else
    { ExpandProblem = CtoName("Unknown user");
      return -1;
    }
#endif

    size += (l = wcslen(value));
    if ( size >= binlen )
    { ExpandProblem = CtoName("Name too long");
      return -1;
    }
    wcscpy(expanded, value);
    expanded += l;

					/* avoid ~/ --> // */
    if ( IsDirSep(expanded[-1]) && IsDirSep(pattern[0]) )
      pattern++;
  }

nouser:
  for( ;; )
  { switch( c = *pattern++ )
    { case EOS:
	break;
      case '$':
	{ size_t varlen = takeWord(pattern);

	  if ( varlen > 0 )
	  { wchar_t *value = GETENV(pattern, varlen);
	    int l;

	    if ( !value )
	    { ExpandProblem = CtoName("Unknown variable");
	      return -1;
	    }
	    size += (l = wcslen(value));
	    if ( size >= binlen )
	    { errno = ENAMETOOLONG;
	      return -1;
	    }
	    wcscpy(expanded, value);
	    expanded += l;

	    pattern += varlen;
	    continue;
	  }

	  /*FALLTHROUGH*/
	}
      default:
	if ( ++size >= binlen )
	{ errno = ENAMETOOLONG;
	  return -1;
	}
	*expanded++ = c;

	continue;
    }
    break;
  }

  *expanded = EOS;

  /*DEBUG(NAME_path, Cprintf("Expanded %s to %s at %p\n",
			     pattern, bin, bin));*/

  return expanded-bin;
}




