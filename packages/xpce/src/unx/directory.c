/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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

/*  Sun Common Lisp 3.0 apparently redefines readdir(3) and associated
    functions by redefining the dirent structure.  In order to avoid
    having to generate two versions of PCE.o, we assume that when PCE
    is used with SCL HOST->system starts with "SCL"
 */

static char *
nameOfDirectoryEntry(struct dirent *d)
{ char *s;
  static int sclhack = -1;

  if ( sclhack < 0 )
    sclhack = prefixstr(strName(HostObject()->system), "SCL");

  s = d->d_name;
  if ( sclhack )
    s -= sizeof(long);			/* subtract one word */

  return s;
}


static status
initialiseDirectory(Directory d, Name name)
{ char *expanded;

  assign(d, name, name);

  if ( !(expanded = expandFileName(strName(name))) )
    return errorPce(d, NAME_badFileName, ExpandProblem);

#ifdef O_XOS
  { char buf[MAXPATHLEN];
    expanded = _xos_canonical_filename(expanded, buf);
  }
#endif

  assign(d, path, CtoName(absolutePath(expanded)));
  assign(d, name, CtoName(baseName(expanded)));

  succeed;
}


static Directory
getConvertDirectory(Class class, Name name)
{ answer(answerObject(ClassDirectory, name, 0));
}


static status
existsDirectory(Directory d)
{ struct stat buf;

  if ( stat(strName(d->path), &buf) == -1 ||
       (buf.st_mode & S_IFMT) != S_IFDIR )
    fail;

  succeed;
}


static status
makeDirectory(Directory d)
{ if ( !existsDirectory(d) )
  { if ( mkdir(strName(d->path), 0777) != 0 )
      return errorPce(d, NAME_mkdir, getOsErrorPce(PCE));
  }

  succeed;
}


static status
removeDirectory(Directory d)
{ if ( rmdir(strName(d->path)) != 0 )
  { if ( existsDirectory(d) )
      return errorPce(d, NAME_rmdir, getOsErrorPce(PCE));
  }

  succeed;
}


status
cdDirectory(Directory d)
{ if ( chdir(strName(d->path)) )
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

  if ( files != dirs )
  { TRY(pushDirectory(d));
    if ( !(dirp = opendir(".")) )
    { errorPce(d, NAME_readDirectory, getOsErrorPce(PCE));
      popDirectory(d);
      fail;
    }

    for (dp=readdir(dirp); dp!=NULL; dp=readdir(dirp))
    { char *name = nameOfDirectoryEntry(dp);
      struct stat buf;

      if ( all != ON && name[0] == '.' )
	continue;

      if ( stat(name, &buf) != 0 )
	continue;

      if ( (notNil(files) && (buf.st_mode & S_IFMT) == S_IFREG) )
      { if ( notDefault(pattern) )
	{ CharArray ca = CtoScratchCharArray(name);

	  if ( !searchRegex(pattern, ca, DEFAULT, DEFAULT) )
	  { doneScratchCharArray(ca);
	    continue;
	  }
	  doneScratchCharArray(ca);
	}
	appendChain(files, CtoName(name));
      } else if ( (notNil(dirs) && (buf.st_mode & S_IFMT) == S_IFDIR) )
	appendChain(dirs, CtoName(name));


    }
    closedir(dirp);
    popDirectory(d);

    if ( notNil(dirs) )
      sortNamesChain(dirs);
    if ( notNil(files) )
      sortNamesChain(files);
  } else if ( notNil(files) )
  { if ( !(dirp = opendir(strName(d->path))) )
      return errorPce(d, NAME_readDirectory, getOsErrorPce(PCE));
    for (dp=readdir(dirp); dp!=NULL; dp=readdir(dirp))
    { char *name = nameOfDirectoryEntry(dp);
      
      if ( notDefault(pattern) )
      { CharArray ca = CtoScratchCharArray(name);

	if ( !searchRegex(pattern, ca, DEFAULT, DEFAULT) )
	{ doneScratchCharArray(ca);
	  continue;
	}
	doneScratchCharArray(ca);
      }

      appendChain(files, CtoName(name));
    }
    closedir(dirp);

    sortNamesChain(files);
  }

  succeed;
}


static Chain
getDirectoriesDirectory(Directory d, Regex pattern, Bool all)
{ Chain dirs = answerObject(ClassChain, 0);

  TRY(scanDirectory(d, NIL, dirs, pattern, all));

  answer(dirs);
}


static Chain
getFilesDirectory(Directory d, Regex pattern, Bool all)
{ Chain files = answerObject(ClassChain, 0);

  TRY(scanDirectory(d, files, NIL, pattern, all));

  answer(files);
}



static Directory
getParentDirectory(Directory d)
{ char *here = strName(d->path);

  if ( IsDirSep(here[0]) && here[1] == EOS ) /* the root */
    fail;

  answer(answerObject(ClassDirectory,
		      CtoName(dirName(here)), 0));
}


static FileObj
getFileDirectory(Directory d, Name name)
{ char buf[MAXPATHLEN];

  sprintf(buf, "%s/%s", strName(d->path), strName(name));

  answer(answerObject(ClassFile, CtoName(buf), 0));
}


static Directory
getDirectoryDirectory(Directory d, Name name)
{ if ( streq(strName(name), "..") )
    return getParentDirectory(d);
  else
  { char buf[MAXPATHLEN];

    sprintf(buf, "%s/%s", strName(d->path), strName(name));

    answer(answerObject(ClassDirectory, CtoName(buf), 0));
  }
}


static status
accessDirectory(Directory d, Name mode)
{ int m;

  if ( equalName(mode, NAME_read) )
    m = R_OK;
  else /*if ( equalName(mode, NAME_write) )*/
    m = W_OK;
  
  if ( access(strName(d->path), m) == 0 )
    succeed;

  fail;
}


static Name
getPrintNameDirectory(Directory dir)
{ answer(isName(dir->path) ? dir->path : dir->name);
}


status
makeClassDirectory(Class class)
{ sourceClass(class, makeClassDirectory, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name", NAME_get,
	     "Name of the directory");
  localClass(class, NAME_path, NAME_name, "name", NAME_get,
	     "Full path name");

  termClass(class, "directory", 1, NAME_name);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "path=name",
	     "Create from name",
	     initialiseDirectory);
  sendMethod(class, NAME_exists, NAME_test, 0,
	     "Test if directory exists",
	     existsDirectory);
  sendMethod(class, NAME_access, NAME_test, 1, "{read,write}",
	     "Test if directory has access {read, write}",
	     accessDirectory);
  sendMethod(class, NAME_make, NAME_edit, 0,
	     "Create the OS counterpart",
	     makeDirectory);
  sendMethod(class, NAME_remove, NAME_edit, 0,
	     "Delete the OS counterpart",
	     removeDirectory);
  sendMethod(class, NAME_cd, NAME_workingDirectory, 0,
	     "Change to this directory",
	     cdDirectory);
  sendMethod(class, NAME_push, NAME_workingDirectory, 0,
	     "->cd until ->pop",
	     pushDirectory);
  sendMethod(class, NAME_pop, NAME_workingDirectory, 0,
	     "->cd back to old directory",
	     popDirectory);
  sendMethod(class, NAME_scan, NAME_contents, 4,
	     "files=chain*", "directories=chain*",
	     "pattern=[regex]", "hidden_too=[bool]",
	     "Get member files and directories",
	     scanDirectory);

  getMethod(class, NAME_parent, NAME_hierarchy, "directory", 0,
	    "New directory for parent directory",
	    getParentDirectory);
  getMethod(class, NAME_files, NAME_contents, "names=chain", 2,
	    "pattern=[regex]", "hidden_too=[bool]",
	    "New chain with names of member files",
	    getFilesDirectory);
  getMethod(class, NAME_file, NAME_contents, "file", 1, "name",
	    "New file object with name in directory",
	    getFileDirectory);
  getMethod(class, NAME_directory, NAME_contents, "directory", 1, "name",
	    "New directory object with name in directory",
	    getDirectoryDirectory);
  getMethod(class, NAME_directories, NAME_contents, "names=chain", 2,
	    "pattern=[regex]", "hidden_too=[bool]",
	    "New chain with names of member directories",
	    getDirectoriesDirectory);
  getMethod(class, NAME_convert, NAME_conversion, "directory", 1, "name",
	    "Convert directory name",
	    getConvertDirectory);
  getMethod(class, NAME_printName, DEFAULT, "text=char_array", 0,
	    "Equivalent to <-path",
	    getPrintNameDirectory);

  DirectoryStack = globalObject(NAME_directoryStack, ClassChain, 0);
  DEBUG(NAME_directory, printf("DirectoryStack = %s\n", pp(DirectoryStack)));

  succeed;
}


		/********************************
		*           PRIMITIVES		*
		********************************/

static char *
canonisePath(register char *path)
{ register char *out = path;
  char *osave[100];
  int  osavep = 0;
  char *bsave = out;

  while( IsDirSep(path[0]) && path[1] == '.' &&
	 path[2] == '.'    && IsDirSep(path[3]) )
    path += 3;

  while(*path)
  { if ( IsDirSep(*path) )
    { while(IsDirSep(path[1]))
	path++;
      while (path[1] == '.' && (IsDirSep(path[2]) || path[2] == EOS))
	path += 2;
      while (path[1] == '.' && path[2] == '.' && IsDirSep(path[3]))
      { out = osave[--osavep];
	path += 3;
      }
      osave[osavep++] = out;
      *out++ = *path++;
    } else
      *out++ = *path++;
  }
  *out++ = *path++;

  return bsave;
}


char *
dirName(char *f)
{ if ( f )
  { static char dir[MAXPATHLEN];
    char *base, *p;

    for(base = p = f; *p; p++)
      if ( IsDirSep(*p) )
	base = p;
    strncpy(dir, f, base-f);
    dir[base-f] = EOS;
    if ( IsDirSep(*f) && dir[0] == EOS )
    { dir[0] = *f;
      dir[1] = EOS;
    }
  
    return dir;
  }
  
  return NULL;
}


char *
baseName(register char *f)
{ if ( f )
  { char *base;

    for(base = f; *f; f++)
      if ( IsDirSep(*f) )
	base = f+1;

    return base;
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
      return NULL;
#else
    if ( getwd(CWDdir) == 0 )
      return NULL;
#endif

#ifdef __unix__
    inode = buf.st_ino;
    device = buf.st_dev;
  }
#endif

  return CtoName(CWDdir);
} 


#define isAbsolutePath(p) ( IsDirSep(p[0]) || p[0] == '~' )
#define isRelativePath(p) ( p[0] == '.' )

char *
absolutePath(char *file)
{ static char path[MAXPATHLEN];
  
  if ( !file ) 
    return NULL;			/* propagate error */
  
  if ( !isAbsolutePath(file) )
  { Name cwd = getWorkingDirectoryPce(PCE);

    if ( strlen(strName(cwd)) + strlen(file) + 2 >= MAXPATHLEN )
    { ExpandProblem = CtoName("Path name too long");
      return NULL;
    }
    strcpy(path, strName(cwd));
    strcat(path, "/");
    strcat(path, file);
  } else
    strcpy(path, file);

  return canonisePath(path);
}

		/********************************
		*       ~ AND $ EXPANSION	*
		********************************/

static char *
takeWord(char **string)
{ static char wrd[MAXPATHLEN];
  register char *s = *string;
  register char *q = wrd;
  register int left = MAXPATHLEN-1;

  while( *s && (isalnum(*s) || *s == '_') )
  { if ( --left < 0 )
    { ExpandProblem = CtoName("Variable or user name too long");
      return (char *) NULL;
    }
    *q++ = *s++;
  }
  *q = EOS;
  
  *string = s;

  return wrd;
}


static inline char *
GETENV(char *var)
{ Name val = getEnvironmentVariablePce(PCE, CtoName(var));

  return val ? strName(val) : NULL;
}


char *
expandFileName(char *pattern)
{ static char bin[MAXPATHLEN];
  char *expanded = bin;
  int size = 0;
  char c;

  if ( *pattern == '~' )
  { static char fred[20];
    static char fredLogin[MAXPATHLEN];
    extern struct passwd *getpwnam(const char *);
    char *user;
    char *value;
    int l;

    pattern++;
    if ( (user = takeWord(&pattern)) == NULL )
      return NULL;

    if ( user[0] != EOS || (value = GETENV("HOME")) == (char *) NULL )
    {
#if HAVE_PWD_H
      struct passwd *pwent;

      if ( !streq(fred, user) )
      { if ( (pwent = getpwnam(user)) == (struct passwd *) NULL )
	{ ExpandProblem = CtoName("Unknown user");
	  return NULL;
	}
	strcpy(fred, user);
	strcpy(fredLogin, pwent->pw_dir);
      }
      value = fredLogin;
#else
      ExpandProblem = CtoName("Unknown user");
      return NULL;
#endif /*HAVE_PWD_H*/
    }	  
    size += (l = (int) strlen(value));
    if ( size >= MAXPATHLEN )
    { ExpandProblem = CtoName("Name too long");
      return NULL;
    }
    strcpy(expanded, value);
    expanded += l;
  }

  for( ;; )
  { switch( c = *pattern++ )
    { case EOS:
	break;
      case '$':
	{ char *var = takeWord(&pattern);
	  char *value = GETENV(var);
	  int l;

	  if ( value == (char *) NULL )
	  { ExpandProblem = CtoName("Unknown variable");
	    return NULL;
	  }
	  size += (l = (int)strlen(value));
	  if ( size >= MAXPATHLEN )
	  { ExpandProblem = CtoName("Name too long");
	    return NULL;
	  }
	  strcpy(expanded, value);
	  expanded += l;

	  continue;
	}
      default:
	if ( ++size >= MAXPATHLEN )
	{ ExpandProblem = CtoName("Name too long");
	  return NULL;
	}
	*expanded++ = c;

	continue;
    }
    break;
  }

  if ( ++size >= MAXPATHLEN )
  { ExpandProblem = CtoName("Name too long");
    return NULL;
  }
  *expanded++ = EOS;

  DEBUG(NAME_path, printf("Expanded %s to %s at %p\n", pattern,bin,bin));

  return bin;
}
