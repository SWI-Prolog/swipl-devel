/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#else
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif
#endif

#if defined(__linux__) && !defined(PATH_MAX)
#include <linux/limits.h>
#endif

static Sheet FileFilters;

static status
initialiseFile(FileObj f, Name name, Name kind)
{ if ( isDefault(kind) )
    kind = NAME_text;

#if O_XOS
  { char buf[MAXPATHLEN];
    
    _xos_canonical_filename(strName(name), buf);
    assign(f, name, CtoName(buf));
  }
#else
  assign(f, name, name);
#endif
  assign(f, path, DEFAULT);
  assign(f, kind, kind);
  assign(f, status, NAME_closed);
  f->fd = NULL;

  succeed;
}


static status
kindFile(FileObj f, Name kind)
{ if ( f->status != NAME_closed )
    return errorPce(f, NAME_noChangeAfterOpen);

  assign(f, kind, kind);

  succeed;
}


Name
getOsNameFile(FileObj f)
{ if ( notDefault(f->path) )
    answer(CtoName(strName(f->path)));

  answer(CtoName(expandFileName(strName(f->name))));
}


static status
unlinkFile(FileObj f)
{ return closeFile(f);
}


static status
storeFile(FileObj f, FileObj file)
{ return storeSlotsObject(f, file);
}


static status
loadFile(FileObj f, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(f, fd, def));	/* reopen? */

  if ( isNil(f->path) )
    assign(f, path, DEFAULT);		/* backward compatibility load */
  if ( isNil(f->kind) )
    assign(f, kind, NAME_binary);	/* same */

  assign(f, status, NAME_closed);
  f->fd = NULL;

  succeed;
}


static FileObj
getConvertFile(Class class, Name name)
{ answer(answerObject(ClassFile, name, 0));
}


status
closeFile(FileObj f)
{ if ( f->status != NAME_closed )
  { status rval = checkErrorFile(f);

#ifdef HAVE_POPEN
    if ( notNil(f->filter) )
      pclose(f->fd);
    else
#endif
    { if ( f->fd != stdin && f->fd != stdout && f->fd != stderr ) 
	fclose(f->fd);
    }
    f->fd = NULL;
    assign(f, status, NAME_closed);

    return rval;
  }

  succeed;
}


status
existsFile(FileObj f, Bool mustbefile)
{ struct stat buf;
  Name name;

  if ( (name = getOsNameFile(f)) )
  { if ( stat(strName(name), &buf) == -1 )
      fail;
    if ( mustbefile != OFF && (buf.st_mode & S_IFMT) != S_IFREG )
      fail;
    succeed;
  }

  fail;
}


static status
sameFile(FileObj f1, FileObj f2)
{ Name n1 = getOsNameFile(f1);
  Name n2 = getOsNameFile(f2);

  if ( !n1 || !n2 )
    fail;

  if ( equalCharArray((CharArray)n1, (CharArray)n2) )
    succeed;

#if O_XOS
					/* verylongfilename == verylong !!! */
  { char b1[MAXPATHLEN];
    char b2[MAXPATHLEN];

    _xos_limited_os_filename(strName(n1), b1);
    _xos_limited_os_filename(strName(n2), b2);
    if ( streq(b1, b2) )
      succeed;
  }
#endif

#if __unix__
  { struct stat buf1;
    struct stat buf2;

    if ( stat(strName(n1), &buf1) == 0 &&
	 stat(strName(n2), &buf2) == 0 &&
	 buf1.st_ino == buf2.st_ino &&
	 buf1.st_dev == buf2.st_dev )
      succeed;
  }
#endif

  fail;
}



static status
absolutePathFile(FileObj f)
{ char *path = absolutePath(expandFileName(strName(f->name)));

  if ( path )
  { assign(f, path, CtoName(path));
    succeed;
  }

  fail;
}


Name
getAbsolutePathFile(FileObj f)
{ if ( notDefault(f->path) )
    answer(f->path);

  answer(CtoName(absolutePath(expandFileName(strName(f->name)))));
}


#define CPBUFSIZE 4096

static status
copyFile(FileObj to, FileObj from)
{ if ( openFile(from, NAME_read, DEFAULT, DEFAULT) &&
       openFile(to, NAME_write, DEFAULT, DEFAULT) )
  { char buf[CPBUFSIZE];
    int fdfrom = fileno(from->fd);
    int fdto   = fileno(to->fd);
    status rval;
    int n;

    while( (n = read(fdfrom, buf, CPBUFSIZE)) > 0 )
      if ( write(fdto, buf, n) != n )
      { errorPce(to, NAME_ioError, getOsErrorPce(PCE));
	rval = FAIL;
	goto out;
      }
    if ( n < 0 )
    { errorPce(from, NAME_ioError, getOsErrorPce(PCE));
      rval = FAIL;
    } else
      rval = SUCCEED;
out:
    closeFile(from);
    closeFile(to);

    return rval;
  }

  fail;
}


#if O_DOSFILENAMES
status
backup_name(char *old, char *ext, char *bak)
{ char base[100];
  char dir[MAXPATHLEN];

  strcpy(base, baseName(old));
  strcpy(dir, dirName(old));

  if ( ext[0] == '.' )
  { char *s;

    if ( (s = strchr(base, '.')) )
      strcpy(s, ext);
  } else
  { char *s;

    if ( (s = strchr(base, '.')) )
    { int l, e;

      s++;				/* skip the '.' */
      l = strlen(s);
      e = strlen(ext);
      if ( (l + e) > 3 )
      { l = 3-e;
	if ( l < 0 )
	  l = 0;
	strncpy(&s[l], ext, 3);
	s[3] = EOS;
      } else
	strcat(s, ext);
    } else
    { int l;

      strcat(base, ext);
      if ( (l=strlen(base)) > 8 )
      { memmove(&base[9], &base[8], l-8);
	base[8] = '.';
	base[l+1] = EOS;
      }
    }
  }

  if ( dir[0] )
    sprintf(bak, "%s/%s", dir, base);
  else
    strcpy(bak, base);

  DEBUG(NAME_backup, printf("Backup %s in %s\n", old, bak));

  succeed;
}

#else /*O_DOSFILENAMES*/

status
backup_name(char *old, char *ext, char *bak)
{ sprintf(bak, "%s%s", old, ext);

  succeed;
}

#endif /*O_DOSFILENAMES*/

static Name
getBackupFileNameFile(FileObj f, Name ext)
{ char bak[MAXPATHLEN];

  backup_name(strName(getOsNameFile(f)),
	      isDefault(ext) ? "~" : strName(ext),
	      bak);

  answer(CtoName(bak));
}


status
backupFile(FileObj f, Name ext)
{ if ( existsFile(f, ON) )
  { Name new = get(f, NAME_backupFileName, ext, 0);
    char *old = strName(getOsNameFile(f));
    int fdfrom = -1, fdto = -1;
    status rval = FAIL;

    if ( (fdfrom = open(old, O_RDONLY)) > 0 &&
	 (fdto   = open(strName(new), O_WRONLY|O_CREAT|O_TRUNC, 0666)) )
    { char buf[CPBUFSIZE];
      int n;

      while( (n = read(fdfrom, buf, CPBUFSIZE)) > 0 )
	if ( write(fdto, buf, n) != n )
	{ rval = FAIL;
	  goto out;
	}
      rval = (n == 0) ? SUCCEED : FAIL;
    }

out:
    if ( rval == FAIL )
      errorPce(f, NAME_backupFile, new, getOsErrorPce(PCE));

    if ( fdfrom >= 0 )
      close(fdfrom);
    if ( fdto >= 0 )
      close(fdto);

    return rval;
  }

  succeed;
}


static status
accessFile(FileObj f, Name mode)
{ int m;
  Name name = getOsNameFile(f);

  if ( name )
  { if ( equalName(mode, NAME_read) )
      m = R_OK;
    else if ( equalName(mode, NAME_write) || equalName(mode, NAME_append) )
      m = W_OK;
    else /*if ( equalName(mode, NAME_execute) )*/
      m = X_OK;
  
    if ( access(strName(name), m) == 0 )
      succeed;
  }

  fail;
}


static Attribute
getFilterFile(FileObj f)
{ Cell cell;

  closeFile(f);

  for_cell(cell, FileFilters->attributes)
  { char path[MAXPATHLEN];
    Attribute a = cell->value;
    Name extension = a->name;
    struct stat buf;

    if ( !isName(extension) )
    { errorPce(extension, NAME_unexpectedType, TypeName);
      fail;
    }

    sprintf(path, "%s%s", strName(f->name), strName(extension));
    if ( stat(path, &buf) == 0 &&
	 (buf.st_mode & S_IFMT) == S_IFREG )
    { if ( !isName(a->value) )
      { errorPce(a->value, NAME_unexpectedType, TypeName);
	fail;
      }

      answer(a);
    }
  }

  fail;
}


status
openFile(FileObj f, Name mode, Name filter, CharArray extension)
{ CharArray path;
  Name name = getOsNameFile(f);
  char fdmode[3];
  
  closeFile(f);

  if ( !name )
    fail;

  if ( isDefault(filter) )
    filter = f->filter;

  if ( isDefault(extension) )
    path = (CharArray) name;
  else
    path = getAppendCharArray((CharArray) name, extension);

  if ( mode == NAME_write )
    fdmode[0] = 'w';
  else if ( mode == NAME_append )
    fdmode[0] = 'a';
  else /* read */
    fdmode[0] = 'r';

  if ( f->kind == NAME_text )
#ifdef __WATCOMC__
    fdmode[1] = 't';
#else
    fdmode[1] = '\0';
#endif
  else
    fdmode[1] = 'b';

  fdmode[2] = '\0';

  if ( isNil(filter) )
  { if ( mode == NAME_read && f->name == NAME_userInput )
      f->fd = stdin;
    else if ( (mode == NAME_write || mode == NAME_append) &&
	       f->name == NAME_userOutput )
      f->fd = stdout;
    else if ( (mode == NAME_write || mode == NAME_append) &&
	       f->name == NAME_userError )
      f->fd = stderr;
    else
    { DEBUG(NAME_file, printf("Opening %s (%s) using mode %s\n",
			      pp(f->name), pp(f), fdmode));
      f->fd = fopen(strName(path), fdmode);
    }
  } else
#ifndef HAVE_POPEN
  { return errorPce(f, CtoName("no_popen"));
  }
#else
  { char cmd[LINESIZE];

    if ( fdmode[0] == 'a' )
      fdmode[0] = 'w';

    sprintf(cmd, "%s %s %s",
	    strName(filter), 
	    mode == NAME_read ? "<" : mode == NAME_write ? ">" : ">>",
	    strName(path));
    f->fd = popen(cmd, fdmode);
  }
#endif /*HAVE_POPEN*/

  if ( f->fd == NULL )
  { if ( isNil(filter) && mode == NAME_read && errno == ENOENT )
    { Attribute a;

      if ( (a = get(f, NAME_filter, 0)) )
      { if ( !isName(a->value) || !isName(a->name) )
	  fail;
	return openFile(f, mode, a->value, a->name);
      }
    }

    return errorPce(f, NAME_openFile, mode, getOsErrorPce(PCE));
  }

  assign(f, filter, filter);
  assign(f, status, mode);

  succeed;
}


status
removeFile(FileObj f)
{ Name name = getOsNameFile(f);

  if ( !name )
    fail;

  closeFile(f);				/* Ok? */

  if ( unlink(strName(name)) == 0 )
    succeed;
  if ( existsFile(f, OFF) )
    return errorPce(f, NAME_removeFile, getOsErrorPce(PCE));
  
  fail;
}


static status
nameFile(FileObj f, Name name)
{ int rval;
  Name nm = getOsNameFile(f);
  char *old, *new;

  if ( !nm || !(new = expandFileName(strName(name))) )
    fail;
  old = strName(nm);

  if ( existsFile(f, OFF) )
  {
#ifndef __unix__
    rval = rename(old, new);
#else
    unlink(new);
    if ((rval = link(old, new)) == 0 && (rval = unlink(old)) != 0)
      unlink(new);
#endif /*__unix__*/

    if ( rval == 0 )
    { assign(f, name, name);
      succeed;
    }

    return errorPce(f, NAME_renameFile, name, getOsErrorPce(PCE));
  } else
  { assign(f, name, name);
    succeed;
  }
}


static status
check_file(FileObj f, Name mode)
{ if ( (mode == f->status) ||
       (mode == NAME_write && f->status == NAME_append) ||
       (mode == NAME_open && f->status != NAME_closed) )
    succeed;

  return errorPce(f, NAME_notOpenFile, mode);
}


static Int
getIndexFile(FileObj f)
{ TRY( check_file(f, NAME_open) );

  answer(toInt(ftell(f->fd)));
}


static status
seekFile(FileObj f, Int index, Name whence)
{ TRY( check_file(f, NAME_open) );
  if ( notNil(f->filter) )
    return errorPce(f, NAME_cannotSeekNonFile);

  if ( isDefault(whence) )
    whence = NAME_start;

  if ( fseek(f->fd, valInt(index), whence == NAME_start ? 0 :
				   whence == NAME_here ? 1 :
				   2 ) == -1 )
    return errorPce(f, NAME_seekFile, index, whence, getOsErrorPce(PCE));

  succeed;
}


static status
append_file(FileObj f, String str)
{ TRY( check_file(f, NAME_write) );
  fwrite(str->s_text,
	 isstr8(str) ? sizeof(char8) : sizeof(char16),
	 str->size, 
	 f->fd);

  succeed;
}


static status
newlineFile(FileObj f)
{ return append_file(f, str_nl(NULL));	/* only ASCII */
}


static status
appendFile(FileObj f, CharArray str)
{ return append_file(f, &str->data);
}


static status
formatFile(FileObj f, CharArray fmt, int argc, Any *argv)
{ string s;

  TRY(str_writefv(&s, fmt, argc, argv));
  append_file(f, &s);
  str_unalloc(&s);

  succeed;
}


static status
flushFile(FileObj f)
{ if ( f->fd )
    fflush(f->fd);

  succeed;
}


Int
getSizeFile(FileObj f)
{ struct stat buf;
  int rval;

  if ( f->fd != NULL )
    rval = fstat(fileno(f->fd), &buf);
  else
  { Name name = getOsNameFile(f);

    if ( !name )
      fail;
    rval = stat(strName(name), &buf);
  }

  if ( rval == -1 )
  { errorPce(f, NAME_cannotStat, getOsErrorPce(PCE));
    fail;
  }

  answer(toInt(buf.st_size));
}


static Date
getTimeFile(FileObj f, Name which)
{ struct stat buf;
  Name name = getOsNameFile(f);

  if ( !name )
    fail;

  if ( isDefault(which) )
    which = NAME_modified;

  if ( stat(strName(name), &buf) < 0 )
  { errorPce(f, NAME_cannotStat, getOsErrorPce(PCE));
    fail;
  }
  
  if ( equalName(which, NAME_modified) )
    answer(CtoDate(buf.st_mtime));
  else
    answer(CtoDate(buf.st_atime));
}


Name
getBaseNameFile(FileObj f)
{ answer(CtoName(baseName(expandFileName(strName(f->name)))));
}


static Name
getDirectoryNameFile(FileObj f)
{ answer(CtoName(dirName(expandFileName(strName(f->name)))));
}


static StringObj
getReadLineFile(FileObj f)
{ char tmp[LINESIZE];

  TRY( check_file(f, NAME_read) );

  if (fgets(tmp, LINESIZE, f->fd) == NULL)
  { closeFile(f);
    fail;
  }

  answer(CtoString(tmp));
}


static StringObj
getReadFile(FileObj f, Int n)
{ int size;
  int m;

  TRY( check_file(f, NAME_read) );
  if ( isDefault(n) )
    TRY(n = getSizeFile(f));

  size = valInt(n);
  { CharBuf(buf, size);

    if ( (m = fread(buf, 1, size, f->fd)) == 0 )
    { errorPce(f, NAME_ioError, getOsErrorPce(PCE), 0);
      fail;
    }
    buf[m] = EOS;

    answer(CtoString(buf));
  }
}


static Int
getCharacterFile(FileObj f)
{ char chr;

  TRY( check_file(f, NAME_read) );
  if ( feof(f->fd) )
    fail;

  chr = getc(f->fd);
  
  answer(toInt(chr));
}


		/********************************
		*       SAVE/LOAD SUPPORT	*
		********************************/

status
reportErrorFile(FileObj f)
{ errorPce(f, NAME_ioError, getOsErrorPce(PCE));
  fail;
}


status
checkErrorFile(FileObj f)
{ if ( f->fd == NULL )
    succeed;

  if ( ferror(f->fd) )
    return reportErrorFile(f);

  succeed;
}


status
storeCharFile(FileObj f, char c)
{ putc(c, f->fd);

  return checkErrorFile(f);
}


void
putstdw(ulong w, FILE *fd)
{
#ifndef WORDS_BIGENDIAN
  union
  { ulong         l;
    unsigned char c[4];
  } cvrt;
  ulong rval;

  cvrt.l = w;
  rval = (cvrt.c[0] << 24) |
         (cvrt.c[1] << 16) |
	 (cvrt.c[2] << 8) |
	  cvrt.c[3];
  putw(rval, fd);
#else /*WORDS_BIGENDIAN*/
  putw(w, fd);
#endif /*WORDS_BIGENDIAN*/
}


status
storeWordFile(FileObj f, Any w)
{ putstdw((ulong) w, f->fd);

  return checkErrorFile(f);
}


status
storeCharpFile(FileObj f, char *s)
{ int l = strlen(s);

  TRY(storeWordFile(f, (Any) l));
  fwrite(s, sizeof(char), l, f->fd);
  
  return checkErrorFile(f);
}


status
storeNameFile(FileObj f, Name n)
{ return storeCharpFile(f, strName(n));
}


status
storeIntFile(FileObj f, Int i)
{ return storeWordFile(f, (Any) valInt(i));
}

		/********************************
		*             PATHS		*
		********************************/

status
findFile(FileObj f, CharArray path, Name mode)
{ char *exp = expandFileName(strName(f->name));
  char base[MAXPATHLEN];
  char name[MAXPATHLEN];
  char *pathstr;
  int m;

  if ( !exp )
    fail;

  if ( exp[0] == '/' || (exp[0] == '.' && exp[1] == '/') )
    succeed;

  if ( isDefault(mode) || equalName(mode, NAME_read) )
    m = R_OK;
  else if ( equalName(mode, NAME_write) || equalName(mode, NAME_append) )
    m = W_OK;
  else /*if ( equalName(mode, NAME_execute) )*/
    m = X_OK;

  if ( notDefault(f->path) && access(strName(f->path), m) == 0 )
    succeed;

  strcpy(base, exp);

  if ( isDefault(path) )
    pathstr = ".";
  else
    pathstr = strName(path);

  while( pathstr && *pathstr )
  { char *end;

    if ( (end = strchr(pathstr, ':')) == NULL )
    { strcpy(name, pathstr);
      pathstr = NULL;
    } else
    { strncpy(name, pathstr, end-pathstr);
      name[end-pathstr] = EOS;
      pathstr = &end[1];
    }
    strcpy(name, expandFileName(name));

    strcat(name, "/");
    strcat(name, base);

    if ( access(name, m) == 0 )
    { assign(f, path, CtoName(name));
      succeed;
    }
  }

  return errorPce(f, NAME_cannotFindFile, path);
}


		/********************************
		*     OBJECT LOADING SAVING	*
		********************************/

status
checkObjectFile(FileObj f)
{ FILE *fd;
  int close = TRUE;
  status rval;
  long l;
  int ls;
  Name okind = f->kind;

  if ( f->status == NAME_read )
  { close = FALSE;
  } else
  { assign(f, kind, NAME_binary);
    TRY(send(f, NAME_open, NAME_read, 0));
  }
  fd = f->fd;
  
  if ( SaveMagic == NULL )
    SaveMagic = SAVEMAGIC;

  ls = strlen(SaveMagic);

  if ( (l=loadWord(fd)) == ls )
  { char tmp[LINESIZE];

    fread(tmp, sizeof(char), sizeof(SAVEMAGIC)-1, fd);
    tmp[ls] = EOS;
    DEBUG(NAME_save, printf("magic = ``%s''; SaveMagic = ``%s''\n",
			    tmp, SaveMagic) );
    if ( strncmp(tmp, SaveMagic, ls - 1) == 0 )
      rval = SUCCEED;
    else
      rval = FAIL;
  } else
  { rval = FAIL;
    DEBUG(NAME_save, printf("First word = %ld, should be %d\n", l, ls) );
  }

  if ( close )
  { assign(f, kind, okind);
    closeFile(f);
  }

  return rval;
}


status
makeClassFile(Class class)
{ sourceClass(class, makeClassFile, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_path, "name=name", NAME_get,
	     "Name of the file");
  localClass(class, NAME_path, NAME_path, "path=[name]", NAME_both,
	     "Full path-name of the file");
  localClass(class, NAME_kind, NAME_fileType, "{text,binary}", NAME_get,
	     "Text or binary file");
  localClass(class, NAME_status, NAME_open, "{closed,read,write}", NAME_get,
	     "One of {closed,read,write}");
  localClass(class, NAME_filter, NAME_filter, "command=name*", NAME_both,
	     "Name of input/output filter used");
  localClass(class, NAME_fd, NAME_internal, "alien:FILE *", NAME_none,
	     "Unix file (stream) handle");

  termClass(class, "file", 1, NAME_name);
  setLoadStoreFunctionClass(class, loadFile, storeFile);
  
  storeMethod(class, NAME_name, nameFile);
  storeMethod(class, NAME_kind, kindFile);
  
  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "path=name", "kind=[{text,binary}]",
	     "Create from name and kind",
	     initialiseFile);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Close file",
	     unlinkFile);
  sendMethod(class, NAME_exists, NAME_test, 1, "must_be_file=[bool]",
	     "Test if file exists",
	     existsFile);
  sendMethod(class, NAME_same, NAME_test, 1, "file=file",
	     "Test if two paths refer to the same physical file",
	     sameFile);
  sendMethod(class, NAME_absolutePath, NAME_path, 0,
	     "Convert <-name to an absolute path",
	     absolutePathFile);
  sendMethod(class, NAME_access, NAME_test, 1,
	     "mode={read,write,append,execute}",
	     "Test if file has access",
	     accessFile);
  sendMethod(class, NAME_append, NAME_write, 1, "text=char_array",
	     "Append string to file",
	     appendFile);
  sendMethod(class, NAME_newline, NAME_write, 0,
	     "Append newline to file",
	     newlineFile);
  sendMethod(class, NAME_format, NAME_write, 2,
	     "format=char_array", "argument=any ...",
	     "Format arguments and ->append",
	     formatFile);
  sendMethod(class, NAME_remove, NAME_delete, 0,
	     "Unlink from Unix file system",
	     removeFile);
  sendMethod(class, NAME_open, NAME_open, 3,
	     "mode={read,write,append}", "filter=[name]",
	     "extension=[char_array]",
	     "Open file in mode, read/write through filter",
	     openFile);
  sendMethod(class, NAME_close, NAME_open, 0,
	     "Close file",
	     closeFile);
  sendMethod(class, NAME_seek, NAME_location, 2,
	     "byte=int", "from=[{start,here,end}]",
	     "Seek to index from {start,here,end}",
	     seekFile);
  sendMethod(class, NAME_checkObject, NAME_file, 0,
	     "Test if file contains a saved PCE object",
	     checkObjectFile);
  sendMethod(class, NAME_find, NAME_location,
	     2, "path=[char_array]", "access=[{read,write,append,execute}]",
	     "Find file in search-path",
	     findFile);
  sendMethod(class, NAME_copy, NAME_copy, 1, "from=file",
	     "Copy to destination file",
	     copyFile);
  sendMethod(class, NAME_backup, NAME_copy, 1, "extension=[name]",
	     "Make a backup by adding extension (~)",
	     backupFile);
  sendMethod(class, NAME_flush, NAME_write, 0,
	     "Flush pending output",
	     flushFile);

  getMethod(class, NAME_character, NAME_read, "char", 0,
	    "Read next character as ASCII value",
	    getCharacterFile);
  getMethod(class, NAME_baseName, NAME_path, "name", 0,
	    "Base name of file in directory",
	    getBaseNameFile);
  getMethod(class, NAME_directoryName, NAME_path, "name", 0,
	    "Directory name of file",
	    getDirectoryNameFile);
  getMethod(class, NAME_backupFileName, NAME_copy, "char_array",
	    1, "extension=[char_array]",
	    "Name for storing ->backup data",
	    getBackupFileNameFile);
  getMethod(class, NAME_read, NAME_read, "string", 1, "count=[int]",
	    "New string width next n characters",
	    getReadFile);
  getMethod(class, NAME_readLine, NAME_read, "string", 0,
	    "New string with next line",
	    getReadLineFile);
  getMethod(class, NAME_size, NAME_dimension, "bytes=int", 0,
	    "Size in characters",
	    getSizeFile);
  getMethod(class, NAME_object, NAME_file, "object=any|function", 0,
	    "New object from file created with ->save_in_file",
	    getObjectFile);
  getMethod(class, NAME_index, NAME_location, "byte=int", 0,
	    "Current index (Unix tell())",
	    getIndexFile);
  getMethod(class, NAME_time, NAME_time, "date=date", 1,
	    "which_time=[{modified,access}]",
	    "New date holding modification/access time",
	    getTimeFile);
  getMethod(class, NAME_convert, DEFAULT, "file", 1, "path=name",
	    "Convert name to file",
	    getConvertFile);
  getMethod(class, NAME_absolutePath, NAME_path, "path=name", 0,
	    "Convert <-name to an absolute path",
	    getAbsolutePathFile);
  getMethod(class, NAME_filter, NAME_filter,
	    "extension_and_filter=attribute", 0,
	    "Determine input filter from extension",
	    getFilterFile);

  FileFilters = globalObject(NAME_compressionFilters, ClassSheet,
			     newObject(ClassAttribute,
				       CtoName(".Z"),
				       CtoName("uncompress"),
				       0),
			     newObject(ClassAttribute,
				       CtoName(".gz"),
				       CtoName("gunzip"),
				       0),
			     0);

  succeed;
}
