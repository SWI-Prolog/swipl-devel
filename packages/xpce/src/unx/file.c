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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
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

#if HAVE_SYS_ACCESS_H			/* AIX 3.2.5 */
#include <sys/access.h>
#endif

#if defined(__linux__) && !defined(PATH_MAX)
#include <linux/limits.h>
#endif

static Sheet FileFilters;

static status
initialiseFile(FileObj f, Name name, Name kind)
{ initialiseSourceSink((SourceSink)f);

  if ( isDefault(kind) )
    kind = NAME_text;

  if ( isDefault(name) )
  {
#ifdef HAVE_TMPNAM
    char namebuf[L_tmpnam];

    name = CtoName(tmpnam(namebuf));
#endif
  }

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
{ char bin[MAXPATHLEN];

  if ( notDefault(f->path) )
    answer(CtoName(strName(f->path)));

  answer(CtoName(expandFileName(strName(f->name), bin)));
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
loadFile(FileObj f, IOSTREAM *fd, ClassDef def)
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
      fclose(f->fd);
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
{ char bin[MAXPATHLEN];
  char *path = absolutePath(expandFileName(strName(f->name), bin));

  if ( path )
  { assign(f, path, CtoName(path));
    succeed;
  }

  fail;
}


Name
getAbsolutePathFile(FileObj f)
{ char bin[MAXPATHLEN];

  if ( notDefault(f->path) )
    answer(f->path);

  answer(CtoName(absolutePath(expandFileName(strName(f->name), bin))));
}


static int
is_absolute_name(const char *s)
{
#ifdef __WIN32__
  if ( isletter(s[0]) && s[1] == ':' )
    succeed;
#endif
  if ( s[0] == '/' )
    succeed;

  fail;
}

status
isAbsoluteFile(FileObj f)
{ char bin[MAXPATHLEN];
  char *name = strName(f->name);
  int n;

  for(n=0; n < 2; n++)
  { if ( is_absolute_name(name) )
      succeed;

    name = expandFileName(name, bin);
  }

  fail;
}


#define CPBUFSIZE 4096

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifdef __WIN32__
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY
#endif

static int
open_file(FileObj f, int access, ...)
{ va_list args;
  int mode;
  int fd = -1;
  Name name;
  
  va_start(args, access);
  mode = va_arg(args, int);
  va_end(args);

  if ( (name = getOsNameFile(f)) )
  { fd = open(strName(name), access, mode);

    if ( fd < 0 )
      errorPce(f, NAME_openFile,
	       (access & O_RDONLY) ? NAME_read : NAME_write,
	       getOsErrorPce(PCE));
  }

  return fd;
}


static status
copyFile(FileObj to, FileObj from)
{ int fdfrom, fdto;
  char buf[CPBUFSIZE];
  status rval;
  int n;
  
  if ( (fdfrom = open_file(from, O_RDONLY|O_BINARY)) < 0 )
    fail;
  if ( (fdto = open_file(to, O_WRONLY|O_BINARY|O_CREAT|O_TRUNC, 0666)) < 0 )
  { close(fdfrom);
    fail;
  }

  while( (n = read(fdfrom, buf, CPBUFSIZE)) > 0 )
  { char *b = buf;

    while(n > 0)
    { int n2;

      if ( (n2=write(fdto, b, n)) < 0 )
      { errorPce(to, NAME_ioError, getOsErrorPce(PCE));
	rval = FAIL;
	goto out;
      }
      b += n2;
      n -= n2;
    }
  }
  if ( n < 0 )
  { errorPce(from, NAME_ioError, getOsErrorPce(PCE));
    rval = FAIL;
  } else
    rval = SUCCEED;

out:
  close(fdfrom);
  close(fdto);

  return rval;
}


#ifdef __WIN32__
static status
dos_backup_name(char *old, char *ext, char *bak)
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

  DEBUG(NAME_backup, Cprintf("Backup %s in %s\n", old, bak));

  succeed;
}

status
backup_name(char *old, char *ext, char *bak)
{ if ( iswin32s() )
    return dos_backup_name(old, ext, bak);
  else
  { sprintf(bak, "%s%s", old, ext);

    succeed;
  }
}

#else /*__WIN32__*/

status
backup_name(char *old, char *ext, char *bak)
{ sprintf(bak, "%s%s", old, ext);

  succeed;
}

#endif /*__WIN32__*/

static Name
getBackupFileNameFile(FileObj f, Name ext)
{ char bak[MAXPATHLEN];

  backup_name(strName(getOsNameFile(f)),
	      isDefault(ext) ? "~" : strName(ext),
	      bak);

  answer(CtoName(bak));
}


static status
backupFile(FileObj f, Name ext)
{ if ( existsFile(f, ON) )
  { Name newname = get(f, NAME_backupFileName, ext, 0);
    char *new;
    char *old = strName(getOsNameFile(f));
    int fdfrom = -1, fdto = -1;
    status rval = FAIL;

    if ( newname )
      new = strName(newname);
    else
      fail;				/* or succeed? */

    if ( (fdfrom = open(old, O_RDONLY)) >= 0 &&
	 (fdto   = open(new, O_WRONLY|O_CREAT|O_TRUNC, 0666)) >= 0 )
    { char buf[CPBUFSIZE];
      int n;

      while( (n = read(fdfrom, buf, CPBUFSIZE)) > 0 )
      { if ( write(fdto, buf, n) != n )
	{ rval = FAIL;
	  goto out;
	}
      }
      rval = (n == 0) ? SUCCEED : FAIL;
    }

out:
    if ( rval == FAIL )
      errorPce(f, NAME_backupFile, newname, getOsErrorPce(PCE));

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
  { if ( mode == NAME_read )
      m = R_OK;
    else if ( mode == NAME_write || mode == NAME_append )
      m = W_OK;
#ifdef X_OK
    else /*if ( mode == NAME_execute )*/
      m = X_OK;
#endif
  
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
  { DEBUG(NAME_file, Cprintf("Opening %s (%s) using mode %s\n",
			     pp(f->name), pp(f), fdmode));
    f->fd = fopen(strName(path), fdmode);
  } else
#ifndef HAVE_POPEN
  { return errorPce(f, NAME_noPopen);
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
  if ( mode == NAME_append )
    mode = NAME_write;
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
  char *old, new[MAXPATHLEN];

  if ( !nm || !expandFileName(strName(name), new) )
    fail;
  old = strName(nm);

  if ( existsFile(f, OFF) )
  {
#ifdef HAVE_RENAME
    remove(new);
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


static Int
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
{ char bin[MAXPATHLEN];

  answer(CtoName(baseName(expandFileName(strName(f->name), bin))));
}


static Name
getDirectoryNameFile(FileObj f)
{ char bin[MAXPATHLEN];

  answer(CtoName(dirName(expandFileName(strName(f->name), bin))));
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
    string q;

    if ( (m = fread(buf, 1, size, f->fd)) < 0 )
    { errorPce(f, NAME_ioError, getOsErrorPce(PCE), 0);
      fail;
    }

    str_inithdr(&q, ENC_ASCII);
    q.size    = m;
    q.s_text8 = buf;

    answer(StringToString(&q));
  }
}


static Int
getCharacterFile(FileObj f)
{ int chr;

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
storeCharFile(FileObj f, int c)
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

#ifndef X_OK
#define X_OK 0
#endif

status
findFile(FileObj f, CharArray path, Name mode)
{ char bin[MAXPATHLEN];
  char *exp = expandFileName(strName(f->name), bin);
  char base[MAXPATHLEN];
  char name[MAXPATHLEN];
  char *pathstr;
  int m;

  if ( !exp )
    fail;

  if ( isAbsolutePath(exp) || streq(exp, ".") )
    succeed;

  if ( isDefault(mode) || mode == NAME_read )
    m = R_OK;
  else if ( mode == NAME_write || mode == NAME_append )
    m = W_OK;
  else /*if ( mode == NAME_execute )*/
    m = X_OK;

  if ( notDefault(f->path) && access(strName(f->path), m) == 0 )
    succeed;

  strcpy(base, exp);
  if ( is_absolute_name(base) )
  { if ( access(base, m) == 0 )
    { assign(f, path, CtoName(base));
      succeed;
    }

    goto nofind;
  }

  if ( isDefault(path) )
    pathstr = ".";
  else
    pathstr = strName(path);

  while( pathstr && *pathstr )
  { char *end = pathstr;

#ifdef __WIN32__
    if ( isletter(end[0]) && end[1] == ':' )
      end += 2;
#endif

    if ( (end = strchr(end, ':')) == NULL )
    { strcpy(name, pathstr);
      pathstr = NULL;
    } else
    { strncpy(name, pathstr, end-pathstr);
      name[end-pathstr] = EOS;
      pathstr = &end[1];
    }
    if ( (exp = expandFileName(name, bin)) )
      strcpy(name, exp);
    else
      continue;

    strcat(name, "/");
    strcat(name, base);

    DEBUG(NAME_find, Cprintf("%s->find: trying %s\n", pp(f), name));

    if ( access(name, m) == 0 )
    { assign(f, path, CtoName(name));
      succeed;
    }
  }

nofind:
  return errorPce(f, NAME_cannotFindFile, path);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_seek[] =
        { "byte=int", "from=[{start,here,end}]" };
static char *T_format[] =
        { "format=char_array", "argument=any ..." };
static char *T_open[] =
        { "mode={read,write,append}", "filter=[name]",
	  "extension=[char_array]" };
static char *T_find[] =
        { "path=[char_array]", "access=[{read,write,append,execute}]" };
static char *T_initialise[] =
        { "path=[name]", "kind=[{text,binary}]" };

/* Instance Variables */

static vardecl var_file[] =
{ SV(NAME_name, "name=name", IV_GET|IV_STORE, nameFile,
     NAME_path, "Name of the file"),
  IV(NAME_path, "path=[name]", IV_BOTH,
     NAME_path, "Full path-name of the file"),
  SV(NAME_kind, "{text,binary}", IV_GET|IV_STORE, kindFile,
     NAME_fileType, "Text or binary file"),
  IV(NAME_status, "{closed,read,write}", IV_GET,
     NAME_open, "One of {closed,read,write}"),
  IV(NAME_filter, "command=name*", IV_BOTH,
     NAME_filter, "Name of input/output filter used"),
  IV(NAME_fd, "alien:FILE *", IV_NONE,
     NAME_internal, "Unix file (stream) handle")
};

/* Send Methods */

static senddecl send_file[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseFile,
     DEFAULT, "Create from name and kind"),
  SM(NAME_unlink, 0, NULL, unlinkFile,
     DEFAULT, "Close file"),
  SM(NAME_backup, 1, "extension=[name]", backupFile,
     NAME_copy, "Make a backup by adding extension (~)"),
  SM(NAME_copy, 1, "from=file", copyFile,
     NAME_copy, "Copy to destination file"),
  SM(NAME_remove, 0, NULL, removeFile,
     NAME_delete, "Unlink from Unix file system"),
  SM(NAME_find, 2, T_find, findFile,
     NAME_location, "Find file in search-path"),
  SM(NAME_seek, 2, T_seek, seekFile,
     NAME_location, "Seek to index from {start,here,end}"),
  SM(NAME_close, 0, NULL, closeFile,
     NAME_open, "Close file"),
  SM(NAME_open, 3, T_open, openFile,
     NAME_open, "Open file in mode, read/write through filter"),
  SM(NAME_absolutePath, 0, NULL, absolutePathFile,
     NAME_path, "Convert <-name to an absolute path"),
  SM(NAME_isAbsolute, 0, NULL, isAbsoluteFile,
     NAME_path, "Test if <-name specifies an absolute path"),
  SM(NAME_access, 1, "mode={read,write,append,execute}", accessFile,
     NAME_test, "Test if file has access"),
  SM(NAME_exists, 1, "must_be_file=[bool]", existsFile,
     NAME_test, "Test if file exists"),
  SM(NAME_same, 1, "file=file", sameFile,
     NAME_test, "Test if two paths refer to the same physical file"),
  SM(NAME_append, 1, "text=char_array", appendFile,
     NAME_write, "Append string to file"),
  SM(NAME_flush, 0, NULL, flushFile,
     NAME_write, "Flush pending output"),
  SM(NAME_format, 2, T_format, formatFile,
     NAME_write, "Format arguments and ->append"),
  SM(NAME_newline, 0, NULL, newlineFile,
     NAME_write, "Append newline to file")
};

/* Get Methods */

static getdecl get_file[] =
{ GM(NAME_convert, 1, "file", "path=name", getConvertFile,
     DEFAULT, "Convert name to file"),
  GM(NAME_backupFileName, 1, "char_array", "extension=[char_array]",
     getBackupFileNameFile,
     NAME_copy, "Name for storing ->backup data"),
  GM(NAME_size, 0, "bytes=int", NULL, getSizeFile,
     NAME_dimension, "Size in characters"),
  GM(NAME_filter, 0, "extension_and_filter=attribute", NULL, getFilterFile,
     NAME_filter, "Determine input filter from extension"),
  GM(NAME_index, 0, "byte=int", NULL, getIndexFile,
     NAME_location, "Current index (Unix tell())"),
  GM(NAME_absolutePath, 0, "path=name", NULL, getAbsolutePathFile,
     NAME_path, "Convert <-name to an absolute path"),
  GM(NAME_baseName, 0, "name", NULL, getBaseNameFile,
     NAME_path, "Base name of file in directory"),
  GM(NAME_directoryName, 0, "name", NULL, getDirectoryNameFile,
     NAME_path, "Directory name of file"),
  GM(NAME_character, 0, "char", NULL, getCharacterFile,
     NAME_read, "Read next character as ASCII value"),
  GM(NAME_read, 1, "string", "count=[int]", getReadFile,
     NAME_read, "New string width next n characters"),
  GM(NAME_readLine, 0, "string", NULL, getReadLineFile,
     NAME_read, "New string with next line"),
  GM(NAME_time, 1, "date=date", "which_time=[{modified,access}]", getTimeFile,
     NAME_time, "New date holding modification/access time")
};

/* Resources */

#define rc_file NULL
/*
static classvardecl rc_file[] =
{ 
};
*/

/* Class Declaration */

static Name file_termnames[] = { NAME_name };

ClassDecl(file_decls,
          var_file, send_file, get_file, rc_file,
          1, file_termnames,
          "$Rev$");

status
makeClassFile(Class class)
{ declareClass(class, &file_decls);
  setLoadStoreFunctionClass(class, loadFile, storeFile);

#if defined(__WIN32__)
{ int w32s = iswin32s();

  featureClass(class, NAME_caseSensitive, OFF);
  featureClass(class, NAME_casePreserving, w32s ? OFF : ON);
  featureClass(class, NAME_8plus3names, w32s ? ON : OFF);
}
#else
  featureClass(class, NAME_caseSensitive, ON);
  featureClass(class, NAME_casePreserving, ON);
  featureClass(class, NAME_8plus3names, OFF);
#endif

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
