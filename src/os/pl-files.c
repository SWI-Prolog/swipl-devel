/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
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

#include "pl-incl.h"
#include "pl-utf8.h"
#include <stdio.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef O_XOS
#define statstruct struct _stati64
#else
#define statstruct struct stat
#define statfunc stat
#endif

#undef LD
#define LD LOCAL_LD

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General file operations and binding to Prolog
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __WINDOWS__
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
#endif /*__WINDOWS__*/


		 /*******************************
		 *	      OS STUFF		*
		 *******************************/

/** int LastModifiedFile(const char *file, double *t)

Return the last modification time of file  as a POSIX timestamp. Returns
(time_t)-1 on failure.

Contains a 64-bit value representing the number of 100-nanosecond
intervals since January 1, 1601 (UTC).
*/

int
LastModifiedFile(const char *name, double *tp)
{
#ifdef __WINDOWS__
  HANDLE hFile;
  wchar_t wfile[MAXPATHLEN];

#define nano * 0.000000001
#define ntick 100.0
#define SEC_TO_UNIX_EPOCH 11644473600.0

  if ( !_xos_os_filenameW(name, wfile, MAXPATHLEN) )
    return FALSE;

  if ( (hFile=CreateFileW(wfile,
			  0,
			  FILE_SHARE_DELETE|FILE_SHARE_READ|FILE_SHARE_WRITE,
			  NULL,
			  OPEN_EXISTING,
			  FILE_FLAG_BACKUP_SEMANTICS,
			  NULL)) != INVALID_HANDLE_VALUE )
  { FILETIME wt;
    int rc;

    rc = GetFileTime(hFile, NULL, NULL, &wt);
    CloseHandle(hFile);

    if ( rc )
    { double t;

      t  = (double)wt.dwHighDateTime * (4294967296.0 * ntick nano);
      t += (double)wt.dwLowDateTime  * (ntick nano);
      t -= SEC_TO_UNIX_EPOCH;

      *tp = t;

      return TRUE;
    }
  }

  set_posix_error(GetLastError());

  return FALSE;
#else
  char tmp[MAXPATHLEN];
  statstruct buf;

  if ( statfunc(OsPath(name, tmp), &buf) < 0 )
    return FALSE;

  *tp = (double)buf.st_mtime;
  return TRUE;
#endif
}


/** static int64_t SizeFile(const char *path)

Return the size of the file path in bytes. Returns -1 if the file cannot
be accessed.
*/

static int64_t
SizeFile(const char *path)
{ char tmp[MAXPATHLEN];
  statstruct buf;

  if ( statfunc(OsPath(path, tmp), &buf) < 0 )
    return -1;

  return buf.st_size;
}


/** int AccessFile(const char *path, int mode)

TRUE if path can be accessed in the   specified modes. Mode is a bitwise
or created from one or more  of the constants ACCESS_EXIST, ACCESS_READ,
ACCESS_WRITE and ACCESS_EXECUTE.
*/

#ifndef F_OK
#define F_OK 0
#endif

int
AccessFile(const char *path, int mode)
{ char tmp[MAXPATHLEN];
#ifdef HAVE_ACCESS
  int m = 0;

  if ( mode == ACCESS_EXIST )
    m = F_OK;
  else
  { if ( mode & ACCESS_READ    ) m |= R_OK;
    if ( mode & ACCESS_WRITE   ) m |= W_OK;
#ifdef X_OK
    if ( mode & ACCESS_EXECUTE ) m |= X_OK;
#endif
  }

  return access(OsPath(path, tmp), m) == 0 ? TRUE : FALSE;
#else
#error "No implementation for AccessFile()"
#endif
}


int
ExistsFile(const char *path)
{
#ifdef O_XOS
  return _xos_exists(path, _XOS_FILE);
#else
  char tmp[MAXPATHLEN];
  statstruct buf;

  if ( statfunc(OsPath(path, tmp), &buf) == -1 || !S_ISREG(buf.st_mode) )
  { DEBUG(2, perror(tmp));
    return FALSE;
  }
  return TRUE;
#endif
}


int
ExistsDirectory(const char *path)
{
#ifdef O_XOS
  return _xos_exists(path, _XOS_DIR);
#else
  char tmp[MAXPATHLEN];
  char *ospath = OsPath(path, tmp);
  statstruct buf;

  if ( statfunc(ospath, &buf) < 0 )
    return FALSE;

  if ( S_ISDIR(buf.st_mode) )
    return TRUE;

  return FALSE;
#endif /*O_XOS*/
}


static char *
ReadLink(const char *f, char *buf)
{
#ifdef HAVE_READLINK
  int n;

  if ( (n=readlink(f, buf, MAXPATHLEN-1)) > 0 )
  { buf[n] = EOS;
    return buf;
  }
#endif

  return NULL;
}


static char *
DeRefLink1(const char *f, char *lbuf)
{ char buf[MAXPATHLEN];
  char *l;

  if ( (l=ReadLink(f, buf)) )
  { if ( l[0] == '/' )			/* absolute path */
    { strcpy(lbuf, buf);
      return lbuf;
    } else
    { char *q;

      strcpy(lbuf, f);
      q = &lbuf[strlen(lbuf)];
      while(q>lbuf && q[-1] != '/')
	q--;
      strcpy(q, l);

      canonicaliseFileName(lbuf);

      return lbuf;
    }
  }

  return NULL;
}


/** char *DeRefLink(const char *link, char *buf)

Dereference a symbolic  link,  returning   its  final  destination.  The
returned filename is canonical  (i.e.,  references   to  ./  and ../ are
removed). Returns NULL if more than 20 links have been followed.
*/

char *
DeRefLink(const	char *link, char *buf)
{ char tmp[MAXPATHLEN];
  char *f;
  int n = 20;				/* avoid loop! */

  while((f=DeRefLink1(link, tmp)) && n-- > 0)
    link = f;

  if ( n > 0 )
  { strcpy(buf, link);
    return buf;
  } else
    return NULL;
}


static int
SameFile(const char *f1, const char *f2)
{ GET_LD

  if ( truePrologFlag(PLFLAG_FILE_CASE) )
  { if ( streq(f1, f2) )
      return TRUE;
  } else
  { if ( strcasecmp(f1, f2) == 0 )
      return TRUE;
  }

#ifdef __unix__				/* doesn't work on most not Unix's */
  { statstruct buf1;
    statstruct buf2;
    char tmp[MAXPATHLEN];

    if ( statfunc(OsPath(f1, tmp), &buf1) != 0 ||
	 statfunc(OsPath(f2, tmp), &buf2) != 0 )
      return FALSE;
    if ( buf1.st_ino == buf2.st_ino && buf1.st_dev == buf2.st_dev )
      return TRUE;
  }
#endif
#ifdef O_XOS
  return _xos_same_file(f1, f2);
#endif /*O_XOS*/
    /* Amazing! There is no simple way to check two files for identity. */
    /* stat() and fstat() both return dummy values for inode and device. */
    /* this is fine as OS'es not supporting symbolic links don't need this */

  return FALSE;
}


/** int RemoveFile(const char *path)

Remove a file from the filesystem.  Returns   TRUE  on success and FALSE
otherwise.
*/

int
RemoveFile(const char *path)
{ char tmp[MAXPATHLEN];

#ifdef HAVE_REMOVE
  return remove(OsPath(path, tmp)) == 0 ? TRUE : FALSE;
#else
  return unlink(OsPath(path, tmp)) == 0 ? TRUE : FALSE;
#endif
}


static int
RenameFile(const char *old, const char *new)
{ char oldbuf[MAXPATHLEN];
  char newbuf[MAXPATHLEN];
  char *osold, *osnew;

  osold = OsPath(old, oldbuf);
  osnew = OsPath(new, newbuf);

#ifdef HAVE_RENAME
  return rename(osold, osnew) == 0 ? TRUE : FALSE;
#else
{ int rval;

  unlink(osnew);
  if ( (rval = link(osold, osnew)) == 0
       && (rval = unlink(osold)) != 0)
    unlink(osnew);

  if ( rval == 0 )
    return TRUE;

  return FALSE;
}
#endif /*HAVE_RENAME*/
}


static int
MarkExecutable(const char *name)
{
#if (defined(HAVE_STAT) && defined(HAVE_CHMOD)) || defined(__unix__)
  statstruct buf;
  mode_t um;

  um = umask(0777);
  umask(um);
  if ( statfunc(name, &buf) == -1 )
  { GET_LD
    term_t file = PL_new_term_ref();

    PL_put_atom_chars(file, name);
    return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		    ATOM_stat, ATOM_file, file);
  }

  if ( (buf.st_mode & 0111) == (~um & 0111) )
    return TRUE;

  buf.st_mode |= 0111 & ~um;
  if ( chmod(name, buf.st_mode) == -1 )
  { GET_LD
    term_t file = PL_new_term_ref();

    PL_put_atom_chars(file, name);
    return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		    ATOM_chmod, ATOM_file, file);
  }
#endif /* defined(HAVE_STAT) && defined(HAVE_CHMOD) */

  return TRUE;
}


		/********************************
		*	FIND FILES FROM C       *
		*********************************/

int
unifyTime(term_t t, time_t time)
{ return PL_unify_time(t, time);
}


static int
add_option(term_t options, functor_t f, atom_t val)
{ GET_LD
  term_t head;

  if ( (head=PL_new_term_ref()) &&
       PL_unify_list(options, head, options) &&
       PL_unify_term(head, PL_FUNCTOR, f, PL_ATOM, val) )
  { PL_reset_term_refs(head);
    return TRUE;
  }

  return FALSE;
}

#define CVT_FILENAME (CVT_ATOM|CVT_STRING|CVT_LIST)

static int
get_file_name(term_t n, char **namep, char *tmp, int flags)
{ GET_LD
  char *name;
  int chflags;
  size_t len;

  if ( flags & PL_FILE_SEARCH )
  { fid_t fid;

    if ( (fid = PL_open_foreign_frame()) )
    { predicate_t pred = PL_predicate("absolute_file_name", 3, "system");
      term_t av = PL_new_term_refs(3);
      term_t options = PL_copy_term_ref(av+2);
      int rc = TRUE;
      int cflags = ((flags&PL_FILE_NOERRORS) ? PL_Q_CATCH_EXCEPTION
					     : PL_Q_PASS_EXCEPTION);

      PL_put_term(av+0, n);

      if ( rc && flags & PL_FILE_EXIST )
	rc = add_option(options, FUNCTOR_access1, ATOM_exist);
      if ( rc && flags & PL_FILE_READ )
	rc = add_option(options, FUNCTOR_access1, ATOM_read);
      if ( rc && flags & PL_FILE_WRITE )
	rc = add_option(options, FUNCTOR_access1, ATOM_write);
      if ( rc && flags & PL_FILE_EXECUTE )
	rc = add_option(options, FUNCTOR_access1, ATOM_execute);

      if ( rc ) rc = PL_unify_nil(options);
      if ( rc ) rc = PL_call_predicate(NULL, cflags, pred, av);
      if ( rc ) rc = PL_get_nchars(av+1, &len, namep,
				   CVT_ATOMIC|BUF_RING|REP_FN);
      if ( rc && strlen(*namep) != len )
      { n = av+1;
	goto code0;
      }

      PL_discard_foreign_frame(fid);
      return rc;
    }

    return FALSE;
  }

  chflags = CVT_FILENAME;
  if ( !(flags&(REP_UTF8|REP_MB)) )
    chflags |= REP_FN;
  if ( !(flags & PL_FILE_NOERRORS) )
    chflags |= CVT_EXCEPTION;
  if ( !PL_get_nchars(n, &len, &name, chflags) )
    return FALSE;
  if ( strlen(name) != len )
  { code0:
    return PL_error(NULL, 0, "file name contains a 0-code",
		    ERR_DOMAIN, ATOM_file_name, n);
  }
  if ( len+1 >= MAXPATHLEN )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
		    ATOM_max_path_length);

  if ( truePrologFlag(PLFLAG_FILEVARS) )
  { if ( !(name = expandVars(name, tmp, MAXPATHLEN)) )
      return FALSE;
  }

  if ( !(flags & PL_FILE_NOERRORS) )
  { atom_t op = 0;

    if ( (flags&(PL_FILE_READ|PL_FILE_WRITE|PL_FILE_EXECUTE|PL_FILE_EXIST)) &&
	 !AccessFile(name, ACCESS_EXIST) )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_file, n);

    if ( (flags&PL_FILE_READ) && !AccessFile(name, ACCESS_READ) )
      op = ATOM_read;
    if ( !op && (flags&PL_FILE_WRITE) && !AccessFile(name, ACCESS_WRITE) )
      op = ATOM_write;
    if ( !op && (flags&PL_FILE_EXECUTE) && !AccessFile(name, ACCESS_EXECUTE) )
      op = ATOM_execute;

    if ( op )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION, op, ATOM_file, n);
  }

  if ( flags & PL_FILE_ABSOLUTE )
  { if ( !(name = AbsoluteFile(name, tmp)) )
      return FALSE;
  }

  *namep = buffer_string(name, BUF_RING);

  return TRUE;
}


int
PL_get_file_name(term_t n, char **namep, int flags)
{ char buf[MAXPATHLEN];
  char ospath[MAXPATHLEN];
  char *name;
  int rc;

  if ( (rc=get_file_name(n, &name, buf, flags)) )
  { if ( (flags & PL_FILE_OSPATH) )
    { if ( !(name = OsPath(name, ospath)) )
	return FALSE;
    }

    *namep = buffer_string(name, BUF_RING);
  }

  return rc;
}


int
PL_get_file_nameW(term_t n, wchar_t **namep, int flags)
{ char buf[MAXPATHLEN];
  char ospath[MAXPATHLEN];
  char *name;
  int rc;

  if ( (rc=get_file_name(n, &name, buf, flags|REP_UTF8)) )
  { Buffer b;
    const char *s;

    if ( (flags & PL_FILE_OSPATH) )
    { if ( !(name = OsPath(name, ospath)) )
	return FALSE;
    }

    b = findBuffer(BUF_RING);
    for(s = name; *s; )
    { int chr;

      s = utf8_get_char(s, &chr);
      addBuffer(b, (wchar_t)chr, wchar_t);
    }
    addBuffer(b, (wchar_t)0, wchar_t);

    *namep = baseBuffer(b, wchar_t);
  }

  return rc;
}


		 /*******************************
		 *	   QUERY FILES		*
		 *******************************/

static
PRED_IMPL("time_file", 2, time_file, 0)
{ char *fn;

  if ( PL_get_file_name(A1, &fn, 0) )
  { double time;

    if ( LastModifiedFile(fn, &time) )
      return PL_unify_float(A2, time);

    return PL_error(NULL, 0, NULL, ERR_FILE_OPERATION,
		    ATOM_time, ATOM_file, A1);
  }

  return FALSE;
}


static
PRED_IMPL("size_file", 2, size_file, 0)
{ PRED_LD
  char *n;

  if ( PL_get_file_name(A1, &n, 0) )
  { int64_t size;

    if ( (size = SizeFile(n)) < 0 )
      return PL_error("size_file", 2, OsError(), ERR_FILE_OPERATION,
		      ATOM_size, ATOM_file, A1);

    return PL_unify_int64(A2, size);
  }

  return FALSE;
}


static
PRED_IMPL("access_file", 2, access_file, 0)
{ PRED_LD
  char *n;
  int md;
  atom_t m;

  term_t name = A1;
  term_t mode = A2;

  if ( !PL_get_atom(mode, &m) )
    return PL_error("access_file", 2, NULL, ERR_TYPE, ATOM_atom, mode);
  if ( !PL_get_file_name(name, &n, 0) )
    return FALSE;

  if ( m == ATOM_none )
    return TRUE;

  if      ( m == ATOM_write || m == ATOM_append )
    md = ACCESS_WRITE;
  else if ( m == ATOM_read )
    md = ACCESS_READ;
  else if ( m == ATOM_execute )
    md = ACCESS_EXECUTE;
  else if ( m == ATOM_exist )
    md = ACCESS_EXIST;
  else
    return PL_error("access_file", 2, NULL, ERR_DOMAIN, ATOM_io_mode, mode);

  if ( AccessFile(n, md) )
    return TRUE;

  if ( md == ACCESS_WRITE && !AccessFile(n, ACCESS_EXIST) )
  { char tmp[MAXPATHLEN];
    char *dir = DirName(n, tmp);

    if ( dir[0] )
    { if ( !ExistsDirectory(dir) )
	return FALSE;
    }
    if ( AccessFile(dir[0] ? dir : ".", md) )
      return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("read_link", 3, read_link, 0)
{ char *n, *l, *t;
  char buf[MAXPATHLEN];

  term_t file = A1;
  term_t link = A2;
  term_t to   = A3;

  if ( !PL_get_file_name(file, &n, 0) )
    return FALSE;

  if ( (l = ReadLink(n, buf)) &&
       PL_unify_atom_chars(link, l) &&
       (t = DeRefLink(n, buf)) &&
       PL_unify_atom_chars(to, t) )
    return TRUE;

  return FALSE;
}


static
PRED_IMPL("exists_file", 1, exists_file, 0)
{ char *n;

  if ( !PL_get_file_name(A1, &n, 0) )
    return FALSE;

  return ExistsFile(n);
}


static
PRED_IMPL("exists_directory", 1, exists_directory, 0)
{ char *n;

  if ( !PL_get_file_name(A1, &n, 0) )
    return FALSE;

  return ExistsDirectory(n);
}


static
PRED_IMPL("is_absolute_file_name", 1, is_absolute_file_name, 0)
{ char *n;

  if ( PL_get_file_name(A1, &n, 0) &&
       IsAbsolutePath(n) )
    return TRUE;

  return FALSE;
}


static
PRED_IMPL("same_file", 2, same_file, 0)
{ char *n1, *n2;

  if ( PL_get_file_name(A1, &n1, 0) &&
       PL_get_file_name(A2, &n2, 0) )
    return SameFile(n1, n2);

  return FALSE;
}


static
PRED_IMPL("file_base_name", 2, file_base_name, 0)
{ char *n;

  if ( !PL_get_chars(A1, &n, CVT_ALL|REP_FN|CVT_EXCEPTION) )
    return FALSE;

  return PL_unify_chars(A2, PL_ATOM|REP_FN, -1, BaseName(n));
}


static
PRED_IMPL("file_directory_name", 2, file_directory_name, 0)
{ char *n;
  char tmp[MAXPATHLEN];

  if ( !PL_get_chars(A1, &n, CVT_ALL|REP_FN|CVT_EXCEPTION) )
    return FALSE;

  return PL_unify_chars(A2, PL_ATOM|REP_FN, -1, DirName(n, tmp));
}


		 /*******************************
		 *	  TEMPORARY FILES	*
		 *******************************/

static
PRED_IMPL("tmp_file", 2, tmp_file, 0)
{ PRED_LD
  char *n;

  term_t base = A1;
  term_t name = A2;

  if ( !PL_get_chars(base, &n, CVT_ALL) )
    return PL_error("tmp_file", 2, NULL, ERR_TYPE, ATOM_atom, base);

  return PL_unify_atom(name, TemporaryFile(n, NULL));
}

/** tmp_file_stream(+Mode, -File, -Stream)
*/

static
PRED_IMPL("tmp_file_stream", 3, tmp_file_stream, 0)
{ PRED_LD
  atom_t fn;
  int fd;
  IOENC enc;
  atom_t encoding;
  const char *mode;

  if ( !PL_get_atom_ex(A1, &encoding) )
    return FALSE;
  if ( (enc = atom_to_encoding(encoding)) == ENC_UNKNOWN )
  { if ( encoding == ATOM_binary )
    { enc = ENC_OCTET;
      mode = "wb";
    } else
    { return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_encoding, A1);
    }
  } else
  { mode = "w";
  }

  if ( (fn=TemporaryFile("", &fd)) )
  { IOSTREAM *s;

    if ( !PL_unify_atom(A2, fn) )
    { close(fd);
      return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION, 2, A2);
    }

    s = Sfdopen(fd, mode);
    s->encoding = enc;
    return PL_unify_stream(A3, s);
  } else
  { return PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_temporary_files);
  }
}



		 /*******************************
		 *	CHANGE FILESYSTEM	*
		 *******************************/


static
PRED_IMPL("delete_file", 1, delete_file, 0)
{ PRED_LD
  char *n;
  atom_t aname;

  if ( PL_get_atom(A1, &aname) &&
       DeleteTemporaryFile(aname) )
    return TRUE;

  if ( !PL_get_file_name(A1, &n, 0) )
    return FALSE;

  if ( RemoveFile(n) )
    return TRUE;

  return PL_error(NULL, 0, MSG_ERRNO, ERR_FILE_OPERATION,
		  ATOM_delete, ATOM_file, A1);
}


static
PRED_IMPL("delete_directory", 1, delete_directory, 0)
{ char *n;

  if ( !PL_get_file_name(A1, &n, 0) )
    return FALSE;

  if ( rmdir(n) == 0 )
    return TRUE;
  else
    return PL_error(NULL, 0, MSG_ERRNO, ERR_FILE_OPERATION,
		    ATOM_delete, ATOM_directory, A1);
}


static
PRED_IMPL("make_directory", 1, make_directory, 0)
{ char *n;

  if ( !PL_get_file_name(A1, &n, 0) )
    return FALSE;

  if ( mkdir(n, 0777) == 0 )
    return TRUE;
  else
    return PL_error(NULL, 0, MSG_ERRNO, ERR_FILE_OPERATION,
		    ATOM_create, ATOM_directory, A1);
}


static
PRED_IMPL("rename_file", 2, rename_file, 0)
{ PRED_LD
  char *o, *n;

  term_t old = A1;
  term_t new = A2;

  if ( PL_get_file_name(old, &o, 0) &&
       PL_get_file_name(new, &n, 0) )
  { if ( SameFile(o, n) )
    { if ( truePrologFlag(PLFLAG_FILEERRORS) )
	return PL_error("rename_file", 2, "same file", ERR_PERMISSION,
			ATOM_rename, ATOM_file, old);
      return FALSE;
    }

    if ( RenameFile(o, n) )
      return TRUE;

    if ( truePrologFlag(PLFLAG_FILEERRORS) )
      return PL_error("rename_file", 2, OsError(), ERR_FILE_OPERATION,
		      ATOM_rename, ATOM_file, old);
    return FALSE;
  }

  return FALSE;
}


static
PRED_IMPL("$absolute_file_name", 2, absolute_file_name, 0)
{ char *n;
  char tmp[MAXPATHLEN];

  term_t name = A1;
  term_t expanded = A2;

  if ( PL_get_file_name(name, &n, 0) &&
       (n = AbsoluteFile(n, tmp)) )
    return PL_unify_chars(expanded, PL_ATOM|REP_FN, -1, n);

  return FALSE;
}


static
PRED_IMPL("working_directory", 2, working_directory, 0)
{ PRED_LD
  char buf[MAXPATHLEN];
  const char *wd;

  term_t old = A1;
  term_t new = A2;

  if ( !(wd = PL_cwd(buf, sizeof(buf))) )
    return FALSE;

  if ( PL_unify_chars(old, PL_ATOM|REP_FN, -1, wd) )
  { if ( PL_compare(old, new) != 0 )
    { char *n;

      if ( PL_get_file_name(new, &n, 0) )
      { if ( ChDir(n) )
	  return TRUE;

	if ( truePrologFlag(PLFLAG_FILEERRORS) )
	  return PL_error(NULL, 0, NULL, ERR_FILE_OPERATION,
			  ATOM_chdir, ATOM_directory, new);
      }

      return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


static int
has_extension(const char *name, const char *ext)
{ GET_LD
  const char *s = name + strlen(name);

  if ( ext[0] == EOS )
    return TRUE;

  while(*s != '.' && *s != '/' && s > name)
    s--;
  if ( *s == '.' && s > name && s[-1] != '/' )
  { if ( ext[0] == '.' )
      ext++;
    if ( truePrologFlag(PLFLAG_FILE_CASE) )
      return strcmp(&s[1], ext) == 0;
    else
      return strcasecmp(&s[1], ext) == 0;
  }

  return FALSE;
}


static int
name_too_long(void)
{ return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_path_length);
}


static
PRED_IMPL("file_name_extension", 3, file_name_extension, 0)
{ PRED_LD
  char *b = NULL, *e = NULL, *f;
  char buf[MAXPATHLEN];

  term_t base = A1;
  term_t ext  = A2;
  term_t full = A3;

  if ( !PL_is_variable(full) )
  { if ( PL_get_chars(full, &f, CVT_ALL|CVT_EXCEPTION|REP_FN) )
    { char *s = f + strlen(f);		/* ?base, ?ext, +full */

      while(*s != '.' && *s != '/' && s > f)
	s--;
      if ( *s == '.' )
      { if ( PL_get_chars(ext, &e, CVT_ALL|REP_FN) )
	{ if ( e[0] == '.' )
	    e++;
	  if ( truePrologFlag(PLFLAG_FILE_CASE) )
	  { TRY(strcmp(&s[1], e) == 0);
	  } else
	  { TRY(strcasecmp(&s[1], e) == 0);
	  }
	} else
	{ TRY(PL_unify_chars(ext, PL_ATOM|REP_FN, -1, &s[1]));
	}

	return PL_unify_chars(base, PL_ATOM|REP_FN, s-f, f);
      }
      if ( PL_unify_atom_chars(ext, "") &&
	   PL_unify(full, base) )
	PL_succeed;
    }
    PL_fail;
  }

  if ( PL_get_chars(base, &b, CVT_ALL|BUF_RING|REP_FN|CVT_EXCEPTION) &&
       PL_get_chars(ext, &e, CVT_ALL|REP_FN|CVT_EXCEPTION) )
  { char *s;

    if ( e[0] == '.' )		/* +Base, +Extension, -full */
      e++;
    if ( has_extension(b, e) )
      return PL_unify(base, full);
    if ( strlen(b) + 1 + strlen(e) + 1 > MAXPATHLEN )
      return name_too_long();
    strcpy(buf, b);
    s = buf + strlen(buf);
    *s++ = '.';
    strcpy(s, e);

    return PL_unify_chars(full, PL_ATOM|REP_FN, -1, buf);
  } else
    return FALSE;
}


static
PRED_IMPL("prolog_to_os_filename", 2, prolog_to_os_filename, 0)
{ PRED_LD

  term_t pl = A1;
  term_t os = A2;

#ifdef O_XOS
  wchar_t *wn;

  if ( !PL_is_variable(pl) )
  { char *n;
    wchar_t buf[MAXPATHLEN];

    if ( PL_get_chars(pl, &n, CVT_ALL|REP_UTF8|CVT_EXCEPTION) )
    { if ( !_xos_os_filenameW(n, buf, MAXPATHLEN) )
	return name_too_long();

      return PL_unify_wchars(os, PL_ATOM, -1, buf);
    }
    return FALSE;
  }

  if ( PL_get_wchars(os, NULL, &wn, CVT_ALL) )
  { wchar_t lbuf[MAXPATHLEN];
    char buf[MAXPATHLEN];

    _xos_long_file_nameW(wn, lbuf, MAXPATHLEN);
    _xos_canonical_filenameW(lbuf, buf, MAXPATHLEN, 0);

    return PL_unify_chars(pl, PL_ATOM|REP_UTF8, -1, buf);
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, pl);
#else /*O_XOS*/
  return PL_unify(pl, os);
#endif /*O_XOS*/
}


static
PRED_IMPL("mark_executable", 1, mark_executable, 0)
{ char *name;

  if ( !PL_get_file_name(A1, &name, 0) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_source_sink, A1);

  return MarkExecutable(name);
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

void
initFiles(void)
{
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(files)
  PRED_DEF("working_directory", 2, working_directory, 0)
  PRED_DEF("access_file", 2, access_file, 0)
  PRED_DEF("time_file", 2, time_file, 0)
  PRED_DEF("size_file", 2, size_file, 0)
  PRED_DEF("read_link", 3, read_link, 0)
  PRED_DEF("exists_file", 1, exists_file, 0)
  PRED_DEF("exists_directory", 1, exists_directory, 0)
  PRED_DEF("tmp_file", 2, tmp_file, 0)
  PRED_DEF("tmp_file_stream", 3, tmp_file_stream, 0)
  PRED_DEF("delete_file", 1, delete_file, 0)
  PRED_DEF("delete_directory", 1, delete_directory, 0)
  PRED_DEF("make_directory", 1, make_directory, 0)
  PRED_DEF("same_file", 2, same_file, 0)
  PRED_DEF("rename_file", 2, rename_file, 0)
  PRED_DEF("is_absolute_file_name", 1, is_absolute_file_name, 0)
  PRED_DEF("file_base_name", 2, file_base_name, 0)
  PRED_DEF("file_directory_name", 2, file_directory_name, 0)
  PRED_DEF("file_name_extension", 3, file_name_extension, 0)
  PRED_DEF("prolog_to_os_filename", 2, prolog_to_os_filename, 0)
  PRED_DEF("$mark_executable", 1, mark_executable, 0)
  PRED_DEF("$absolute_file_name", 2, absolute_file_name, 0)
EndPredDefs
