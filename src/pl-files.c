/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

#include "pl-incl.h"
#undef LD
#define LD LOCAL_LD

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General file operations and binding to Prolog
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*	FIND FILES FROM C       *
		*********************************/

int
unifyTime(term_t t, time_t time)
{ return PL_unify_float(t, (double)time);
}


static void
add_option(term_t options, functor_t f, atom_t val)
{ GET_LD
  term_t head = PL_new_term_ref();

  PL_unify_list(options, head, options);
  PL_unify_term(head, PL_FUNCTOR, f, PL_ATOM, val);

  PL_reset_term_refs(head);
}

#define CVT_FILENAME (CVT_ATOM|CVT_STRING|CVT_LIST)

int
PL_get_file_name(term_t n, char **namep, int flags)
{ GET_LD
  char *name;
  char tmp[MAXPATHLEN];
  char ospath[MAXPATHLEN];

  if ( flags & PL_FILE_SEARCH )
  { predicate_t pred = PL_predicate("absolute_file_name", 3, "system");
    term_t av = PL_new_term_refs(3);
    term_t options = PL_copy_term_ref(av+2);
    int cflags = ((flags&PL_FILE_NOERRORS) ? PL_Q_CATCH_EXCEPTION
					   : PL_Q_PASS_EXCEPTION);

    PL_put_term(av+0, n);
    
    if ( flags & PL_FILE_EXIST )
      add_option(options, FUNCTOR_access1, ATOM_exist);
    if ( flags & PL_FILE_READ )
      add_option(options, FUNCTOR_access1, ATOM_read);
    if ( flags & PL_FILE_WRITE )
      add_option(options, FUNCTOR_access1, ATOM_write);
    if ( flags & PL_FILE_EXECUTE )
      add_option(options, FUNCTOR_access1, ATOM_execute);

    PL_unify_nil(options);

    if ( !PL_call_predicate(NULL, cflags, pred, av) )
      return FALSE;
    
    return PL_get_chars_ex(av+1, namep, CVT_ATOMIC|BUF_RING|REP_FN);
  }

  if ( flags & PL_FILE_NOERRORS )
  { if ( !PL_get_chars(n, &name, CVT_FILENAME|REP_FN) )
      return FALSE;
  } else
  { if ( !PL_get_chars_ex(n, &name, CVT_FILENAME|REP_FN) )
      return FALSE;
  }

  if ( truePrologFlag(PLFLAG_FILEVARS) )
  { if ( !(name = ExpandOneFile(name, tmp)) )
      return FALSE;
  }
  
  if ( !(flags & PL_FILE_NOERRORS) )
  { atom_t op = 0;

    if ( (flags&PL_FILE_READ) && !AccessFile(name, ACCESS_READ) )
      op = ATOM_read;
    if ( !op && (flags&PL_FILE_WRITE) && !AccessFile(name, ACCESS_WRITE) )
      op = ATOM_write;
    if ( !op && (flags&PL_FILE_EXECUTE) && !AccessFile(name, ACCESS_EXECUTE) )
      op = ATOM_execute;

    if ( op )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION, ATOM_file, op, n);
		    
    if ( (flags & PL_FILE_EXIST) && !AccessFile(name, ACCESS_EXIST) )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_file, n);
  }

  if ( flags & PL_FILE_ABSOLUTE )
  { if ( !(name = AbsoluteFile(name, tmp)) )
      return FALSE;
  }

  if ( flags & PL_FILE_OSPATH )
  { if ( !(name = OsPath(name, ospath)) )
      return FALSE;
  }
    
  *namep = buffer_string(name, BUF_RING);
  return TRUE;
}


		 /*******************************
		 *	   QUERY FILES		*
		 *******************************/

static
PRED_IMPL("time_file", 2, time_file, 0)
{ char *fn;

  if ( PL_get_file_name(A1, &fn, 0) )
  { time_t time;

    if ( (time = LastModifiedFile(fn)) == (time_t)-1 )
      return PL_error(NULL, 0, NULL, ERR_FILE_OPERATION,
		      ATOM_time, ATOM_file, A1);

    return unifyTime(A2, time);
  }

  return FALSE;
}


static
PRED_IMPL("size_file", 2, size_file, 0)
{ char *n;

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

  if ( !PL_get_chars_ex(A1, &n, CVT_ALL|REP_FN) )
    return FALSE;

  return PL_unify_chars(A2, PL_ATOM|REP_FN, -1, BaseName(n));
}


static
PRED_IMPL("file_directory_name", 2, file_directory_name, 0)
{ char *n;
  char tmp[MAXPATHLEN];

  if ( !PL_get_chars_ex(A1, &n, CVT_ALL|REP_FN) )
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

  return PL_unify_atom(name, TemporaryFile(n));
}


		 /*******************************
		 *	CHANGE FILESYSTEM	*
		 *******************************/


static
PRED_IMPL("delete_file", 1, delete_file, 0)
{ char *n;

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
  const char *wd;

  term_t old = A1;
  term_t new = A2;

  if ( !(wd = PL_cwd()) )
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
	return FALSE;
      }
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
name_too_long()
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
	if ( s-f > MAXPATHLEN )
	  return name_too_long();
	strncpy(buf, f, s-f);
	buf[s-f] = EOS;
  
	return PL_unify_chars(base, PL_ATOM|REP_FN, -1, buf);
      }
      if ( PL_unify_atom_chars(ext, "") &&
	   PL_unify(full, base) )
	PL_succeed;
    }
    PL_fail;
  }

  if ( PL_get_chars_ex(base, &b, CVT_ALL|BUF_RING|REP_FN) &&
       PL_get_chars_ex(ext, &e, CVT_ALL|REP_FN) )
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

    if ( PL_get_chars_ex(pl, &n, CVT_ALL|REP_UTF8) )
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
