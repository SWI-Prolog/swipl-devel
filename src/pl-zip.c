/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2022, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
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

#define __MINGW_USE_VC2005_COMPAT		/* Get Windows time_t as 64-bit */

#include "pl-incl.h"

#if defined(HAVE_MMAP) || defined(__WINDOWS__)
#define HAVE_FILE_MAPPING 1

typedef struct mapped_file
{ char *start;
  char *end;
#ifdef __WINDOWS__
  HANDLE hfile;					/* handle to the file */
  HANDLE hmap;					/* handle to the map */
#endif
} mapped_file;

static void	unmap_file(mapped_file *mf);
#endif

#include "pl-zip.h"
#include "pl-fli.h"
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef __WINDOWS__
#include <windows.h>
#endif

#ifndef VERSIONMADEBY
# define VERSIONMADEBY   (0x0) /* platform dependent */
#endif

static int  unify_zipper(term_t t, zipper *zipper);

int rc_errno;

char *
rc_strerror(int eno)
{ return "Unknown resource error";
}

		 /*******************************
		 *	     LOCKING		*
		 *******************************/

static void
zown(zipper *z)
{ int tid = PL_thread_self();

  if ( z->owner != tid )
  { simpleMutexLock(&z->lock);
    z->owner = tid;
  }
}

static void
zdisown(zipper *z)
{
#ifndef NDEBUG
  int tid = PL_thread_self();
  assert(z->owner == tid);
#endif

  if ( z->lock_count == 0 )
  { z->owner = 0;
    simpleMutexUnlock(&z->lock);
  }
}

static int
zlock(zipper *z)
{ int tid = PL_thread_self();

  if ( z->owner != tid )
  { simpleMutexLock(&z->lock);
    z->owner = tid;
    z->lock_count = 1;
  } else
  { z->lock_count++;
  }

  return TRUE;
}

static int
zunlock(zipper *z)
{ int tid = PL_thread_self();

  if ( z->owner == tid )
  { if ( z->lock_count == 0 )
    { term_t t;

    error:
    { GET_LD
      return ( (t=PL_new_term_ref()) &&
	       unify_zipper(t, z) &&
	       PL_permission_error("unlock", "zipper", t)
	     );
    }
    }
    if ( --z->lock_count == 0 )
    { z->owner = 0;
      simpleMutexUnlock(&z->lock);
    }
  } else
  { goto error;
  }

  return TRUE;
}

typedef struct valid_transition
{ zipper_state from;
  zipper_state to;
} valid_transition;

static const valid_transition valid_transitions[] =
{ { ZIP_IDLE,	    ZIP_SCAN        },
  { ZIP_SCAN,	    ZIP_SCAN        },
  { ZIP_SCAN,	    ZIP_READ_ENTRY  },
  { ZIP_IDLE,	    ZIP_WRITE_ENTRY },
  { ZIP_IDLE,	    ZIP_CLOSE	    },
  { ZIP_SCAN,	    ZIP_CLOSE	    },
  { ZIP_READ_ENTRY, ZIP_CLOSE	    },
  { ZIP_END,	    ZIP_END         }
};

static int
zacquire(zipper *z,
	 zipper_state state, zipper_state *prev_state,
	 const char *action)
{ const valid_transition *tr;

  zown(z);
  for(tr = valid_transitions; tr->from != ZIP_END; tr++)
  { if ( tr->from == z->state &&
	 tr->to   == state )
      goto ok;
  }

  { GET_LD
    term_t t;

    DEBUG(MSG_ZIP_STATE, Sdprintf("State %d to %d\n", z->state, state));

    return ( (t=PL_new_term_ref()) &&
	     unify_zipper(t, z) &&
	     PL_permission_error(action, "zipper", t)
	   );
  }

ok:
  if ( prev_state )
    *prev_state = z->state;
  z->state = state;

  return TRUE;
}

static int
zrelease(zipper *z)
{ z->state = ZIP_IDLE;
  zdisown(z);

  return TRUE;
}

		 /*******************************
		 *  ACCESS ARCHIVES AS STREAMS  *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) As we map the resource file, we should  not write it in place. If we
do so anyway, a Prolog process running on   this  state will crash if it
tries to access its resources.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static voidpf
zopen64_file(voidpf opaque, const void* filename, int mode)
{ char modes[8];
  char *m = modes;

  if ( (mode&ZLIB_FILEFUNC_MODE_CREATE) )
  { *m++ = 'w';
    strcpy(m, "m444");
    m += strlen(m);
    if ( ExistsFile(filename) )
    { if ( !RemoveFile(filename) )
	return NULL;			/* see (*) */
    }
  } else
  { *m++ = 'r';
  }
  *m++ = 'b';
  *m = EOS;

  return Sopen_file(filename, modes);
}

static uLong
zread_file(voidpf opaque, voidpf stream, void* buf, uLong size)
{ return Sfread(buf, 1, size, stream);
}

static uLong
zwrite_file(voidpf opaque, voidpf stream, const void* buf, uLong size)
{ return Sfwrite(buf, 1, size, stream);
}

static ZPOS64_T
ztell64_file(voidpf opaque, voidpf stream)
{ return Stell64(stream);
}

static long
zseek64_file(voidpf opaque, voidpf stream, ZPOS64_T offset, int origin)
{ return Sseek64(stream, offset, origin);
}

static int
zclose_file(voidpf opaque, voidpf stream)
{ return Sclose(stream);
}

static int
zerror_file(voidpf opaque, voidpf stream)
{ return Sferror(stream);
}

static zlib_filefunc64_def zfile_functions =
{ zopen64_file,
  zread_file,
  zwrite_file,
  ztell64_file,
  zseek64_file,
  zclose_file,
  zerror_file,
  NULL					/* opaque */
};


static voidpf
zopen64_stream(voidpf opaque, const void* stream, int mode)
{ return (voidpf)stream;
}


static int
zclose_stream(voidpf opaque, voidpf stream)
{ IOSTREAM *s = stream;
  int rc;

  rc = Sflush(s);
  PL_release_stream(s);

  return rc;
}


static zlib_filefunc64_def zstream_functions =
{ zopen64_stream,
  zread_file,
  zwrite_file,
  ztell64_file,
  zseek64_file,
  zclose_stream,
  zerror_file,
  NULL						/* opaque */
};

		 /*******************************
		 *	  MEMORY REGION		*
		 *******************************/

typedef struct mem_stream
{ const char *start;				/* Start of the region */
  const char *end;				/* End of the region */
  const char *here;				/* Current file pointer */
} mem_stream;


static voidpf
zopen64_mem(voidpf opaque, const void *stream, int mode)
{ mem_stream *mem = (mem_stream*)stream;

  assert(!(mode&ZLIB_FILEFUNC_MODE_CREATE));
  mem->here = mem->start;

  return (voidpf) mem;
}

static uLong
zread_mem(voidpf opaque, voidpf stream, void* buf, uLong size)
{ mem_stream *mem = stream;
  uLong copy;

  if ( mem->here + size > mem->end )
    copy = mem->end - mem->here;
  else
    copy = size;

  memcpy(buf, mem->here, copy);
  mem->here += copy;

  return copy;
}

static uLong
zwrite_mem(voidpf opaque, voidpf stream, const void* buf, uLong size)
{ errno = EACCES;
  return -1;
}

static ZPOS64_T
ztell64_mem(voidpf opaque, voidpf stream)
{ mem_stream *mem = stream;

  return mem->here - mem->start;
}

static long
zseek64_mem(voidpf opaque, voidpf stream, ZPOS64_T offset, int origin)
{ mem_stream *mem = stream;
  const char *p;

  switch(origin)
  { case SEEK_SET: p = mem->start + offset; break;
    case SEEK_CUR: p = mem->here + offset;  break;
    case SEEK_END: p = mem->end - offset;   break;
    default:	   errno = EINVAL;
		   return -1;
  }

  if ( p < mem->start )
  { errno = EINVAL;
    return -1;
  } else if ( p > mem->end )
  { p = mem->end;
  }

  mem->here = p;

  return 0;
}

static int
zclose_mem(voidpf opaque, voidpf stream)
{ mem_stream *mem = stream;

  free(mem);
  return 0;
}

static int
zerror_mem(voidpf opaque, voidpf stream)
{ return 0;
}

static voidpf
zclone_mem(voidpf opaque, voidpf stream)
{ mem_stream *mem = stream;
  mem_stream *clone;

  if ( (clone=malloc(sizeof(*clone))) )
  { *clone = *mem;
  }

  return clone;
}

static zlib_filefunc64_def zmem_functions =
{ zopen64_mem,
  zread_mem,
  zwrite_mem,
  ztell64_mem,
  zseek64_mem,
  zclose_mem,
  zerror_mem,
  NULL,					/* opaque */
  zclone_mem
};



		 /*******************************
		 *	   ARCHIVE BLOB		*
		 *******************************/

static int
write_zipper(IOSTREAM *s, atom_t aref, int flags)
{ zipper *ref = PL_blob_data(aref, NULL, NULL);

  Sfprintf(s, "<zipper>(%p)", ref);
  return TRUE;
}

static void
acquire_zipper(atom_t aref)
{ zipper *ref = PL_blob_data(aref, NULL, NULL);

  ref->symbol = aref;
}

/* close_zipper() may be called twice: one time explitly and one time
   due to atom-GC.
*/

static int
close_zipper(zipper *z)
{ zipFile zf;
  unzFile uzf;
  char *path;
  int rc = 0;

  if ( (zf=z->writer) )
  { z->writer = NULL;
    rc = zipClose(zf, NULL);
  } else if ( (uzf=z->reader) )
  { z->reader = NULL;
    rc = unzClose(uzf);
  }
  if ( (path=(char*)z->path) )
  { z->path = NULL;
    free(path);
  }
  if ( z->input.any )
  { switch(z->input_type)
    { case ZIP_STREAM:
	if ( true(z, ZIP_CLOSE_STREAM_ON_CLOSE) )
	{ IOSTREAM *in = z->input.stream;

	  z->input.stream = NULL;
	  Sclose(in);
	}
        break;
      default:
	break;
    }
    z->input.any = NULL;
  }
#ifdef HAVE_FILE_MAPPING
  if ( z->mapped_file )
  { unmap_file(z->mapped_file);
    z->mapped_file = NULL;
  }
#endif

  return rc;
}

static int
release_zipper(atom_t aref)
{ zipper *z = PL_blob_data(aref, NULL, NULL);

  if ( z == GD->resources.DB )
    GD->resources.DB = NULL;
  close_zipper(z);

#ifdef O_PLMT
  simpleMutexDelete(&z->lock);
#endif
  free(z);

  return TRUE;
}

static int
save_zipper(atom_t aref, IOSTREAM *fd)
{ zipper *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <zipper>(%p)",
		    ref);
}

static atom_t
load_zipper(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<zipper>");
}

static PL_blob_t zipper_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "zipper",
  release_zipper,
  NULL,
  write_zipper,
  acquire_zipper,
  save_zipper,
  load_zipper
};

static int
unify_zipper(term_t t, zipper *zipper)
{ GET_LD

  if ( zipper->symbol )
    return PL_unify_atom(t, zipper->symbol);
  else
    return PL_unify_blob(t, zipper, sizeof(*zipper), &zipper_blob);
}

static int
get_zipper(term_t t, zipper **zipper)
{ void *p;
  size_t len;
  PL_blob_t *type;

  if ( PL_get_blob(t, &p, &len, &type) && type == &zipper_blob )
  { *zipper = p;
    return TRUE;
  }

  PL_type_error("zipper", t);
  return FALSE;
}

		 /*******************************
		 *     OPEN CLOSE ARCHIVES	*
		 *******************************/

/** zip_open_stream(+Stream, -Zipper, +Options)
*/

static const opt_spec zip_open_stream_options[] =
{ { ATOM_close_parent,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};

static
PRED_IMPL("zip_open_stream", 3, zip_open_stream, 0)
{ zipper *z = NULL;
  IOSTREAM *stream = NULL;
  int close_parent = FALSE;

  if ( !scan_options(A3, 0, ATOM_zip_options, zip_open_stream_options,
		     &close_parent) )
    return FALSE;

  if ( !PL_get_stream(A1, &stream, 0) )
    return FALSE;

  if ( !(z=malloc(sizeof(*z))) )
    return PL_resource_error("memory");
  memset(z, 0, sizeof(*z));
  if ( close_parent )
    set(z, ZIP_CLOSE_STREAM_ON_CLOSE);
  z->input_type = ZIP_STREAM;
  z->input.stream = stream;
  z->path = strdup("<stream>");
#ifdef O_PLMT
  simpleMutexInit(&z->lock);
#endif

  if ( (stream->flags&SIO_OUTPUT) )
  { if ( (z->writer=zipOpen2_64(stream, FALSE,
				NULL,
				&zstream_functions)) )
    { return unify_zipper(A2, z);
    } else
    { goto error;
    }
  } else
  { if ( (z->reader=unzOpen2_64(stream, &zstream_functions)) )
    { return unify_zipper(A2, z);
    } else
    { goto error;
    }
  }

error:
  if ( stream ) PL_release_stream(stream);
  if ( z ) free(z);
  return PL_warning("zip_open/4 failed");
}

/** zip_clone(+Zipper, -Clone)
*/

static
PRED_IMPL("zip_clone", 2, zip_clone, 0)
{ //PRED_LD
  zipper *z;

  if ( get_zipper(A1, &z) )
  { zipper *clone;

    if ( !z->reader || z->input_type != ZIP_MEMORY )
      return PL_permission_error("clone", "zipper", A1);

    if ( !(clone=malloc(sizeof(*clone))) )
      return PL_resource_error("memory");

    *clone = *z;
    clone->symbol     = 0;
    clone->owner      = 0;
    clone->lock_count = 0;
#ifdef O_PLMT
    simpleMutexInit(&z->lock);
#endif
    if ( clone->path )
      clone->path = strdup(clone->path);
    clone->reader = unzClone(clone->reader);
    clone->mapped_file = NULL;			/* I'm just a clone */

    return unify_zipper(A2, clone);
  }

  return FALSE;
}

/** zip_close(+Zipper, +Comment)
*/

static
PRED_IMPL("zip_close_", 2, zip_close, 0)
{ PRED_LD
  char *comment = NULL;
  zipper *z;
  int flags = (CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  zipper_state prev_state = ZIP_END;

  if ( get_zipper(A1, &z) &&
       (PL_is_variable(A2) || PL_get_chars(A2, &comment, flags)) &&
       zacquire(z, ZIP_CLOSE, &prev_state, "close") )
  { if ( prev_state == ZIP_READ_ENTRY )
    { PL_register_atom(z->symbol);
      return TRUE;				/* delay */
    } else if ( close_zipper(z) == 0 )
    { return TRUE;
    } else
    { return PL_warning("zip_close/2 failed");
    }
  }

  return FALSE;
}

/** zip_lock(+Zipper)
*/

static
PRED_IMPL("zip_lock", 1, zip_lock, 0)
{ zipper *z;

  if ( get_zipper(A1, &z) )
  { return zlock(z);
  }

  return FALSE;
}

static
PRED_IMPL("zip_unlock", 1, zip_unlock, 0)
{ zipper *z;

  if ( get_zipper(A1, &z) )
  { return zunlock(z);
  }

  return FALSE;
}

		 /*******************************
		 *	  ENTRY STREAMS		*
		 *******************************/

static ssize_t
Sread_zip_entry(void *handle, char *buf, size_t size)
{ zipper *z = handle;

  if ( z->reader )
  { return unzReadCurrentFile(z->reader, buf, size);
  } else
  { errno = EPERM;
    return -1;
  }
}

static ssize_t
Swrite_zip_entry(void *handle, char *buf, size_t size)
{ zipper *z = handle;

  if ( z->writer )
  { ssize_t rc = zipWriteInFileInZip(z->writer, buf, size);
    return rc == 0 ? size : -1;
  } else
  { errno = EPERM;
    return -1;
  }
}

static long
Sseek_zip_entry(void *handle, long pos, int whence)
{ (void) handle;
  (void) pos;
  (void) whence;

  errno = ESPIPE;
  return -1;
}

static int
Sclose_zip_entry(void *handle)
{ zipper *z = handle;
  int rc = -1;

  if ( z->writer )
    rc = zipCloseFileInZip(z->writer);
  else if ( z->reader )
    rc = unzCloseCurrentFile(z->reader);

  if ( z->state == ZIP_CLOSE )
  { zrelease(z);
    close_zipper(z);
    PL_register_atom(z->symbol);		/* revert delayed zip_close_/2 */
  } else if ( true(z, ZIP_RELEASE_ON_CLOSE) )
  { zrelease(z);
  } else
  { z->state = ZIP_IDLE;
  }

  return rc;
}

static int
Scontrol_zip_entry(void *handle, int action, void *arg)
{ zipper *z = handle;

  switch(action)
  { case SIO_GETSIZE:
    { if ( z->reader )
      { unz_file_info64 info;

	if ( unzGetCurrentFileInfo64(z->reader,
				     &info,
				     NULL, 0,
				     NULL, 0,
				     NULL, 0) == UNZ_OK )
	{ int64_t *rval = arg;
	  *rval = info.uncompressed_size;
	  return 0;
	}
	Sdprintf("Failed to get size\n");
      }
      return -1;
    }
    case SIO_FLUSHOUTPUT:
    case SIO_SETENCODING:
      return 0;
    default:
      return -1;
  }
}

IOFUNCTIONS Szipfunctions =
{ Sread_zip_entry,
  Swrite_zip_entry,
  NULL,
  Sclose_zip_entry,
  Scontrol_zip_entry,
  NULL						/* seek64 */
};

IOFUNCTIONS Szipfunctions_repositioning =
{ Sread_zip_entry,
  Swrite_zip_entry,
  Sseek_zip_entry,
  Sclose_zip_entry,
  Scontrol_zip_entry,
  NULL						/* seek64 */
};

		 /*******************************
		 *	  HANDLE ENTRIES	*
		 *******************************/

static void
zset_time(zip_fileinfo *info, double t)
{ struct tm rb, *r;
  tm_zip *tmzip = &info->tmz_date;
  time_t itime = t;

  r = PL_localtime_r(&itime, &rb);
  tmzip->tm_sec  = r->tm_sec;
  tmzip->tm_min  = r->tm_min;
  tmzip->tm_hour = r->tm_hour;
  tmzip->tm_mday = r->tm_mday;
  tmzip->tm_mon  = r->tm_mon ;
  tmzip->tm_year = r->tm_year;
}


static double
zget_time(const unz_file_info64 *info)
{ const tm_unz *tmzip = &info->tmu_date;
  struct tm tm = {0};

  tm.tm_sec   = tmzip->tm_sec;
  tm.tm_min   = tmzip->tm_min;
  tm.tm_hour  = tmzip->tm_hour;
  tm.tm_mday  = tmzip->tm_mday;
  tm.tm_mon   = tmzip->tm_mon ;
  tm.tm_year  = tmzip->tm_year - 1900;
  tm.tm_isdst = -1;

  return (double)mktime(&tm);
}

/** zipper_open_new_file_in_zip(+Zipper, +Name, -Stream, +Options)
*/

static const opt_spec zip_new_file_options[] =
{ { ATOM_extra,		    OPT_STRING },
  { ATOM_comment,	    OPT_STRING },
  { ATOM_time,		    OPT_DOUBLE },
  { ATOM_method,	    OPT_ATOM   },
  { ATOM_level,		    OPT_INT    },
  { ATOM_zip64,		    OPT_BOOL   },
  { NULL_ATOM,		    0          }
};


static
PRED_IMPL("zipper_open_new_file_in_zip", 4, zipper_open_new_file_in_zip, 0)
{ PRED_LD
  zipper *z;
  char *fname;
  int flags = (CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  char *extra = NULL;
  char *comment = NULL;
  int extralen = 0;
  double ftime = (double)time(NULL);
  int level = 6;
  atom_t method = ATOM_deflated;
  int imethod;
  int zip64 = FALSE;

  if ( !scan_options(A4, 0, ATOM_zip_options, zip_new_file_options,
		     &extra, &comment, &ftime, &method, &level, &zip64) )
    return FALSE;

  if ( extra )
    extralen = strlen(extra);

  if ( level < 1 || level > 9 )
  { term_t ex;
    return ( (ex=PL_new_term_ref()) &&
	     PL_put_integer(ex, level) &&
	     PL_domain_error("zip_level", ex) );
  }

  if ( method == ATOM_deflated )
    imethod = Z_DEFLATED;
  else if ( method == ATOM_store )
    imethod = 0;
  else
  { term_t ex;
    return ( (ex=PL_new_term_ref()) &&
	     PL_put_atom(ex, method) &&
	     PL_domain_error("zip_method", ex) );
  }

  if ( get_zipper(A1, &z) &&
       PL_get_chars(A2, &fname, flags) &&
       zacquire(z, ZIP_WRITE_ENTRY, NULL, "new_file") )
  { int rc;
    zip_fileinfo zipfi;

    memset(&zipfi, 0, sizeof(zipfi));
    zset_time(&zipfi, ftime);
    rc = zipOpenNewFileInZip4_64(z->writer, fname,
				 &zipfi,
				 extra, extralen,
				 NULL, 0,	/* extrafield global */
				 comment,	/* comment */
				 imethod,	/* method */
				 level,		/* level */
				 FALSE,		/* raw */
				 -MAX_WBITS,	/* windowBits */
				 DEF_MEM_LEVEL,	/* memLevel */
				 Z_DEFAULT_STRATEGY, /* strategy */
				 NULL,		/* password */
				 0,		/* crc */
				 VERSIONMADEBY,	/* versionMadeBy */
				 0,		/* flagBase */
				 zip64);	/* zip64 */

    if ( rc == 0 )
    { IOSTREAM *s = Snew(z, SIO_OUTPUT, &Szipfunctions);

      if ( s )
      { set(z, ZIP_RELEASE_ON_CLOSE);
	return PL_unify_stream(A3, s);
      }
    }
  }

  return FALSE;
}

/** zipper_goto(+Zipper, +File) is det.
 *
 * File is one of `first`, `next` or file(Name)
 */

static
PRED_IMPL("zipper_goto", 2, zipper_goto, 0)
{ PRED_LD
  zipper *z;

  if ( get_zipper(A1, &z) )
  { atom_t a;

    if ( !z->reader )
      return PL_warning("Not open for reading");

    if ( !zacquire(z, ZIP_SCAN, NULL, "goto") )
      return FALSE;

    if ( PL_get_atom(A2, &a) )
    { int rc;

      if ( a == ATOM_first )
	rc = unzGoToFirstFile(z->reader);
      else if (	a == ATOM_next )
	rc = unzGoToNextFile(z->reader);
      else
	return PL_domain_error("zipper_goto", A2);

      if ( rc == UNZ_OK )
	return TRUE;
      if ( rc == UNZ_END_OF_LIST_OF_FILE )
      { zrelease(z);
	return FALSE;
      }
      Sdprintf("zipper_goto/2: rc=%d (%s)\n", rc,
	       a == ATOM_first ? "first" : "next");
      assert(0);
    } else if ( PL_is_functor(A2, FUNCTOR_file1) )
    { term_t arg = PL_new_term_ref();
      char *fname;
      int flags = (CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
      int rc;

      if ( PL_get_arg(1, A2, arg) &&
	   PL_get_chars(arg, &fname, flags) )
      { switch((rc=unzLocateFile(z->reader, fname, TRUE)))
	{ case UNZ_OK:
	    return TRUE;
	  case UNZ_END_OF_LIST_OF_FILE:
	    zrelease(z);
	    return FALSE;
	}
	Sdprintf("zipper_goto/2: rc=%d file(%s)\n", rc, fname);
	assert(0);
      }
    } else if ( PL_is_functor(A2, FUNCTOR_offset1) )
    { term_t arg = PL_new_term_ref();
      int64_t offset;

      if ( PL_get_arg(1, A2, arg) &&
	   PL_get_int64_ex(arg, &offset) )
      { switch(unzSetOffset64(z->reader, offset))
	{ case UNZ_OK:
	    return TRUE;
	  default:
	    zrelease(z);
	    return PL_existence_error("zip_entry", arg);
	}
      }
    } else
    { zrelease(z);
      return PL_type_error("zipper_goto", A2);
    }
  }

  return FALSE;
}

/** zip_open_current(+Zipper, -Stream, +Options) is det.
 *
 *  Open the current file as an input stream
 */

static const opt_spec zipopen3_options[] =
{ { ATOM_type,		 OPT_ATOM },
  { ATOM_encoding,	 OPT_ATOM },
  { ATOM_bom,		 OPT_BOOL },
  { ATOM_release,	 OPT_BOOL },
  { ATOM_reposition,	 OPT_BOOL },
  { NULL_ATOM,	         0 }
};

static
PRED_IMPL("zipper_open_current", 3, zipper_open_current, 0)
{ PRED_LD
  zipper *z;
  atom_t type       = ATOM_text;
  atom_t encoding   = NULL_ATOM;
  int	 bom        = -1;
  int    release    = TRUE;
  int	 reposition = FALSE;
  size_t size       = 0;
  int flags         = SIO_INPUT|SIO_RECORDPOS|SIO_FBUF;
  IOENC enc;

  if ( !scan_options(A3, 0, ATOM_stream_option, zipopen3_options,
		     &type, &encoding, &bom, &release, &reposition) )
    return FALSE;
  if ( !stream_encoding_options(type, encoding, &bom, &enc) )
    return FALSE;
  if ( bom == -1 )
    bom = TRUE;

  if ( type == ATOM_text )
  { flags |= SIO_TEXT;
  } else if ( type != ATOM_binary )
  { term_t t = PL_new_term_ref();
    PL_put_atom(t, type);
    return PL_domain_error("type", t);
  }

  if ( get_zipper(A1, &z) )
  { if ( !z->reader )
      return PL_warning("Not open for reading");

    if ( !zacquire(z, ZIP_READ_ENTRY, NULL, "open_current") )
      return FALSE;

    if ( release )
      set(z, ZIP_RELEASE_ON_CLOSE);

    if ( reposition )
    { unz_file_info64 info;
      char fname[PATH_MAX];
      char extra[1024];
      char comment[1024];

      if ( unzGetCurrentFileInfo64(z->reader,
				 &info,
				 fname, sizeof(fname),
				 extra, sizeof(extra),
				 comment, sizeof(comment)) == UNZ_OK )
	size = info.uncompressed_size;
    }

    if ( unzOpenCurrentFile(z->reader) == UNZ_OK )
    { IOSTREAM *s = Snew(z, flags, reposition ? &Szipfunctions_repositioning
					      : &Szipfunctions);

      if ( s )
      { s->encoding = enc;

	if ( reposition && size )
	  Ssetbuffer(s, NULL, size);

	if ( bom && ScheckBOM(s) < 0 )
	  return PL_release_stream(PL_acquire_stream(s));

	return PL_unify_stream(A2, s);
      }

      return PL_resource_error("memory");
    }
    PL_warning("Failed to open current");
  }

  return FALSE;
}

static
PRED_IMPL("zip_file_info_", 3, zip_file_info, 0)
{ PRED_LD
  zipper *z;

  if ( get_zipper(A1, &z) )
  { unz_file_info64 info;
    char fname[PATH_MAX];
    char extra[1024];
    char comment[1024];

    if ( !z->reader )
      return PL_warning("Not open for reading");

    extra[0] = comment[0] = EOS;

    if ( unzGetCurrentFileInfo64(z->reader,
				 &info,
				 fname, sizeof(fname),
				 extra, sizeof(extra),
				 comment, sizeof(comment)) == UNZ_OK )
    { ZPOS64_T offset = unzGetOffset64(z->reader);

      return ( PL_unify_chars(A2, PL_ATOM|REP_UTF8, (size_t)-1, fname) &&
	       PL_unify_term(A3, PL_FUNCTOR_CHARS, "info", 6,
			       PL_INT64, (int64_t)info.compressed_size,
			       PL_INT64, (int64_t)info.uncompressed_size,
			       PL_UTF8_CHARS, extra,
			       PL_UTF8_STRING, comment,
			       PL_DOUBLE, zget_time(&info),
			       PL_INT64, offset) );
    }
  }

  return FALSE;
}

static
PRED_IMPL("$rc_handle", 1, rc_handle, 0)
{ PRED_LD

  if ( !GD->resources.handle )
  { if ( GD->resources.DB &&
	 unify_zipper(A1, GD->resources.DB) &&
	 PL_get_atom(A1, &GD->resources.handle) )
    { PL_register_atom(GD->resources.handle);
      return TRUE;
    }

    return FALSE;
  } else
  { return PL_unify_atom(A1, GD->resources.handle);
  }
}

		 /*******************************
		 *	      MAPPING		*
		 *******************************/

#ifdef HAVE_FILE_MAPPING

#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

static mapped_file *
map_file(const char *name)
{ mapped_file *mf;

  if ( !(mf=malloc(sizeof(*mf))) )
    return NULL;
  memset(mf, 0, sizeof(*mf));

#ifdef HAVE_MMAP
  int fd;

  mf->start = MAP_FAILED;
  if ( (fd = open(name, O_RDONLY)) >= 0 )
  { struct stat buf;

    if ( fstat(fd, &buf) == 0 )
    { mf->start = mmap(NULL,
		       buf.st_size,
		       PROT_READ,
		       MAP_SHARED,
		       fd,
		       0);
      mf->end = mf->start + buf.st_size;
    }

    close(fd);
  }

  if ( mf->start == MAP_FAILED )
  { free(mf);
    return NULL;
  }

  return mf;
#else /*HAVE_MMAP*/
#ifdef __WINDOWS__
  DWORD fsize;
  wchar_t buf[PATH_MAX];

  if ( !_xos_os_filenameW(name, buf, PATH_MAX) )
    goto errio;
  mf->hfile = CreateFileW(buf,
			  GENERIC_READ,
			  FILE_SHARE_READ,
			  NULL,
			  OPEN_EXISTING,
			  FILE_ATTRIBUTE_NORMAL,
			  NULL);
  if ( !mf->hfile )
    goto errio;

  if ( (fsize = GetFileSize(mf->hfile, NULL)) == (DWORD)~0L )
    goto errio;

  mf->hmap = CreateFileMapping(mf->hfile,
			       NULL,
			       PAGE_READONLY,
			       0L,
			       (DWORD)fsize, /* WIN64: Truncated! */
			       NULL);
  if ( !mf->hmap )
    goto errio;

  mf->start = MapViewOfFile(mf->hmap,
			    FILE_MAP_READ,
			    0L, 0L, /* offset */
			    0L);	/* size (0=all) */

  if ( !mf->start )
    goto errio;

  mf->end = mf->start + fsize;
  return mf;

errio:
  if ( mf->hmap )
    CloseHandle(mf->hmap);
  if ( mf->hfile )
    CloseHandle(mf->hfile);
  mf->start = NULL;
  mf->hfile = NULL;
  mf->hmap  = NULL;
  return NULL;
#endif  /*__WINDOWS__*/
#endif /*HAVE_MMAP*/
}

static void
unmap_file(mapped_file *mf)
{
#ifdef HAVE_MMAP
  if ( mf->start )
    munmap(mf->start, mf->end - mf->start);
#endif
#ifdef __WINDOWS__
  if ( mf->start )
    UnmapViewOfFile(mf->start);
  if ( mf->hmap )
    CloseHandle(mf->hmap);
  if ( mf->hfile )
    CloseHandle(mf->hfile);
#endif

  free(mf);
}

#endif /*HAVE_FILE_MAPPING*/

		 /*******************************
		 *	 ARCHIVE EMULATION	*
		 *******************************/

zipper *
zip_open_archive(const char *file, int flags)
{ zipper z = {0};
  zipper *r = NULL;

  if ( (flags&RC_RDONLY) )
  {
#ifdef HAVE_FILE_MAPPING
    mapped_file *mf;

    DEBUG(MSG_ZIP, Sdprintf("Opening %s using file mapping\n", file));

    if ( (mf=map_file(file)) )
    { if ( (r=zip_open_archive_mem((const unsigned char *)mf->start,
				    mf->end-mf->start, flags)) )
      { r->mapped_file = mf;
      } else
      { unmap_file(mf);
      }
    }
#else
    DEBUG(MSG_ZIP, Sdprintf("Opening %s as stream\n", file));

    if ( !(z.reader = unzOpen2_64(file, &zfile_functions)) )
      return NULL;

    if ( (r = malloc(sizeof(*r))) )
    { memcpy(r, &z, sizeof(*r));
#ifdef O_PLMT
      simpleMutexInit(&r->lock);
#endif
      r->input_type  = ZIP_FILE;
      r->input.any   = NULL;
    }
#endif
  } else
  { DEBUG(MSG_ZIP, Sdprintf("Opening %s as stream\n", file));

    if ( !(z.writer = zipOpen2_64(file, FALSE, NULL, &zfile_functions)) )
      return NULL;

    if ( (r = malloc(sizeof(*r))) )
    { memcpy(r, &z, sizeof(*r));
#ifdef O_PLMT
      simpleMutexInit(&r->lock);
#endif
      r->input_type = ZIP_FILE;
      r->input.any  = NULL;
    }
  }

  if ( r )
    r->path = strdup(file);

  return r;
}

zipper *
zip_open_archive_mem(const unsigned char *mem, size_t mem_size, int flags)
{ zipper z = {0};
  zipper *r;
  mem_stream *mems;

  assert((flags&RC_RDONLY));

  if ( !(mems = malloc(sizeof(*mems))) )
    return NULL;
  mems->start = (const char*)mem;
  mems->end   = mems->start+mem_size;

  if ( !(z.reader = unzOpen2_64(mems, &zmem_functions)) )
    return NULL;

  if ( (r = malloc(sizeof(*r))) )
  { memcpy(r, &z, sizeof(*r));
    set(r, ZIP_CLOSE_STREAM_ON_CLOSE);
#ifdef O_PLMT
    simpleMutexInit(&r->lock);
#endif
    r->input_type   = ZIP_MEMORY;
    r->input.memory = mems;
  }

  return r;
}

int
zip_close_archive(zipper *z)
{ int rc = close_zipper(z);

  free(z);
  return rc;
}


IOSTREAM *
SopenZIP(zipper *z, const char *name, int flags)
{ if ( z->reader )
  { if ( zacquire(z, ZIP_SCAN, NULL, "goto") &&
	 unzLocateFile(z->reader, name, TRUE) == UNZ_OK &&
	 zacquire(z, ZIP_READ_ENTRY, NULL, "open_current") &&
	 unzOpenCurrentFile(z->reader) == UNZ_OK )
    { set(z, ZIP_RELEASE_ON_CLOSE);
      return Snew(z, SIO_INPUT, &Szipfunctions);
    }
  } else
  { int rc;
    zip_fileinfo zipfi;

    memset(&zipfi, 0, sizeof(zipfi));
    if ( !zacquire(z, ZIP_WRITE_ENTRY, NULL, "new_file") )
      return NULL;

    zset_time(&zipfi, (double)time(NULL));
    rc = zipOpenNewFileInZip4_64(z->writer, name,
				 &zipfi,
				 NULL, 0,	/* extrafield local */
				 NULL, 0,	/* extrafield global */
				 NULL,		/* comment */
				 Z_DEFLATED,	/* method */
				 6,		/* level */
				 FALSE,		/* raw */
				 -MAX_WBITS,	/* windowBits */
				 DEF_MEM_LEVEL,	/* memLevel */
				 Z_DEFAULT_STRATEGY, /* strategy */
				 NULL,		/* password */
				 0,		/* crc */
				 VERSIONMADEBY,	/* versionMadeBy */
				 0,		/* flagBase */
				 FALSE);	/* zip64 */
    if ( rc == 0 )
    { set(z, ZIP_RELEASE_ON_CLOSE);
      return Snew(z, SIO_OUTPUT, &Szipfunctions);
    }
  }

  return NULL;
}

const char *
zipper_file(const zipper *z)
{ return z->path;
}



		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(zip)
  PRED_DEF("zip_open_stream",		  3, zip_open_stream,	          0)
  PRED_DEF("zip_clone",		          2, zip_clone,	                  0)
  PRED_DEF("zip_close_",		  2, zip_close,		          0)
  PRED_DEF("zip_lock",		          1, zip_lock,		          0)
  PRED_DEF("zip_unlock",	          1, zip_unlock,		  0)
  PRED_DEF("zipper_open_new_file_in_zip", 4, zipper_open_new_file_in_zip, 0)
  PRED_DEF("zipper_goto",		  2, zipper_goto,		  0)
  PRED_DEF("zipper_open_current",         3, zipper_open_current,         0)
  PRED_DEF("zip_file_info_",		  3, zip_file_info,               0)
  PRED_DEF("$rc_handle",		  1, rc_handle,		          0)
EndPredDefs
