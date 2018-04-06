/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

#include "minizip/zip.h"
#include "minizip/unzip.h"
#include "pl-incl.h"

#ifndef VERSIONMADEBY
# define VERSIONMADEBY   (0x0) /* platform dependent */
#endif

		 /*******************************
		 *  ACCESS ARCHIVES AS STREAMS  *
		 *******************************/

static voidpf
zopen64_file(voidpf opaque, const void* filename, int mode)
{ char modes[4];
  char *m = modes;

  if ( (mode&ZLIB_FILEFUNC_MODE_CREATE) )
    *m++ = 'w';
  else
    *m++ = 'r';
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
  NULL						/* opaque */
};

		 /*******************************
		 *	   ARCHIVE BLOB		*
		 *******************************/

typedef struct zipper
{ zipFile writer;
  unzFile reader;
} zipper;

static int
write_zipper(IOSTREAM *s, atom_t aref, int flags)
{ zipper *ref = PL_blob_data(aref, NULL, NULL);

  Sfprintf(s, "<zipper>(%p)", ref);
  return TRUE;
}

static void
acquire_zipper(atom_t aref)
{ zipper *ref = PL_blob_data(aref, NULL, NULL);
  (void)ref;
}

static int
release_zipper(atom_t aref)
{ zipper *ref = PL_blob_data(aref, NULL, NULL);
  zipFile zf;
  unzFile uzf;

  if ( (zf=ref->writer) )
  { ref->writer = NULL;
    zipClose(zf, NULL);
  } else if ( (uzf=ref->reader) )
  { ref->reader = NULL;
    unzClose(uzf);
  }
  free(ref);

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
{ return PL_unify_blob(t, zipper, sizeof(*zipper), &zipper_blob);
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

/** zip_open(+File, +Mode, -Zip, +Options)
*/

static
PRED_IMPL("zip_open", 4, zip_open, 0)
{ PRED_LD
  char *fname;
  atom_t mode;
  int fflags = PL_FILE_OSPATH;
  zipper *z;

  if ( !PL_get_atom_ex(A2, &mode) )
    return FALSE;
  if ( mode == ATOM_read )
    fflags |= PL_FILE_EXIST;
  else if ( mode == ATOM_write || mode == ATOM_append )
    (void)0;
  else
    return PL_domain_error("file_mode", A2);

  if ( !PL_get_file_name(A1, &fname, fflags) )
    return FALSE;

  if ( !(z=malloc(sizeof(*z))) )
    return PL_resource_error("memory");
  memset(z, 0, sizeof(*z));

  if ( mode == ATOM_write || mode == ATOM_append )
  { if ( (z->writer=zipOpen2_64(fname, mode == ATOM_append,
				NULL,
				&zfile_functions)) )
    { return unify_zipper(A3, z);
    } else
    { goto error;
    }
  } else
  { if ( (z->reader=unzOpen2_64(fname, &zfile_functions)) )
    { return unify_zipper(A3, z);
    } else
    { goto error;
    }
  }

error:
  return PL_warning("zip_open/4 failed");
}

/** zip_close(+Zipper, +Comment)
*/

static
PRED_IMPL("zip_close", 2, zip_close, 0)
{ PRED_LD
  char *comment = NULL;
  zipper *z;
  int flags = (CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);

  if ( get_zipper(A1, &z) &&
       (PL_is_variable(A2) || PL_get_chars(A2, &comment, flags)) )
  { zipFile zf;
    unzFile uzf;

    if ( (zf=z->writer) )
    { z->writer = NULL;

      if ( zipClose(zf, comment) == 0 )
	return TRUE;
      else
	return PL_warning("zip_close/2 failed");
    } else if ( (uzf=z->reader) )
    { z->reader = NULL;

      if ( unzClose(uzf)  == 0 )
	return TRUE;
      else
	return PL_warning("zip_close/2 failed");
    }
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

static int
Sclose_zip_entry(void *handle)
{ zipper *z = handle;

  if ( z->writer )
    return zipCloseFileInZip(z->writer);
  else if ( z->reader )
    return unzCloseCurrentFile(z->reader);

  return -1;
}

IOFUNCTIONS Szipfunctions =
{ Sread_zip_entry,
  Swrite_zip_entry,
  NULL,						/* seek */
  Sclose_zip_entry,
  NULL,						/* control */
  NULL						/* seek64 */
};


		 /*******************************
		 *	  HANDLE ENTRIES	*
		 *******************************/

/** zip_open_new_file_in_zip(+Zipper, +Name, -Stream, +Options)
*/

static const opt_spec zip_new_file_options[] =
{ { ATOM_extra,		    OPT_STRING },
  { ATOM_comment,	    OPT_STRING },
  { NULL_ATOM,		    0 }
};


static
PRED_IMPL("zip_open_new_file_in_zip", 4, zip_open_new_file_in_zip, 0)
{ //PRED_LD
  zipper *z;
  char *fname;
  int flags = (CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  char *extra = NULL;
  char *comment = NULL;
  int extralen = 0;

  if ( !scan_options(A4, 0, ATOM_zip_options, zip_new_file_options,
		     &extra, &comment) )
    return FALSE;
  if ( extra )
    extralen = strlen(extra);

  if ( get_zipper(A1, &z) &&
       PL_get_chars(A2, &fname, flags))
  { int rc;
    zip_fileinfo zipfi = {0};

    rc = zipOpenNewFileInZip4_64(z->writer, fname,
				 &zipfi,
				 extra, extralen,
				 NULL, 0,	/* extrafield global */
				 comment,	/* comment */
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
    { IOSTREAM *s = Snew(z, SIO_OUTPUT, &Szipfunctions);

      if ( s )
	return PL_unify_stream(A3, s);
    }
  }

  return FALSE;
}

/** zip_goto(+Zipper, +File) is det.
 *
 * File is one of `first`, `next` or file(Name)
 */

static
PRED_IMPL("zip_goto", 2, zip_goto, 0)
{ PRED_LD
  zipper *z;

  if ( get_zipper(A1, &z) )
  { atom_t a;

    if ( PL_get_atom(A2, &a) )
    { int rc;

      if ( a == ATOM_first )
	rc = unzGoToFirstFile(z->reader);
      else if (	a == ATOM_next )
	rc = unzGoToNextFile(z->reader);
      else
	return PL_domain_error("zip_goto", A2);

      if ( rc == UNZ_OK )
	return TRUE;
      if ( rc == UNZ_END_OF_LIST_OF_FILE )
	return FALSE;
      assert(0);
    } else if ( PL_is_functor(A2, FUNCTOR_file1) )
    { term_t arg = PL_new_term_ref();
      char *fname;
      int flags = (CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);

      if ( PL_get_arg(1, A2, arg) &&
	   PL_get_chars(arg, &fname, flags) )
      { switch(unzLocateFile(z->reader, fname, TRUE))
	{ case UNZ_OK:
	    return TRUE;
	  default:
	    return PL_existence_error("zip_entry", arg);
	}
      }
    } else
    { return PL_type_error("zip_goto", A2);
    }
  }

  return FALSE;
}

/** zip_open_current(+Zipper, -Stream) is det.
 *
 *  Open the current file as an input stream
 */

static
PRED_IMPL("zip_open_current", 2, zip_open_current, 0)
{ zipper *z;

  if ( get_zipper(A1, &z) )
  { if ( !z->reader )
      return PL_warning("Not open for reading");
    if ( unzOpenCurrentFile(z->reader) == UNZ_OK )
    { IOSTREAM *s = Snew(z, SIO_INPUT|SIO_RECORDPOS, &Szipfunctions);

      if ( s )
	return PL_unify_stream(A2, s);

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
    char fname[MAXPATHLEN];
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
    { return ( PL_unify_chars(A2, PL_ATOM|REP_UTF8, (size_t)-1, fname) &&
	       PL_unify_term(A3, PL_FUNCTOR_CHARS, "info", 4,
			       PL_INT64, (int64_t)info.compressed_size,
			       PL_INT64, (int64_t)info.uncompressed_size,
			       PL_UTF8_CHARS, extra,
			       PL_UTF8_STRING, comment) );
    }
  }

  return FALSE;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(zip)
  PRED_DEF("zip_open",			4, zip_open,		     0)
  PRED_DEF("zip_close",			2, zip_close,		     0)
  PRED_DEF("zip_open_new_file_in_zip",	4, zip_open_new_file_in_zip, 0)
  PRED_DEF("zip_goto",			2, zip_goto,		     0)
  PRED_DEF("zip_open_current",          2, zip_open_current,         0)
  PRED_DEF("zip_file_info_",            3, zip_file_info,            0)
EndPredDefs
