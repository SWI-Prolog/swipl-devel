/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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

#define O_DEBUG 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <zlib.h>
/* Some distributions do not include this ... */
#ifdef HAVE_ZUTIL_H
#include <zutil.h>
#else
#include "zutil.h"
#endif

static functor_t FUNCTOR_error2;	/* error(Formal, Context) */
static functor_t FUNCTOR_type_error2;	/* type_error(Term, Expected) */
static functor_t FUNCTOR_domain_error2;	/* domain_error(Term, Expected) */

static atom_t ATOM_format;		/* format(Format) */
static atom_t ATOM_level;		/* level(Int) */
static atom_t ATOM_close_parent;	/* close_parent(Bool) */
static atom_t ATOM_gzip;
static atom_t ATOM_deflate;
static int debuglevel = 0;

#ifdef O_DEBUG
#define DEBUG(n, g) if ( debuglevel >= n ) g
#else
#define DEBUG(n, g) (void)0
#endif

		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
domain_error(term_t actual, const char *domain)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_domain_error2,
		        PL_CHARS, domain,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
get_atom_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    return TRUE;

  return type_error(t, "atom");
}

static int
get_int_ex(term_t t, int *i)
{ if ( PL_get_integer(t, i) )
    return TRUE;

  return type_error(t, "integer");
}

static int
get_bool_ex(term_t t, int *i)
{ if ( PL_get_bool(t, i) )
    return TRUE;

  return type_error(t, "boolean");
}


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef enum
{ F_UNKNOWN = 0,
  F_GZIP,				/* gzip output */
  F_GZIP_CRC,				/* end of gzip output */
  F_DEFLATE				/* zlib data */
} zformat;

typedef struct z_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *zstream;		/* Compressed stream (I'm handle of) */
  int		    close_parent;	/* close parent on close */
  zformat	    format;		/* current format */
  uLong		    crc;		/* CRC check */
  z_stream	    zstate;		/* Zlib state */
} z_context;


static z_context*
alloc_zcontext(IOSTREAM *s)
{ z_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->stream       = s;
  ctx->close_parent = TRUE;

  return ctx;
}


static void
free_zcontext(z_context *ctx)
{ PL_release_stream(ctx->stream);
  PL_free(ctx);
}


		 /*******************************
		 *	     GZIP HEADER	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Code based on gzio.c from the zlib source distribution.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int gz_magic[2] = {0x1f, 0x8b}; /* gzip magic header */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define HEAD_CRC     0x02 /* bit 1 set: header CRC present */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define RESERVED     0xE0 /* bits 5..7: reserved */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gz_skip_header() parses the gzip file-header.  return

	* If ok: pointer to first byte following header
	* If not a gzip file: NULL
	* If still ok, but incomplete: GZHDR_SHORT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define HDR_SHORT ((Bytef*)-1)		/* Header is incomplete */
#define SKIP_STRING \
	{ while ( *in && avail > 0 ) \
	    in++, avail--; \
	  if ( avail > 0 ) \
	    in++, avail--; \
	}

static Bytef *
gz_skip_header(z_context *ctx, Bytef *in, int avail)
{ int method; /* method byte */
  int flags;  /* flags byte */
  int len;

  if ( avail < 10 )			/* 2-byte magic, method, flags, */
    return HDR_SHORT;			/* time, xflags and OS code */

  if ( in[0] != gz_magic[0] &&
       in[1] != gz_magic[1] )
    return NULL;
  in += 2;

  method = *in++;
  flags  = *in++;
  if ( method != Z_DEFLATED || (flags & RESERVED ) != 0)
    return NULL;

  in += 6;				/* Discard time, xflags and OS code */
  avail -= 10;

  if ((flags & EXTRA_FIELD) != 0)
  { /* skip the extra field */
    len  =  *in++;
    len += (*in++)<<8;
    len &= 0xffff;

    if ( avail > len )
    { in += len;
      avail -= len;
    } else
    { return HDR_SHORT;
    }
  }
  if ((flags & ORIG_NAME) != 0)
  { /* skip the original file name */
    SKIP_STRING
  }
  if ((flags & COMMENT) != 0)
  {   /* skip the .gz file comment */
    SKIP_STRING
  }
  if ((flags & HEAD_CRC) != 0)
  {  /* skip the header crc */
    in += 2;
    avail -= 2;
  }

  if ( avail <= 0 )
    return HDR_SHORT;

  return in;
}


static int
write_ulong_lsb(IOSTREAM *s, unsigned long x)
{ Sputc((x)    &0xff, s);
  Sputc((x>>8) &0xff, s);
  Sputc((x>>16)&0xff, s);
  Sputc((x>>24)&0xff, s);

  return Sferror(s) ? -1 : 0;
}


static int
write_gzip_header(z_context *ctx)
{ IOSTREAM *s = ctx->stream;
  time_t stamp = time(NULL);

  Sputc(gz_magic[0], s);
  Sputc(gz_magic[1], s);
  Sputc(Z_DEFLATED, s);			/* method */
  Sputc(0, s);				/* flags */
  write_ulong_lsb(s, stamp);		/* time stamp */
  Sputc(0, s);				/* xflags */
  Sputc(OS_CODE, s);			/* OS identifier */

  return Sferror(s) ? FALSE : TRUE;	/* TBD: Error */
}


static int
write_gzip_footer(z_context *ctx)
{ IOSTREAM *s = ctx->stream;

  write_ulong_lsb(s, ctx->crc);		/* CRC32 */
  write_ulong_lsb(s, ctx->zstate.total_in);	/* Total length */

  return Sferror(s) ? -1 : 0;
}


static Bytef *
get_ulong_lsb(const Bytef *in, uLong *v)
{ *v = (in[0] |
	in[1] << 8 |
	in[2] << 16 |
	in[3] << 24) & 0xffffffff;

  return (Bytef*)in+4;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	0: ok
       -1: CRC/size error
       -2: not enough data
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static int
gz_skip_footer(z_context *ctx)
{ if ( ctx->zstate.avail_in >= 8 )
  { uLong crc, size;
    Bytef *in = ctx->zstate.next_in;

    in = get_ulong_lsb(in, &crc);
    in = get_ulong_lsb(in, &size);

    ctx->zstate.next_in = in;
    ctx->zstate.avail_in -= 8;

    if ( crc != ctx->crc )
    { char msg[256];

      Ssprintf(msg, "CRC error (%08lx != %08lx)", crc, ctx->crc);
      Sseterr(ctx->zstream, SIO_FERR, msg);
      return -1;
    }
    if ( size != ctx->zstate.total_out )
    { char msg[256];
      
      Ssprintf(msg, "Size mismatch (%ld != %ld)", size, ctx->zstate.total_out);
      Sseterr(ctx->zstream, SIO_FERR, msg);
      return -1;
    }

    return 0;
  }

  return -2;
}


		 /*******************************
		 *	       GZ I/O		*
		 *******************************/

static int				/* inflate */
zread(void *handle, char *buf, size_t size)
{ z_context *ctx = handle;
  int flush = Z_SYNC_FLUSH;
  int rc;

  if ( ctx->zstate.avail_in == 0 )
  { if ( Sfeof(ctx->stream) )
    { flush = Z_FINISH;
    } else
    { ctx->zstate.next_in  = (Bytef*)ctx->stream->bufp;
      ctx->zstate.avail_in = ctx->stream->limitp - ctx->stream->bufp;
      ctx->stream->bufp    = ctx->stream->limitp; /* empty buffer */
    }
  }

  DEBUG(1, Sdprintf("Processing %d bytes\n", ctx->zstate.avail_in));
  ctx->zstate.next_out  = (Bytef*)buf;
  ctx->zstate.avail_out = size;
  
  if ( ctx->format == F_UNKNOWN )
  { Bytef *p;

    DEBUG(1, Sdprintf("Trying gzip header\n"));
    while( (p = gz_skip_header(ctx, ctx->zstate.next_in,
			       ctx->zstate.avail_in)) == HDR_SHORT )
    { 					/* TBD: read more */
    }

    if ( p )
    { int m = p - ctx->zstate.next_in;

      ctx->format = F_GZIP;
      DEBUG(1, Sdprintf("Skipped gzip header (%d bytes)\n", m));
      ctx->zstate.next_in = p;
      ctx->zstate.avail_in -= m;
      
					/* init without header */
      switch(inflateInit2(&ctx->zstate, -MAX_WBITS))
      { case Z_OK:
	  ctx->crc = crc32(0L, Z_NULL, 0);
	  DEBUG(1, Sdprintf("inflateInit2(): Z_OK\n"));
	  break;
	case Z_MEM_ERROR:		/* no memory */
        case Z_VERSION_ERROR:		/* bad library version */
	  PL_warning("ERROR: TBD");
	  return -1;
	default:
	  assert(0);
	  return -1;
      }
    } else
    { switch(inflateInit(&ctx->zstate))
      { case Z_OK:
	  ctx->format = F_DEFLATE;
	  DEBUG(1, Sdprintf("inflateInit(): Z_OK\n"));
	  break;
	case Z_MEM_ERROR:		/* no memory */
        case Z_VERSION_ERROR:		/* bad library version */
	  PL_warning("ERROR: TBD");
	  return -1;
	default:
	  assert(0);
	  return -1;
      }
    }
  } else if ( ctx->format == F_GZIP_CRC )
  { int rc;

    while( (rc=gz_skip_footer(ctx)) == -2 )
    {					/* TBD: read more */
    }

    if ( rc == 0 )
    { int avail = ctx->zstate.avail_in;

					/* copy back unprocessed bytes */
      DEBUG(1, Sdprintf("GZIP footer ok; copying %d bytes back\n", avail));
      memmove(ctx->stream->buffer, ctx->zstate.next_in, avail);
      ctx->stream->bufp   = ctx->stream->buffer;
      ctx->stream->limitp = ctx->stream->bufp + avail;
      
      return 0;			/* EOF */
    } else
    { DEBUG(1, Sdprintf("GZIP CRC/length error\n"));
      return -1;
    }
  }

  switch((rc=inflate(&ctx->zstate, Z_NO_FLUSH)))
  { case Z_OK:
    case Z_STREAM_END:
    { int n = size - ctx->zstate.avail_out;
      
      if ( ctx->format == F_GZIP && n > 0 )
	ctx->crc = crc32(ctx->crc, (Bytef*)buf, n);

      if ( rc == Z_STREAM_END )
      { DEBUG(1, Sdprintf("Z_STREAM_END: %d bytes\n", n));

	if ( ctx->format == F_GZIP )
	  ctx->format = F_GZIP_CRC;
      } else
      { DEBUG(1, Sdprintf("Z_OK: %d bytes\n", n));
      }

      return n;
    }
    case Z_NEED_DICT:
    case Z_DATA_ERROR:
    case Z_STREAM_ERROR:
    case Z_MEM_ERROR:
    case Z_BUF_ERROR:
    default:
      if ( ctx->zstate.msg )
	Sdprintf("ERROR: zread(): %s\n", ctx->zstate.msg);
      return -1;
  }
}


static int				/* deflate */
zwrite4(void *handle, char *buf, size_t size, int flush)
{ z_context *ctx = handle;
  Bytef buffer[SIO_BUFSIZE];
  int rc;

  ctx->zstate.next_in = (Bytef*)buf;
  ctx->zstate.avail_in = size;
  if ( ctx->format == F_GZIP && size > 0 )
    ctx->crc = crc32(ctx->crc, ctx->zstate.next_in, ctx->zstate.avail_in);

  DEBUG(1, Sdprintf("Compressing %d bytes\n", ctx->zstate.avail_in));
  ctx->zstate.next_out  = buffer;
  ctx->zstate.avail_out = sizeof(buffer);
  switch( (rc = deflate(&ctx->zstate, flush)) )
  { case Z_OK:
    case Z_STREAM_END:
    { int n = sizeof(buffer) - ctx->zstate.avail_out;

      DEBUG(1, Sdprintf("Compressed (%s) to %d bytes; left %d\n",
			rc == Z_OK ? "Z_OK" : "Z_STREAM_END",
			n, ctx->zstate.avail_in));

      if ( Sfwrite(buffer, 1, n, ctx->stream) < 0 )
	return -1;
      if ( size == 0 && Sflush(ctx->stream) < 0 )
	return -1;

      break;
    }
    case Z_BUF_ERROR:
      if ( ctx->zstate.avail_in == 0 && ctx->zstate.avail_out > 0 )
	return size;			/* nothing to progress: not an error */
					/* i.e. flush twice.  Better check? */
      /*FALLTHROUGH*/
    case Z_STREAM_ERROR:
    default:
      Sdprintf("ERROR: zwrite(): %s\n", ctx->zstate.msg);
      return -1;
  }

  return size;
}


static int				/* deflate */
zwrite(void *handle, char *buf, size_t size)
{ return zwrite4(handle, buf, size, Z_NO_FLUSH);
}


static int
zcontrol(void *handle, int op, void *data)
{ z_context *ctx = handle;

  switch(op)
  { case SIO_FLUSHOUTPUT:
      return zwrite4(handle, NULL, 0, Z_SYNC_FLUSH);
    case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
      if ( ctx->stream->functions->control )
	return (*ctx->stream->functions->control)(ctx->stream->handle, op, data);
      return -1;
  }
}


static int
zclose(void *handle)
{ z_context *ctx = handle;
  int rc;

  DEBUG(1, Sdprintf("zclose() ...\n"));

  if ( (ctx->stream->flags & SIO_INPUT) )
  { rc = inflateEnd(&ctx->zstate);
  } else
  { rc = zwrite4(handle, NULL, 0, Z_FINISH);	/* flush */
    if ( rc == 0 && ctx->format == F_GZIP )
      rc = write_gzip_footer(ctx);
    if ( rc == 0 )
      rc = deflateEnd(&ctx->zstate);
    else
      deflateEnd(&ctx->zstate);
  }

  switch(rc)
  { case Z_OK:
      if ( ctx->close_parent )
      { IOSTREAM *parent = ctx->stream;
	free_zcontext(ctx);
	return Sclose(parent);
      } else
      { free_zcontext(ctx);
	return 0;
      } 
    case Z_STREAM_ERROR:		/* inconsistent state */
    case Z_DATA_ERROR:			/* premature end */
    default:
      if ( ctx->close_parent )
      { IOSTREAM *parent = ctx->stream;
	free_zcontext(ctx);
	Sclose(parent);
	return -1;
      }

      free_zcontext(ctx);
      return -1;
  }
}


static IOFUNCTIONS zfunctions =
{ zread,
  zwrite,
  NULL,					/* seek */
  zclose,
  zcontrol,				/* zcontrol */
  NULL,					/* seek64 */
};


		 /*******************************
		 *	 PROLOG CONNECTION	*
		 *******************************/

#define COPY_FLAGS (SIO_INPUT|SIO_OUTPUT| \
		    SIO_TEXT| \
		    SIO_REPXML|SIO_REPPL|\
		    SIO_RECORDPOS)

static foreign_t
pl_zopen(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  z_context *ctx;
  zformat fmt = F_UNKNOWN;
  int level = Z_DEFAULT_COMPRESSION;
  IOSTREAM *s, *s2;
  int close_parent = TRUE;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    PL_get_arg(1, head, arg);

    if ( name == ATOM_format )
    { atom_t a;
      
      if ( !get_atom_ex(arg, &a) )
	return FALSE;
      if ( a == ATOM_gzip )
	fmt = F_GZIP;
      else if ( a == ATOM_deflate )
	fmt = F_DEFLATE;
      else
	return domain_error(arg, "compression_format");
    } else if ( name == ATOM_level )
    { if ( !get_int_ex(arg, &level) )
	return FALSE;
      if ( level < 0 || level > 9 )
	return domain_error(arg, "compression_level");
    } else if ( name == ATOM_close_parent )
    { if ( !get_bool_ex(arg, &close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */
  ctx = alloc_zcontext(s);
  ctx->close_parent = close_parent;
  if ( (s->flags & SIO_OUTPUT) )
  { int rc;

    ctx->format = fmt;
    if ( fmt == F_GZIP )
    { if ( write_gzip_header(ctx) < 0 )
      { free_zcontext(ctx);
	return FALSE;
      }
      rc = deflateInit2(&ctx->zstate, level, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);
    } else
    { rc = deflateInit(&ctx->zstate, level);
    }

    if ( rc != Z_OK )
    { free_zcontext(ctx);
      return FALSE;			/* TBD: Error */
    }
  }

  

  if ( !(s2 = Snew(ctx,
		   (s->flags&COPY_FLAGS)|SIO_FBUF,
		   &zfunctions))	)
  { free_zcontext(ctx);			/* no memory */

    return FALSE;
  }

  ctx->zstream = s2;
  return PL_unify_stream(new, s2);
}


#ifdef O_DEBUG
static foreign_t
zdebug(term_t level)
{ return PL_get_integer(level, &debuglevel);
}
#endif

		 /*******************************
		 *	       INSTALL		*
		 *******************************/

#define MKFUNCTOR(name, arity) PL_new_functor(PL_new_atom(name), arity)

install_t
install_zlib4pl()
{ FUNCTOR_error2        = MKFUNCTOR("error", 2);
  FUNCTOR_type_error2   = MKFUNCTOR("type_error", 2);
  FUNCTOR_domain_error2 = MKFUNCTOR("domain_error", 2);

  ATOM_format       = PL_new_atom("format");
  ATOM_level        = PL_new_atom("level");
  ATOM_close_parent = PL_new_atom("close_parent");
  ATOM_gzip	    = PL_new_atom("gzip");
  ATOM_deflate	    = PL_new_atom("define");

  PL_register_foreign("zopen",  3, pl_zopen,  0);
#ifdef O_DEBUG
  PL_register_foreign("zdebug", 1, zdebug, 0);
#endif
}
