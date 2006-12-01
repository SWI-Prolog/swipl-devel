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
#include <zutil.h>

static functor_t FUNCTOR_error2;	/* error(Formal, Context) */
static functor_t FUNCTOR_type_error2;	/* type_error(Term, Expected) */
static functor_t FUNCTOR_domain_error2;	/* domain_error(Term, Expected) */
static functor_t FUNCTOR_format1;	/* format(Format) */
static functor_t FUNCTOR_level1;	/* level(Int) */
static atom_t	 ATOM_gzip;
static atom_t	 ATOM_deflate;
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


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef enum
{ F_UNKNOWN = 0,
  F_GZIP,				/* gzip output */
  F_ZLIB				/* zlib data */
} zformat;

typedef struct z_context
{ IOSTREAM	   *stream;		/* modified stream */
  void	           *wrapped_handle;	/* saved handle of stream */
  IOFUNCTIONS      *wrapped_functions;	/* saved IO functions */
  zformat	    format;		/* current format */
  uLong		    crc;		/* CRC check */
  z_stream	    zstate;		/* Zlib state */
  Bytef		    buffer[BUFSIZE];	/* Raw data buffer */
} z_context;


static IOSTREAM *unwrap_stream(z_context *ctx);


static z_context*
alloc_zcontext(IOSTREAM *s)
{ z_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->stream            = s;
  ctx->wrapped_handle    = s->handle;
  ctx->wrapped_functions = s->functions;

  return ctx;
}


static int
write_wrapped(z_context *ctx, char *buf, int size)
{ char *from = buf;
  int left = size;

  while(left>0)
  { int n;

    if ( (n=(*ctx->wrapped_functions->write)(ctx->wrapped_handle, from, left)) < 0 )
      return -1;
    from += n;
    left -= n;
  }

  return size;
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
  uInt len;

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


static Bytef *
add_ulong_lsb(Bytef *out, unsigned long x)
{ *out++ = (x)    &0xff;
  *out++ = (x>>8) &0xff;
  *out++ = (x>>16)&0xff;
  *out++ = (x>>24)&0xff;

  return out;
}


static int
write_gzip_header(z_context *ctx)
{ Bytef *out = ctx->buffer;
  time_t stamp = time(NULL);

  *out++ = gz_magic[0];
  *out++ = gz_magic[1];
  *out++ = Z_DEFLATED;			/* method */
  *out++ = 0;				/* flags */
  out = add_ulong_lsb(out, stamp);
  *out++ = 0;				/* xflags */
  *out++ = OS_CODE;

  Sfwrite(ctx->buffer, 1, out - ctx->buffer, ctx->stream);
  if ( Sflush(ctx->stream) < 0 )
    return FALSE;			/* TBD: error? */
  
  return TRUE;
}


static int
write_gzip_footer(z_context *ctx)
{ Bytef *out = ctx->buffer;

  out = add_ulong_lsb(out, ctx->crc);			/* CRC32 */
  out = add_ulong_lsb(out, ctx->zstate.total_in);	/* Total length */

  if ( write_wrapped(ctx, (char*)ctx->buffer, out - ctx->buffer) < 0 )
    return -1;

  return 0;
}


static uLong
read_lsb_ulong(IOSTREAM *s)
{ uLong v;

  v = ((unsigned)Sgetc(s) |
       ((unsigned)Sgetc(s) << 8) |
       ((unsigned)Sgetc(s) << 16) |
       ((unsigned)Sgetc(s) << 24));

  return v;		/* we do not care about errors; they will mismatch anyway */
}


static int
finish_gzip_input(z_context *ctx)
{ IOSTREAM *s = ctx->stream;
  uLong crc  = ctx->crc;
  uLong size = ctx->zstate.total_out;
  uLong got;

  unwrap_stream(ctx);
  if ( (got=read_lsb_ulong(s)) != crc )
  { char msg[256];

    Ssprintf(msg, "CRC error (%08lx != %08lx)", got, crc);
    Sseterr(s, SIO_FERR, msg);
    return -1;
  }
  if ( (got=read_lsb_ulong(s)) != size )
  { char msg[256];

    Ssprintf(msg, "Size mismatch (%ld != %ld)", got, size);
    Sseterr(s, SIO_FERR, msg);
    return -1;
  }

  return 0;
}



		 /*******************************
		 *	       GZ I/O		*
		 *******************************/

static int				/* inflate */
zread(void *handle, char *buf, int size)
{ z_context *ctx = handle;
  int flush = Z_SYNC_FLUSH;
  int rc;

  if ( ctx->zstate.avail_in == 0 )
  { int n;

    n = (*ctx->wrapped_functions->read)(ctx->wrapped_handle, 
					(char*)ctx->buffer, BUFSIZE);
    DEBUG(1, Sdprintf("Read %d bytes from %p\n", n, ctx->wrapped_handle));
    if ( n < 0 )
    { return -1;
    } else if ( n == 0 )		/* end-of-file */
    { flush = Z_FINISH;
    } else
    { ctx->zstate.next_in  = ctx->buffer;
      ctx->zstate.avail_in = n;
    }
  } else
  { DEBUG(1, Sdprintf("Processing %d bytes\n", ctx->zstate.avail_in));
  }

  ctx->zstate.next_out  = (Bytef*)buf;
  ctx->zstate.avail_out = size;
  
  if ( ctx->format == F_UNKNOWN )
  { Bytef *p;

    DEBUG(1, Sdprintf("Trying gzip header\n"));
    while( (p = gz_skip_header(ctx, ctx->zstate.next_in,
			       ctx->zstate.avail_in)) == HDR_SHORT )
    {					/* TBD: read more */
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
	case Z_MEM_ERROR:			/* no memory */
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
  }

  switch((rc=inflate(&ctx->zstate, Z_NO_FLUSH)))
  { case Z_OK:
    case Z_STREAM_END:
    { int n = size - ctx->zstate.avail_out;
      
      if ( ctx->format == F_GZIP  && n > 0 )
	ctx->crc = crc32(ctx->crc, (Bytef*)buf, n);

      if ( rc == Z_STREAM_END )
      { DEBUG(1, Sdprintf("Z_STREAM_END: %d bytes\n", n));

	if ( ctx->format == F_GZIP )
	  return finish_gzip_input(ctx);
      } else
      { DEBUG(1, Sdprintf("Z_OK: %d bytes\n"));
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
	Sdprintf("ERROR: %s\n", ctx->zstate.msg);
      return -1;
  }
}


static int				/* deflate */
zwrite(void *handle, char *buf, int size)
{ z_context *ctx = handle;
  int flush = (size == 0 ? Z_FINISH : Z_NO_FLUSH);

  ctx->zstate.next_in = (Bytef*)buf;
  ctx->zstate.avail_in = size;
  if ( ctx->format == F_GZIP && size > 0 )
    ctx->crc = crc32(ctx->crc, ctx->zstate.next_in, ctx->zstate.avail_in);

  do
  { int rc;

    DEBUG(1, Sdprintf("Compressing %d bytes\n", ctx->zstate.avail_in));
    ctx->zstate.next_out  = ctx->buffer;
    ctx->zstate.avail_out = sizeof(ctx->buffer);
    switch( (rc = deflate(&ctx->zstate, flush)) )
    { case Z_OK:
      case Z_STREAM_END:
      { int n = sizeof(ctx->buffer) - ctx->zstate.avail_out;

	DEBUG(1, Sdprintf("Compressed (%s) to %d bytes; left %d\n",
			  rc == Z_OK ? "Z_OK" : "Z_STREAM_END",
			  n, ctx->zstate.avail_in));

	if ( write_wrapped(ctx, (char*)ctx->buffer, n) < 0 )
	  return -1;

	break;
      }
      case Z_STREAM_ERROR:
      case Z_BUF_ERROR:
      default:
	Sdprintf("zwrite(): %s\n", ctx->zstate.msg);
        return -1;
    }
  } while( ctx->zstate.avail_in > 0 );

  return size;
}


static int
zcontrol(void *handle, int op, void *data)
{ z_context *ctx = handle;

  switch(op)
  { case SIO_FLUSH:
      return zwrite(handle, NULL, 0);
    case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
      if ( ctx->wrapped_functions->control )
	return (*ctx->wrapped_functions->control)(ctx->wrapped_handle, op, data);
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
  { rc = zwrite(handle, NULL, 0);	/* flush */
    if ( rc == 0 && ctx->format == F_GZIP )
      rc = write_gzip_footer(ctx);
    if ( rc == 0 )
      rc = deflateEnd(&ctx->zstate);
    else
      deflateEnd(&ctx->zstate);
  }

  switch(rc)
  { case Z_OK:
      return (*ctx->wrapped_functions->close)(ctx->wrapped_handle);
    case Z_STREAM_ERROR:		/* inconsistent state */
    case Z_DATA_ERROR:			/* premature end */
    default:
      (void)(*ctx->wrapped_functions->close)(ctx->wrapped_handle);
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


static IOSTREAM *
unwrap_stream(z_context *ctx)
{ IOSTREAM *s = ctx->stream;

  s->handle    = ctx->wrapped_handle;
  s->functions = ctx->wrapped_functions;

  if ( (s->flags & SIO_INPUT) )
  { if ( ctx->zstate.avail_in > 0 )
    { DEBUG(1, Sdprintf("unwrap: move back %d unprocessed bytes\n", ctx->zstate.avail_in));

      memmove(s->buffer, ctx->zstate.next_in, ctx->zstate.avail_in);
      s->bufp = s->buffer;
      s->limitp = &s->bufp[ctx->zstate.avail_in];
    }

    inflateEnd(&ctx->zstate);
  } else
  { deflateEnd(&ctx->zstate);
  }
  PL_free(ctx);

  return s;
}


static int
enable_compressed_input(IOSTREAM *s, term_t opt)
{ z_context *ctx = alloc_zcontext(s);
  term_t tail = PL_copy_term_ref(opt);
  term_t head = PL_new_term_ref();

					/* option processing */
  while(PL_get_list(tail, head, tail))
  {
  }
  if ( !PL_get_nil(tail) )
  { PL_free(s);
    return type_error(tail, "list");
  }

  s->functions = &zfunctions;
  s->handle    = ctx;

  if ( s->bufp < s->limitp )		/* copy unprocessed data */
  { int n = s->limitp - s->bufp;

    memcpy(ctx->buffer, s->bufp, n);
    ctx->zstate.avail_in = n;
    ctx->zstate.next_in  = ctx->buffer;
    DEBUG(1, Sdprintf("--> uncompress: copied %d unprocessed bytes\n", n));

    s->bufp = s->limitp = s->buffer;
  }

  return TRUE;
}


static int
enable_compressed_output(IOSTREAM *s, term_t opt)
{ z_context *ctx = alloc_zcontext(s);
  term_t tail = PL_copy_term_ref(opt);
  term_t head = PL_new_term_ref();
  int level = Z_DEFAULT_COMPRESSION;
  int rc;

  while(PL_get_list(tail, head, tail))
  { if ( PL_is_functor(head, FUNCTOR_format1) )
    { term_t arg = PL_new_term_ref();
      atom_t a;
      
      PL_get_arg(1, head, arg);
      if ( !get_atom_ex(arg, &a) )
	goto error;
      if ( a == ATOM_gzip )
	ctx->format = F_GZIP;
 <     else if ( a == ATOM_deflate )
	ctx->format = F_DEFLATE;
      else
	return domain_error(arg, "compression_format");
    } else if ( PL_is_functor(head, FUNCTOR_level1) )
    { term_t arg = PL_new_term_ref();
      
      PL_get_arg(1, head, arg);
      if ( !get_int_ex(arg, &level) )
	goto error;
      if ( level < 0 || level > 9 )
      { domain_error(arg, "compression_level");
	goto error;
      }
    }
  }
  if ( !PL_get_nil(tail) )
  { type_error(tail, "list");
  error:
    PL_free(s);
    return FALSE;
  }

  if ( ctx->format == F_GZIP )
  { if ( !write_gzip_header(ctx) )
      return FALSE;
    rc = deflateInit2(&ctx->zstate, level, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);
    ctx->crc = crc32(0L, Z_NULL, 0);
  } else
  { rc = deflateInit(&ctx->zstate, level);
  }

  switch( rc )
  { case Z_OK:
      s->functions = &zfunctions;
      s->handle    = ctx;
      return TRUE;
    case Z_MEM_ERROR:			/* no memory */
    case Z_STREAM_ERROR:		/* bad compression level */
    case Z_VERSION_ERROR:		/* bad library version */
      return PL_warning("ERROR: TBD");
    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
zset_stream(term_t stream, term_t opt)
{ IOSTREAM *s;

  if ( !PL_get_stream_handle(stream, &s) )
    return type_error(stream, "stream");

  if ( (s->flags & SIO_INPUT) )
    return enable_compressed_input(s, opt);
  else
    return enable_compressed_output(s, opt);
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
  FUNCTOR_format1       = MKFUNCTOR("format", 1);
  FUNCTOR_level1        = MKFUNCTOR("level", 1);
  ATOM_gzip	        = PL_new_atom("gzip");
  ATOM_deflate	        = PL_new_atom("define");

  PL_register_foreign("zset_stream", 2, zset_stream, 0);
#ifdef O_DEBUG
  PL_register_foreign("zdebug", 1, zdebug, 0);
#endif
}
