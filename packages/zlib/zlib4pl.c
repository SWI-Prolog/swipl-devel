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
#include <zlib.h>

static functor_t FUNCTOR_error2;	/* error(Formal, Context) */
static functor_t FUNCTOR_type_error2;	/* type_error(Term, Expected) */
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


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef struct z_context
{ IOSTREAM	   *stream;		/* modified stream */
  void	           *wrapped_handle;	/* saved handle of stream */
  IOFUNCTIONS      *wrapped_functions;	/* saved IO functions */
  int		    header_done;	/* have we seen the header? */
  z_stream	    zstate;		/* Zlib state */
  Bytef		    buffer[BUFSIZE];	/* Raw data buffer */
} z_context;


static z_context*
alloc_zcontext(IOSTREAM *s)
{ z_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->stream            = s;
  ctx->wrapped_handle    = s->handle;
  ctx->wrapped_functions = s->functions;

  return ctx;
}


		 /*******************************
		 *	     GZIP HEADER	*
		 *******************************/

static int gz_magic[2] = {0x1f, 0x8b}; /* gzip magic header */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define HEAD_CRC     0x02 /* bit 1 set: header CRC present */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define RESERVED     0xE0 /* bits 5..7: reserved */

static Bytef *
gz_skip_header(z_context *ctx, Bytef *in, int avail)
{ int method; /* method byte */
  int flags;  /* flags byte */
  uInt len;

  if ( avail < 10 )			/* 2-byte magic, method, flags, */
    return NULL;			/* time, xflags and OS code */

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
    { avail = 0;
    }
  }
  if ((flags & ORIG_NAME) != 0)
  { /* skip the original file name */
    while ( *in && avail > 0 )
      in++, avail--;
  }
  if ((flags & COMMENT) != 0)
  {   /* skip the .gz file comment */
    while ( *in && avail > 0 )
      in++, avail--;
  }
  if ((flags & HEAD_CRC) != 0)
  {  /* skip the header crc */
    in += 2;
    avail -= 2;
  }

  if ( avail <= 0 )
    return NULL;

  return in;
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

  if ( !ctx->header_done )
  { Bytef *p;

    DEBUG(1, Sdprintf("Trying gzip header\n"));
    if ( (p = gz_skip_header(ctx, ctx->zstate.next_in, ctx->zstate.avail_in)) )
    { int m = p-ctx->zstate.next_in;

      DEBUG(1, Sdprintf("Skipped gzip header (%d bytes)\n", m));
      ctx->zstate.next_in = p;
      ctx->zstate.avail_in -= m;
      
      ctx->header_done = TRUE;
    }
  }

  ctx->zstate.next_out  = (Bytef*)buf;
  ctx->zstate.avail_out = size;
  
  switch((rc=inflate(&ctx->zstate, Z_NO_FLUSH)))
  { case Z_OK:
    case Z_STREAM_END:
    { int n = size - ctx->zstate.avail_out;
      DEBUG(1, Sdprintf("%s: %d bytes\n", 
			rc == Z_OK ? "Z_OK" : "Z_STREAM_END", n));
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
{ //z_context *ctx = handle;

#if 0
  if ( ctx->zstate.in_avail == 0 )
  { int n;

    n = (*ctx->wrapped_functions->read)(ctx->handle, ctx->buffer, BUFSIZE);
    if ( n < 0 )
    { return -1;
    } else if ( n == 0 )		/* end-of-file */
    {
    } else
    { ctx->state.next_in = ctx->buffer;
      ctx->state.avail_in = n;
    }
  }

  ctx->state.next_out = buf;
  ctx->state.avail_out = size;
  
  switch(deflate(&ctx->state, Z_NO_FLUSH))
  {
  }
#endif

  return -1;
}


static int
zclose(void *handle)
{ z_context *ctx = handle;
  int rc;

  if ( (ctx->stream->flags & SIO_INPUT) )
    rc = inflateEnd(&ctx->zstate);
  else
    rc = deflateEnd(&ctx->zstate);

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
  NULL,					/* zcontrol */
  NULL,					/* seek64 */
};


		 /*******************************
		 *	 PROLOG CONNECTION	*
		 *******************************/


static int
enable_compressed_input(IOSTREAM *s, term_t opt)
{ z_context *ctx = alloc_zcontext(s);
  term_t tail = PL_copy_term_ref(opt);
  term_t head = PL_new_term_ref();

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
    DEBUG(1, Sdprintf("--> compressed: copied %d unprocessed bytes\n", n));

    s->bufp = s->limitp = s->buffer;
  }

  switch(inflateInit2(&ctx->zstate, -MAX_WBITS))
  { case Z_OK:
      DEBUG(1, Sdprintf("inflateInit(): Z_OK\n"));
      return TRUE;
    case Z_MEM_ERROR:			/* no memory */
    case Z_VERSION_ERROR:		/* bad library version */
      return PL_warning("ERROR: TBD");
    default:
      assert(0);
      return FALSE;
  }
}


static int
enable_compressed_output(IOSTREAM *s, term_t opt)
{ z_context *ctx = alloc_zcontext(s);

  s->functions = &zfunctions;
  s->handle    = ctx;

  switch(deflateInit(&ctx->zstate, Z_DEFAULT_COMPRESSION))
  { case Z_OK:
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
zset(term_t stream, term_t opt)
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
{ FUNCTOR_error2		 = MKFUNCTOR("error", 2);
  FUNCTOR_type_error2		 = MKFUNCTOR("type_error", 2);

  PL_register_foreign("zset",   2, zset,   0);
#ifdef O_DEBUG
  PL_register_foreign("zdebug", 1, zdebug, 0);
#endif
}
