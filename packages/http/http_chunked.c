/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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
#include <errno.h>

#define MAXHDR 1024			/* max size of chink header line */

static atom_t ATOM_close_parent;	/* close_parent(Bool) */
static atom_t ATOM_max_chunk_size;	/* max_chunk_size(Int) */


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef struct chunked_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *chunked_stream;	/* Stream I'm handle of */
  int		    close_parent;	/* close parent on close */
  IOENC		    parent_encoding;	/* Saved encoding of parent */
  size_t	    avail;		/* data available */
} chunked_context;


static chunked_context*
alloc_chunked_context(IOSTREAM *s)
{ chunked_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->stream       = s;
  ctx->close_parent = FALSE;

  return ctx;
}


static void
free_chunked_context(chunked_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);
  else
    PL_release_stream(ctx->stream);

  PL_free(ctx);
}


		 /*******************************
		 *	    CHUNKED I/O		*
		 *******************************/

static ssize_t				/* decode */
chunked_read(void *handle, char *buf, size_t size)
{ chunked_context *ctx = handle;

  for(;;)
  { if ( ctx->avail > 0 )			/* data waiting */
    { size_t  max_rd = ctx->avail < size ? ctx->avail : size;
      ssize_t rc;
  
      if ( (rc = Sfread(buf, sizeof(char), max_rd, ctx->stream)) > 0 )
      { ctx->avail -= rc;
  
	if ( ctx->avail == 0 )
	{ if ( Sgetc(ctx->stream) != '\r' ||
	       Sgetc(ctx->stream) != '\n' )
	  { Sseterr(ctx->chunked_stream, 0, "Chunk not followed by \\r\\n");
	    return -1;
	  }
	}

	return rc;
      } else if ( rc == 0 )
      { Sseterr(ctx->chunked_stream, 0, "Unexpected EOF in chunked data");
	return -1;
      } else
      { return -1;
      }    
    } else
    { char hdr[MAXHDR];
      char *s;
      
  
      if ( (s = Sfgets(hdr, sizeof(hdr), ctx->stream)) )
      { char *ehdr;
	long len;
  
	errno = 0;
	len = strtol(hdr, &ehdr, 16);
	if ( errno || len < 0 )
	{ Sseterr(ctx->chunked_stream, 0, "Bad chunk length");
	  return -1;
	}
	if ( len == 0 )
	{ do
	  { s = Sfgets(hdr, sizeof(hdr), ctx->stream);
	  } while ( s && strcmp(s, "\r\n") != 0 );
	  if ( s )
	    return 0;
	  Sseterr(ctx->chunked_stream, 0, "Bad end-of-stream");
	  return -1;
	}
	ctx->avail = len;
	/*continue*/
      }
    }
  }
}


static ssize_t				/* encode */
chunked_write(void *handle, char *buf, size_t size)
{ chunked_context *ctx = handle;

  if ( Sfprintf(ctx->stream, "%x\r\n", size) >= 0 &&
       Sfwrite(buf, sizeof(char), size, ctx->stream) == size &&
       Sfprintf(ctx->stream, "\r\n") >= 0 )
    return size;

  return -1;
}


static int
chunked_control(void *handle, int op, void *data)
{ chunked_context *ctx = handle;

  switch(op)
  { case SIO_FLUSHOUTPUT:
    case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
      if ( ctx->stream->functions->control )
	return (*ctx->stream->functions->control)(ctx->stream->handle, op, data);
      return -1;
  }
}


static int
chunked_close(void *handle)
{ chunked_context *ctx = handle;
  int rc;

  DEBUG(1, Sdprintf("chunked_close() ...\n"));

  if ( Sfprintf(ctx->stream, "0\r\n\r\n") >= 0 )
    rc = 0;
  else
    rc = -1;

  ctx->stream->encoding = ctx->parent_encoding;

  if ( ctx->close_parent )
  { IOSTREAM *parent = ctx->stream;
    int rc2;

    free_chunked_context(ctx);
    rc2 = Sclose(parent);
    return (rc2 == 0 && rc == 0) ? 0 : -1;
  } else
  { free_chunked_context(ctx);
    return rc;
  } 
}


static IOFUNCTIONS chunked_functions =
{ chunked_read,
  chunked_write,
  NULL,					/* seek */
  chunked_close,
  chunked_control,			/* zcontrol */
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
pl_http_chunked_open(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  chunked_context *ctx;
  IOSTREAM *s, *s2;
  int close_parent = FALSE;
  int max_chunk_size = 0;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    PL_get_arg(1, head, arg);

    if ( name == ATOM_max_chunk_size )
    { if ( !get_int_ex(arg, &max_chunk_size) )
	return FALSE;
      if ( max_chunk_size <= 0 )
	return domain_error(arg, "positive_integer");
    } else if ( name == ATOM_close_parent )
    { if ( !get_bool_ex(arg, &close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */
  ctx = alloc_chunked_context(s);
  ctx->close_parent = close_parent;

  if ( !(s2 = Snew(ctx,
		   (s->flags&COPY_FLAGS)|SIO_FBUF,
		   &chunked_functions))	)
  { free_chunked_context(ctx);			/* no memory */

    return FALSE;
  }

  if ( max_chunk_size > 0 )
  { char *buf = PL_malloc(max_chunk_size);
    Ssetbuffer(s2, buf, max_chunk_size);
  }
  
  s2->encoding = s->encoding;
  ctx->parent_encoding = s->encoding;
  s->encoding = ENC_OCTET;
  ctx->chunked_stream = s2;
  if ( PL_unify_stream(new, s2) )
  { Sset_filter(s, s2);
    PL_release_stream(s);

    return TRUE;
  } else
  { return instantiation_error();
  }    
}


		 /*******************************
		 *	       INSTALL		*
		 *******************************/

#define MKFUNCTOR(name, arity) PL_new_functor(PL_new_atom(name), arity)

static void
install_http_chunked()
{ ATOM_close_parent   = PL_new_atom("close_parent");
  ATOM_max_chunk_size = PL_new_atom("max_chunk_size");

  PL_register_foreign("http_chunked_open",  3, pl_http_chunked_open,  0);
}
