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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <errno.h>

static atom_t ATOM_size;		/* size(Int) */

		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef struct range_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *range_stream;	/* Stream I'm handle of */
  IOENC		    parent_encoding;	/* Saved encoding of parent */
  size_t	    read;		/* data already read */
  size_t	    size;		/* #bytes of data available */
} range_context;


static range_context*
alloc_range_context(IOSTREAM *s)
{ range_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->stream       = s;

  return ctx;
}


static void
free_range_context(range_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);
  else
    PL_release_stream(ctx->stream);

  PL_free(ctx);
}


		 /*******************************
		 *	RANGE LIMITED INPUT	*
		 *******************************/

static ssize_t				/* range-limited read */
range_read(void *handle, char *buf, size_t size)
{ range_context *ctx = handle;
  size_t max_rd;
  ssize_t rd;

  if ( ctx->read == ctx->size )
    return 0;

  if ( ctx->size - ctx->read < size )
    max_rd = ctx->size - ctx->read;
  else
    max_rd = size;

  if ( (rd = Sfread(buf, sizeof(char), max_rd, ctx->stream)) >= 0 )
  { ctx->read += rd;

    return rd;
  }

  return rd;
}
 

static ssize_t				/* no writing! */
range_write(void *handle, char *buf, size_t size)
{ return -1;
}


static int
range_control(void *handle, int op, void *data)
{ range_context *ctx = handle;

  switch(op)
  { case SIO_FLUSHOUTPUT:
    case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    case SIO_GETSIZE:
    { size_t *rval = data;
      *rval = ctx->size;
      return 0;
    }
    default:
      if ( ctx->stream->functions->control )
	return (*ctx->stream->functions->control)(ctx->stream->handle, op, data);
      return -1;
  }
}


static int
range_close(void *handle)
{ range_context *ctx = handle;

  ctx->stream->encoding = ctx->parent_encoding;
  free_range_context(ctx);

  return 0;
}


static IOFUNCTIONS range_functions =
{ range_read,
  range_write,
  NULL,					/* seek */
  range_close,
  range_control,			/* zcontrol */
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
pl_stream_range_open(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  range_context *ctx;
  IOSTREAM *s, *s2;
  int close_parent = FALSE;
  int size = 0;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    PL_get_arg(1, head, arg);

    if ( name == ATOM_size )
    { if ( !get_int_ex(arg, &size) )
	return FALSE;
      if ( size <= 0 )
	return domain_error(arg, "nonneg");
    }
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */
  ctx = alloc_range_context(s);
  ctx->size = size;

  if ( !(s2 = Snew(ctx,
		   (s->flags&COPY_FLAGS)|SIO_FBUF,
		   &range_functions))	)
  { free_range_context(ctx);			/* no memory */

    return FALSE;
  }

  s2->encoding = s->encoding;
  ctx->parent_encoding = s->encoding;
  s->encoding = ENC_OCTET;
  ctx->range_stream = s2;
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

static void
install_stream_range()
{  ATOM_size = PL_new_atom("size");

  PL_register_foreign("stream_range_open",  3, pl_stream_range_open,  0);
}
