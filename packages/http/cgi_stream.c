/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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


		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

static atom_t ATOM_header;		/* header */
static atom_t ATOM_data;		/* data */
static predicate_t PREDICATE_call3;	/* Goal, Event, Data */


		 /*******************************
		 *	      CONTEXT		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef enum
{ CGI_HDR  = 0,
  CGI_DATA
} cgi_state;

#define CGI_MAGIC 0xa85ce042

typedef struct cgi_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *cgi_stream;		/* Stream I'm handle of */
  IOENC		    parent_encoding;	/* Saved encoding of parent */
  module_t	    module;		/* Calling module */
  record_t	    hook;		/* Hook called on action */
  cgi_state	    state;		/* Current state */
  size_t	    data_offset;	/* Start of real data */
  char		   *data;		/* Buffered data */
  size_t	    datasize;		/* #bytes buffered */
  size_t	    dataallocated;	/* #bytes allocated */
  int		    magic;		/* CGI_MAGIC */
} cgi_context;


static cgi_context*
alloc_cgi_context(IOSTREAM *s)
{ cgi_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->magic  = CGI_MAGIC;
  ctx->stream = s;

  return ctx;
}


static void
free_cgi_context(cgi_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);
  else
    PL_release_stream(ctx->stream);

  if ( ctx->data )
    free(ctx->data);
  if ( ctx->hook )
    PL_erase(ctx->hook);

  ctx->magic = 0;
  PL_free(ctx);
}


static int 
grow_data_buffer(cgi_context *ctx, size_t size)
{ size_t newsize;

  if ( ctx->dataallocated == 0 )
    newsize = SIO_BUFSIZE;
  else
    newsize = ctx->dataallocated;

  while(newsize < size)
    newsize *= 2;
  if ( ctx->data )
  { void *p;

    if ( !(p=realloc(ctx->data, newsize)) )
      return -1;
    ctx->data = p;
    ctx->dataallocated = newsize;
  } else
  { if ( !(ctx->data = malloc(newsize)) )
      return -1;

    ctx->dataallocated = newsize;
  }

  return 0;
}


		 /*******************************
		 *	      HOOKS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call hook on the data we collected sofar.   The  hook is called with the
following additional arguments:

    * Event-type (header, data)
    * An input stream pointing to the collected data
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
call_hook(cgi_context *ctx, atom_t event)
{ IOSTREAM *in;
  fid_t fid = PL_open_foreign_frame();
  term_t av = PL_new_term_refs(3);
  qid_t qid;
  int rc;

  if ( !(in=Sopenmem(&ctx->data, &ctx->datasize, "r")) )
    return FALSE;
  
  PL_recorded(ctx->hook, av+0);
  PL_put_atom(av+1, event);
  PL_unify_stream(av+2, in);
  qid = PL_open_query(ctx->module, PL_Q_CATCH_EXCEPTION, PREDICATE_call3, av);
  rc = PL_next_solution(qid);
  Sclose(in);

  if ( !rc )
  { term_t ex;
    
    if ( (ex = PL_exception(qid)) )
    { Sdprintf("Got exception from hook\n");
    } else
    { Sdprintf("Hook failed\n");
    }    

    PL_cut_query(qid);
    PL_close_foreign_frame(fid);
    return FALSE;			/* TBD: Error */
  }
  PL_close_query(qid);
  PL_discard_foreign_frame(fid);

  return TRUE;
}


static size_t
find_data(cgi_context *ctx, size_t start)
{ const char *s = &ctx->data[start];
  const char *e = &s[ctx->datasize];

  for(; s<e; s++)
  { if ( s[0] == '\r' && s[1] == '\n' &&
	 s[2] == '\r' && s[3] == '\n' )
      return &s[4] - ctx->data;
    if ( s[0] == '\n' && s[1] == '\n' )
      return &s[2] - ctx->data;
  }

  return (size_t)-1;
}



		 /*******************************
		 *	   IO FUNCTIONS		*
		 *******************************/

static ssize_t
cgi_write(void *handle, char *buf, size_t size)
{ cgi_context *ctx = handle;
  size_t osize = ctx->datasize;
  size_t dstart;
  
  if ( osize+size > ctx->dataallocated )
  { if ( grow_data_buffer(ctx, size) < 0 )
      return -1;			/* no memory */
  }
  memcpy(&ctx->data[osize], buf, size);
  ctx->datasize = osize+size;
  osize = (osize > 4 ? osize-4 : 0);	/* 4 is max size of the separator */

  if ( (ctx->state = CGI_HDR) &&
       (dstart=find_data(ctx, osize)) != ((size_t)-1) )
  { ctx->data_offset = dstart;
    ctx->state = CGI_DATA;		/* TBD: fix encoding (UTF-8 --> octet)!!! */
    if ( !call_hook(ctx, ATOM_header) )
      return -1;			/* TBD: pass error kindly */
  }

  return size;
}


static int
cgi_control(void *handle, int op, void *data)
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
cgi_close(void *handle)
{ cgi_context *ctx = handle;
  int rc = 0;

  if ( !call_hook(ctx, ATOM_data) )	/* what if we had no header sofar? */
    rc = -1;				/* TBD: pass error kindly */

  ctx->stream->encoding = ctx->parent_encoding;
  free_cgi_context(ctx);

  return rc;
}


static IOFUNCTIONS cgi_functions =
{ NULL,					/* read */
  cgi_write,
  NULL,					/* seek */
  cgi_close,
  cgi_control,				/* control */
  NULL,					/* seek64 */
};


		 /*******************************
		 *	       OPEN		*
		 *******************************/

#define CGI_COPY_FLAGS (SIO_OUTPUT| \
			SIO_TEXT| \
			SIO_REPXML|SIO_REPPL|\
			SIO_RECORDPOS)

static foreign_t
pl_cgi_open(term_t org, term_t new, term_t closure, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  cgi_context *ctx;
  IOSTREAM *s, *s2;
  term_t hook = PL_new_term_ref();
  module_t module = NULL;

  PL_strip_module(closure, &module, hook);
  if ( !PL_is_callable(hook) )
    return type_error(closure, "callable");

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    PL_get_arg(1, head, arg);
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */
  if ( !(s->flags&SIO_OUTPUT) )		/* only allow output stream */
  { PL_release_stream(s);
    return permission_error("stream", "write", org);
  }

  ctx = alloc_cgi_context(s);
  ctx->hook = PL_record(hook);
  ctx->module = module;
  if ( !(s2 = Snew(ctx,
		   (s->flags&CGI_COPY_FLAGS)|SIO_FBUF,
		   &cgi_functions)) )
  { free_cgi_context(ctx);			/* no memory */

    return FALSE;
  }

  s2->encoding = s->encoding;
  ctx->parent_encoding = s->encoding;
  s->encoding = ENC_OCTET;
  ctx->cgi_stream = s2;
  if ( PL_unify_stream(new, s2) )
  { Sset_filter(s, s2);
    PL_release_stream(s);

    return TRUE;
  } else
  { return instantiation_error();
  }    
}


static void
install_cgi_stream()
{ ATOM_header     = PL_new_atom("header");
  ATOM_data       = PL_new_atom("data");
  PREDICATE_call3 = PL_predicate("call", 3, "system");

  PL_register_foreign("cgi_open", 4, pl_cgi_open, PL_FA_TRANSPARENT);
}
