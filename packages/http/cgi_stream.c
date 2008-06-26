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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The task of cgi_stream.c is to interface between the actual wrapper code
that implements an HTTP location and the   socket sending a reply to the
client.  In particular, we want to deal with:

    * Separating the header from the body of the reply
    * Chunked or traditional transfer encoding
    * Connection management (Keep-alife)
    * Thread management

The original HTTP infrastructure has an `accept thread' that accepts new
connections. The connection is handed to a   thread  that reads the HTTP
header and calls a handler with the  output redirected to a memory file,
processing the reply-header and reply-data   after the handler finished.
This is a clean and modular design,   but it cannot deal with especially
chunked encoding and  thread  management.   This  module  remedies these
issues.

To do this, the  stream  provides   a  three  call-backs. Initially, the
stream is in line-buffering mode (SIO_LBUF),   waiting for the header to
become complete. At that moment it calls   the  hook, passing event type
'header' and the stream. This processes the   head and combines the head
with the request, deciding on:

    * The final header
    * The transfer encoding (chunked/none)
    * The content encoding (octet/utf8)
    * The connection (Keep-Alife/close)

Now, the stream is placed in  full   buffering  mode  (SIO_FBUF). If the
transfer encoding is 'chunked'  it  immediately   calls  the  hook using
'send_header' to emit the current header.   Output continues. In chunked
mode sending the chunks, otherwisse collecting   the  data. On close, it
writes an empty block (chunked mode)  or   (normal  mode) calls the hook
'send_header' which now has access to   the  content-length, followed by
the data.

Note that the work-flow is kept with the stream. This allows passing the
cgi stream from thread to thread while keeping track of the work-flow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TODO

	* Error handling (many places)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

static atom_t ATOM_header;		/* header */
static atom_t ATOM_header_codes;	/* header_codes */
static atom_t ATOM_send_header;		/* send_header */
static atom_t ATOM_data;		/* data */
static atom_t ATOM_request;		/* request */
static atom_t ATOM_client;		/* client */
static atom_t ATOM_thread;		/* thread */
static atom_t ATOM_chunked;		/* chunked */
static atom_t ATOM_none;		/* none */
static atom_t ATOM_transfer_encoding;	/* transfer_encoding */
static atom_t ATOM_connection;		/* connection */
static atom_t ATOM_keep_alife;		/* keep_alife */
static atom_t ATOM_close;		/* close */
static atom_t ATOM_content_length;	/* content_length */
static predicate_t PREDICATE_call3;	/* Goal, Event, Handle */


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
					/* Prolog attributes */
  int		    thread;		/* Associated thread */
  module_t	    module;		/* Calling module */
  record_t	    hook;		/* Hook called on action */
  record_t	    request;		/* Associated request term */
  record_t	    header;		/* Associated reply header term */
  atom_t	    transfer_encoding;	/* Current transfer encoding */
  atom_t	    connection;		/* Keep alife? */
					/* state */
  cgi_state	    state;		/* Current state */
					/* data buffering */
  size_t	    data_offset;	/* Start of real data */
  char		   *data;		/* Buffered data */
  size_t	    datasize;		/* #bytes buffered */
  size_t	    dataallocated;	/* #bytes allocated */
  unsigned int	    magic;		/* CGI_MAGIC */
} cgi_context;


static int start_chunked_encoding(cgi_context *ctx);
static ssize_t cgi_chunked_write(cgi_context *ctx, char *buf, size_t size);


		 /*******************************
		 *	     ALLOC/FREE		*
		 *******************************/

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

  if ( ctx->data )       free(ctx->data);
  if ( ctx->hook )       PL_erase(ctx->hook);
  if ( ctx->request )    PL_erase(ctx->request);
  if ( ctx->header )     PL_erase(ctx->header);
  if ( ctx->connection ) PL_unregister_atom(ctx->connection);

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
		 *	     PROPERTIES		*
		 *******************************/

static IOFUNCTIONS cgi_functions;

static int
get_cgi_stream(term_t t, IOSTREAM **sp, cgi_context **ctx)
{ IOSTREAM *s;
  
  if ( !PL_get_stream_handle(t, &s) || s->functions != &cgi_functions )
    return type_error(t, "cgi_stream");
  
  *sp = s;
  *ctx = s->handle;

  return TRUE;
}


static int
unify_record(term_t t, record_t r)
{ if ( r )
  { term_t t2 = PL_new_term_ref();
    PL_recorded(r, t2);
    return PL_unify(t, t2);
  }
  return FALSE;
}


static foreign_t
cgi_property(term_t cgi, term_t prop)
{ IOSTREAM *s;
  cgi_context *ctx;
  term_t arg = PL_new_term_ref();
  atom_t name;
  int arity;
  int rc = TRUE;

  if ( !get_cgi_stream(cgi, &s, &ctx) )
    return FALSE;

  if ( !PL_get_name_arity(prop, &name, &arity) || arity != 1 )
  { rc = type_error(prop, "cgi_property");
    goto out;
  }
  
  PL_get_arg(1, prop, arg);
  if ( name == ATOM_request )
  { if ( ctx->request )
      rc = unify_record(arg, ctx->request);
    else
      rc = PL_unify_nil(arg);
  } else if ( name == ATOM_header )
  { rc = unify_record(arg, ctx->header);
  } else if ( name == ATOM_client )
  { rc = PL_unify_stream(arg, ctx->stream);
  } else if ( name == ATOM_thread )
  { rc = PL_unify_thread_id(arg, ctx->thread);
  } else if ( name == ATOM_transfer_encoding )
  { rc = PL_unify_atom(arg, ctx->transfer_encoding);
  } else if ( name == ATOM_connection )
  { rc = PL_unify_atom(arg, ctx->connection ? ctx->connection : ATOM_close);
  } else if ( name == ATOM_content_length )
  { rc = PL_unify_int64(arg, ctx->datasize - ctx->data_offset);
  } else if ( name == ATOM_header_codes )
  { if ( ctx->data_offset > 0 )
      rc = PL_unify_chars(arg, PL_CODE_LIST, ctx->data_offset, ctx->data);
    else
      rc = existence_error(cgi, "header");
  } else
  { return existence_error(prop, "cgi_property");
  }

out:
  PL_release_stream(s);
  return rc;
}


static int
set_term(record_t *r, term_t t)
{ if ( *r )
    PL_erase(*r);
  *r = PL_record(t);

  return TRUE;
}


static int
set_atom(atom_t *a, term_t t)
{ atom_t new;

  if ( !PL_get_atom(t, &new) )
    return type_error(t, "atom");

  if ( *a != new )
  { if ( *a )
      PL_unregister_atom(*a);
    *a = new;
    PL_register_atom(new);
  }

  return TRUE;
}


static foreign_t
cgi_set(term_t cgi, term_t prop)
{ IOSTREAM *s;
  cgi_context *ctx;
  term_t arg = PL_new_term_ref();
  atom_t name;
  int arity;
  int rc = TRUE;

  if ( !get_cgi_stream(cgi, &s, &ctx) )
    return FALSE;

  if ( !PL_get_name_arity(prop, &name, &arity) || arity != 1 )
  { rc = type_error(prop, "cgi_property");
    goto out;
  }
  
  PL_get_arg(1, prop, arg);
  if ( name == ATOM_request )
  { rc = set_term(&ctx->request, arg);
  } else if ( name == ATOM_header )
  { rc = set_term(&ctx->header, arg);
  } else if ( name == ATOM_connection )
  { rc = set_atom(&ctx->connection, arg);
  } else if ( name == ATOM_transfer_encoding )
  { atom_t enc;

    if ( !PL_get_atom(arg, &enc) )
      return type_error(arg, "atom");

    if ( ctx->transfer_encoding != enc )
    { if ( enc == ATOM_chunked )
      { ctx->transfer_encoding = enc;
	rc = start_chunked_encoding(ctx);
      } else
      { rc = domain_error(arg, "transfer_encoding");
      }
    }
  } else
  { return existence_error(prop, "cgi_property");
  }
  
out:
  PL_release_stream(s);
  return rc;
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
{ fid_t fid = PL_open_foreign_frame();
  term_t av = PL_new_term_refs(3);
  qid_t qid;
  int rc;

  PL_recorded(ctx->hook, av+0);
  PL_put_atom(av+1, event);
  PL_unify_stream(av+2, ctx->cgi_stream);
  qid = PL_open_query(ctx->module, PL_Q_CATCH_EXCEPTION, PREDICATE_call3, av);
  rc = PL_next_solution(qid);

  if ( !rc )
  { term_t ex;
    
    if ( (ex = PL_exception(qid)) )
    { Sset_exception(ctx->cgi_stream, ex);
      
    } else
    { char buf[256];
      Ssprintf(buf, "CGI Hook %s failed", PL_atom_chars(event));

      Sseterr(ctx->cgi_stream, SIO_WARN, buf);
    }    

    PL_cut_query(qid);
    PL_close_foreign_frame(fid);

    return FALSE;
  }
  PL_close_query(qid);
  PL_discard_foreign_frame(fid);

  return TRUE;
}


static int
start_chunked_encoding(cgi_context *ctx)
{ if ( call_hook(ctx, ATOM_send_header) )
  { if ( ctx->datasize > ctx->data_offset )
    { int rc = cgi_chunked_write(ctx,
				 &ctx->data[ctx->data_offset],
				 ctx->datasize - ctx->data_offset);
      if ( rc == -1 )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
} 


static size_t
find_data(cgi_context *ctx, size_t start)
{ const char *s = &ctx->data[start];
  const char *e = &ctx->data[ctx->datasize-2];

  for(; s<e; s++)
  { if ( s[0] == '\r' && s[1] == '\n' &&
	 s < e-2 &&
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

static ssize_t				/* encode */
cgi_chunked_write(cgi_context *ctx, char *buf, size_t size)
{ if ( Sfprintf(ctx->stream, "%x\r\n", size) < 0 )
    return -1;
  if ( size > 0 && 
       Sfwrite(buf, sizeof(char), size, ctx->stream) != size )
    return -1;
  if ( Sfprintf(ctx->stream, "\r\n") < 0 )
    return -1;

  return size;
}


static ssize_t
cgi_write(void *handle, char *buf, size_t size)
{ cgi_context *ctx = handle;

  DEBUG(1, Sdprintf("cgi_write(%ld bytes)\n", (long)size));

  if ( ctx->transfer_encoding == ATOM_chunked )
  { return cgi_chunked_write(ctx, buf, size);
  } else
  { size_t osize = ctx->datasize;
    size_t dstart;
  
    if ( osize+size > ctx->dataallocated )
    { if ( grow_data_buffer(ctx, osize+size) < 0 )
	return -1;			/* no memory */
    }
    memcpy(&ctx->data[osize], buf, size);
    ctx->datasize = osize+size;
    osize = (osize > 4 ? osize-4 : 0);	/* 4 is max size of the separator */

    if ( ctx->state == CGI_HDR &&
	 (dstart=find_data(ctx, osize)) != ((size_t)-1) )
    { assert(dstart <= ctx->datasize);
      ctx->data_offset = dstart;
      ctx->state = CGI_DATA;
      if ( !call_hook(ctx, ATOM_header) )
	return -1;			/* TBD: pass error kindly */
      ctx->cgi_stream->flags &= ~(SIO_FBUF|SIO_LBUF|SIO_NBUF);
      ctx->cgi_stream->flags |= SIO_FBUF;
    }
    
    return size;
  }
}


static int
cgi_control(void *handle, int op, void *data)
{ cgi_context *ctx = handle;

  assert(ctx->magic == CGI_MAGIC);

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

  DEBUG(1, Sdprintf("cgi_close()\n"));

  if ( ctx->transfer_encoding == ATOM_chunked )
  { if ( cgi_chunked_write(ctx, NULL, 0) < 0 )
    { rc = -1;
      goto out;
    }
  } else
  { size_t clen = ctx->datasize - ctx->data_offset;
    const char *dstart = &ctx->data[ctx->data_offset];

    if ( !call_hook(ctx, ATOM_send_header) )
    { rc = -1;
      goto out;
    }
    if ( Sfwrite(dstart, sizeof(char), clen, ctx->stream) != clen )
    { rc = -1;
      goto out;
    }
  }

  if ( !call_hook(ctx, ATOM_close) )	/* what if we had no header sofar? */
    rc = -1;				/* TBD: pass error kindly */

out:
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
  module_t module = NULL;
  term_t hook = PL_new_term_ref();
  record_t request = 0;

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
    if ( name == ATOM_request )
    { request = PL_record(arg);
    } else
      return existence_error(head, "cgi_open_option");
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
  ctx->thread = PL_thread_self();
  ctx->request = request;
  ctx->transfer_encoding = ATOM_none;
  if ( !(s2 = Snew(ctx,
		   (s->flags&CGI_COPY_FLAGS)|SIO_LBUF,
		   &cgi_functions)) )
  { free_cgi_context(ctx);			/* no memory */

    return FALSE;
  }

  s2->encoding = ENC_ASCII;		/* Header is ASCII only */
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
{ ATOM_header		 = PL_new_atom("header");
  ATOM_header_codes	 = PL_new_atom("header_codes");
  ATOM_send_header	 = PL_new_atom("send_header");
  ATOM_data		 = PL_new_atom("data");
  ATOM_request		 = PL_new_atom("request");
  ATOM_header		 = PL_new_atom("header");
  ATOM_client		 = PL_new_atom("client");
  ATOM_thread		 = PL_new_atom("thread");
  ATOM_chunked		 = PL_new_atom("chunked");
  ATOM_none		 = PL_new_atom("none");
  ATOM_transfer_encoding = PL_new_atom("transfer_encoding");
  ATOM_close             = PL_new_atom("close");
  ATOM_keep_alife        = PL_new_atom("keep_alife");
  ATOM_connection        = PL_new_atom("connection");
  ATOM_content_length    = PL_new_atom("content_length");

  PREDICATE_call3   = PL_predicate("call", 3, "system");

  PL_register_foreign("cgi_open",     4, pl_cgi_open, PL_FA_TRANSPARENT);
  PL_register_foreign("cgi_property", 2, cgi_property, 0);
  PL_register_foreign("cgi_set",      2, cgi_set, 0);
}
