/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
			      VU University Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is far too big.  It defines a layer around open(), etc.   to
get  opening  and  closing  of  files to the symbolic level required for
Prolog.  It also defines basic I/O  predicates,  stream  based  I/O  and
finally  a  bundle  of  operations  on  files,  such  as name expansion,
renaming, deleting, etc.  Most of this module is rather straightforward.

If time is there I will have a look at all this to  clean  it.   Notably
handling times must be cleaned, but that not only holds for this module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*#define O_DEBUG 1*/
/*#define O_DEBUG_MT 1*/

#ifdef __WINDOWS__
#define SWIPL_WINDOWS_NATIVE_ACCESS 1
#include <winsock2.h>
#include <windows.h>
#endif

#define NEEDS_SWINSOCK
#include "pl-incl.h"
#include "pl-arith.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include "pl-stream.h"
#include "../pl-fli.h"
#include "../pl-pro.h"
#include "../pl-write.h"
#include "../pl-proc.h"
#include "../pl-prims.h"
#include "../pl-trace.h"
#include <errno.h>

#if defined(HAVE_POLL_H) && defined(HAVE_POLL)
#include <poll.h>
#elif defined(HAVE_SYS_SELECT_H)
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#include <fcntl.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif

#undef LD				/* fetch LD once per function */
#define LD LOCAL_LD

#define STD_HANDLE_MASK 0x10

/* there are two types of stream property functions. In the usual case,
   they have an argument, but in a few cases they don't */
typedef int LDFUNCP (*property0_t)(DECL_LD IOSTREAM *s);
typedef int LDFUNCP (*property_t)(DECL_LD IOSTREAM *s, term_t prop);

static int	bad_encoding(const char *msg, atom_t name);
static int	noprotocol(void);
static PL_blob_t stream_blob;

const atom_t standardStreams[] =
{ ATOM_user_input,			/* 0 */
  ATOM_user_output,			/* 1 */
  ATOM_user_error,			/* 2 */
  ATOM_current_input,			/* 3 */
  ATOM_current_output,			/* 4 */
  ATOM_protocol,			/* 5 */
  NULL_ATOM
};


static int
standardStreamIndexFromName(atom_t name)
{ const atom_t *ap;

  for(ap=standardStreams; *ap; ap++)
  { if ( *ap == name )
      return (int)(ap - standardStreams);
  }

  return -1;
}


static int
standardStreamIndexFromStream(IOSTREAM *s)
{ GET_LD
  IOSTREAM **sp = LD->IO.streams;

  for(int i=0; i <= SNO_MAX; i++, sp++ )
  { if ( *sp == s )
      return i;
  }

  return -1;
}


		 /*******************************
		 *	   BOOKKEEPING		*
		 *******************************/

static void aliasStream(IOSTREAM *s, atom_t alias);
static void unaliasStream(IOSTREAM *s, atom_t name);

static TableWP streamAliases;		/* alias --> stream */
static TablePP streamContext;		/* stream --> extra data */

typedef struct _alias
{ struct _alias *next;
  atom_t name;
} alias;


#define IO_TELL	0x001			/* opened by tell/1 */
#define IO_SEE  0x002			/* opened by see/1 */

typedef struct
{ alias *alias_head;
  alias *alias_tail;
  atom_t filename;			/* associated filename */
  unsigned flags;
} stream_context;


static stream_context *
getStreamContext(IOSTREAM *s)
{ if ( !s->context )
  { stream_context *ctx = allocHeapOrHalt(sizeof(*ctx));

    DEBUG(1, Sdprintf("Created ctx=%p for stream %p\n", ctx, s));

    if ( s->erased )
      Sdprintf("WARNING: created stream context for erased stream\n");

    ctx->alias_head = ctx->alias_tail = NULL;
    ctx->filename = NULL_ATOM;
    ctx->flags = 0;
    if ( COMPARE_AND_SWAP_PTR(&s->context, NULL, ctx) )
    { GET_LD
      addNewHTablePP(streamContext, s, ctx);
    } else
      freeHeap(ctx, sizeof(*ctx));
  }

  return (stream_context*)s->context;
}

static stream_context *
getExistingStreamContext(IOSTREAM *s)
{ return (stream_context*)s->context;
}


/* MT: Must be called locked */

static void
aliasStream(IOSTREAM *s, atom_t name)
{ GET_LD
  stream_context *ctx;
  IOSTREAM *sp;
  alias *a;

					/* ensure name is free (error?) */
  if ( (sp = lookupHTableWP(streamAliases, name)) )
    unaliasStream(sp, name);

  ctx = getStreamContext(s);
  addNewHTableWP(streamAliases, name, s);
  PL_register_atom(name);
  Sacquire(s);

  a = allocHeapOrHalt(sizeof(*a));
  a->next = NULL;
  a->name = name;

  if ( ctx->alias_tail )
  { ctx->alias_tail->next = a;
    ctx->alias_tail = a;
  } else
  { ctx->alias_head = ctx->alias_tail = a;
  }
}

/* MT: Locked by freeStream()
*/

static void
unaliasStream(IOSTREAM *s, atom_t name)
{ GET_LD
  if ( name )
  { if ( lookupHTableWP(streamAliases, name) )
    { stream_context *ctx;

      deleteHTableWP(streamAliases, name);

      if ( (ctx=getExistingStreamContext(s)) )
      { alias **a;

	for(a = &ctx->alias_head; *a; a = &(*a)->next)
	{ if ( (*a)->name == name )
	  { alias *tmp = *a;

	    *a = tmp->next;
	    freeHeap(tmp, sizeof(*tmp));
	    if ( tmp == ctx->alias_tail )
	      ctx->alias_tail = NULL;

	    break;
	  }
	}
      }

      PL_unregister_atom(name);
      Srelease(s);
    }
  } else				/* delete them all */
  { stream_context *ctx;

    if ( (ctx=getExistingStreamContext(s)) )
    { alias *a, *n;

      for(a = ctx->alias_head; a; a=n)
      { n = a->next;

	if ( lookupHTableWP(streamAliases, a->name) )
	{ deleteHTableWP(streamAliases, a->name);
	  PL_unregister_atom(a->name);
	  Srelease(s);
	}

	freeHeap(a, sizeof(*a));
      }

      ctx->alias_head = ctx->alias_tail = NULL;
    }
  }
}


void
referenceStandardStreams(PL_local_data_t *ld)
{ WITH_LD(ld)
  { int i;
    const atom_t *np;

    for(i=0, np = standardStreams; *np; np++, i++ )
    { IOSTREAM *s;

      if ( (s=LD->IO.streams[i]) )
	Sacquire(s);
    }
  }
}


void
unreferenceStandardStreams(PL_local_data_t *ld)
{ WITH_LD(ld)
  { int i;
    const atom_t *np;

    for(i=0, np = standardStreams; *np; np++, i++ )
    { IOSTREAM *s;

      if ( (s=LD->IO.streams[i]) )
	Srelease(s);
    }
  }
}


#define setStandardStream(i, s) LDFUNC(setStandardStream, i, s)

static void
setStandardStream(DECL_LD int i, IOSTREAM *s)
{ IOSTREAM *old = LD->IO.streams[i];

  if ( old != s )
  { if ( s ) Sacquire(s);
    LD->IO.streams[i] = s;
    if ( old ) Srelease(old);
  }
}


void
copyStandardStreams(PL_local_data_t *ldnew, PL_local_data_t *ldold,
		    intptr_t flags)
{ ldnew->IO.stream_type_check = ldold->IO.stream_type_check;
  int upto = (flags&PL_THREAD_CUR_STREAMS) ? SNO_MAX : SNO_USER_ERROR;

  for(int i=0; i <= upto; i++)
  { IOSTREAM *s = ldold->IO.streams[i];
    if ( s )
      Sacquire(s);
    ldnew->IO.streams[i] = s;
  }

  if ( upto == SNO_USER_ERROR )
  { WITH_LD(ldnew)
    { Scurin  = Suser_input;
      Scurout = Suser_output;
      Sacquire(Scurin);
      Sacquire(Scurout);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
restoreStandardStream() is called  if one of the  standard streams was
lost, for  example because it was  rebound and the stream  was closed.
Note that we  keep reference counts on the stream,  so replacing it is
safe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define restoreStandardStream(i) LDFUNC(restoreStandardStream, i)

static IOSTREAM *
restoreStandardStream(DECL_LD int i)
{ IOSTREAM *s;

  switch(i)
  { case SNO_USER_INPUT:
    case SNO_CURRENT_INPUT:
      s = Sinput;
      break;
    case SNO_USER_ERROR:
      s = Serror;
      break;
    default:
      s = Soutput;
      break;
  }

  setStandardStream(i, s);
  return s;
}



static void
freeStream(IOSTREAM *s)
{ GET_LD
  stream_context *ctx;
  int i;
  IOSTREAM **sp;

  DEBUG(1, Sdprintf("freeStream(%p)\n", s));

  PL_LOCK(L_FILE);
  if ( streamAliases )
    unaliasStream(s, NULL_ATOM);

  ctx = s->context;
  if ( ctx && COMPARE_AND_SWAP_PTR(&s->context, ctx, NULL) )
  { if ( streamContext )
    { deleteHTablePP(streamContext, s);
      if ( ctx->filename != NULL_ATOM )
      { PL_unregister_atom(ctx->filename);

	if ( ctx->filename == source_file_name )
	{ source_file_name = NULL_ATOM;	/* TBD: pop? */
	  source_line_no = -1;
	}
      }
    }

    freeHeap(ctx, sizeof(*ctx));
  }
					/* if we are a standard stream */
					/* reassociate with standard I/O */
					/* NOTE: there may be more! */
  if (
#ifdef O_PLMT
       LD &&
#endif
       (sp=LD->IO.streams) )
  { for(i=0; i <= SNO_MAX; i++, sp++)
    { if ( *sp == s )
      { IOSTREAM *new;

	switch(i)
	{ case SNO_USER_INPUT:
	    new = Sinput;
	    break;
	  case SNO_USER_OUTPUT:
	    new = Soutput;
	    break;
	  case SNO_USER_ERROR:
	    new = Serror;
	    break;
	  case SNO_CURRENT_INPUT:
	    new = Suser_input;
	    break;
	  case SNO_CURRENT_OUTPUT:
	    new = Suser_output;
	    break;
	  case SNO_PROTOCOL:
	    new = NULL;
	    break;
	}
	setStandardStream(i, new);
      }
    }
  }
  PL_UNLOCK(L_FILE);
}


/* MT: locked by caller (openStream()) */
/* name must be registered by the caller */

static void
setFileNameStream_unlocked(IOSTREAM *s, atom_t name)
{ stream_context *ctx = getStreamContext(s);

  if ( ctx->filename )
  { PL_unregister_atom(ctx->filename);
    ctx->filename = NULL_ATOM;
  }
  if ( !(name == NULL_ATOM || name == ATOM_) )
    ctx->filename = name;
}


int
setFileNameStream(IOSTREAM *s, atom_t name)
{ PL_LOCK(L_FILE);
  setFileNameStream_unlocked(s, name);
  PL_register_atom(name);
  PL_UNLOCK(L_FILE);

  return true;
}


atom_t
fileNameStream(IOSTREAM *s)
{ atom_t name;

  PL_LOCK(L_FILE);
  name = getStreamContext(s)->filename;
  PL_UNLOCK(L_FILE);

  return name;
}

void
initIO(void)
{ GET_LD
  const atom_t *np;
  int i;

  streamAliases = newHTableWP(16);
  streamContext = newHTablePP(16);
  PL_register_blob_type(&stream_blob);

  if ( isoff(Sinput, SIO_ISATTY) ||
       isoff(Soutput, SIO_ISATTY) )
  { /* clear PLFLAG_TTY_CONTROL */
    PL_set_prolog_flag("tty_control", PL_BOOL, false);
  }

  ResetTty();

  Sclosehook(freeStream);

  Sinput->position  = &Sinput->posbuf;	/* position logging */
  Soutput->position = &Sinput->posbuf;
  Serror->position  = &Sinput->posbuf;

  PushTty(Sinput, &ttytab, TTY_SAVE);
  ttymodified = false;
  ttyfileno = Sfileno(Sinput);
  LD->prompt.current = ATOM_prompt;
  PL_register_atom(ATOM_prompt);

  Suser_input  = Sinput;
  Suser_output = Soutput;
  Suser_error  = Serror;
  Scurin       = Sinput;		/* see/tell */
  Scurout      = Soutput;
  Sprotocol    = NULL;			/* protocolling */
  referenceStandardStreams(LD);

  getStreamContext(Sinput);		/* add for enumeration */
  getStreamContext(Soutput);
  getStreamContext(Serror);

  for( i=0, np = standardStreams; *np; np++, i++ )
    addNewHTableWP(streamAliases,
		   *np,
		   (IOSTREAM*)(uintptr_t)(i ^ STD_HANDLE_MASK));

  GD->io_initialised = true;
}

		 /*******************************
		 *	     GET HANDLES	*
		 *******************************/

static inline IOSTREAM *
getStream(IOSTREAM *s)
{ if ( s && s->magic == SIO_MAGIC && Slock(s) == 0 )
  { if ( unlikely(s->magic == SIO_CMAGIC) )
    { Sunlock(s);
      return NULL;
    }
    return s;
  }

  return NULL;
}

static inline IOSTREAM *
tryGetStream(IOSTREAM *s)
{ if ( s && s->magic == SIO_MAGIC && StryLock(s) == 0 )
  { if ( unlikely(s->magic == SIO_CMAGIC) )
    { Sunlock(s);
      return NULL;
    }
    return s;
  }

  return NULL;
}

static bool
releaseStream(IOSTREAM *s)
{ if ( s->magic == SIO_MAGIC )
    return Sunlock(s) == 0;
  return true;
}

bool
PL_release_stream(IOSTREAM *s)
{ return streamStatus(s);
}

bool
PL_release_stream_noerror(IOSTREAM *s)
{ if ( !releaseStream(s) )
    PL_clear_exception();

  return true;
}

IOSTREAM *
PL_acquire_stream(IOSTREAM *s)
{ return getStream(s);
}



		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static bool symbol_no_stream(atom_t symbol);

static bool
no_stream(term_t t, atom_t name)
{ if ( t )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_stream, t);
  else
    return symbol_no_stream(name);
}

static bool
not_a_stream(term_t t)
{ return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_stream_or_alias, t);
}

static bool
symbol_no_stream(atom_t symbol)
{ GET_LD
  term_t t;

  if ( (t = PL_new_term_ref()) )
  { PL_put_atom(t, symbol);
    return no_stream(t, 0);
  } else
    return false;
}

static bool
symbol_not_a_stream(atom_t symbol)
{ GET_LD
  term_t t = PL_new_term_ref();
  PL_put_atom(t, symbol);
  return not_a_stream(t);
}


static bool
symbol_stream_pair_not_allowed(atom_t symbol)
{ GET_LD
  term_t t = PL_new_term_ref();
  PL_put_atom(t, symbol);

  return PL_error(NULL, 0, "operation is ambiguous on a stream pair",
		  ERR_TYPE, ATOM_stream, t);
}



		 /*******************************
		 *	  PROLOG HANDLES	*
		 *******************************/

typedef struct stream_ref
{ IOSTREAM *read;
  IOSTREAM *write;
} stream_ref;


static int
write_stream_ref(IOSTREAM *s, atom_t aref, int flags)
{ stream_ref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  if ( ref->read && ref->write )
    Sfprintf(s, "<stream>(%p,%p)", ref->read, ref->write);
  else if ( ref->read )
    Sfprintf(s, "<stream>(%p)", ref->read);
  else
    Sfprintf(s, "<stream>(%p)", ref->write);

  return true;
}


static void
acquire_stream_ref(atom_t aref)
{ stream_ref *ref = PL_blob_data(aref, NULL, NULL);

  if ( ref->read )
    Sacquire(ref->read);
  if ( ref->write )
    Sacquire(ref->write);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A garbage collected stream that is  open   cannot  be subject to any I/O
operations. Previously we left these open. Now we close them if they are
not yet closed. We only so so  when   the  stream is unlocked though. In
theory we could force destruction of the stream   but for now we stay on
the safe side.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
gc_close_stream(atom_t aref, IOSTREAM *s)
{ if ( s->erased )
  { unallocStream(s);
  } else if ( s->magic == SIO_MAGIC && !ison(s, SIO_CLOSING) )
  { int doit;

    WITH_LD(&PL_local_data)
      doit = truePrologFlag(PLFLAG_AGC_CLOSE_STREAMS);

    doit = doit && standardStreamIndexFromStream(s) < 0;

    if ( doit )
    { int rc = Sgcclose(s, SIO_CLOSE_TRYLOCK);

      if ( s != Serror )
      { Slock(Serror);
	if ( rc == 0 )
	  Sdprintf("WARNING: AGC: closed ");
	else
	  Sdprintf("WARNING: AGC: failed to close (locked) ");
	write_stream_ref(Serror, aref, 0);
	Sdprintf("\n");
	Sunlock(Serror);
      }
    }
  }
}


static int
release_stream_ref(atom_t aref)
{ stream_ref *ref = PL_blob_data(aref, NULL, NULL);

  if ( ref->read )
  { if ( Sunreference(ref->read) == 0 )
      gc_close_stream(aref, ref->read);
  }
  if ( ref->write )
  { if ( Sunreference(ref->write) == 0 )
      gc_close_stream(aref, ref->write);
  }

  return true;
}


static int
save_stream_ref(atom_t aref, IOSTREAM *fd)
{ stream_ref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <stream>(%p,%p)",
		    ref->read, ref->write);
}


static atom_t
load_stream_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-stream-ref>");
}


static PL_blob_t stream_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "stream",
  release_stream_ref,
  NULL,
  write_stream_ref,
  acquire_stream_ref,
  save_stream_ref,
  load_stream_ref
};


#define SH_ERRORS   0x01		/* generate errors */
#define SH_ALIAS    0x02		/* allow alias */
#define SH_UNLOCKED 0x04		/* don't lock the stream */
#define SH_OUTPUT   0x08		/* We want an output stream */
#define SH_INPUT    0x10		/* We want an input stream */
#define SH_NOPAIR   0x20		/* Do not allow for a pair */
#define SH_TRYLOCK  0x40		/* Fail if we cannot lock */

#define get_stream_handle(a, sp, flags) LDFUNC(get_stream_handle, a, sp, flags)
static bool
get_stream_handle(DECL_LD atom_t a, IOSTREAM **sp, int flags)
{ stream_ref *ref;
  PL_blob_t *type;
  IOSTREAM *s;

  ref = PL_blob_data(a, NULL, &type);
  if ( type == &stream_blob )
  { if ( ref->read  ) assert(ref->read->references);
    if ( ref->write ) assert(ref->write->references);

    if ( ref->read )
    { if ( ref->write )
      { if ( (flags&SH_OUTPUT) )
	  s = ref->write;
	else if ( (flags&SH_INPUT) )
	  s = ref->read;
	else if ( (flags&SH_NOPAIR) )
	{ if ( truePrologFlag(PLFLAG_ERROR_AMBIGUOUS_STREAM_PAIR) )
	  { return symbol_stream_pair_not_allowed(a);
	  } else
	  { term_t t;

	    if ( (t=PL_new_term_ref()) &&
		 PL_put_atom(t, a) )
	    { if ( !printMessage(ATOM_warning,
				 PL_FUNCTOR_CHARS, "ambiguous_stream_pair", 1,
				   PL_TERM, t) )
		return false;
	    }
	    s = ref->read;
	  }
	} else
	  s = ref->read;			/* dubious */
      } else
	s = ref->read;
    } else
      s = ref->write;

    if ( s->erased )
       goto noent;

    if ( flags & SH_UNLOCKED )
    { assert( s->magic == SIO_MAGIC || s->magic == SIO_CMAGIC );
      *sp = s;
      return true;
    } else if ( flags & SH_TRYLOCK )
    { if ( (s=tryGetStream(s)) )
      { *sp = s;
	return true;
      } else
	return false;			/* exception */
    } else if ( (s=getStream(s)) )
    { *sp = s;
      return true;
    }

    return symbol_no_stream(a);
  } else
  { void *s0;

    if ( !(flags & SH_UNLOCKED) )
      PL_LOCK(L_FILE);
    if ( (s0 = lookupHTableWP(streamAliases, a)) )
    { IOSTREAM *stream;
      uintptr_t n = (uintptr_t)s0 & ~STD_HANDLE_MASK;

      if ( n <= SNO_MAX )		/* standard stream! */
      { stream = LD->IO.streams[n];	/* TBD: No need to lock for std-streams */
	if ( stream->magic == SIO_CMAGIC )
	  stream = restoreStandardStream((int)n);
      } else
	stream = s0;

      if ( !(flags & SH_UNLOCKED) )
	PL_UNLOCK(L_FILE);

      if ( stream )
      { if ( (flags & SH_UNLOCKED) )
	{ if ( stream->magic == SIO_MAGIC )
	  { *sp = stream;
	    return true;
	  }
	} else if ( flags & SH_TRYLOCK )
	{ if ( (s=tryGetStream(stream)) )
	  { *sp = s;
	    return true;
	  } else
	    return false;		/* exception? */
	} else if ( (*sp = getStream(stream)) )
	  return true;
	goto noent;
      }
    }
    if ( !(flags & SH_UNLOCKED) )
      PL_UNLOCK(L_FILE);

    goto noent;
  }

  if ( flags & SH_ERRORS )
    symbol_not_a_stream(a);

  return false;

noent:
  if ( flags & SH_ERRORS )
    symbol_no_stream(a);

  return false;
}



#define term_stream_handle(t, s, flags) LDFUNC(term_stream_handle, t, s, flags)
static bool
term_stream_handle(DECL_LD term_t t, IOSTREAM **s, int flags)
{ atom_t a;

  if ( !PL_get_atom(t, &a) )
    return not_a_stream(t);

  return get_stream_handle(a, s, flags);
}


bool
PL_get_stream_handle(term_t t, IOSTREAM **s)
{ GET_LD

  return term_stream_handle(t, s, SH_ERRORS|SH_ALIAS|SH_NOPAIR);
}

bool
PL_get_stream(term_t t, IOSTREAM **s, int flags)
{ GET_LD
  atom_t a;

  if ( !PL_get_atom(t, &a) )
    return not_a_stream(t);

  return PL_get_stream_from_blob(a, s, flags);
}

bool
PL_get_stream_from_blob(atom_t a, IOSTREAM **s, int flags)
{ GET_LD
  int myflags = SH_ERRORS|SH_ALIAS;

  if ( flags&SIO_INPUT   ) myflags |= SH_INPUT;
  if ( flags&SIO_OUTPUT  ) myflags |= SH_OUTPUT;
  if ( flags&SIO_TRYLOCK ) myflags |= SH_TRYLOCK;
  if ( flags&SIO_NOERROR ) myflags &= ~SH_ERRORS;
  if ( !(flags&(SIO_INPUT|SIO_OUTPUT)) )
    myflags |= SH_NOPAIR;

  return get_stream_handle(a, s, myflags);
}


static bool
unify_stream_ref(term_t t, IOSTREAM *s)
{ GET_LD
  stream_ref ref;
  bool rval;

  memset(&ref, 0, sizeof(ref));
  if ( s->flags & SIO_INPUT )
    ref.read = s;
  else
    ref.write = s;

  rval = PL_unify_blob(t, &ref, sizeof(ref), &stream_blob);

  if ( !rval && !PL_is_variable(t) )
    return PL_error(NULL, 0, "stream-argument", ERR_UNINSTANTIATION, 0, t);

  return rval;
}


bool
PL_unify_stream_or_alias(term_t t, IOSTREAM *s)
{ GET_LD
  bool rval;
  stream_context *ctx;
  int i;

  if ( (i=standardStreamIndexFromStream(s)) >= 0 && i < 3 )
    return PL_unify_atom(t, standardStreams[i]);

  if ( (ctx=getExistingStreamContext(s)) && ctx->alias_head )
  { PL_LOCK(L_FILE);
    if ( ctx->alias_head )
      rval = PL_unify_atom(t, ctx->alias_head->name);
    else
      rval = unify_stream_ref(t, s);
    PL_UNLOCK(L_FILE);
  } else
  { rval = unify_stream_ref(t, s);
  }

  return rval;
}


bool
PL_unify_stream(term_t t, IOSTREAM *s)
{ (void)getStreamContext(s);		/* get stream known to Prolog */

  return unify_stream_ref(t, s);
}


IOSTREAM **				/* provide access to Suser_input, */
_PL_streams(void)			/* Suser_output and Suser_error */
{ GET_LD
  return LD ? &Suser_input : NULL;
}

IOSTREAM *				/* provide access to Suser_input, */
_PL_stream(int which)			/* Suser_output and Suser_error */
{ GET_LD
  if ( LD )
    return LD->IO.streams[which];

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getInputStream(term_t t, IOSTREAM **s)
getOutputStream(term_t t, IOSTREAM **s)
    These functions are the basis used by all Prolog predicates to get
    a input or output stream handle.  If t = 0, current input/output is
    returned.  This allows us to define the standard-stream based version
    simply by calling the explicit stream-based version with 0 for the
    stream argument.

    MT: The returned stream is always locked and should be returned
    using releaseStream() or streamStatus().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum
{ S_DONTCARE = 0,
  S_TEXT,
  S_BINARY
} s_type;


#define checkStreamType(text, s, error) LDFUNC(checkStreamType, text, s, error)
static int
checkStreamType(DECL_LD s_type text, IOSTREAM *s, atom_t *error)
{ if ( text == S_DONTCARE || LD->IO.stream_type_check == ST_FALSE )
    return true;			/* no checking */

					/* ok? */
  if ( text == S_TEXT && (s->flags&SIO_TEXT) )
    return true;
  if ( text == S_BINARY && !(s->flags&SIO_TEXT) )
    return true;
					/* no */
  if ( LD->IO.stream_type_check == ST_LOOSE )
  { if ( text == S_TEXT )
      return true;
    if ( s->encoding == ENC_ISO_LATIN_1 ||
	 s->encoding == ENC_OCTET )
      return true;
  }

  *error = (text == S_TEXT ? ATOM_binary_stream : ATOM_text_stream);
  return false;
}


#define getOutputStream(t, text, stream) LDFUNC(getOutputStream, t, text, stream)
static int
getOutputStream(DECL_LD term_t t, s_type text, IOSTREAM **stream)
{ atom_t a;
  IOSTREAM *s;
  atom_t tp;

  if ( t == 0 )
  { if ( (s = getStream(
#ifdef O_DEBUG
			LD->internal_debug.depth > 0 ? Serror :
#endif
			Scurout)) )
      goto ok;
    no_stream(t, ATOM_current_output);
    return false;
  }

  if ( !PL_get_atom(t, &a) )
  { not_a_stream(t);
    return false;
  }

  if ( a == ATOM_user )
  { if ( (s = getStream(Suser_output)) )
      goto ok;
    no_stream(t, ATOM_user);
    return false;
  }

  if ( !get_stream_handle(a, &s, SH_ERRORS|SH_ALIAS|SH_OUTPUT) )
    return false;

ok:
  if ( !(s->flags&SIO_OUTPUT) )
  { tp = ATOM_stream;
  } else if ( checkStreamType(text, s, &tp) )
  { *stream = s;
    return true;
  }

  if ( !releaseStream(s) )
    return false;
  if ( t == 0 )
  { if ( (t = PL_new_term_ref()) )
      PL_put_atom(t, ATOM_current_output);
    else
      return false;				/* resource error */
  }
  PL_error(NULL, 0, NULL, ERR_PERMISSION, ATOM_output, tp, t);

  return false;
}


int
getTextOutputStream(DECL_LD term_t t, IOSTREAM **stream)
{ return getOutputStream(t, S_TEXT, stream);
}


int
getBinaryOutputStream(DECL_LD term_t t, IOSTREAM **stream)
{ return getOutputStream(t, S_BINARY, stream);
}


#define getInputStream(t, text, stream) LDFUNC(getInputStream, t, text, stream)
static int
getInputStream(DECL_LD term_t t, s_type text, IOSTREAM **stream)
{ atom_t a;
  IOSTREAM *s = NULL;				/* make compiler happy  */
  atom_t tp;

  if ( t == 0 )
  { if ( (s = getStream(Scurin)) )
      goto ok;
    no_stream(t, ATOM_current_input);
    return false;
  }

  if ( !PL_get_atom(t, &a) )
  { not_a_stream(t);
    return false;
  }

  if ( a == ATOM_user )
  { if ( (s = getStream(Suser_input)) )
      goto ok;
    no_stream(t, ATOM_user);
    return false;
  }

  if ( !get_stream_handle(a, &s, SH_ERRORS|SH_ALIAS|SH_INPUT) )
    return false;

ok:
  if ( !(s->flags&SIO_INPUT) )
  { tp = ATOM_stream;
  } else if ( checkStreamType(text, s, &tp) )
  { *stream = s;
    return true;
  }

  if ( !releaseStream(s) )
    return false;
  if ( t == 0 )
  { if ( (t = PL_new_term_ref()) )
      PL_put_atom(t, ATOM_current_input);
    else
      return false;				/* resource error */
  }
  PL_error(NULL, 0, NULL, ERR_PERMISSION, ATOM_input, tp, t);

  return false;
}

int
getTextInputStream(DECL_LD term_t t, IOSTREAM **stream)
{ return getInputStream(t, S_TEXT, stream);
}

int
getBinaryInputStream(DECL_LD term_t t, IOSTREAM **stream)
{ return getInputStream(t, S_BINARY, stream);
}


/** stream_pairs(+Pair, -Read, -Write)
    stream_pairs(-Pair, +Read, +Write)
*/

static
PRED_IMPL("stream_pair", 3, stream_pair, 0)
{ PRED_LD
  IOSTREAM *in = NULL, *out = NULL;
  int rc = false;

  if ( !PL_is_variable(A1) )
  { stream_ref *ref;
    atom_t a = 0;
    PL_blob_t *type;
    int rc = true;

    if ( PL_get_atom(A1, &a) &&
	 (ref=PL_blob_data(a, NULL, &type)) &&
	 type == &stream_blob )
    { if ( ref->read && !ref->read->erased )
	rc = rc && PL_unify_stream_or_alias(A2, ref->read);
      if ( ref->write && !ref->write->erased )
	rc = rc && PL_unify_stream_or_alias(A3, ref->write);

      return rc;
    } else
    { IOSTREAM *s;

      if ( a && get_stream_handle(a, &s, SH_ERRORS|SH_ALIAS|SH_UNLOCKED) )
      { if ( (s->flags & SIO_INPUT) )
	  rc = PL_unify_stream_or_alias(A2, s);
	else
	  rc = PL_unify_stream_or_alias(A3, s);

	return rc;
      }

      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_stream_pair, A1);
    }
  }

  if ( getInputStream(A2, S_DONTCARE, &in) &&
       getOutputStream(A3, S_DONTCARE, &out) )
  { stream_ref ref;

    ref.read = in;
    ref.write = out;

    rc = PL_unify_blob(A1, &ref, sizeof(ref), &stream_blob);
    if ( rc )
    { assert(ref.read->references >= 2);
      assert(ref.write->references >= 2);
    }
  }

  if ( in )
    rc = releaseStream(in) && rc;
  if ( out )
    rc = releaseStream(out) && rc;

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
In windows GUI applications, the IO-streams  are   not  bound. We do not
wish to generate an error on the  stream   errors  that may be caused by
this. It is a bit of a hack, but   the alternative is to define a stream
that ignores the error. This might get hairy if the user is playing with
these streams too.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __WINDOWS__
static bool
isConsoleStream(IOSTREAM *s)
{ int i = standardStreamIndexFromStream(s);

  return i >= 1 && i < 3;			/* only output streams */
}
#else
#define isConsoleStream(s) false
#endif


bool
reportStreamError(IOSTREAM *s)
{ if ( GD->cleaning >= CLN_IO ||
       isConsoleStream(s) )
    return true;

  if ( (s->flags & (SIO_FERR|SIO_WARN)) )
  { GET_LD
    atom_t op;
    term_t stream;
    char *msg;

    if ( !HAS_LD ||
	 !(stream=PL_new_term_ref()) ||
	 !PL_unify_stream_or_alias(stream, s) )
      return false;

    if ( (s->flags & SIO_FERR) )
    { if ( exception_term )
	return false;

      if ( s->exception )
      { fid_t fid;
	term_t ex;
	int rc;

	LD->exception.processing = true;	/* allow using spare stack */
	if ( !(fid = PL_open_foreign_frame()) )
	  return false;
	ex = PL_new_term_ref();
	rc = PL_recorded(s->exception, ex);
	PL_erase(s->exception);
	s->exception = NULL;
	if ( rc )
	  rc = PL_raise_exception(ex);
	Sclearerr(s);
	PL_close_foreign_frame(fid);
	return rc;
      }

      if ( s->flags & SIO_INPUT )
      { if ( Sfpasteof(s) )
	{ return PL_error(NULL, 0, NULL, ERR_PERMISSION,
			  ATOM_input, ATOM_past_end_of_stream, stream);
	} else if ( (s->flags & SIO_TIMEOUT) )
	{ PL_error(NULL, 0, NULL, ERR_TIMEOUT,
		   ATOM_read, stream);
	  Sclearerr(s);
	  return false;
	} else
	  op = ATOM_read;
      } else
      { if ( (s->flags & SIO_TIMEOUT) )
	{ PL_error(NULL, 0, NULL, ERR_TIMEOUT,
		   ATOM_write, stream);
	  Sclearerr(s);
	  return false;
	} else
	  op = ATOM_write;
      }

      if ( s->message )
      { msg = s->message;
      } else
      { msg = MSG_ERRNO;
	if ( s->io_errno )
	  errno = s->io_errno;
      }

      PL_error(NULL, 0, msg, ERR_STREAM_OP, op, stream);
      Sclearerr(s);

      return false;
    } else
    { int rc;

      rc = printMessage(ATOM_warning,
			PL_FUNCTOR_CHARS, "io_warning", 2,
			  PL_TERM, stream,
			  PL_CHARS, s->message);
      Sseterr(s, 0, NULL);

      return rc;
    }
  }

  return true;
}


bool
streamStatus(IOSTREAM *s)
{ if ( (s->flags & (SIO_FERR|SIO_WARN)) )
  { bool ret = reportStreamError(s);
    return releaseStream(s) && ret;
  }

  return releaseStream(s);
}


		 /*******************************
		 *	     TTY MODES		*
		 *******************************/

ttybuf	ttytab;				/* saved terminal status on entry */
int	ttymodified;			/* is tty modified? */
int	ttyfileno = -1;

typedef struct input_context * InputContext;
typedef struct output_context * OutputContext;

struct input_context
{ IOSTREAM *    stream;                 /* pushed input */
  atom_t	type;			/* Type of input */
  atom_t        term_file;              /* old term_position file */
  int           term_line;              /* old term_position line */
  InputContext  previous;               /* previous context */
};


struct output_context
{ IOSTREAM *    stream;                 /* pushed output */
  OutputContext previous;               /* previous context */
};

#define input_context_stack  (LD->IO.input_stack)
#define output_context_stack (LD->IO.output_stack)

static IOSTREAM *openStream(term_t file, term_t mode, term_t options);

void
dieIO(void)
{ if ( GD->io_initialised )
  { GD->io_initialised = false;

    noprotocol();
    closeFiles(true);

    if ( streamAliases )
    { destroyHTableWP(streamAliases);
      streamAliases = NULL;
    }
    if ( streamContext )
    { destroyHTablePP(streamContext);
      streamContext = NULL;
    }

    for(int i=0; i<=2; i++)
    { IOSTREAM *s = &S__iob[i];

      if ( s->context )
      { freeHeap(s->context, sizeof(stream_context));
	s->context = NULL;
      }
    }

    if ( ttymodified && ttyfileno == Sfileno(Sinput) )
      PopTty(Sinput, &ttytab, true);
    freeHeap(ttytab.state, 0);
    memset(&ttytab, 0, sizeof(ttytab));
    ttymodified = false;
    ttyfileno = -1;
  }
}

/* True if the  three standard user streams are not  in an error mode.
 * Getting in  an error state is  typically the case for  threads that
 * run  the REPL  loop.  When  in error  state, should  we switch  the
 * output streams  back to Soutput  and Serror?  Surely not  the input
 * stream as that may deadlock.
 */
bool
validUserStreams(DECL_LD)
{ return ( !Sferror(Suser_input) &&
	   !Sferror(Suser_output) &&
	   !Sferror(Suser_error) );
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
closeStream() performs Prolog-level closing. Most important right now is
to to avoid closing the user-streams. If a stream cannot be flushed (due
to a write-error), an exception is  generated.

MT: We assume the stream is locked and will unlock it here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
closeStream(IOSTREAM *s)
{ if ( s == Sinput )
  { Sclearerr(s);
    return releaseStream(s);
  } else if ( s == Soutput || s == Serror )
  { if ( Sflush(s) < 0 )
      return streamStatus(s);
    return releaseStream(s);
  } else
  { if ( !Sferror(s) && Sflush(s) < 0 )
    { bool rc = reportStreamError(s);
      Sclose(s);
      return rc;
    }
    return Sclose(s) == 0;		/* will unlock as well */
  }
}


/* Close all files.  As this only happens during termination we report,
 * but otherwise ignore possible errors.
 */

void
closeFiles(int all)
{ GET_LD
  TableEnum e;

  e = newTableEnumPP(streamContext);
  table_key_t tk;
  while( advanceTableEnum(e, &tk, NULL) )
  { IOSTREAM *s = key2ptr(tk);

    if ( all || !(s->flags & SIO_NOCLOSE) )
    { IOSTREAM *s2 = tryGetStream(s);

      if ( s2 )
      { if ( !all )
	{ term_t t = PL_new_term_ref();

	  PL_unify_stream_or_alias(t, s2);
	  if ( !printMessage(ATOM_informational,
			     PL_FUNCTOR, FUNCTOR_close_on_abort1,
			       PL_TERM, t) )
	    PL_clear_exception();
	  PL_reset_term_refs(t);
	}

	if ( !closeStream(s2) && exception_term )
	{ int rc = printMessage(ATOM_warning, PL_TERM, exception_term);
	  (void)rc;
	  PL_clear_exception();
	}
      }
    }
  }
  freeTableEnum(e);
}


void
protocol(const char *str, size_t n)
{ GET_LD
  IOSTREAM *s;

  if ( HAS_LD && Sprotocol && (s = getStream(Sprotocol)) )
  { while( n-- > 0 )
      Sputcode(*str++&0xff, s);
    Sflush(s);
    if ( !releaseStream(s) )		/* we do not check errors */
      PL_clear_exception();
  }
}


		 /*******************************
		 *	  TEMPORARY I/O		*
		 *******************************/


int
push_input_context(atom_t type)
{ GET_LD
  InputContext c = allocHeapOrHalt(sizeof(struct input_context));

  PL_register_atom(type);

  c->stream           = Scurin;
  c->type	      = type;
  c->term_file        = source_file_name;
  c->term_line        = source_line_no;
  c->previous         = input_context_stack;
  input_context_stack = c;

  return true;
}


int
pop_input_context(void)
{ GET_LD
  InputContext c = input_context_stack;

  if ( c )
  { Scurin              = c->stream;
    source_file_name    = c->term_file;
    source_line_no      = c->term_line;
    input_context_stack = c->previous;
    PL_unregister_atom(c->type);
    freeHeap(c, sizeof(struct input_context));

    return true;
  } else
  { Scurin		= Sinput;
    return false;
  }
}


static
PRED_IMPL("$push_input_context", 1, push_input_context, 0)
{ PRED_LD
  atom_t type;

  if ( PL_get_atom_ex(A1, &type) )
    return push_input_context(type);

  return false;
}


static
PRED_IMPL("$pop_input_context", 0, pop_input_context, 0)
{ return pop_input_context();
}


/** '$input_context'(-List) is det.

True if List is a  list   of  input(Type,File,Line) terms describing the
current input context.
*/

static
PRED_IMPL("$input_context", 1, input_context, 0)
{ PRED_LD
  term_t tail   = PL_copy_term_ref(A1);
  term_t head   = PL_new_term_ref();
  term_t stream = PL_new_term_ref();
  InputContext c = input_context_stack;

  for(c=input_context_stack; c; c=c->previous)
  { atom_t file = c->term_file ? c->term_file : ATOM_minus;
    int line = c->term_file ? c->term_line : 0;

    PL_put_variable(stream);

    if ( !PL_unify_stream_or_alias(stream, c->stream) ||
	 !PL_unify_list(tail, head, tail) ||
	 !PL_unify_term(head, PL_FUNCTOR, FUNCTOR_input4,
			PL_ATOM, c->type,
			PL_ATOM, file,
			PL_INT,  line,
			PL_TERM, stream) )
      return false;
  }

  return PL_unify_nil(tail);
}


void
pushOutputContext(DECL_LD IOSTREAM *s)
{ OutputContext c      = allocHeapOrHalt(sizeof(struct output_context));
  c->stream            = Scurout;
  c->previous          = output_context_stack;
  output_context_stack = c;

  Sacquire(c->stream);
  setStandardStream(SNO_CURRENT_OUTPUT, s);
}


void
popOutputContext(DECL_LD)
{ OutputContext c = output_context_stack;

  if ( c )
  { IOSTREAM *s = c->stream;

    if ( s->magic != SIO_MAGIC )
    { Sdprintf("[%d] current_output closed; set to user_output\n",
	       PL_thread_self());
      s = Soutput;
    }
    setStandardStream(SNO_CURRENT_OUTPUT, s);
    output_context_stack = c->previous;
    Srelease(c->stream);
    freeHeap(c, sizeof(struct output_context));
  } else
    setStandardStream(SNO_CURRENT_OUTPUT, Soutput);
}


int
setupOutputRedirect(term_t to, redir_context *ctx, int redir)
{ GET_LD
  atom_t a;

  ctx->term = to;
  ctx->redirected = redir;

  if ( to == 0 )
  { if ( !(ctx->stream = getStream(Scurout)) )
      return no_stream(to, ATOM_current_output);
    ctx->is_stream = true;
  } else if ( PL_get_atom(to, &a) )
  { if ( a == ATOM_user )
    { if ( !(ctx->stream = getStream(Suser_output)) )
	return no_stream(to, ATOM_user);
      ctx->is_stream = true;
    } else if ( get_stream_handle(a, &ctx->stream, SH_OUTPUT|SH_ERRORS) )
    { if ( !(ctx->stream->flags &SIO_OUTPUT) )
      { releaseStream(ctx->stream);
	return PL_error(NULL, 0, NULL, ERR_PERMISSION,
			ATOM_output, ATOM_stream, to);
      }

      ctx->is_stream = true;
    } else
      return false;
  } else
  { if ( PL_is_functor(to, FUNCTOR_codes2) )
    { ctx->out_format = PL_CODE_LIST;
      ctx->out_arity = 2;
    } else if ( PL_is_functor(to, FUNCTOR_codes1) )
    { ctx->out_format = PL_CODE_LIST;
      ctx->out_arity = 1;
    } else if ( PL_is_functor(to, FUNCTOR_chars2) )
    { ctx->out_format = PL_CHAR_LIST;
      ctx->out_arity = 2;
    } else if ( PL_is_functor(to, FUNCTOR_chars1) )
    { ctx->out_format = PL_CHAR_LIST;
      ctx->out_arity = 1;
    } else if ( PL_is_functor(to, FUNCTOR_string1) )
    { ctx->out_format = PL_STRING;
      ctx->out_arity = 1;
    } else if ( PL_is_functor(to, FUNCTOR_atom1) )
    { ctx->out_format = PL_ATOM;
      ctx->out_arity = 1;
    } else
    { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_output, to);
    }

    ctx->is_stream = false;
    ctx->data = ctx->buffer;
    ctx->size = sizeof(ctx->buffer);
    ctx->stream = Sopenmem(&ctx->data, &ctx->size, "w");
    ctx->stream->encoding = ENC_WCHAR;
  }

  ctx->magic = REDIR_MAGIC;

  if ( redir )
    pushOutputContext(ctx->stream);

  return true;
}


int
closeOutputRedirect(redir_context *ctx)
{ int rval = true;

  if ( ctx->magic != REDIR_MAGIC )
    return rval;			/* already done */
  ctx->magic = 0;

  if ( ctx->redirected )
  { GET_LD
    popOutputContext();
  }

  if ( ctx->is_stream )
  { rval = streamStatus(ctx->stream);
  } else
  { GET_LD
    term_t out  = PL_new_term_ref();
    term_t diff, tail;

    if ( Sclose(ctx->stream) == 0 )
    { _PL_get_arg(1, ctx->term, out);
      if ( ctx->out_arity == 2 )
      { diff = PL_new_term_ref();
	_PL_get_arg(2, ctx->term, diff);
	tail = PL_new_term_ref();
      } else
      { diff = tail = 0;
      }

      rval = PL_unify_wchars_diff(out, tail, ctx->out_format,
				  ctx->size/sizeof(wchar_t),
				  (wchar_t*)ctx->data);
      if ( rval && tail )
	rval = PL_unify(tail, diff);
    } else
      rval = false;

    if ( ctx->data != ctx->buffer )
      Sfree(ctx->data);
  }

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
discardOutputRedirect() is called if the `implementation' failed. One of
the reasons for failure  can  be   that  the  implementation  detected a
pending I/O stream error, in which case continuation is meaningless.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
discardOutputRedirect(redir_context *ctx)
{ if ( ctx->magic != REDIR_MAGIC )
    return;				/* already done */

  ctx->magic = 0;

  if ( ctx->redirected )
  { GET_LD
    popOutputContext();
  }

  if ( ctx->is_stream )
  { streamStatus(ctx->stream);
  } else
  { closeStream(ctx->stream);
    if ( ctx->data != ctx->buffer )
      Sfree(ctx->data);
  }
}


static
PRED_IMPL("with_output_to", 2, with_output_to, PL_FA_TRANSPARENT)
{ redir_context outctx;

  if ( setupOutputRedirect(A1, &outctx, true) )
  { term_t ex = 0;
    int rval;

    if ( (rval = callProlog(NULL, A2, PL_Q_CATCH_EXCEPTION, &ex)) )
      return closeOutputRedirect(&outctx);
    discardOutputRedirect(&outctx);
    if ( ex )
      return PL_raise_exception(ex);
  }

  return false;
}



void
PL_write_prompt(bool dowrite)
{ GET_LD
  IOSTREAM *s = getStream(Suser_output);

  if ( s )
  { if ( dowrite )
    { atom_t a = PrologPrompt();

      if ( a )
	writeAtomToStream(s, a);
    }

    Sflush(s);
    releaseStream(s);
  }

  LD->prompt.next = false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a single character from Sinput  without   waiting  for a return. The
character should not be echoed.  If   PLFLAG_TTY_CONTROL  is false this
function will read the first character and  then skip all character upto
and including the newline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
Sgetcode_intr(IOSTREAM *s, int signals)
{ int c;

#ifdef __WINDOWS__
  int newline = s->newline;
  s->newline = SIO_NL_POSIX;		/* avoid blocking \r */
#endif
  do
  { c = Sgetcode(s);
    Sclearerr(s);
  } while ( c == -1 &&
	    errno == EINTR &&
	    (!signals || PL_handle_signals() >= 0)
	  );
#ifdef __WINDOWS__
  s->newline = newline;
#endif

  return c;
}


int
getSingleChar(IOSTREAM *stream, int signals)
{ GET_LD
  int c;
  ttybuf buf;

  suspendTrace(true);
  Slock(stream);
  Sflush(stream);
  PushTty(stream, &buf, TTY_RAW);	/* just donot prompt */

  if ( !truePrologFlag(PLFLAG_TTY_CONTROL) )
  { int c2;

    c2 = Sgetcode_intr(stream, signals);
    while( c2 == ' ' || c2 == '\t' )	/* skip blanks */
      c2 = Sgetcode_intr(stream, signals);
    c = c2;
    while( c2 != EOF && c2 != '\n' )	/* read upto newline */
      c2 = Sgetcode_intr(stream, signals);
  } else
  { if ( stream->position )
    { IOPOS oldpos = *stream->position;
      c = Sgetcode_intr(stream, signals);
      *stream->position = oldpos;
    } else
      c = Sgetcode_intr(stream, signals);
  }

  if ( c == 4 || c == 26 )		/* should ask the terminal! */
    c = -1;

  PopTty(stream, &buf, true);
  suspendTrace(false);
  Sunlock(stream);

  return c;
}


static
PRED_IMPL("with_tty_raw", 1, with_tty_raw, PL_FA_TRANSPARENT)
{ PRED_LD
  int rval;
  ttybuf buf;
  int save;
  IOSTREAM *stream = getStream(Suser_input);

  if ( !stream )
    return symbol_no_stream(ATOM_user_input);
  save = ison(Sinput, SIO_ISATTY);

  Slock(stream);
  Sflush(stream);
  if ( save )
    PushTty(stream, &buf, TTY_RAW);

  rval = callProlog(NULL, A1, PL_Q_PASS_EXCEPTION, NULL);

  if ( save )
    PopTty(stream, &buf, true);
  Sunlock(stream);

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
readLine() reads a line from the terminal.  It is used only by the tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef DEL
#define DEL 127
#endif

int
readLine(IOSTREAM *in, IOSTREAM *out, char *buffer)
{ GET_LD
  int c;
  char *buf = &buffer[strlen(buffer)];
  ttybuf tbuf;

  Slock(in);
  Slock(out);

  PushTty(in, &tbuf, TTY_RAW);		/* just donot prompt */

  for(;;)
  { Sflush(out);

    switch( (c=Sgetcode_intr(in, false)) )
    { case '\n':
      case '\r':
      case EOF:
	*buf++ = EOS;
	PopTty(in, &tbuf, true);
	Sunlock(in);
	Sunlock(out);

	return c == EOF ? false : true;
      case '\b':
      case DEL:
	if ( truePrologFlag(PLFLAG_TTY_CONTROL) && buf > buffer )
	{ Sfputs("\b \b", out);
	  buf--;
	  continue;
	}
      default:
	if ( truePrologFlag(PLFLAG_TTY_CONTROL) )
	  Sputcode(c, out);
	*buf++ = (char)c;
    }
  }
}


IOSTREAM *
PL_current_input()
{ GET_LD
  return getStream(Scurin);
}


IOSTREAM *
PL_current_output()
{ GET_LD
  return getStream(Scurout);
}


static int
openProtocol(term_t f, int appnd)
{ GET_LD
  IOSTREAM *s;
  term_t mode = PL_new_term_ref();

  noprotocol();

  PL_put_atom(mode, appnd ? ATOM_append : ATOM_write);
  if ( (s = openStream(f, mode, 0)) )
  { s->flags |= SIO_NOCLOSE;		/* do not close on abort */

    Sprotocol = s;
    Suser_input->tee = s;
    Suser_output->tee = s;
    Suser_error->tee = s;

    return true;
  }

  return false;
}


static int
noprotocol(void)
{ GET_LD
  IOSTREAM *s;

  if ( Sprotocol && (s = getStream(Sprotocol)) )
  { TableEnum e;
    table_key_t tk;

    e = newTableEnumPP(streamContext);
    while( advanceTableEnum(e, &tk, NULL) )
    { IOSTREAM *p = key2ptr(tk);

      if ( p->tee == s )
	p->tee = NULL;
    }
    freeTableEnum(e);

    closeStream(s);
    Sprotocol = NULL;
  }

  return true;
}


static
PRED_IMPL("noprotocol", 0, noprotocol, 0)
{ return noprotocol();
}


		 /*******************************
		 *	 STREAM ATTRIBUTES	*
		 *******************************/

static int
setCloseOnExec(IOSTREAM *s, int val)
{ int fd;

  if ( (fd = Sfileno(s)) < 0)
    return false;

#if defined(F_SETFD) && defined(FD_CLOEXEC)
  { int fd_flags = fcntl(fd, F_GETFD);

    if ( fd_flags == -1 )
      return false;
    if ( val )
      fd_flags |= FD_CLOEXEC;
    else
      fd_flags &= ~FD_CLOEXEC;

    if ( fcntl(fd, F_SETFD, fd_flags) == -1 )
      return false;
  }
#elif defined __WINDOWS__
  { if ( !SetHandleInformation((HANDLE)_get_osfhandle(fd),
			       HANDLE_FLAG_INHERIT, !val) )
      return false;
  }
#else
  return -1;
#endif

  return true;
}


static int
set_eof_action(IOSTREAM *s, atom_t action)
{ if ( action == ATOM_eof_code )
  { s->flags &= ~(SIO_NOFEOF|SIO_FEOF2ERR);
  } else if ( action == ATOM_reset )
  { s->flags &= ~SIO_FEOF2ERR;
    s->flags |= SIO_NOFEOF;
  } else if ( action == ATOM_error )
  { s->flags &= ~SIO_NOFEOF;
    s->flags |= SIO_FEOF2ERR;
  } else
  { GET_LD
    term_t t;

    return ((t=PL_new_term_ref()) &&
	    PL_put_atom(t, action) &&
	    PL_domain_error("eof_action", t));
  }

  return true;
}


static int
set_buffering(IOSTREAM *s, atom_t b)
{
#define SIO_ABUF (SIO_FBUF|SIO_LBUF|SIO_NBUF)

  if ( b == ATOM_full )
  { s->flags &= ~SIO_ABUF;
    s->flags |= SIO_FBUF;
  } else if ( b == ATOM_line )
  { s->flags &= ~SIO_ABUF;
    s->flags |= SIO_LBUF;
  } else if ( b == ATOM_false )
  { Sflush(s);
    s->flags &= ~SIO_ABUF;
    s->flags |= SIO_NBUF;
  } else
  { GET_LD
    term_t t;

    return ((t=PL_new_term_ref()) &&
	    PL_put_atom(t, b) &&
	    PL_domain_error("buffer", t));
  }

  return true;
}


static int
atom_to_newline_mode(atom_t val, unsigned int flags, unsigned int *mode)
{ if ( val == ATOM_posix )
    *mode = SIO_NL_POSIX;
  else if ( val == ATOM_dos )
    *mode = SIO_NL_DOS;
  else if ( val == ATOM_detect )
  { if ( !(flags&SIO_INPUT) )
      return PL_error(NULL, 0, "detect only allowed for input streams",
		      ERR_DOMAIN, ATOM_newline, val),false;
    *mode = SIO_NL_DETECT;
  } else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_newline, val),false;

  return true;
}


/* returns true: ok, false: error, -1: not available
*/

#define set_stream(s, stream, aname, a) LDFUNC(set_stream, s, stream, aname, a)
static int
set_stream(DECL_LD IOSTREAM *s, term_t stream, atom_t aname, term_t a)
{ if ( aname == ATOM_alias )	/* alias(name) */
  { atom_t alias;
    int i;

    if ( !PL_get_atom_ex(a, &alias) )
      return false;

    if ( (i=standardStreamIndexFromName(alias)) >= 0 )
    { setStandardStream(i, s);
      if ( i == 0 )
	LD->prompt.next = true;		/* changed Sinput: prompt! */
      return true;
    }

    PL_LOCK(L_FILE);
    aliasStream(s, alias);
    PL_UNLOCK(L_FILE);
    return true;
  } else if ( aname == ATOM_buffer )	/* buffer(Buffering) */
  { atom_t b;

    if ( !PL_get_atom_ex(a, &b) )
      return false;
    return set_buffering(s, b);
  } else if ( aname == ATOM_buffer_size )
  { int size;

    if ( !PL_get_integer_ex(a, &size) )
      return false;
    if ( size < 1 )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_not_less_than_one, a);
    Ssetbuffer(s, NULL, size);
    return true;
  } else if ( aname == ATOM_eof_action ) /* eof_action(Action) */
  { atom_t action;

    if ( !PL_get_atom_ex(a, &action) )
      return false;

    return set_eof_action(s, action);
  } else if ( aname == ATOM_type ) /* type(Type) */
  { atom_t type;

    if ( !PL_get_atom_ex(a, &type) )
      return false;
    if ( type == ATOM_text )
    { if ( isoff(s, SIO_TEXT) && Ssetenc(s, LD->encoding, NULL) != 0 )
	return PL_error(NULL, 0, NULL, ERR_PERMISSION,
			ATOM_encoding, ATOM_stream, stream);
      s->flags |= SIO_TEXT;
    } else if ( type == ATOM_binary )
    { if ( ison(s, SIO_TEXT) && Ssetenc(s, ENC_OCTET, NULL) != 0 )
	return PL_error(NULL, 0, NULL, ERR_PERMISSION,
			ATOM_encoding, ATOM_stream, stream);

      s->flags &= ~SIO_TEXT;
    } else
    { return PL_error("set_stream", 2, NULL, ERR_DOMAIN,
		      ATOM_type, a);
    }

    return true;
  } else if ( aname == ATOM_close_on_abort ) /* close_on_abort(Bool) */
  { int close;

    if ( !PL_get_bool_ex(a, &close) )
      return false;

    if ( close )
      s->flags &= ~SIO_NOCLOSE;
    else
      s->flags |= SIO_NOCLOSE;

    return true;
  } else if ( aname == ATOM_record_position )
  { int rec;

    if ( !PL_get_bool_ex(a, &rec) )
      return false;

    if ( rec ) {
      memset(&s->posbuf, 0, sizeof(s->posbuf));
      s->posbuf.lineno = 1;
      s->position = &s->posbuf;
    } else
      s->position = NULL;

    return true;
  } else if ( aname == ATOM_line_position )
  { int lpos;

    if ( !PL_get_integer_ex(a, &lpos) )
      return false;

    if ( s->position )
      s->position->linepos = lpos;
    else
      return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		      ATOM_line_position, ATOM_stream, stream);

    return true;
  } else if ( aname == ATOM_file_name ) /* file_name(Atom) */
  { atom_t fn;

    if ( !PL_get_text_as_atom(a, &fn, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
      return false;

    setFileNameStream(s, fn);

    return true;
  } else if ( aname == ATOM_timeout )
  { double f;
    atom_t v;
    int tmo;

    if ( PL_get_atom(a, &v) && v == ATOM_infinite )
    { tmo = -1;
    } else
    { if ( !PL_get_float_ex(a, &f) )
	return false;

      if ( (tmo = (int)(f*1000.0)) < 0 )
	tmo = 0;
    }

    if ( Sset_timeout(s, tmo) == 0 )
      return true;
    return PL_permission_error("timeout", "stream", stream);
  } else if ( aname == ATOM_tty )	/* tty(bool) */
  { int val;

    if ( !PL_get_bool_ex(a, &val) )
      return false;

    if ( val )
      set(s, SIO_ISATTY);
    else
      clear(s, SIO_ISATTY);

    return true;
  } else if ( aname == ATOM_encoding )	/* encoding(atom) */
  { atom_t val;
    IOENC enc;

    if ( !PL_get_atom_ex(a, &val) )
      return false;
    if ( val == ATOM_bom )
    { IOSTREAM *s2;

      if ( (s2 = getStream(s)) )
      { if ( ScheckBOM(s2) == 0 )
	{ releaseStream(s2);
	  return (s2->flags&SIO_BOM) ? true:false;
	}
	return streamStatus(s2);
      }
      return streamStatus(s);
    } else if ( (enc = PL_atom_to_encoding(val)) == ENC_UNKNOWN )
    { bad_encoding(NULL, val);
      return false;
    }

    if ( Ssetenc(s, enc, NULL) == 0 )
      return true;

    return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		    ATOM_encoding, ATOM_stream, stream);
#ifdef O_LOCALE
  } else if ( aname == ATOM_locale )	/* locale(Locale) */
  { PL_locale *val;

    if ( !getLocaleEx(a, &val) )
      return false;
    if ( Ssetlocale(s, val, NULL) == 0 )
      return true;

    return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		    ATOM_locale, ATOM_stream, stream);
#endif
  } else if ( aname == ATOM_representation_errors )
  { atom_t val;

    if ( !PL_get_atom_ex(a, &val) )
      return false;

    clear(s, SIO_REPXML|SIO_REPPL|SIO_REPPLU);

    if ( val == ATOM_error )
      ;
    else if ( val == ATOM_xml )
      set(s, SIO_REPXML);
    else if ( val == ATOM_prolog )
      set(s, SIO_REPPL);
    else if ( val == ATOM_unicode )
      set(s, SIO_REPPLU);
    else
      return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		      ATOM_representation_errors, a);

    return true;
  } else if ( aname == ATOM_write_errors )
  { atom_t val;

    if ( !PL_get_atom_ex(a, &val) )
      return false;

    if ( val == ATOM_error )
      clear(s, SIO_NOERROR);
    else if ( val == ATOM_ignore )
      set(s, SIO_NOERROR);
    else
      return PL_domain_error("write_errors", a);

    return true;
  } else if ( aname == ATOM_newline )
  { atom_t val;
    unsigned int mode;

    if ( !PL_get_atom_ex(a, &val) )
      return false;

    if ( atom_to_newline_mode(val, s->flags, &mode) )
    { s->newline = mode&0x3;
      return true;
    } else
      return false;

  } else if ( aname == ATOM_close_on_exec ) /* close_on_exec(bool) */
  { int val;

    if ( !PL_get_bool_ex(a, &val) )
      return false;

    return setCloseOnExec(s, val);
  } else
  { assert(0);
    return false;
  }
}


typedef struct set_stream_info
{ atom_t name;
  int    flags;
} set_stream_info;

#define SS_READ		0x01
#define SS_WRITE	0x02
#define SS_BOTH		(SS_READ|SS_WRITE)
#define SS_NOPAIR	(0x4)
#define SS_EITHER	(SS_BOTH|SS_NOPAIR)

#define SS_INFO(name, flags) { name, flags }

static const set_stream_info ss_info[] =
{ SS_INFO(ATOM_alias,		      SS_EITHER),
  SS_INFO(ATOM_buffer,		      SS_BOTH),
  SS_INFO(ATOM_buffer_size,	      SS_BOTH),
  SS_INFO(ATOM_eof_action,	      SS_READ),
  SS_INFO(ATOM_type,		      SS_BOTH),
  SS_INFO(ATOM_close_on_abort,	      SS_BOTH),
  SS_INFO(ATOM_record_position,	      SS_BOTH),
  SS_INFO(ATOM_line_position,	      SS_EITHER),
  SS_INFO(ATOM_file_name,	      SS_BOTH),
  SS_INFO(ATOM_timeout,		      SS_BOTH),
  SS_INFO(ATOM_tty,		      SS_BOTH),
  SS_INFO(ATOM_encoding,	      SS_BOTH),
  SS_INFO(ATOM_locale,		      SS_BOTH),
  SS_INFO(ATOM_representation_errors, SS_WRITE),
  SS_INFO(ATOM_write_errors,          SS_WRITE),
  SS_INFO(ATOM_newline,		      SS_BOTH),
  SS_INFO(ATOM_close_on_exec,	      SS_BOTH),
  SS_INFO((atom_t)0,		      0)
};


static
PRED_IMPL("set_stream", 2, set_stream, 0)
{ PRED_LD
  IOSTREAM *s;
  atom_t sblob, aname;
  stream_ref *ref;
  PL_blob_t *type;
  int rc;
  size_t arity;
  const set_stream_info *info;
  term_t aval = PL_new_term_ref();

  term_t stream = A1;
  term_t attr = A2;

  if ( PL_get_name_arity(attr, &aname, &arity) && arity == 1 )
  { for(info = ss_info; info->name; info++)
    { if ( info->name == aname )
	goto found;
    }
    return PL_domain_error("stream_attribute", attr);
  } else
    return PL_type_error("stream_attribute", attr);

found:
  _PL_get_arg(1, attr, aval);

  if ( !PL_get_atom(stream, &sblob) )
    return not_a_stream(stream);

  ref = PL_blob_data(sblob, NULL, &type);
  if ( type == &stream_blob )		/* got a stream handle */
  { if ( ref->read && ref->write &&	/* stream pair */
	 (info->flags & SS_NOPAIR) )
      return symbol_stream_pair_not_allowed(sblob);

    rc = true;
    if ( ref->read && (info->flags&SS_READ))
    { if ( !(s = getStream(ref->read)) )
	return symbol_no_stream(sblob);
      rc = set_stream(s, stream, aname, aval);
      releaseStream(ref->read);
    }
    if ( rc && ref->write && (info->flags&SS_WRITE) )
    { if ( !(s = getStream(ref->write)) )
	return symbol_no_stream(sblob);
      rc = set_stream(s, stream, aname, aval);
      releaseStream(ref->write);
    }
  } else if ( PL_get_stream_handle(stream, &s) )
  { rc = set_stream(s, stream, aname, aval);
    releaseStream(s);
  } else
    rc = false;

  if ( rc < 0 )				/* not on this OS */
    return PL_domain_error("stream_attribute", attr);

  return rc;
}


#ifdef _MSC_VER					/* defined in pl-nt.c */
extern int ftruncate(int fileno, int64_t length);
#define HAVE_FTRUNCATE
#endif

static
PRED_IMPL("set_end_of_stream", 1, set_end_of_stream, 0)
{ IOSTREAM *s;
  int rc;

  if ( (rc=PL_get_stream(A1, &s, SIO_OUTPUT)) )
  {
#ifdef HAVE_FTRUNCATE
    int fileno = Sfileno(s);

    if ( fileno >= 0 )
    { if ( ftruncate(fileno, Stell64(s)) != 0 )
	rc = PL_error(NULL, 0, MSG_ERRNO, ERR_FILE_OPERATION,
		      ATOM_set_end_of_stream, ATOM_stream,
		      A1);
    } else
    { rc = PL_error(NULL, 0, "not a file", ERR_PERMISSION,
		    ATOM_set_end_of_stream, ATOM_stream, A1);
    }
#else
    rc = notImplemented("set_end_of_stream", 1);
#endif

    releaseStream(s);
  }

  return rc;
}



		/********************************
		*          STRING I/O           *
		*********************************/

extern IOFUNCTIONS Smemfunctions;

bool
tellString(char **s, size_t *size, IOENC enc)
{ GET_LD
  IOSTREAM *stream;

  stream = Sopenmem(s, size, "w");
  stream->encoding = enc;
  pushOutputContext(stream);

  return true;
}


bool
toldString(void)
{ GET_LD
  IOSTREAM *s = getStream(Scurout);

  if ( !s )
    return true;

  if ( s->functions == &Smemfunctions )
  { popOutputContext();
    closeStream(s);
  } else
    releaseStream(s);

  return true;
}


		/********************************
		*       WAITING FOR INPUT	*
		********************************/

#if defined(HAVE_SELECT) || defined(HAVE_POLL)
#define HAVE_PRED_WAIT_FOR_INPUT 1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows<->Unix note. This function uses the   Windows socket API for its
implementation and defines the Unix API  in   terms  of the Windows API.
This approach allows full support  of   the  restrictions of the Windows
implementation. Because the Unix emulation is   more generic, this still
supports  the  generic  facilities  of  Unix  select()  that  make  this
predicate work on pipes, serial devices, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __WINDOWS__
typedef int SOCKET;
#define INVALID_SOCKET -1
#define NFDS(max) (max+1)			/* see also S__wait() */
#else
#define NFDS(n) 0
#ifdef HAVE_WSAPOLL
#define HAVE_POLL 1
static inline int
poll(struct pollfd *pfd, int nfds, int timeout)
{ return WSAPoll(pfd, nfds, timeout);
}
#endif
#endif

typedef struct fdentry
{ SOCKET fd;
  term_t stream;
} fdentry;

#define FASTMAP_SIZE 32

#ifdef HAVE_POLL
#define ADD_FD(i) do { poll_map[i].fd = map[i].fd; \
		       poll_map[i].events = POLLIN; \
		     } while(0)
#define IS_SETFD(i) (poll_map[i].revents & (POLLIN|POLLERR|POLLHUP))
#define ACTION_WAIT ATOM_poll
#else
#define ADD_FD(i) do { FD_SET(map[i].fd, &fds); } while(0)
#define IS_SETFD(i) FD_ISSET(map[i].fd, &fds)
#define ACTION_WAIT ATOM_select
#endif

static
PRED_IMPL("wait_for_input", 3, wait_for_input, 0)
{ PRED_LD
  double time;
  fdentry map_buf[FASTMAP_SIZE];
  fdentry *map;
#ifdef HAVE_POLL
  struct pollfd poll_buf[FASTMAP_SIZE];
  struct pollfd *poll_map;
  int to;
#else
  SOCKET max = 0;
  fd_set fds;
  struct timeval t, *to;
#endif
  term_t head      = PL_new_term_ref();
  term_t streams   = PL_copy_term_ref(A1);
  term_t available = PL_copy_term_ref(A2);
  term_t ahead     = PL_new_term_ref();
  int from_buffer  = 0;
  atom_t a;
  size_t count;
  int i, nfds;
  int rc = false;

  term_t timeout = A3;

  switch ( PL_skip_list(A1, 0, &count) )
  { case PL_LIST:
      break;
    case PL_PARTIAL_LIST:
      return PL_instantiation_error(A1);
    default:
      return PL_type_error("list", A1);
  }

  if ( count <= FASTMAP_SIZE )
    map = map_buf;
  else if ( !(map = malloc(count*sizeof(*map))) )
    return PL_no_memory();
  memset(map, 0, count*sizeof(*map));

#ifdef HAVE_POLL
  if ( count <= FASTMAP_SIZE )
    poll_map = poll_buf;
  else if ( !(poll_map = malloc(count*sizeof(*poll_map))) )
    return PL_no_memory();
  memset(poll_map, 0, count*sizeof(*poll_map));
#else
#ifdef __WINDOWS__
  if ( count > FD_SETSIZE )
  { PL_representation_error("FD_SETSIZE");
    goto out;
  }
#endif
  FD_ZERO(&fds);
#endif

  for(nfds=0; PL_get_list(streams, head, streams); )
  { IOSTREAM *s;
    SOCKET fd;
    fdentry *e;
    int ifd;

    if ( PL_get_integer(head, &ifd) )
    { fd = ifd;
    } else
    { if ( !PL_get_stream(head, &s, SIO_INPUT) )
	goto out;

#ifdef __WINDOWS__
      if ( (fd = Swinsock(s)) == INVALID_SOCKET )
#else
      if ( (fd = Sfileno(s)) == INVALID_SOCKET )
#endif
      { releaseStream(s);
	PL_domain_error("waitable_stream", head);
	goto out;
      }
				/* check for input in buffer */
      if ( Spending(s) > 0 )
      { if ( !PL_unify_list(available, ahead, available) ||
	     !PL_unify(ahead, head) )
	{ releaseStream(s);
	  goto out;
	}
	from_buffer++;
      }

      releaseStream(s);			/* dubious, but what else? */
    }

    e	      = &map[nfds];
    e->fd     = fd;
    e->stream = PL_copy_term_ref(head);
    ADD_FD(nfds);
    nfds++;

#ifndef HAVE_POLL
    if ( fd > max )
      max = fd;
#endif
  }

  if ( from_buffer > 0 )
  { rc = PL_unify_nil(available);
    goto out;
  }

#ifdef HAVE_POLL
  if ( PL_get_atom(timeout, &a) && a == ATOM_infinite )
  { to = -1;
  } else if ( PL_is_integer(timeout) )
  { int i;

    if ( PL_get_integer(timeout, &i) )
    { if ( i <= 0 )
      { to = 0;
      } else if ( (int64_t)i*1000 <= INT_MAX )
      { to = i*1000;
      } else
      { PL_representation_error("timeout");
	goto out;
      }
    } else
    { PL_representation_error("timeout");
      goto out;
    }
  } else if ( PL_get_float_ex(timeout, &time) )
  { if ( time > 0.0 )
    { if ( time * 1000.0 <= (double)INT_MAX )
      { to = (int)(time*1000.0);
      } else
      { PL_domain_error("timeout", timeout);
	goto out;
      }
    } else
    { to = 0;
    }
  } else
    goto out;
#else /*HAVE_POLL*/
  if ( PL_get_atom(timeout, &a) && a == ATOM_infinite )
  { to = NULL;
  } else if ( PL_is_integer(timeout) )
  { long v;

    PL_get_long(timeout, &v);
    if ( v > 0L )
    { t.tv_sec = v;
      t.tv_usec = 0;
      to = &t;
    } else
    { t.tv_sec  = 0;
      t.tv_usec = 0;
      to = &t;
    }
  } else
  { if ( !PL_get_float_ex(timeout, &time) )
      goto out;

    if ( time >= 0.0 )
    { t.tv_sec  = (int)time;
      t.tv_usec = ((int)(time * 1000000) % 1000000);
    } else
    { t.tv_sec  = 0;
      t.tv_usec = 0;
    }
    to = &t;
  }
#endif /*HAVE_POLL*/

#ifdef HAVE_POLL
  while ( (rc=poll(poll_map, nfds, to)) == -1 &&
	   errno == EINTR )
  { if ( PL_handle_signals() < 0 )
      goto out;				/* exception */
  }
#else
#if defined(FD_SETSIZE) && !defined(__WINDOWS__)
  if ( max >= FD_SETSIZE )
  { PL_representation_error("FD_SETSIZE");
    goto out;
  }
#endif

  while( (rc=select(NFDS(max), &fds, NULL, NULL, to)) == -1 &&
	 errno == EINTR )
  { if ( PL_handle_signals() < 0 )
      goto out;				/* exception */

    FD_ZERO(&fds);			/* EINTR may leave fds undefined */
    for(i=0; i<count; i++)		/* so we rebuild it to be safe */
      FD_SET(map[i].fd, &fds);
  }
#endif

  switch(rc)
  { case -1:
      PL_error("wait_for_input", 3, MSG_ERRNO, ERR_FILE_OPERATION,
	       ACTION_WAIT, ATOM_stream, A1);
      goto out;

    case 0: /* Timeout */
      break;

    default: /* Something happend -> check fds */
    { for(i=0; i<nfds; i++)
      { if ( IS_SETFD(i) )
	{ if ( !PL_unify_list(available, ahead, available) ||
	       !PL_unify(ahead, map[i].stream) )
	    goto out;
	}
      }
      break;
    }
  }

  rc = PL_unify_nil(available);

out:
  if ( map != map_buf )
    free(map);
#ifdef HAVE_POLL
  if ( poll_map != poll_buf )
    free(poll_map);
#endif

  return rc;
}

#endif /* HAVE_SELECT */


		/********************************
		*      PROLOG CONNECTION        *
		*********************************/

#define MAX_PENDING SIO_BUFSIZE		/* 4096 */

static void
re_buffer(IOSTREAM *s, const char *from, size_t len)
{ if ( s->bufp < s->limitp )
  { size_t size = s->limitp - s->bufp;

    memmove(s->buffer, s->bufp, size);
    s->bufp = s->buffer;
    s->limitp = &s->bufp[size];
  } else
  { s->bufp = s->limitp = s->buffer;
  }

  memcpy(s->bufp, from, len);
  s->limitp = s->bufp + len;
}


#ifndef HAVE_MBSNRTOWCS
static size_t
mbsnrtowcs(wchar_t *dest, const char **src,
	   size_t nms, size_t len, mbstate_t *ps)
{ wchar_t c;
  const char *us = *src;
  const char *es = us+nms;
  size_t count = 0;

  assert(dest == NULL);			/* incomplete implementation */

  while(us<es)
  { size_t skip = mbrtowc(&c, us, es-us, ps);

    if ( skip == (size_t)-1 )		/* error */
    { DEBUG(1, Sdprintf("mbsnrtowcs(): bad multibyte seq\n"));
      return skip;
    }
    if ( skip == (size_t)-2 )		/* incomplete */
    { *src = us;
      return count;
    }

    count++;
    us += skip;
  }

  *src = us;
  return count;
}
#else
#if defined(HAVE_DECL_MBSNRTOWCS) && !HAVE_DECL_MBSNRTOWCS
size_t mbsnrtowcs(wchar_t *dest, const char **src,
		  size_t nms, size_t len, mbstate_t *ps);
#endif
#endif /*HAVE_MBSNRTOWCS*/

static int
skip_cr(IOSTREAM *s)
{ if ( s->flags&SIO_TEXT )
  { switch(s->newline)
    { case SIO_NL_DETECT:
	s->newline = SIO_NL_DOS;
	/*FALLTHROUGH*/
      case SIO_NL_DOS:
	return true;
    }
  }
  return false;
}

static int
get_ucs2(const char *us, int be)
{ if ( be )
    return ((us[0]&0xff)<<8)+(us[1]&0xff);
  else
    return ((us[1]&0xff)<<8)+(us[0]&0xff);
}


#define read_pending_input(input, list, tail, chars) \
	LDFUNC(read_pending_input, input, list, tail, chars)

static foreign_t
read_pending_input(DECL_LD term_t input, term_t list, term_t tail, int chars)
{ IOSTREAM *s;

#define ADD_CODE(c) \
	do \
	{ if ( likely(chars==false) ) \
	    addSmallIntList(&ctx, c); \
	  else \
	    addCharList(&ctx, c); \
	} while(0)

  if ( getInputStream(input, S_DONTCARE, &s) )
  { char buf[MAX_PENDING];
    ssize_t n;
    int64_t off0 = Stell64(s);
    IOPOS pos0;
    list_ctx ctx;

    if ( Sferror(s) )
      return streamStatus(s);

    n = Sread_pending(s, buf, sizeof(buf), SIO_RP_NOPOS);
    if ( n < 0 )			/* should not happen */
      return streamStatus(s);
    if ( n == 0 )			/* end-of-file */
    { return ( streamStatus(s) &&
	       PL_unify(list, tail) &&
	       PL_unify_nil(list) );
    }
    if ( s->position )
    { pos0 = *s->position;
    } else
    { memset(&pos0, 0, sizeof(pos0));	/* make compiler happy */
    }

    switch(s->encoding)
    { case ENC_OCTET:
      case ENC_ISO_LATIN_1:
      case ENC_ASCII:
      { ssize_t i;

	if ( !allocList(n, &ctx) )
	  goto failure;

	for(i=0; i<n; i++)
	{ int c = buf[i]&0xff;

	  if ( c == '\r' && skip_cr(s) )
	    continue;

	  if ( s->position )
	    S__fupdatefilepos_getc(s, c);

	  ADD_CODE(c);
	}
	if ( s->position )
	  s->position->byteno = pos0.byteno+n;

	break;
      }
      case ENC_ANSI:
      { size_t count, i;
	mbstate_t s0;
	const char *us = buf;
	const char *es = buf+n;

	if ( !s->mbstate )
	{ if ( !(s->mbstate = malloc(sizeof(*s->mbstate))) )
	  { PL_error(NULL, 0, NULL, ERR_NOMEM);
	    goto failure;
	  }
	  memset(s->mbstate, 0, sizeof(*s->mbstate));
	}
	s0 = *s->mbstate;
	count = mbsnrtowcs(NULL, &us, n, 0, &s0);
	if ( count == (size_t)-1 )
	{ Sseterr(s, SIO_WARN, "Illegal multibyte Sequence");
	  goto failure;
	}

	DEBUG(2, Sdprintf("Got %ld codes from %d bytes; incomplete: %ld\n",
			  count, n, es-us));

	if ( !allocList(count, &ctx) )
	  goto failure;

	for(us=buf,i=0; i<count; i++)
	{ wchar_t c;

	  us += mbrtowc(&c, us, es-us, s->mbstate);
	  if ( c == '\r' && skip_cr(s) )
	    continue;
	  if ( s->position )
	    S__fupdatefilepos_getc(s, c);

	  ADD_CODE(c);
	}
	if ( s->position )
	  s->position->byteno = pos0.byteno+us-buf;

	re_buffer(s, us, es-us);
	break;
      }
      case ENC_UTF8:
      { const char *us = buf;
	const char *es = buf+n;
	size_t count = 0, i;

	while(us<es)
	{ if ( !(us[0]&0x80) )
	  { count++;
	    us++;
	  } else
	  { int ex = UTF8_FBN(us[0]);

	    if ( ex >= 0 )
	    { const char *ec = us + ex + 1;

	      if ( ec <= es )
	      { count++;
		us=ec;
	      } else			/* incomplete multi-byte */
		break;
	    } else
	    { Sseterr(s, SIO_WARN, "Illegal multibyte Sequence");
	      goto failure;
	    }
	  }
	}

	DEBUG(2, Sdprintf("Got %ld codes from %d bytes; incomplete: %ld\n",
			  count, n, es-us));

	if ( !allocList(count, &ctx) )
	  goto failure;

	for(us=buf,i=0; i<count; i++)
	{ int c;

	  PL_utf8_code_point(&us, es, &c);
	  if ( c == '\r' && skip_cr(s) )
	    continue;
	  if ( s->position )
	    S__fupdatefilepos_getc(s, c);

	  ADD_CODE(c);
	}
	if ( s->position )
	  s->position->byteno = pos0.byteno+us-buf;

	re_buffer(s, us, es-us);
	break;
      }
      case ENC_UTF16BE:
      case ENC_UTF16LE:
#if SIZEOF_WCHAR_T == 2
      case ENC_WCHAR:
#endif
      { size_t count = 0;
	const char *us = buf;
	const char *es = buf+n;
	size_t done = 0, i;
	bool be;		/* big endian */

#if SIZEOF_WCHAR_T == 2
	if ( s->encoding == ENC_WCHAR )
	{ union
	  { wchar_t wc;
	    char c[2];
	  } t = { .wc = 'A' };
	  be = t.c[1] == 'A';
	} else
#endif
	{ be = s->encoding == ENC_UTF16BE;
	}

	while(us+2<=es)
	{ int c = get_ucs2(us, be);

	  us += 2;
	  if ( IS_UTF16_LEAD(c) )
	  { if ( us+2 <= es )
	    { int c2 = get_ucs2(us, be);

	      if ( IS_UTF16_TRAIL(c2) )
	      { count++;
		us += 2;
	      } else
	      { Sseterr(s, SIO_WARN, "Illegal UTF-16 surrogate pair");
		goto failure;
	      }
	    }
	  } else
	  { count++;
	  }
	}

	if ( !allocList(count, &ctx) )
	  goto failure;

	for(us=buf,i=0; i<count; i++)
	{ int c = get_ucs2(us, s->encoding == ENC_UTF16BE);

	  us += 2;
	  done += 2;
	  if ( c == '\r' && skip_cr(s) )
	    continue;

	  if ( IS_UTF16_LEAD(c) )
	  { int c2 = get_ucs2(us, s->encoding == ENC_UTF16BE);

	    done += 2;
	    us += 2;
	    c = utf16_decode(c, c2);
	  }

	  if ( s->position )
	    S__fupdatefilepos_getc(s, c);

	  ADD_CODE(c);
	}

	if ( s->position )
	  s->position->byteno = pos0.byteno+done;
	re_buffer(s, buf+done, n-done);
	break;
      }
#if SIZEOF_WCHAR_T != 2
      case ENC_WCHAR:
      { const pl_wchar_t *ws = (const pl_wchar_t*)buf;
	size_t count = (size_t)n/sizeof(pl_wchar_t);
	size_t done, i;

	if ( !allocList(count, &ctx) )
	  goto failure;

	for(i=0; i<count; i++)
	{ int c = ws[i];

	  if ( c == '\r' && skip_cr(s) )
	    continue;
	  if ( s->position )
	    S__fupdatefilepos_getc(s, c);

	  ADD_CODE(c);
	}

	done = count*sizeof(pl_wchar_t);
	if ( s->position )
	  s->position->byteno = pos0.byteno+done;
	re_buffer(s, buf+done, n-done);
	break;
      }
#endif
      case ENC_UNKNOWN:
      default:
	assert(0);
	return false;
    }

    if ( !unifyDiffList(list, tail, &ctx) )
      goto failure;

    releaseStream(s);
    return true;

  failure:
    Sseek64(s, off0, SIO_SEEK_SET);	/* TBD: error? */
    if ( s->position )
      *s->position = pos0;
    releaseStream(s);
    return false;
  }

  return false;
}


static
PRED_IMPL("read_pending_codes", 3, read_pending_codes, 0)
{ PRED_LD

  return read_pending_input(A1, A2, A3, false);
}


static
PRED_IMPL("read_pending_chars", 3, read_pending_chars, 0)
{ PRED_LD

  return read_pending_input(A1, A2, A3, true);
}


/** peek_string(+Stream, +Len, -String) is det.

Peek input from Stream for  Len  characters   or  the  entire content of
Stream.
*/

static
PRED_IMPL("peek_string", 3, peek_string, 0)
{ PRED_LD
  IOSTREAM *s;
  size_t len;

  if ( !PL_get_size_ex(A2, &len) )
    return false;

  if ( getInputStream(A1, S_DONTCARE, &s) )
  { for(;;)
    { if ( s->limitp > s->bufp )
      { PL_chars_t text;

	text.text.t    = s->bufp;
	text.length    = s->limitp - s->bufp;
	text.storage   = PL_CHARS_HEAP;
	text.canonical = false;
	text.encoding  = s->encoding;

	if ( !PL_canonicalise_text_ex(&text) )
	{ releaseStream(s);
	  return false;
	}
	if ( text.length >= len )
	{ int rc = PL_unify_text_range(A3, &text, 0, len, PL_STRING);
	  PL_free_text(&text);
	  releaseStream(s);
	  return rc;
	}

	PL_free_text(&text);
      }

      if ( s->limitp - s->bufp == s->bufsize )
	Ssetbuffer(s, NULL, s->bufsize*2);

      if ( S__fillbuf(s) < 0 )
      { PL_chars_t text;
	int rc;

	if ( Sferror(s) )
	  return streamStatus(s);
	s->flags &= ~SIO_FEOF;

	text.text.t    = s->bufp;
	text.length    = s->limitp - s->bufp;
	text.storage   = PL_CHARS_HEAP;
	text.canonical = false;
	text.encoding  = s->encoding;

	PL_STRINGS_MARK();
	rc = ( PL_canonicalise_text_ex(&text) &&
	       PL_unify_text(A3, 0, &text, PL_STRING) );
	PL_STRINGS_RELEASE();
	releaseStream(s);
	return rc;
      }
      s->bufp--;
    }
  }

  return false;
}


#define put_byte(stream, byte) LDFUNC(put_byte, stream, byte)
static foreign_t
put_byte(DECL_LD term_t stream, term_t byte)
{ IOSTREAM *s;
  int c;

  if ( !PL_get_integer(byte, &c) || c < 0 || c > 255 )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_byte, byte);
  if ( !getBinaryOutputStream(stream, &s) )
    return false;

  Sputc(c, s);

  return streamStatus(s);
}


static
PRED_IMPL("put_byte", 2, put_byte2, 0)
{ PRED_LD

  return put_byte(A1, A2);
}


static
PRED_IMPL("put_byte", 1, put_byte1, 0)
{ PRED_LD

  return put_byte(0, A1);
}


#define put_code(stream, chr) LDFUNC(put_code, stream, chr)
static foreign_t
put_code(DECL_LD term_t stream, term_t chr)
{ IOSTREAM *s;
  int c = 0;

  if ( !PL_get_char(chr, &c, false) )
    return false;
  if ( !getTextOutputStream(stream, &s) )
    return false;

  Sputcode(c, s);

  return streamStatus(s);
}


static
PRED_IMPL("put_code", 2, put_code2, 0)
{ PRED_LD

  return put_code(A1, A2);
}


static
PRED_IMPL("put_code", 1, put_code1, 0)
{ PRED_LD

  return put_code(0, A1);
}


static
PRED_IMPL("put", 2, put2, 0)
{ PRED_LD

  return put_code(A1, A2);
}


static
PRED_IMPL("put", 1, put1, 0)
{ PRED_LD

  return put_code(0, A1);
}


#define get_nonblank(in, chr) LDFUNC(get_nonblank, in, chr)
static foreign_t
get_nonblank(DECL_LD term_t in, term_t chr)
{ IOSTREAM *s;

  if ( getTextInputStream(in, &s) )
  { int c;

    for(;;)
    { c = Sgetcode(s);

      if ( c == EOF )
      { TRY(PL_unify_integer(chr, -1));
	return streamStatus(s);
      }

      if ( !isBlankW(c) )
      { releaseStream(s);
	return PL_unify_integer(chr, c);
      }
    }
  }

  return false;
}


static
PRED_IMPL("get", 1, get1, 0)
{ PRED_LD

  return get_nonblank(0, A1);
}


static
PRED_IMPL("get", 2, get2, 0)
{ PRED_LD

  return get_nonblank(A1, A2);
}


#define skip(in, chr) LDFUNC(skip, in, chr)
static foreign_t
skip(DECL_LD term_t in, term_t chr)
{ int c = -1;
  int r;
  IOSTREAM *s;

  if ( !PL_get_char(chr, &c, false) )
    return false;
  if ( !getTextInputStream(in, &s) )
    return false;

  while((r=Sgetcode(s)) != c && r != EOF )
    ;

  return streamStatus(s);
}


static
PRED_IMPL("skip", 1, skip1, 0)
{ PRED_LD

  return skip(0, A1);
}


static
PRED_IMPL("skip", 2, skip2, 0)
{ PRED_LD

  return skip(A1, A2);
}


static
PRED_IMPL("get_single_char", 1, get_single_char, 0)
{ GET_LD
  IOSTREAM *s = getStream(Suser_input);
  int c;

  if ( !s )
    return symbol_no_stream(ATOM_user_input);

  c = getSingleChar(s, true);
  if ( c == EOF )
  { if ( PL_exception(0) )
    { releaseStream(s);
      return false;
    }

    PL_unify_integer(A1, -1);
    return streamStatus(s);
  }

  releaseStream(s);

  return PL_unify_integer(A1, c);
}


#define get_byte2(in, chr) LDFUNC(get_byte2, in, chr)
static foreign_t
get_byte2(DECL_LD term_t in, term_t chr)
{ IOSTREAM *s;

  if ( getBinaryInputStream(in, &s) )
  { int c = Sgetc(s);

    if ( PL_unify_integer(chr, c) )
      return streamStatus(s);

    if ( Sferror(s) )
      return streamStatus(s);

    PL_get_char(chr, &c, true);		/* set type-error */
  }

  return false;
}


static
PRED_IMPL("get_byte", 2, get_byte2, 0)
{ PRED_LD

  return get_byte2(A1, A2);
}


static
PRED_IMPL("get_byte", 1, get_byte1, 0)
{ PRED_LD

  return get_byte2(0, A1);
}


#define get_code2(in, chr) LDFUNC(get_code2, in, chr)
static foreign_t
get_code2(DECL_LD term_t in, term_t chr)
{ IOSTREAM *s;

  if ( getTextInputStream(in, &s) )
  { int c = Sgetcode(s);

    if ( !streamStatus(s) )		/* I/O error */
      return false;

    if ( PL_unify_integer(chr, c) )
      return true;

    PL_get_char(chr, &c, true);		/* set type-error */
  }

  return false;
}


static
PRED_IMPL("get_code", 2, get_code2, 0)
{ PRED_LD
  return get_code2(A1, A2);
}


static
PRED_IMPL("get_code", 1, get_code1, 0)
{ PRED_LD
  return get_code2(0, A1);
}


#define get_char2(in, chr) LDFUNC(get_char2, in, chr)
static foreign_t
get_char2(DECL_LD term_t in, term_t chr)
{ IOSTREAM *s;

  if ( getTextInputStream(in, &s) )
  { int c = Sgetcode(s);

    if ( !streamStatus(s) )		/* I/O error */
      return false;

    if ( PL_unify_atom(chr, c == -1 ? ATOM_end_of_file : codeToAtom(c)) )
      return true;

    PL_get_char(chr, &c, true);		/* set type-error */
  }

  return false;
}


static
PRED_IMPL("get_char", 2, get_char2, 0)
{ PRED_LD
  return get_char2(A1, A2);
}


static
PRED_IMPL("get_char", 1, get_char1, 0)
{ PRED_LD
  return get_char2(0, A1);
}


static
PRED_IMPL("ttyflush", 0, ttyflush, 0)
{ PRED_LD
  IOSTREAM *s = getStream(Suser_output);

  if ( s )
  { Sflush(s);

    return streamStatus(s);
  }

  return symbol_no_stream(ATOM_user_output);
}


static
PRED_IMPL("protocol", 1, protocol, 0)
{ return openProtocol(A1, false);
}


static
PRED_IMPL("protocola", 1, protocola, 0)
{ return openProtocol(A1, true);
}


static
PRED_IMPL("protocolling", 1, protocolling, 0)
{ PRED_LD
  IOSTREAM *s;

  if ( (s = Sprotocol) )
  { atom_t a;

    if ( (a = fileNameStream(s)) )
      return PL_unify_atom(A1, a);
    else
      return PL_unify_stream_or_alias(A1, s);
  }

  return false;
}


static
PRED_IMPL("prompt", 2, prompt, 0)
{ PRED_LD
  atom_t a;

  term_t old = A1;
  term_t new = A2;

  if ( !PL_unify_atom(old, LD->prompt.current) )
    return false;
  if ( PL_compare(A1,A2) == 0 )
    return true;

  if ( PL_get_atom_ex(new, &a) )
  { if ( LD->prompt.current )
      PL_unregister_atom(LD->prompt.current);
    LD->prompt.current = a;
    PL_register_atom(a);
    return true;
  }

  return false;
}


void
prompt1(atom_t prompt)
{ GET_LD

  if ( LD->prompt.first != prompt )
  { if ( LD->prompt.first )
      PL_unregister_atom(LD->prompt.first);
    LD->prompt.first = prompt;
    PL_register_atom(LD->prompt.first);
  }

  LD->prompt.first_used = false;
}


static
PRED_IMPL("prompt1", 1, prompt1, 0)
{ PRED_LD
  atom_t a;
  PL_chars_t txt;

  if ( PL_get_atom(A1, &a) )
  { prompt1(a);
  } else if ( PL_is_variable(A1) )
  { return LD->prompt.first && PL_unify_atom(A1, LD->prompt.first);
  } else if ( PL_get_text(A1, &txt,  CVT_ALL|CVT_EXCEPTION) )
  { prompt1(textToAtom(&txt));
  } else
    return false;

  return true;
}


atom_t
PrologPrompt(void)
{ GET_LD
  IOSTREAM *in;

  if ( !LD->prompt.first_used && LD->prompt.first )
  { LD->prompt.first_used = true;

    return LD->prompt.first;
  }

  if ( (in=Suser_input) &&
       in->position &&
       in->position->linepos == 0 )
    return LD->prompt.current;
  else
    return 0;				/* "" */
}


#define tab(out, spaces) LDFUNC(tab, out, spaces)
static int
tab(DECL_LD term_t out, term_t spaces)
{ int64_t count;
  IOSTREAM *s;

  if ( !getTextOutputStream(out, &s) )
    return false;
  if ( !PL_eval_expression_to_int64_ex(spaces, &count) )
    return false;

  while(count-- > 0)
  { if ( Sputcode(' ', s) < 0 )
      break;
  }

  return streamStatus(s);
}


static
PRED_IMPL("tab", 2, tab2, 0)
{ PRED_LD

  return tab(A1, A2);
}

static
PRED_IMPL("tab", 1, tab1, 0)
{ PRED_LD

  return tab(0, A1);
}


		 /*******************************
		 *	      ENCODING		*
		 *******************************/

static struct encname
{ IOENC  code;
  atom_t name;
} encoding_names[] =
{ { ENC_UNKNOWN,     ATOM_unknown },
  { ENC_OCTET,       ATOM_octet },
  { ENC_ASCII,       ATOM_ascii },
  { ENC_ISO_LATIN_1, ATOM_iso_latin_1 },
  { ENC_ANSI,	     ATOM_text },
  { ENC_UTF8,        ATOM_utf8 },
  { ENC_UTF16BE,     ATOM_utf16be },
  { ENC_UTF16LE,     ATOM_utf16le },
  { ENC_WCHAR,	     ATOM_wchar_t },
					/* Aliases */
  { ENC_ISO_LATIN_1, ATOM_ISO_8859_1 },
  { ENC_UTF8,        ATOM_UTF_8 },
  { ENC_UTF16BE,     ATOM_unicode_be },
  { ENC_UTF16LE,     ATOM_unicode_le },
  { ENC_UTF16BE,     ATOM_UTF_16BE },
  { ENC_UTF16LE,     ATOM_UTF_16LE },
  { ENC_UNKNOWN,     0 },
};


IOENC
PL_atom_to_encoding(atom_t a)
{ struct encname *en;

  for(en=encoding_names; en->name; en++)
  { if ( en->name == a )
      return en->code;
  }

  return ENC_UNKNOWN;
}


atom_t
PL_encoding_to_atom(IOENC enc)
{ if ( (int)enc > 0 && (int)enc <= ENC_WCHAR )
    return encoding_names[enc].name;
  return NULL_ATOM;
}


static int
bad_encoding(const char *msg, atom_t name)
{ GET_LD
  term_t t = PL_new_term_ref();

  PL_put_atom(t, name);
  return PL_error(NULL, 0, msg, ERR_DOMAIN, ATOM_encoding, t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
file_name_to_atom() translates a 8-bit filename into a unicode atom. The
encoding is generic `multibyte' on Unix systems   and  fixed to UTF-8 on
Windows, where the uxnt layer  translates   the  UTF-8  sequences to the
Windows *W() functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

atom_t
file_name_to_atom(const char *fn)
{ GET_LD
  PL_chars_t text;
  atom_t a;

  text.text.t    = (char *)fn;
  text.encoding  = ((REP_FN&REP_UTF8) ? ENC_UTF8 :
		    (REP_FN&REP_MB)   ? ENC_ANSI : ENC_ISO_LATIN_1);
  text.storage   = PL_CHARS_HEAP;
  text.length    = strlen(fn);
  text.canonical = false;

  a = textToAtom(&text);
  PL_free_text(&text);

  return a;
}


		 /*******************************
		 *	     IRI HOOKS		*
		 *******************************/

int /* returns length of iri scheme, i.e., 4 for `http://` */
file_name_is_iri(const char *path)
{ const char *s;

  for(s=path; *s >= 'a' && *s <= 'z'; )
    s++;
  if ( s >= path+2 &&				/* >= two letter scheme */
       s[0] == ':' && s[1] == '/' && s[2] == '/' )
    return s-path;

  return 0;
}


static int
call_iri_hook(term_t argv, iri_op op, va_list args)
{ GET_LD

  if ( !GD->procedures.iri_hook4 )
    GD->procedures.iri_hook4 = PL_predicate("iri_hook", 4, "$iri");

  if ( !hasClausesDefinition(GD->procedures.iri_hook4->definition) )
  { sysError("IRI scheme handler not yet installed");
    return false;
  }

  switch(op)
  { case IRI_OPEN:
    { atom_t mode    = va_arg(args, atom_t);
      term_t options = va_arg(args, term_t);

      if ( !options )
      { options = PL_new_term_ref();
	PL_put_nil(options);
      }

      if ( !PL_unify_term(argv+2,
			  PL_FUNCTOR, FUNCTOR_open2,
			    PL_ATOM, mode,
			    PL_TERM, options) )
	return false;
      break;
    }
    case IRI_ACCESS:
    { int md = va_arg(args, int);
      atom_t mode;

      switch(md)
      { case ACCESS_WRITE:     mode = ATOM_write;     break;
	case ACCESS_READ:      mode = ATOM_read;      break;
	case ACCESS_EXECUTE:   mode = ATOM_execute;   break;
	case ACCESS_EXIST:     mode = ATOM_exist;     break;
	case ACCESS_FILE:      mode = ATOM_file;      break;
	case ACCESS_DIRECTORY: mode = ATOM_directory; break;
	default: assert(0); return false;
      }
      if ( !PL_unify_term(argv+2,
			  PL_FUNCTOR, FUNCTOR_access1,
			    PL_ATOM, mode) )
	return false;
      break;
    }
    case IRI_TIME:
      if ( !PL_put_atom(argv+2, ATOM_time) )
	return false;
      break;
    case IRI_SIZE:
      if ( !PL_put_atom(argv+2, ATOM_size) )
	return false;
      break;
    default:
      assert(0);
      return false;
  }

  if ( PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION,
			 GD->procedures.iri_hook4, argv) )
  { switch(op)
    { case IRI_OPEN:
      {	IOSTREAM **vp = va_arg(args, IOSTREAM**);
	return PL_get_stream(argv+3, vp, (SIO_INPUT|SIO_OUTPUT));
      }
      case IRI_ACCESS:
      { int *vp = va_arg(args, int*);
	return PL_get_bool_ex(argv+3, vp);
      }
      case IRI_TIME:
      { double *vp = va_arg(args, double*);
	return PL_get_float_ex(argv+3, vp);
      }
      case IRI_SIZE:
      { int64_t *vp = va_arg(args, int64_t*);
	return PL_get_int64_ex(argv+3, vp);
      }
      default:
	assert(0);
	return false;
    }
  } else
  { return false;
  }
}


static int
iri_hook_va(const char *url, iri_op op, va_list args)
{ GET_LD
  fid_t fid;
  const char *escheme = strchr(url, ':');

  if ( (fid = PL_open_foreign_frame()) )
  { term_t argv;
    int rc;

    rc = ( (argv = PL_new_term_refs(4)) &&
	   PL_put_atom_nchars(argv+0, escheme-url, url) &&
	   PL_unify_chars(argv+1, PL_STRING|REP_FN, (size_t)-1, url) &&
	   call_iri_hook(argv, op, args) );

    PL_close_foreign_frame(fid);

    return rc;
  }

  return false;
}

int
iri_hook(const char *url, iri_op op, ...)
{ int rc;
  va_list args;

  va_start(args, op);
  rc = iri_hook_va(url, op, args);
  va_end(args);

  return rc;
}

		/********************************
		*       STREAM BASED I/O        *
		*********************************/

static const PL_option_t open4_options[] =
{ { ATOM_type,		 OPT_ATOM },
  { ATOM_reposition,     OPT_BOOL },
  { ATOM_alias,		 OPT_ATOM },
  { ATOM_eof_action,     OPT_ATOM },
  { ATOM_close_on_abort, OPT_BOOL },
  { ATOM_buffer,	 OPT_ATOM },
  { ATOM_lock,		 OPT_ATOM },
  { ATOM_wait,		 OPT_BOOL },
  { ATOM_encoding,	 OPT_ATOM },
  { ATOM_newline,	 OPT_ATOM },
  { ATOM_bom,		 OPT_BOOL },
  { ATOM_create,	 OPT_TERM },
#ifdef O_LOCALE
  { ATOM_locale,	 OPT_LOCALE },
#endif
  { NULL_ATOM,		 0 }
};


int
stream_encoding_options(atom_t type, atom_t encoding, int *bom, IOENC *enc)
{ GET_LD

  if ( encoding != NULL_ATOM )
  { *enc = PL_atom_to_encoding(encoding);

    if ( *enc == ENC_UNKNOWN )
      return bad_encoding(NULL, encoding);
    if ( type == ATOM_binary && *enc != ENC_OCTET )
      return bad_encoding("type(binary) implies encoding(octet)", encoding);

    switch(*enc)			/* explicitely specified: do not */
    { case ENC_OCTET:			/* switch to Unicode.  For implicit */
      case ENC_ASCII:			/* and unicode types we must detect */
      case ENC_ISO_LATIN_1:		/* and skip the BOM */
      case ENC_WCHAR:
	*bom = false;
	break;
      default:
	;
    }
  } else if ( type == ATOM_binary )
  { *enc = ENC_OCTET;
    *bom = false;
  } else if ( type == ATOM_text )
  { *enc = LD->encoding;
  } else
  { term_t ex;

    return ( (ex = PL_new_term_ref()) &&
	     PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_type1, PL_ATOM, type) &&
	     PL_domain_error("stream_option", ex)
	   );
  }

  return true;
}

/* MT: openStream() must be called unlocked */
#define SIO_NL_UNDEF 4		/* not one of SIO_NL_* */

IOSTREAM *
openStream(term_t file, term_t mode, term_t options)
{ GET_LD
  atom_t mname;
  atom_t type           = ATOM_text;
  int    reposition     = true;
  atom_t alias		= NULL_ATOM;
  atom_t eof_action     = ATOM_eof_code;
  atom_t buffer         = ATOM_full;
  atom_t lock		= ATOM_none;
  atom_t newline	= 0;
  unsigned int fnewline = SIO_NL_UNDEF;
  int	 wait		= true;
  atom_t encoding	= NULL_ATOM;
#ifdef O_LOCALE
  PL_locale *locale	= NULL;
#endif
  int    close_on_abort = true;
  int	 bom		= -1;
  term_t create		= 0;
  char   how[16];
  char  *h		= how;
  char *path;
  IOSTREAM *s;
  IOENC enc;

#ifdef O_LOCALE
#define LOCALE_ARG , &locale
#else
#define LOCALE_ARG
#endif

  if ( options )
  { if ( !PL_scan_options(options, 0, "stream_option", open4_options,
			  &type, &reposition, &alias, &eof_action,
			  &close_on_abort, &buffer, &lock, &wait,
			  &encoding, &newline, &bom, &create
			  LOCALE_ARG) )
      return false;
  }

					/* MODE */
  if ( PL_get_atom_ex(mode, &mname) )
  { if ( mname == ATOM_write )
    { *h++ = 'w';
    } else if ( mname == ATOM_append )
    { *h++ = 'a';
    } else if ( mname == ATOM_update )
    { *h++ = 'u';
    } else if ( mname == ATOM_read )
    { *h++ = 'r';
    } else
    { PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_io_mode, mode);
      return NULL;
    }
  } else
  { return NULL;
  }
  if ( create )
  { term_t tail = PL_copy_term_ref(create);
    term_t head = PL_new_term_ref();
    int mode = 0;
    int n = 0;

    while(PL_get_list(tail, head, tail))
    { atom_t a;

      if ( !PL_get_atom_ex(head, &a) )
	return false;
      if ( a == ATOM_read )
	mode |= 0444;
      else if ( a == ATOM_write )
	mode |= 0666;
      else if ( a == ATOM_execute )
	mode |= 0111;
      else if ( a == ATOM_default )
	mode |= 0666;
      else if ( a == ATOM_all )
	mode |= 0777;

      if ( ++n == 10 && PL_skip_list(tail, 0, NULL) != PL_LIST )
      { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, create);
	return NULL;
      }
    }
    if ( !PL_get_nil_ex(tail) )
      return false;
    *h++ = 'm';
    *h++ = ((mode >> 6) & 07) + '0';
    *h++ = ((mode >> 3) & 07) + '0';
    *h++ = ((mode >> 0) & 07) + '0';
  }

  if ( !stream_encoding_options(type, encoding, &bom, &enc) )
    return NULL;
  if ( newline && type != ATOM_binary )
  { if ( !atom_to_newline_mode(newline,
			       how[0] == 'r' ? SIO_INPUT : SIO_OUTPUT,
			       &fnewline) )
      return false;
  }

  if ( bom == -1 )
    bom = (mname == ATOM_read ? true : false);
  if ( type == ATOM_binary )
    *h++ = 'b';

					/* File locking */
  if ( lock != ATOM_none )
  { *h++ = (wait ? 'l' : 'L');
    if ( lock == ATOM_read || lock == ATOM_shared )
      *h++ = 'r';
    else if ( lock == ATOM_write || lock == ATOM_exclusive )
      *h++ = 'w';
    else
    { term_t l = PL_new_term_ref();
      PL_put_atom(l, lock);
      PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_lock, l);
      return NULL;
    }
  }

  *h = EOS;

  if ( alias != NULL_ATOM &&
       streamAliases &&
       lookupHTableWP(streamAliases, alias) )
  { term_t aliast;

    if ( (aliast = PL_new_term_ref()) &&
	 PL_unify_term(aliast,
		       PL_FUNCTOR, FUNCTOR_alias1,
			 PL_ATOM, alias) )
      PL_error(NULL, 0, NULL, ERR_PERMISSION,
	       ATOM_open, ATOM_source_sink, aliast);

    return NULL;
  }

					/* FILE */
#ifdef HAVE_POPEN
  if ( PL_is_functor(file, FUNCTOR_pipe1) )
  { term_t a;
    char *cmd;

    PL_clear_exception();
    a = PL_new_term_ref();
    _PL_get_arg(1, file, a);
    if ( !PL_get_chars(a, &cmd, CVT_ATOM|CVT_STRING|REP_FN) )
    { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, a);
      return NULL;
    }

    if ( !(s = Sopen_pipe(cmd, how)) )
    { PL_error(NULL, 0, MSG_ERRNO, ERR_FILE_OPERATION,
	       ATOM_open, ATOM_source_sink, file);
      return NULL;
    }
  } else
#endif /*HAVE_POPEN*/
  if ( PL_get_file_name(file, &path, 0) )
  { int sl;

    if ( (sl=file_name_is_iri(path)) )
    { if ( !iri_hook(path, IRI_OPEN, mname, options, &s) )
	goto error;
    } else
    { s = Sopen_file(path, how);
    }

    if ( s == NULL )
    { error:
      if ( !PL_exception(0) )
	PL_error(NULL, 0, MSG_ERRNO, ERR_FILE_OPERATION,
		 ATOM_open, ATOM_source_sink, file);
      return NULL;
    }
    setFileNameStream_unlocked(s, file_name_to_atom(path));
  } else
  { return NULL;
  }

  s->encoding = enc;
  if ( fnewline != SIO_NL_UNDEF )
    s->newline = fnewline&0x3;
#ifdef O_LOCALE
  if ( locale )
  { Ssetlocale(s, locale, NULL);
    releaseLocale(locale);			/* acquired by PL_scan_options() */
  }
#endif
  if ( !close_on_abort )
    s->flags |= SIO_NOCLOSE;

  if ( how[0] == 'r' )
  { if ( !set_eof_action(s, eof_action) )
    { Sclose(s);
      return NULL;
    }
  } else
  { if ( buffer != ATOM_full &&
	 !set_buffering(s, buffer) )
    { Sclose(s);
      return NULL;
    }
  }

  if ( alias != NULL_ATOM )
  { PL_LOCK(L_FILE);
    aliasStream(s, alias);
    PL_UNLOCK(L_FILE);
  }
  if ( !reposition )
    s->position = NULL;

  if ( bom )
  { if ( mname == ATOM_read )
    { if ( ScheckBOM(s) < 0 )
      { bom_error:

	streamStatus(getStream(s));
	Sclose(s);
	return NULL;
      }
    } else
    { if ( mname == ATOM_write ||
	   ( (mname == ATOM_append || mname == ATOM_update) &&
	     Ssize(s) == 0 ) )
      { if ( SwriteBOM(s) < 0 )
	  goto bom_error;
      }
    }
  }

  return s;
}


static
PRED_IMPL("open", 4, open4, PL_FA_ISO)
{ IOSTREAM *s = openStream(A1, A2, A4);

  if ( s )
    return PL_unify_stream_or_alias(A3, s);

  return false;
}


static
PRED_IMPL("open", 3, open3, PL_FA_ISO)
{ IOSTREAM *s = openStream(A1, A2, 0);

  if ( s )
    return PL_unify_stream_or_alias(A3, s);

  return false;
}

		 /*******************************
		 *	  EDINBURGH I/O		*
		 *******************************/

static IOSTREAM *
findStreamFromFile(atom_t name, unsigned int flags)
{ TableEnum e;
  IOSTREAM *s = NULL;

  e = newTableEnumPP(streamContext);
  table_key_t tk;
  table_value_t tv;
  while( advanceTableEnum(e, &tk, &tv) )
  { IOSTREAM *s0 = key2ptr(tk);
    stream_context *ctx = val2ptr(tv);

    if ( ctx->filename == name &&
	 ison(ctx, flags) )
    { s = s0;
      break;
    }
  }
  freeTableEnum(e);

  return s;
}


int
pl_see(term_t f)
{ GET_LD
  IOSTREAM *s;
  atom_t a;
  term_t mode;

  if ( !PL_get_atom_ex(f, &a) )
    return false;

  PL_LOCK(L_SEETELL);
  if ( get_stream_handle(a, &s, SH_ALIAS|SH_UNLOCKED) )
  { Scurin = s;
    goto ok;
  }
  if ( a == ATOM_user )
  { Scurin = Suser_input;
    goto ok;
  }
  if ( (s = findStreamFromFile(a, IO_SEE)) )
  { Scurin = s;
    goto ok;
  }

  mode = PL_new_term_ref();
  PL_put_atom(mode, ATOM_read);
  if ( !(s = openStream(f, mode, 0)) )
  { PL_UNLOCK(L_SEETELL);
    return false;
  }

  set(getStreamContext(s), IO_SEE);
  push_input_context(ATOM_see);
  Scurin = s;

ok:
  PL_UNLOCK(L_SEETELL);

  return true;
}

int
pl_seen(void)
{ GET_LD
  IOSTREAM *s = getStream(Scurin);

  pop_input_context();

  if ( s && s->flags & SIO_NOFEOF )
    return true;

  if ( s )
    return closeStream(s);

  return symbol_no_stream(ATOM_current_input);
}

static
PRED_IMPL("see", 1, see, 0)
{ return pl_see(A1);
}


static
PRED_IMPL("seen", 0, seen, 0)
{ return pl_seen();
}


static
PRED_IMPL("seeing", 1, seeing, 0)
{ PRED_LD

  if ( Scurin == Suser_input )
    return PL_unify_atom(A1, ATOM_user);

  return PL_unify_stream(A1, Scurin);
}


/* MT: Does not create a lock on the stream
*/

static int
do_tell(term_t f, atom_t m)
{ GET_LD
  IOSTREAM *s;
  atom_t a;
  term_t mode;

  if ( !PL_get_atom_ex(f, &a) )
    return false;

  PL_LOCK(L_SEETELL);
  if ( get_stream_handle(a, &s, SH_UNLOCKED) )
  { setStandardStream(SNO_CURRENT_OUTPUT, s);
    goto ok;
  }
  if ( a == ATOM_user )
  { setStandardStream(SNO_CURRENT_OUTPUT, Suser_output);
    goto ok;
  }
  if ( (s = findStreamFromFile(a, IO_TELL)) )
  { setStandardStream(SNO_CURRENT_OUTPUT, s);
    goto ok;
  }

  mode = PL_new_term_ref();
  PL_put_atom(mode, m);
  if ( !(s = openStream(f, mode, 0)) )
  { PL_UNLOCK(L_SEETELL);
    return false;
  }

  set(getStreamContext(s), IO_TELL);
  pushOutputContext(s);

ok:
  PL_UNLOCK(L_SEETELL);
  return true;
}

static
PRED_IMPL("tell", 1, tell, 0)
{ return do_tell(A1, ATOM_write);
}

static
PRED_IMPL("append", 1, append, 0)
{ return do_tell(A1, ATOM_append);
}

static
PRED_IMPL("telling", 1, telling, 0)
{ PRED_LD

  if ( Scurout == Suser_output )
    return PL_unify_atom(A1, ATOM_user);

  return PL_unify_stream(A1, Scurout);
}

static
PRED_IMPL("told", 0, told, 0)
{ PRED_LD
  IOSTREAM *s = getStream(Scurout);

  popOutputContext();

  if ( s && s->flags & SIO_NOFEOF )
    return true;

  if ( s )
    return closeStream(s);

  return symbol_no_stream(ATOM_current_output);
}

		 /*******************************
		 *	   NULL-STREAM		*
		 *******************************/

static ssize_t
Swrite_null(void *handle, char *buf, size_t size)
{ (void)handle;
  (void)buf;

  return size;
}


static ssize_t
Sread_null(void *handle, char *buf, size_t size)
{ (void)handle;
  (void)buf;
  (void)size;

  return 0;
}


static long
Sseek_null(void *handle, long offset, int whence)
{ (void)handle;

  switch(whence)
  { case SIO_SEEK_SET:
	return offset;
    case SIO_SEEK_CUR:
    case SIO_SEEK_END:
    default:
	return -1;
  }
}


static int
Sclose_null(void *handle)
{ (void)handle;

  return 0;
}


static const IOFUNCTIONS nullFunctions =
{ Sread_null,
  Swrite_null,
  Sseek_null,
  Sclose_null
};


static
PRED_IMPL("open_null_stream", 1, open_null_stream, 0)
{ int sflags = SIO_NBUF|SIO_RECORDPOS|SIO_OUTPUT|SIO_TEXT;
  IOSTREAM *s = Snew((void *)NULL, sflags, (IOFUNCTIONS *)&nullFunctions);

  if ( s )
  { s->encoding = ENC_UTF8;
    return PL_unify_stream_or_alias(A1, s);
  }

  return false;
}


static int
do_close(IOSTREAM *s, int force)
{ if ( force )
  { if ( !s )
      return true;
    if ( s == Sinput )
    { Sclearerr(s);
    } else if ( s == Soutput || s == Serror )
    { Sflush(s);
      Sclearerr(s);
    } else
    { Sflush(s);
      if ( Sclose(s) < 0 )
	PL_clear_exception();
    }

    return true;
  } else if ( s )
  { return closeStream(s);
  } else
  { return false;
  }
}


#define pl_close(stream, force) LDFUNC(pl_close, stream, force)
static int
pl_close(DECL_LD term_t stream, int force)
{ IOSTREAM *s;
  atom_t a;
  stream_ref *ref;
  PL_blob_t *type;

  if ( !PL_get_atom(stream, &a) )
    return not_a_stream(stream);

  ref = PL_blob_data(a, NULL, &type);
  if ( type == &stream_blob )		/* close(Stream[pair], ...) */
  { int rc = true;

    if ( ref->read && ref->write )
    { assert(ref->read->references);
      assert(ref->write->references);
      if ( ref->write && !ref->write->erased )
	rc = do_close(getStream(ref->write), force);
      if ( ref->read && !ref->read->erased )
	rc = do_close(getStream(ref->read), force) && rc;
    } else
    { if ( ref->read )
      { assert(ref->read->references);
	rc = do_close(getStream(ref->read), force);
      } else if ( ref->write )
      { assert(ref->write->references);
	rc = do_close(getStream(ref->write), force);
      }
    }

    if ( rc == false && !PL_exception(0) )
      rc = PL_error(NULL, 0, "already closed",
		    ERR_EXISTENCE, ATOM_stream, stream);



    return rc;
  }

					/* close(Alias, ...) */
  if ( get_stream_handle(a, &s, SH_ERRORS|SH_ALIAS) )
    return do_close(s, force);

  return false;
}


static
PRED_IMPL("close", 1, close, PL_FA_ISO)
{ PRED_LD

  return pl_close(A1, false);
}


static const PL_option_t close2_options[] =
{ { ATOM_force,		 OPT_BOOL },
  { NULL_ATOM,		 0 }
};


static
PRED_IMPL("close", 2, close2, PL_FA_ISO)
{ PRED_LD
  int force = false;

  if ( !PL_scan_options(A2, 0, "close_option", close2_options, &force) )
    return false;

  return pl_close(A1, force);
}


		 /*******************************
		 *	 STREAM-PROPERTY	*
		 *******************************/

#define stream_file_name_propery(s, prop) LDFUNC(stream_file_name_propery, s, prop)
static int
stream_file_name_propery(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t name;

  for(; s && s->magic == SIO_MAGIC; s=s->downstream)
  { if ( s->context &&
	 (name = getStreamContext(s)->filename) )
    { return PL_unify_atom(prop, name);
    }
  }

  return false;
}


#define stream_mode_property(s, prop) LDFUNC(stream_mode_property, s, prop)
static int
stream_mode_property(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t mode;

  if ( s->flags & SIO_INPUT )
    mode = ATOM_read;
  else
  { assert(s->flags & SIO_OUTPUT);

    if ( s->flags & SIO_APPEND )
      mode = ATOM_append;
    else if ( s->flags & SIO_UPDATE )
      mode = ATOM_update;
    else
      mode = ATOM_write;
  }

  return PL_unify_atom(prop, mode);
}


#define stream_input_prop(s) LDFUNC(stream_input_prop, s)
static int
stream_input_prop(DECL_LD IOSTREAM *s)
{ IGNORE_LD

  return (s->flags & SIO_INPUT) ? true : false;
}


#define stream_output_prop(s) LDFUNC(stream_output_prop, s)
static int
stream_output_prop(DECL_LD IOSTREAM *s)
{ IGNORE_LD

  return (s->flags & SIO_OUTPUT) ? true : false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Incomplete: should be non-deterministic if the stream has multiple aliases!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define stream_alias_prop(s, prop) LDFUNC(stream_alias_prop, s, prop)
static int
stream_alias_prop(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t name;
  stream_context *ctx;
  int i;

  if ( s->magic != SIO_MAGIC || !(ctx=s->context) )
    return false;

  if ( PL_get_atom(prop, &name) )
  { alias *a;

    for( a = ctx->alias_head; a; a = a->next )
    { if ( a->name == name )
	return true;
    }

    if ( (i=standardStreamIndexFromName(name)) >= 0 &&
	 i < 6 &&
	 s == LD->IO.streams[i] )
      return true;

    return false;
  }

  if ( (i=standardStreamIndexFromStream(s)) >= 0 && i < 3 )
    return PL_unify_atom(prop, standardStreams[i]);
  if ( ctx->alias_head )
    return PL_unify_atom(prop, ctx->alias_head->name);

  return false;
}


#define stream_position_prop(s, prop) LDFUNC(stream_position_prop, s, prop)
static int
stream_position_prop(DECL_LD IOSTREAM *s, term_t prop)
{ IGNORE_LD
  IOPOS pos;

  if ( s->magic == SIO_MAGIC && s->position )
  { pos = *s->position;
    return PL_unify_term(prop,
			 PL_FUNCTOR, FUNCTOR_dstream_position4,
			   PL_INT64, pos.charno,
			   PL_INT, pos.lineno,
			   PL_INT, pos.linepos,
			   PL_INT64, pos.byteno);
  }

  return false;
}


#define stream_end_of_stream_prop(s, prop) \
	LDFUNC(stream_end_of_stream_prop, s, prop)

static int
stream_end_of_stream_prop(DECL_LD IOSTREAM *s, term_t prop)
{ if ( s->magic == SIO_MAGIC && (s->flags & SIO_INPUT) )
  { atom_t val;

    if ( s->flags & SIO_FEOF2 )
      val = ATOM_past;
    else if ( s->flags & SIO_FEOF )
      val = ATOM_at;
    else
      val = ATOM_not;

    return PL_unify_atom(prop, val);
  }

  return false;
}


#define stream_error_prop(s, prop) \
	LDFUNC(stream_error_prop, s, prop)

static int
stream_error_prop(DECL_LD IOSTREAM *s, term_t prop)
{ if ( s->magic == SIO_MAGIC )
    return PL_unify_bool(prop, Sferror(s));

  return false;
}


#define stream_eof_action_prop(s, prop) LDFUNC(stream_eof_action_prop, s, prop)
static int
stream_eof_action_prop(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t val;

  if ( s->flags & SIO_NOFEOF )
    val = ATOM_reset;
  else if ( s->flags & SIO_FEOF2ERR )
    val = ATOM_error;
  else
    val = ATOM_eof_code;

  return PL_unify_atom(prop, val);
}


#ifdef HAVE_FSTAT
#include <sys/stat.h>
#endif

#if !defined(S_ISREG) && defined(S_IFREG)
#define S_ISREG(m) ((m&S_IFMT) == S_IFREG)
#endif

#define stream_reposition_prop(s, prop) LDFUNC(stream_reposition_prop, s, prop)
static int
stream_reposition_prop(DECL_LD IOSTREAM *s, term_t prop)
{ int val;

  if ( s->magic == SIO_MAGIC &&
       (s->functions->seek || s->functions->seek64) )
  { if ( s->functions->control &&
	 (*s->functions->control)(s->handle, SIO_GETREPOSITION, &val) == 0 )
      return PL_unify_bool(prop, val);

#ifdef HAVE_FSTAT
    int fd = Sfileno(s);
    struct stat buf;

    if ( fd != -1 && fstat(fd, &buf) == 0 && S_ISREG(buf.st_mode) )
      val = true;
    else
      val = false;
#else
    val = true;
#endif
  } else
    val = false;

  return PL_unify_bool(prop, val);
}


#define stream_close_on_abort_prop(s, prop) LDFUNC(stream_close_on_abort_prop, s, prop)
static int
stream_close_on_abort_prop(DECL_LD IOSTREAM *s, term_t prop)
{ IGNORE_LD

  return PL_unify_bool_ex(prop, !(s->flags & SIO_NOCLOSE));
}


#define stream_type_prop(s, prop) LDFUNC(stream_type_prop, s, prop)
static int
stream_type_prop(DECL_LD IOSTREAM *s, term_t prop)
{ return PL_unify_atom(prop, (s->flags & SIO_TEXT) ? ATOM_text : ATOM_binary);
}


#define stream_file_no_prop(s, prop) LDFUNC(stream_file_no_prop, s, prop)
static int
stream_file_no_prop(DECL_LD IOSTREAM *s, term_t prop)
{ int fd;

  if ( (fd = Sfileno(s)) >= 0 )
    return PL_unify_integer(prop, fd);

  return false;
}


#define stream_tty_prop(s, prop) LDFUNC(stream_tty_prop, s, prop)
static int
stream_tty_prop(DECL_LD IOSTREAM *s, term_t prop)
{ IGNORE_LD

  if ( (s->flags & SIO_ISATTY) )
    return PL_unify_bool_ex(prop, true);

  return false;
}


#define stream_bom_prop(s, prop) LDFUNC(stream_bom_prop, s, prop)
static int
stream_bom_prop(DECL_LD IOSTREAM *s, term_t prop)
{ IGNORE_LD

  if ( (s->flags & SIO_BOM) )
    return PL_unify_bool_ex(prop, true);

  return false;
}


#define stream_newline_prop(s, prop) LDFUNC(stream_newline_prop, s, prop)
static int
stream_newline_prop(DECL_LD IOSTREAM *s, term_t prop)
{ switch ( s->newline )
  { case SIO_NL_POSIX:
    case SIO_NL_DETECT:
      return PL_unify_atom(prop, ATOM_posix);
    case SIO_NL_DOS:
      return PL_unify_atom(prop, ATOM_dos);
  }

  return false;
}


#define stream_encoding_prop(s, prop) LDFUNC(stream_encoding_prop, s, prop)
static int
stream_encoding_prop(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t ename = PL_encoding_to_atom(s->encoding);

  if ( ename )
    return PL_unify_atom(prop, ename);
  return false;
}


#ifdef O_LOCALE
#define stream_locale_prop(s, prop) LDFUNC(stream_locale_prop, s, prop)
static int
stream_locale_prop(DECL_LD IOSTREAM *s, term_t prop)
{ if ( s->locale )
    return unifyLocale(prop, s->locale, true);
  return false;
}
#endif

#define stream_reperror_prop(s, prop) LDFUNC(stream_reperror_prop, s, prop)
static int
stream_reperror_prop(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t a;

  if ( (s->flags & SIO_REPXML) )
    a = ATOM_xml;
  else if ( (s->flags & SIO_REPPL) )
    a = ATOM_prolog;
  else if ( (s->flags & SIO_REPPLU) )
    a = ATOM_unicode;
  else
    a = ATOM_error;

  return PL_unify_atom(prop, a);
}


#define stream_writeerror_prop(s, prop) LDFUNC(stream_writeerror_prop, s, prop)
static int
stream_writeerror_prop(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t a;

  if ( (s->flags & SIO_NOERROR) )
    a = ATOM_ignore;
  else
    a = ATOM_error;

  return PL_unify_atom(prop, a);
}


#define stream_buffer_prop(s, prop) LDFUNC(stream_buffer_prop, s, prop)
static int
stream_buffer_prop(DECL_LD IOSTREAM *s, term_t prop)
{ atom_t b;

  if ( s->flags & SIO_FBUF )
    b = ATOM_full;
  else if ( s->flags & SIO_LBUF )
    b = ATOM_line;
  else /*if ( s->flags & SIO_NBUF )*/
    b = ATOM_false;

  return PL_unify_atom(prop, b);
}


#define stream_buffer_size_prop(s, prop) LDFUNC(stream_buffer_size_prop, s, prop)
static int
stream_buffer_size_prop(DECL_LD IOSTREAM *s, term_t prop)
{ int size;

  if ( (s->flags & SIO_NBUF) )
    return false;

  if ( (size = s->bufsize) == 0 )
    size = SIO_BUFSIZE;

  return PL_unify_integer(prop, size);
}


#define stream_timeout_prop(s, prop) LDFUNC(stream_timeout_prop, s, prop)
static int
stream_timeout_prop(DECL_LD IOSTREAM *s, term_t prop)
{ if ( s->timeout == -1 )
    return PL_unify_atom(prop, ATOM_infinite);

  return PL_unify_float(prop, (double)s->timeout/1000.0);
}


#define stream_nlink_prop(s, prop) LDFUNC(stream_nlink_prop, s, prop)
static int
stream_nlink_prop(DECL_LD IOSTREAM *s, term_t prop)
{ int fd;

  if ( (fd = Sfileno(s)) >= 0 )
  { struct stat buf;

    if ( fstat(fd, &buf) == 0 )
    { return PL_unify_integer(prop, buf.st_nlink);
    }
  }

  return false;
}

#define stream_close_on_exec_prop(s, prop) LDFUNC(stream_close_on_exec_prop, s, prop)
static int
stream_close_on_exec_prop(DECL_LD IOSTREAM *s, term_t prop)
{  int fd;
#ifdef __WINDOWS__
   DWORD Flags;
#else
   int fd_flags;
#endif
   IGNORE_LD

   if ( (fd = Sfileno(s)) < 0)
     return false;

#if defined(F_GETFD) && defined(FD_CLOEXEC)

   if ( (fd_flags = fcntl(fd, F_GETFD)) == -1)
     return false;

   return PL_unify_bool_ex(prop, (fd_flags&FD_CLOEXEC) != 0 );

#elif defined __WINDOWS__

   if ( GetHandleInformation((HANDLE)_get_osfhandle(fd), &Flags) == 0 )
     return false;

   return PL_unify_bool_ex(prop, (Flags & HANDLE_FLAG_INHERIT) == 0);

#endif

   return false;
}

typedef struct
{ functor_t functor;			/* functor of property */
  property_t function; /* function to generate */
  property0_t function0; /* arity-0 function */
} sprop;


#define _SP0(functor, f0) {functor, NULL, LDFUNC_REF(f0)}
#define _SP1(functor, f1) {functor, LDFUNC_REF(f1), NULL}
static const sprop sprop_list [] =
{ _SP1( FUNCTOR_file_name1,	stream_file_name_propery ),
  _SP1( FUNCTOR_mode1,		stream_mode_property ),
  _SP0( FUNCTOR_input0,		stream_input_prop ),
  _SP0( FUNCTOR_output0,	stream_output_prop ),
  _SP1( FUNCTOR_alias1,		stream_alias_prop ),
  _SP1( FUNCTOR_position1,	stream_position_prop ),
  _SP1( FUNCTOR_end_of_stream1,	stream_end_of_stream_prop ),
  _SP1( FUNCTOR_error1,		stream_error_prop ),
  _SP1( FUNCTOR_eof_action1,	stream_eof_action_prop ),
  _SP1( FUNCTOR_reposition1,	stream_reposition_prop ),
  _SP1( FUNCTOR_type1,		stream_type_prop ),
  _SP1( FUNCTOR_file_no1,	stream_file_no_prop ),
  _SP1( FUNCTOR_buffer1,	stream_buffer_prop ),
  _SP1( FUNCTOR_buffer_size1,	stream_buffer_size_prop ),
  _SP1( FUNCTOR_close_on_abort1,stream_close_on_abort_prop ),
  _SP1( FUNCTOR_tty1,		stream_tty_prop ),
  _SP1( FUNCTOR_encoding1,	stream_encoding_prop ),
#ifdef O_LOCALE
  _SP1( FUNCTOR_locale1,	stream_locale_prop ),
#endif
  _SP1( FUNCTOR_bom1,		stream_bom_prop ),
  _SP1( FUNCTOR_newline1,	stream_newline_prop ),
  _SP1( FUNCTOR_representation_errors1, stream_reperror_prop ),
  _SP1( FUNCTOR_write_errors1,	stream_writeerror_prop ),
  _SP1( FUNCTOR_timeout1,	stream_timeout_prop ),
  _SP1( FUNCTOR_nlink1,		stream_nlink_prop ),
  _SP1( FUNCTOR_close_on_exec1,	stream_close_on_exec_prop ),
  { 0, NULL, NULL }
};


/** '$stream_property'(+Stream, +Property) is det.
    '$stream_properties'(+Stream, -PropertyList) is det.
    '$streams_properties'(?Property, -Pairs) is det.
*/

#define get_stream_property_def(t) LDFUNC(get_stream_property_def, t)
static const sprop *
get_stream_property_def(DECL_LD term_t t)
{ functor_t f;

  if ( PL_get_functor(t, &f) )
  { const sprop *p;

    for(p = sprop_list; p->functor; p++ )
    { if ( f == p->functor )
	return p;
    }
  }

  return NULL;
}


static
PRED_IMPL("$stream_property", 2, dstream_property, 0)
{ PRED_LD
  const sprop *p;
  IOSTREAM *s;
  int rc;

  if ( !(p=get_stream_property_def(A2)) )
    return false;

  PL_LOCK(L_FILE);
  if ( (rc=term_stream_handle(A1, &s, SH_ERRORS|SH_UNLOCKED)) )
  { switch(arityFunctor(p->functor))
    { case 0:
	rc = LDFUNCP(*p->function0)(s);
	break;
      case 1:
	{ term_t a1 = PL_new_term_ref();

	  _PL_get_arg(1, A2, a1);
	  rc = LDFUNCP(*p->function)(s, a1);
	  break;
	}
      default:
	assert(0);
	rc = false;
    }
  }
  PL_UNLOCK(L_FILE);
  return rc;
}


#define unify_stream_property_list(s, plist) LDFUNC(unify_stream_property_list, s, plist)
static int
unify_stream_property_list(DECL_LD IOSTREAM *s, term_t plist)
{ term_t tail = PL_copy_term_ref(plist);
  term_t head = PL_new_term_ref();
  term_t prop = PL_new_term_ref();
  const sprop *p;
  int rc;

  for(p = sprop_list; p->functor; p++)
  { if ( !(rc=PL_put_functor(prop, p->functor)) )
      break;

    switch(arityFunctor(p->functor))
    { case 0:
	rc = LDFUNCP(*p->function0)(s);
	break;
      case 1:
      { term_t a1 = PL_new_term_ref();
	_PL_get_arg(1, prop, a1);
	rc = LDFUNCP(*p->function)(s, a1);
	break;
      }
      default:
	assert(0);
	rc = false;
    }
    if ( rc )
    { rc = ( PL_unify_list(tail, head, tail) &&
	     PL_unify(head, prop) );
      if ( !rc )
	break;
    } else
    { if ( PL_exception(0) )
	break;
      rc = true;
    }
  }

  rc = (rc && PL_unify_nil(tail));
  PL_reset_term_refs(tail);

  return rc;
}


static
PRED_IMPL("$stream_properties", 2, dstream_properties, 0)
{ PRED_LD
  int rc;
  IOSTREAM *s;

  PL_LOCK(L_FILE);
  rc = ( term_stream_handle(A1, &s, SH_ERRORS|SH_UNLOCKED) &&
	 unify_stream_property_list(s, A2)
       );
  PL_UNLOCK(L_FILE);

  return rc;
}


#define unify_stream_property(s, p, t) LDFUNC(unify_stream_property, s, p, t)
static int
unify_stream_property(DECL_LD IOSTREAM *s, const sprop *p, term_t t)
{ int rc;

  if ( !(rc=PL_put_functor(t, p->functor)) )
    return false;
  switch(arityFunctor(p->functor))
  { case 0:
      rc = LDFUNCP(*p->function0)(s);
      break;
    case 1:
    { term_t a1 = PL_new_term_ref();
      _PL_get_arg(1, t, a1);
      rc = LDFUNCP(*p->function)(s, a1);
      PL_reset_term_refs(a1);
      break;
    }
    default:
      assert(0);
      rc = false;
  }

  return rc;
}


static
PRED_IMPL("$streams_properties", 2, dstreams_properties, 0)
{ PRED_LD
  int rc = false;
  const sprop *p;
  term_t tail = PL_copy_term_ref(A2);
  term_t head = PL_new_term_ref();

  if ( (p=get_stream_property_def(A1)) )
  { TableEnum e = newTableEnumPP(streamContext);
    term_t st = PL_new_term_ref();
    term_t pt = PL_new_term_ref();
    term_t ex = PL_new_term_ref();

    PL_LOCK(L_FILE);
    table_key_t tk;
    while( advanceTableEnum(e, &tk, NULL))
    { IOSTREAM *s = key2ptr(tk);

      Sacquire(s);
      rc = ( s->magic == SIO_MAGIC &&
	     s->context != NULL &&
	     unify_stream_property(s, p, pt) );
      if ( Srelease(s) )
	rc = false;
      rc = ( rc &&
	     can_unify(valTermRef(A1), valTermRef(pt), ex) &&
	     PL_unify_list(tail, head, tail) &&
	     PL_unify_functor(head, FUNCTOR_minus2) &&
	     PL_get_arg(1, head, st) &&
	     unify_stream_ref(st, s) &&
	     PL_unify_arg(2, head, pt)
	   );
      if ( !rc && (!PL_is_variable(ex) || PL_exception(0)) )
	break;
    }
    freeTableEnum(e);
    PL_UNLOCK(L_FILE);
    if ( !PL_is_variable(ex) )
      rc = PL_raise_exception(ex);
    else
      rc = !PL_exception(0) && PL_unify_nil(tail);
  } else if ( PL_is_variable(A1) )
  { TableEnum e = newTableEnumPP(streamContext);
    term_t st = PL_new_term_ref();
    term_t pl = PL_new_term_ref();

    rc = true;
    PL_LOCK(L_FILE);
    table_key_t tk;
    while( rc && advanceTableEnum(e, &tk, NULL))
    { IOSTREAM *s = key2ptr(tk);

      rc = ( s->context != NULL &&
	     PL_unify_list(tail, head, tail) &&
	     PL_unify_functor(head, FUNCTOR_minus2) &&
	     PL_get_arg(1, head, st) &&
	     unify_stream_ref(st, s) &&
	     PL_get_arg(2, head, pl) &&
	     unify_stream_property_list(s, pl)
	   );
    }
    freeTableEnum(e);
    PL_UNLOCK(L_FILE);
    rc = !PL_exception(0) && PL_unify_nil(tail);
  }

  return rc;
}

static
PRED_IMPL("$alias_stream", 2, dalias_stream, 0)
{ PRED_LD
  atom_t a;
  IOSTREAM *s;
  int rc;

  PL_LOCK(L_FILE);
  rc = ( PL_get_atom_ex(A1, &a) &&
	 get_stream_handle(a, &s, SH_UNLOCKED) &&
	 s->context &&
	 unify_stream_ref(A2, s)
       );
  PL_UNLOCK(L_FILE);

  return rc;
}


static
PRED_IMPL("is_stream", 1, is_stream, 0)
{ GET_LD
  IOSTREAM *s;
  atom_t a;

  if ( PL_get_atom(A1, &a) &&
       get_stream_handle(a, &s, SH_UNLOCKED) )
    return true;

  return false;
}



		 /*******************************
		 *	      FLUSH		*
		 *******************************/


#define flush_output(out) LDFUNC(flush_output, out)
static int
flush_output(DECL_LD term_t out)
{ IOSTREAM *s;

  if ( getOutputStream(out, S_DONTCARE, &s) )
  { Sflush(s);
    return streamStatus(s);
  }

  return false;
}

static
PRED_IMPL("flush_output", 0, flush_output, PL_FA_ISO)
{ PRED_LD

  return flush_output(0);
}

static
PRED_IMPL("flush_output", 1, flush_output1, PL_FA_ISO)
{ PRED_LD

  return flush_output(A1);
}


static int
getStreamWithPosition(term_t stream, IOSTREAM **sp)
{ IOSTREAM *s;

  if ( PL_get_stream(stream, &s, 0) )
  { if ( !s->position )
    { PL_error(NULL, 0, NULL, ERR_PERMISSION, /* non-ISO */
	       ATOM_property, ATOM_position, stream);
      releaseStream(s);
      return false;
    }

    *sp = s;
    return true;
  }

  return false;
}


static int
getRepositionableStream(term_t stream, IOSTREAM **sp)
{ GET_LD
  IOSTREAM *s;
  atom_t a;

  if ( !PL_get_atom(stream, &a) )
    return not_a_stream(stream);

  if ( get_stream_handle(a, &s, SH_ERRORS) )
  { if ( !s->position || !s->functions || !s->functions->seek )
    { PL_error(NULL, 0, NULL, ERR_PERMISSION,
	       ATOM_reposition, ATOM_stream, stream);
      releaseStream(s);
      return false;
    }

    *sp = s;
    return true;
  }

  return false;
}


static
PRED_IMPL("set_stream_position", 2, set_stream_position, PL_FA_ISO)
{ PRED_LD
  IOSTREAM *s = NULL;			/* make compiler happy */
  int64_t charno, byteno;
  long linepos, lineno;
  term_t a = PL_new_term_ref();

  term_t stream = A1;
  term_t pos = A2;

  if ( !(getRepositionableStream(stream, &s)) )
    return false;

  if ( !PL_is_functor(pos, FUNCTOR_dstream_position4) ||
       !PL_get_arg(1, pos, a) ||
       !PL_get_int64(a, &charno) ||
       !PL_get_arg(2, pos, a) ||
       !PL_get_long(a, &lineno) ||
       !PL_get_arg(3, pos, a) ||
       !PL_get_long(a, &linepos) ||
       !PL_get_arg(4, pos, a) ||
       !PL_get_int64(a, &byteno) )
  { releaseStream(s);
    return PL_error("stream_position", 3, NULL,
		    ERR_DOMAIN, ATOM_stream_position, pos);
  }

  if ( Sseek64(s, byteno, SIO_SEEK_SET) != 0 )
    return PL_error(NULL, 0, MSG_ERRNO, ERR_FILE_OPERATION,
		    ATOM_reposition, ATOM_stream, stream);

  s->position->byteno  = byteno;
  s->position->charno  = charno;
  s->position->lineno  = (int)lineno;
  s->position->linepos = (int)linepos;

  releaseStream(s);

  return true;
}


static
PRED_IMPL("seek", 4, seek, 0)
{ PRED_LD
  atom_t m;
  int whence = -1;
  int64_t off, new;
  IOSTREAM *s;

  term_t stream = A1;
  term_t offset = A2;
  term_t method = A3;
  term_t newloc = A4;

  if ( !(PL_get_atom_ex(method, &m)) )
    return false;

  if ( m == ATOM_bof )
    whence = SIO_SEEK_SET;
  else if ( m == ATOM_current )
    whence = SIO_SEEK_CUR;
  else if ( m == ATOM_eof )
    whence = SIO_SEEK_END;
  else
    return PL_error("seek", 4, NULL, ERR_DOMAIN, ATOM_seek_method, method);

  if ( !PL_get_int64(offset, &off) )
    return PL_error("seek", 4, NULL, ERR_DOMAIN, ATOM_integer, offset);

  if ( PL_get_stream_handle(stream, &s) )
  { int unit = Sunit_size(s);

    off *= unit;
    if ( Sseek64(s, off, whence) < 0 )
    { if ( errno == EINVAL )
	PL_error("seek", 4, "offset out of range", ERR_DOMAIN,
		 ATOM_position, offset);
      else
	PL_error("seek", 4, MSG_ERRNO, ERR_PERMISSION,
		 ATOM_reposition, ATOM_stream, stream);
      Sclearerr(s);
      releaseStream(s);
      return false;
    }

    new = Stell64(s);
    releaseStream(s);
    new /= unit;

    return PL_unify_int64(newloc, new);
  }

  return false;
}


static
PRED_IMPL("set_input", 1, set_input, PL_FA_ISO)
{ PRED_LD
  IOSTREAM *s;

  if ( getInputStream(A1, S_DONTCARE, &s) )
  { setStandardStream(SNO_CURRENT_INPUT, s);
    releaseStream(s);
    return true;
  }

  return false;
}


static
PRED_IMPL("set_output", 1, set_output, PL_FA_ISO)
{ PRED_LD
  IOSTREAM *s;

  if ( getOutputStream(A1, S_DONTCARE, &s) )
  { setStandardStream(SNO_CURRENT_OUTPUT, s);
    releaseStream(s);
    return true;
  }

  return false;
}


#define current_io(t, cur) LDFUNC(current_io, t, cur)
static int
current_io(DECL_LD term_t t, IOSTREAM *cur)
{ if ( PL_is_variable(t) )
  { return PL_unify_stream(t, cur);
  } else
  { IOSTREAM *s;

    if ( term_stream_handle(t, &s, SH_ERRORS|SH_ALIAS|SH_UNLOCKED) )
      return s == cur;
    return false;
  }
}

static
PRED_IMPL("current_input", 1, current_input, PL_FA_ISO)
{ PRED_LD
  return current_io(A1, Scurin);
}


static
PRED_IMPL("current_output", 1, current_output, PL_FA_ISO)
{ PRED_LD
  return current_io(A1, Scurout);
}


static
PRED_IMPL("byte_count", 2, byte_count, 0)
{ PRED_LD
  IOSTREAM *s;

  if ( getStreamWithPosition(A1, &s) )
  { int64_t n = s->position->byteno;

    releaseStream(s);
    return PL_unify_int64(A2, n);
  }

  return false;
}


static
PRED_IMPL("character_count", 2, character_count, 0)
{ PRED_LD
  IOSTREAM *s;

  if ( getStreamWithPosition(A1, &s) )
  { int64_t n = s->position->charno;

    releaseStream(s);
    return PL_unify_int64(A2, n);
  }

  return false;
}


static
PRED_IMPL("line_count", 2, line_count, 0)
{ GET_LD
  IOSTREAM *s;

  if ( getStreamWithPosition(A1, &s) )
  { intptr_t n = s->position->lineno;

    releaseStream(s);
    return PL_unify_integer(A2, n);
  }

  return false;
}


static
PRED_IMPL("line_position", 2, line_position, 0)
{ GET_LD
  IOSTREAM *s;

  if ( getStreamWithPosition(A1, &s) )
  { intptr_t n = s->position->linepos;

    releaseStream(s);
    return PL_unify_integer(A2, n);
  }

  return false;
}


static
PRED_IMPL("source_location", 2, source_location, 0)
{ PRED_LD
  if ( ReadingSource &&
       PL_unify_atom(A1, source_file_name) &&
       PL_unify_integer(A2, source_line_no) )
    return true;

  return false;
}


static
PRED_IMPL("$set_source_location", 2, set_source_location, 0)
{ PRED_LD
  return ( PL_get_atom_ex(A1, &source_file_name) &&
	   PL_get_integer_ex(A2, &source_line_no) );
}


#define at_end_of_stream(stream) LDFUNC(at_end_of_stream, stream)
static int
at_end_of_stream(DECL_LD term_t stream)
{ IOSTREAM *s;

  if ( getInputStream(stream, S_DONTCARE, &s) )
  { int rval = Sfeof(s);

    if ( rval < 0 )
    { PL_error(NULL, 0, "not-buffered stream", ERR_PERMISSION,
	       ATOM_end_of_stream, ATOM_stream, stream);
      rval = false;
    }

    if ( rval && Sferror(s) )		/* due to error */
      return streamStatus(s);
    else
      releaseStream(s);

    return rval;
  }

  return false;				/* exception */
}

static
PRED_IMPL("at_end_of_stream", 1, at_end_of_stream, PL_FA_ISO)
{ PRED_LD
  return at_end_of_stream(A1);
}

static
PRED_IMPL("at_end_of_stream", 0, at_end_of_stream0, PL_FA_ISO)
{ PRED_LD
  return at_end_of_stream(0);
}


/** fill_buffer(+Stream)
 *
 *  Fill the buffer of Stream.
 */

static
PRED_IMPL("fill_buffer", 1, fill_buffer, 0)
{ PRED_LD
  IOSTREAM *s;

  term_t stream = A1;

  if ( getInputStream(stream, S_DONTCARE, &s) )
  { if ( (s->flags & SIO_NBUF) )
    { return ( PL_release_stream(s) &&
	       PL_permission_error("fill_buffer", "stream", stream) );
    }

    if ( !(s->flags & SIO_FEOF) )
    { if ( S__fillbuf(s) < 0 )
	return PL_release_stream(s);

      s->bufp--;
    }
    return PL_release_stream(s);
  }

  return false;
}


#define peek(stream, chr, how) LDFUNC(peek, stream, chr, how)
static foreign_t
peek(DECL_LD term_t stream, term_t chr, int how)
{ IOSTREAM *s;
  int c;

  if ( !getInputStream(stream, how == PL_BYTE ? S_BINARY : S_TEXT, &s) )
    return false;
  if ( ison(s, SIO_NBUF) || (s->bufsize && s->bufsize < PL_MB_LEN_MAX) )
  { releaseStream(s);
    return PL_error(NULL, 0, "stream is unbuffered", ERR_PERMISSION,
		    ATOM_peek, ATOM_stream, stream);
  }

  if ( how == PL_BYTE )
  { IOPOS pos = s->posbuf;

    c = Sgetc(s);
    if ( c != EOF )
      Sungetc(c, s);
    s->posbuf = pos;
  } else
  { c = Speekcode(s);
  }
  if ( Sferror(s) )
    return streamStatus(s);
  releaseStream(s);

  return PL_unify_char(chr, c, how);
}


static
PRED_IMPL("peek_byte", 2, peek_byte2, 0)
{ PRED_LD
  return peek(A1, A2, PL_BYTE);
}


static
PRED_IMPL("peek_byte", 1, peek_byte1, 0)
{ PRED_LD
  return peek(0, A1, PL_BYTE);
}


static
PRED_IMPL("peek_code", 2, peek_code2, 0)
{ PRED_LD
  return peek(A1, A2, PL_CODE);
}


static
PRED_IMPL("peek_code", 1, peek_code1, 0)
{ PRED_LD
  return peek(0, A1, PL_CODE);
}


static
PRED_IMPL("peek_char", 2, peek_char2, 0)
{ PRED_LD
  return peek(A1, A2, PL_CHAR);
}


static
PRED_IMPL("peek_char", 1, peek_char1, 0)
{ PRED_LD
  return peek(0, A1, PL_CHAR);
}


		 /*******************************
		 *	    INTERACTION		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
set_prolog_IO(+In, +Out, +Error)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define WRAP_CLEAR_FLAGS (SIO_FILE)

typedef struct wrappedIO
{ void		   *wrapped_handle;	/* original handle */
  IOFUNCTIONS      *wrapped_functions;	/* original functions */
  IOSTREAM	   *wrapped_stream;	/* stream we wrapped */
  IOFUNCTIONS       functions;		/* new function block */
  int		    saved_flags;	/* SIO_flags we must restore */
} wrappedIO;


static ssize_t
Sread_user(void *handle, char *buf, size_t size)
{ GET_LD
  wrappedIO *wio = handle;
  ssize_t rc;

  if ( LD->prompt.next && Sttymode(wio->wrapped_stream) != TTY_RAW )
    PL_write_prompt(true);
  else
    Sflush(Suser_output);

  rc = (*wio->wrapped_functions->read)(wio->wrapped_handle, buf, size);
  if ( rc == 0 )			/* end-of-file */
  { Sclearerr(Suser_input);
    LD->prompt.next = true;
  } else if ( rc == 1 && buf[0] == 04 )
  { rc = 0;				/* Map ^D to end-of-file */
  } else if ( rc > 0 && buf[rc-1] == '\n' )
    LD->prompt.next = true;

  return rc;
}


static int
closeWrappedIO(void *handle)
{ wrappedIO *wio = handle;
  int rval;

  if ( wio->wrapped_functions->close )
    rval = (*wio->wrapped_functions->close)(wio->wrapped_handle);
  else
    rval = 0;

  wio->wrapped_stream->functions = wio->wrapped_functions;
  wio->wrapped_stream->handle = wio->wrapped_handle;
  clear(wio->wrapped_stream, WRAP_CLEAR_FLAGS);
  set(wio->wrapped_stream, wio->saved_flags);
  PL_free(wio);

  return rval;
}


static int
controlWrappedIO(void *handle, int action, void *arg)
{ wrappedIO *wio = handle;
  int rval;

  if ( wio->wrapped_functions->control )
    rval = (*wio->wrapped_functions->control)(wio->wrapped_handle,
					      action,
					      arg);
  else
    rval = 0;

  return rval;
}


static void
wrapIO(IOSTREAM *s,
       ssize_t (*read)(void *, char *, size_t),
       ssize_t (*write)(void *, char *, size_t))
{ wrappedIO *wio = PL_malloc(sizeof(*wio));

  wio->wrapped_functions = s->functions;
  wio->wrapped_handle =	s->handle;
  wio->wrapped_stream = s;
  wio->saved_flags    = s->flags & WRAP_CLEAR_FLAGS;
  clear(s, WRAP_CLEAR_FLAGS);

  wio->functions = *s->functions;
  if ( read  ) wio->functions.read  = read;
  if ( write ) wio->functions.write = write;
  wio->functions.close = closeWrappedIO;
  wio->functions.control = controlWrappedIO;

  s->functions = &wio->functions;
  s->handle = wio;
}


static ssize_t
Swrite_stderr(void *handle, char *buf, size_t size)
{ IOSTREAM *org = handle;
  ssize_t rc;

  return ( (rc=Sfwrite(buf, 1, size, org)) > 0 &&
	   Sflush(org) == 0
	 ) ? rc : -1;
}

static IOFUNCTIONS Sstderrfunctions =
{ NULL,
  Swrite_stderr,
  NULL,
  NULL,
  NULL,
#ifdef O_LARGEFILES
  NULL,
#else
  NULL
#endif
};


static int
getIOStreams(term_t tin, term_t tout, term_t terror,
	     IOSTREAM **in, IOSTREAM **out, IOSTREAM **error)
{
  if ( !PL_get_stream(tin, in, SIO_INPUT) )
    return false;

  if ( !PL_get_stream(tout, out, SIO_OUTPUT) )
    return false;

  if ( PL_compare(tout, terror) == 0 )	/* == */
  { *error = getStream(Snew((*out),
			    (*out)->flags & ~WRAP_CLEAR_FLAGS,
			    &Sstderrfunctions));
    if ( !*error )
      return false;
  } else
  { if ( !PL_get_stream(terror, error, SIO_OUTPUT) )
      return false;
  }

  (*out)->flags &= ~SIO_ABUF;		/* output: line buffered */
  (*out)->flags |= SIO_LBUF;

  (*error)->flags &= ~SIO_ABUF;		/* disable buffering */
  (*error)->flags |= SIO_NBUF;

  return true;
}


static
PRED_IMPL("set_prolog_IO", 3, set_prolog_IO, 0)
{ PRED_LD
  IOSTREAM *in = NULL, *out = NULL, *error = NULL;
  int rval = false;
  int wrapin = false;
  int i;

  if ( !getIOStreams(A1, A2, A3, &in, &out, &error) )
    goto out;

  wrapin = (LD->IO.streams[0] != in);

  PL_LOCK(L_FILE);

  setStandardStream(SNO_USER_OUTPUT,    out);
  setStandardStream(SNO_USER_ERROR,     error);
  setStandardStream(SNO_CURRENT_OUTPUT, out);

  if ( wrapin )
  { setStandardStream(SNO_CURRENT_INPUT, in);
    setStandardStream(SNO_USER_INPUT,    in);
    wrapIO(in, Sread_user, NULL);
    LD->prompt.next = true;
  }

  for(i=SNO_USER_INPUT; i<=SNO_USER_ERROR; i++)
  { LD->IO.streams[i]->position = &LD->IO.streams[0]->posbuf;
    LD->IO.streams[i]->flags |= SIO_RECORDPOS;
  }

  PL_UNLOCK(L_FILE);
  rval = true;

out:
  if ( in )
    releaseStream(in);
  if ( out )
    releaseStream(out);
  if ( error )
    releaseStream(error);

  return rval;
}


static int
sys_io_stream(IOSTREAM *s, IOSTREAM *ref, term_t t)
{ int fd = Sfileno(s);

  if ( s != ref && fd < 0 )
    return PL_domain_error("file_stream", t),-1;

  return fd;
}

static
PRED_IMPL("set_system_IO", 3, set_system_IO, 0)
{ IOSTREAM *in = NULL, *out = NULL, *error = NULL;
  int fd_in, fd_out, fd_error;
  int rval = false;

  if ( !getIOStreams(A1, A2, A3, &in, &out, &error) )
    goto out;

  if ( (fd_in    = sys_io_stream(in,    Sinput, A1)) < 0 ||
       (fd_out   = sys_io_stream(out,   Soutput, A2)) < 0 ||
       (fd_error = sys_io_stream(error, Serror,  A3)) < 0 )
    goto out;

  PL_LOCK(L_FILE);
  if ( in != Sinput )
    dup2(fd_in, 0);			/* stdin */
  if ( out != Soutput )
    dup2(fd_out, 1);			/* stdout */
  if ( error != Serror )
    dup2(fd_error, 2);			/* stderr */
  PL_UNLOCK(L_FILE);
  rval = true;

out:
  if ( in )
    releaseStream(in);
  if ( out )
    releaseStream(out);
  if ( error && error != out )
    releaseStream(error);

  return rval;
}


static
PRED_IMPL("$size_stream", 2, size_stream, 0)
{ GET_LD
  IOSTREAM *s;
  int64_t sz;

  if ( !PL_get_stream_handle(A1, &s) )
    return false;
  sz = Ssize(s);
  if ( !PL_release_stream(s) )
    return false;

  if ( sz >= 0 )
    return PL_unify_int64(A2, sz);

  return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		  ATOM_reposition, ATOM_stream, A1);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
copy_stream_data(+StreamIn, +StreamOut, [Len])
	Copy all data from StreamIn to StreamOut.  Should be somewhere else,
	and maybe we need something else to copy resources.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define copy_stream_data(in, out, len) LDFUNC(copy_stream_data, in, out, len)
static int
copy_stream_data(DECL_LD term_t in, term_t out, term_t len)
{ IOSTREAM *i, *o;
  int c, rc;
  int count = 0;

  if ( !getInputStream(in, S_DONTCARE, &i) )
    return false;
  if ( !getOutputStream(out, S_DONTCARE, &o) )
  { releaseStream(i);
    return false;
  }

  if ( !len )
  { while ( (c = Sgetcode(i)) != EOF )
    { if ( (++count % 4096) == 0 && PL_handle_signals() < 0 )
      { releaseStream(i);
	releaseStream(o);
	return false;
      }
      if ( Sputcode(c, o) < 0 )
      { releaseStream(i);
	return streamStatus(o);
      }
    }
  } else
  { int64_t n;

    if ( !PL_get_int64_ex(len, &n) )
      return false;

    while ( n-- > 0 && (c = Sgetcode(i)) != EOF )
    { if ( (++count % 4096) == 0 && PL_handle_signals() < 0 )
      { releaseStream(i);
	releaseStream(o);
	return false;
      }
      if ( Sputcode(c, o) < 0 )
	break;
    }
  }

  rc = streamStatus(o);
  rc = streamStatus(i) && rc;

  return rc;
}

static
PRED_IMPL("copy_stream_data", 3, copy_stream_data3, 0)
{ PRED_LD
  return copy_stream_data(A1, A2, A3);
}

static
PRED_IMPL("copy_stream_data", 2, copy_stream_data2, 0)
{ PRED_LD
  return copy_stream_data(A1, A2, 0);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(file)
					/* ISO IO */
  PRED_DEF("open", 4, open4, PL_FA_ISO)
  PRED_DEF("open", 3, open3, PL_FA_ISO)
  PRED_DEF("close", 1, close, PL_FA_ISO)
  PRED_DEF("close", 2, close2, PL_FA_ISO)
  PRED_DEF("set_input", 1, set_input, PL_FA_ISO)
  PRED_DEF("set_output", 1, set_output, PL_FA_ISO)
  PRED_DEF("current_input", 1, current_input, PL_FA_ISO)
  PRED_DEF("current_output", 1, current_output, PL_FA_ISO)
  PRED_DEF("get_code", 2, get_code2, PL_FA_ISO)
  PRED_DEF("get_code", 1, get_code1, PL_FA_ISO)
  PRED_DEF("get_char", 2, get_char2, PL_FA_ISO)
  PRED_DEF("get_char", 1, get_char1, PL_FA_ISO)
  PRED_DEF("get_byte", 2, get_byte2, PL_FA_ISO)
  PRED_DEF("get_byte", 1, get_byte1, PL_FA_ISO)
  PRED_DEF("peek_code", 2, peek_code2, PL_FA_ISO)
  PRED_DEF("peek_code", 1, peek_code1, PL_FA_ISO)
  PRED_DEF("peek_char", 2, peek_char2, PL_FA_ISO)
  PRED_DEF("peek_char", 1, peek_char1, PL_FA_ISO)
  PRED_DEF("peek_byte", 2, peek_byte2, PL_FA_ISO)
  PRED_DEF("peek_byte", 1, peek_byte1, PL_FA_ISO)
  PRED_DEF("peek_string", 3, peek_string, 0)
  PRED_DEF("put_byte", 2, put_byte2, PL_FA_ISO)
  PRED_DEF("put_byte", 1, put_byte1, PL_FA_ISO)
  PRED_DEF("put_code", 2, put_code2, PL_FA_ISO)
  PRED_DEF("put_code", 1, put_code1, PL_FA_ISO)
  PRED_DEF("put_char", 2, put_code2, PL_FA_ISO)
  PRED_DEF("put_char", 1, put_code1, PL_FA_ISO)
  PRED_DEF("flush_output", 0, flush_output, PL_FA_ISO)
  PRED_DEF("flush_output", 1, flush_output1, PL_FA_ISO)
  PRED_DEF("at_end_of_stream", 1, at_end_of_stream, PL_FA_ISO)
  PRED_DEF("at_end_of_stream", 0, at_end_of_stream0, PL_FA_ISO)
  PRED_DEF("fill_buffer", 1, fill_buffer, 0)
  PRED_DEF("set_stream_position", 2, set_stream_position, PL_FA_ISO)
  PRED_DEF("$stream_property", 2, dstream_property, 0)
  PRED_DEF("$stream_properties", 2, dstream_properties, 0)
  PRED_DEF("$streams_properties", 2, dstreams_properties, 0)
  PRED_DEF("$alias_stream", 2, dalias_stream, 0)

					/* edinburgh IO */
  PRED_DEF("see", 1, see, 0)
  PRED_DEF("seen", 0, seen, 0)
  PRED_DEF("seeing", 1, seeing, 0)
  PRED_DEF("tell", 1, tell, 0)
  PRED_DEF("append", 1, append, 0)
  PRED_DEF("told", 0, told, 0)
  PRED_DEF("telling", 1, telling, 0)
  PRED_DEF("put", 2, put2, 0)
  PRED_DEF("put", 1, put1, 0)
  PRED_DEF("skip", 1, skip1, 0)
  PRED_DEF("skip", 2, skip2, 0)
  PRED_DEF("get", 1, get1, 0)
  PRED_DEF("get", 2, get2, 0)
  PRED_DEF("get0", 2, get_code2, 0)
  PRED_DEF("get0", 1, get_code1, 0)
  PRED_DEF("ttyflush", 0, ttyflush, 0)
  PRED_DEF("prompt", 2, prompt, 0)
  PRED_DEF("tab", 2, tab2, 0)
  PRED_DEF("tab", 1, tab1, 0)
					/* Quintus IO */
  PRED_DEF("byte_count", 2, byte_count, 0)
  PRED_DEF("character_count", 2, character_count, 0)
  PRED_DEF("line_count", 2, line_count, 0)
  PRED_DEF("line_position", 2, line_position, 0)
  PRED_DEF("open_null_stream", 1, open_null_stream, 0)

					/* SWI specific */
  PRED_DEF("is_stream", 1, is_stream, 0)
  PRED_DEF("set_stream", 2, set_stream, 0)
  PRED_DEF("with_output_to", 2, with_output_to, PL_FA_TRANSPARENT)
  PRED_DEF("set_prolog_IO", 3, set_prolog_IO, 0)
  PRED_DEF("set_system_IO", 3, set_system_IO, 0)
  PRED_DEF("protocol", 1, protocol, 0)
  PRED_DEF("protocola", 1, protocola, 0)
  PRED_DEF("noprotocol", 0, noprotocol, 0)
  PRED_DEF("protocolling", 1, protocolling, 0)
  PRED_DEF("prompt1", 1, prompt1, 0)
  PRED_DEF("seek", 4, seek, 0)
#ifdef HAVE_PRED_WAIT_FOR_INPUT
  PRED_DEF("wait_for_input", 3, wait_for_input, 0)
#endif
  PRED_DEF("get_single_char", 1, get_single_char, 0)
  PRED_DEF("read_pending_codes", 3, read_pending_codes, 0)
  PRED_DEF("read_pending_chars", 3, read_pending_chars, 0)
  PRED_DEF("source_location", 2, source_location, 0)
  PRED_DEF("$set_source_location", 2, set_source_location, 0)
  PRED_DEF("copy_stream_data", 3, copy_stream_data3, 0)
  PRED_DEF("copy_stream_data", 2, copy_stream_data2, 0)
  PRED_DEF("stream_pair", 3, stream_pair, 0)
  PRED_DEF("set_end_of_stream", 1, set_end_of_stream, 0)

					/* SWI internal */
  PRED_DEF("$push_input_context", 1, push_input_context, 0)
  PRED_DEF("$pop_input_context", 0, pop_input_context, 0)
  PRED_DEF("$input_context", 1, input_context, 0)
  PRED_DEF("$size_stream", 2, size_stream, 0)
  PRED_DEF("with_tty_raw", 1, with_tty_raw, PL_FA_TRANSPARENT)
EndPredDefs
