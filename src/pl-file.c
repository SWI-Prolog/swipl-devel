/*  pl-file.c,v 1.22 1994/04/11 08:37:36 jan Exp

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: file system i/o
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

/*#define O_DEBUG_MT 1*/

#ifdef WIN32
#include "windows.h"
#endif

#include "pl-incl.h"
#include "pl-ctype.h"

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif

#define LOCK()   PL_LOCK(L_FILE)	/* MT locking */
#define UNLOCK() PL_UNLOCK(L_FILE)

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
      return (ap - standardStreams);
  }

  return -1;
}


static int
standardStreamIndexFromStream(IOSTREAM *s)
{ IOSTREAM **sp = LD->IO.streams;
  int i = 0;

  for( ; i<6; i++, sp++ )
  { if ( *sp == s )
      return i;
  }

  return -1;
}


		 /*******************************
		 *	   BOOKKEEPING		*
		 *******************************/

static void aliasStream(IOSTREAM *s, atom_t alias);

static Table streamAliases;		/* alias --> stream */
static Table streamContext;		/* stream --> extra data */

typedef struct _alias
{ struct _alias *next;
  atom_t name;
} alias;


typedef struct
{ alias *alias_head;
  alias *alias_tail;
  atom_t filename;			/* associated filename */
} stream_context;


static stream_context *
getStreamContext(IOSTREAM *s)
{ Symbol symb;

  if ( !(symb = lookupHTable(streamContext, s)) )
  { stream_context *ctx = allocHeap(sizeof(*ctx));

    ctx->alias_head = ctx->alias_tail = NULL;
    ctx->filename = NULL_ATOM;
    addHTable(streamContext, s, ctx);

    return ctx;
  }

  return symb->value;
}


void
aliasStream(IOSTREAM *s, atom_t name)
{ stream_context *ctx;
  alias *a;

  ctx = getStreamContext(s);
  addHTable(streamAliases, (void *)name, s);
  
  a = allocHeap(sizeof(*a));
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
{ Symbol symb;

  if ( name )
  { if ( (symb = lookupHTable(streamAliases, (void *)name)) )
    { deleteSymbolHTable(streamAliases, symb);

      if ( (symb=lookupHTable(streamContext, s)) )
      { stream_context *ctx = symb->value;
	alias **a;
      
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
    }
  } else				/* delete them all */
  { if ( (symb=lookupHTable(streamContext, s)) )
    { stream_context *ctx = symb->value;
      alias *a, *n;

      for(a = ctx->alias_head; a; a=n)
      { Symbol s2;

	n = a->next;

	if ( (s2 = lookupHTable(streamAliases, (void *)a->name)) )
	  deleteSymbolHTable(streamAliases, s2);

	freeHeap(a, sizeof(*a));
      }

      ctx->alias_head = ctx->alias_tail = NULL;
    }
  }
}


static void
freeStream(IOSTREAM *s)
{ Symbol symb;
  int i;
  IOSTREAM **sp;

  LOCK();
  unaliasStream(s, NULL_ATOM);
  if ( (symb=lookupHTable(streamContext, s)) )
  { stream_context *ctx = symb->value;

    freeHeap(ctx, sizeof(*ctx));
    deleteSymbolHTable(streamContext, symb);
  }
  UNLOCK();

					/* if we are a standard stream */
					/* reassociate with standard I/O */
  for(i=0, sp = LD->IO.streams; i<6; i++, sp++)
  { if ( *sp == s )
    { if ( s->flags & SIO_INPUT )
	*sp = Sinput;
      else if ( sp == &Suser_error )
	*sp = Serror;
      else if ( sp == &Sprotocol )
	*sp = NULL;
      else
	*sp = Soutput;

      break;
    }
  }
}


static void
setFileNameStream(IOSTREAM *s, atom_t name)
{ LOCK();
  getStreamContext(s)->filename = name;
  UNLOCK();
}


atom_t
fileNameStream(IOSTREAM *s)
{ atom_t name;

  LOCK();
  name = getStreamContext(s)->filename;
  UNLOCK();

  return name;
}


void
initIO()
{ const atom_t *np;
  int i;

  streamAliases = newHTable(16);
  streamContext = newHTable(16);

  fileerrors = TRUE;
#ifdef __unix__
{ int fd;

  if ( (fd=Sfileno(Sinput))  < 0 || !isatty(fd) ||
       (fd=Sfileno(Soutput)) < 0 || !isatty(fd) )
    defFeature("tty_control", FT_BOOL, FALSE);
}
#endif
  ResetTty();

  Sclosehook(freeStream);

  Sinput->position  = &Sinput->posbuf;	/* position logging */
  Soutput->position = &Sinput->posbuf;
  Serror->position  = &Sinput->posbuf;

  ttymode = TTY_COOKED;
  PushTty(Sinput, &ttytab, TTY_SAVE);
  LD->prompt.current = ATOM_prompt;

  Suser_input  = Sinput;
  Suser_output = Soutput;
  Suser_error  = Serror;
  Scurin       = Sinput;		/* see/tell */
  Scurout      = Soutput;
  Sprotocol    = NULL;			/* protocolling */

  getStreamContext(Sinput);		/* add for enumeration */
  getStreamContext(Soutput);
  getStreamContext(Serror);
  for( i=0, np = standardStreams; *np; np++, i++ )
    addHTable(streamAliases, (void *)*np, (void *)i);

  GD->io_initialised = TRUE;
}

		 /*******************************
		 *	     GET HANDLES	*
		 *******************************/

#ifdef O_PLMT

static inline IOSTREAM *
getStream(IOSTREAM *s)
{ if ( s )
    Slock(s);

  return s;
}

static inline void
releaseStream(IOSTREAM *s)
{ Sunlock(s);
}

#else /*O_PLMT*/

#define getStream(s) (s)
#define releaseStream(s)

#endif /*O_PLMT*/

void
PL_release_stream(IOSTREAM *s)
{ releaseStream(s);
}


int
PL_get_stream_handle(term_t t, IOSTREAM **s)
{ atom_t alias;

  if ( PL_is_functor(t, FUNCTOR_dstream1) )
  { void *p;
    term_t a = PL_new_term_ref();

    PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &p) )
    { LOCK();
      if ( ((IOSTREAM *)p)->magic == SIO_MAGIC )
      { *s = getStream(p);
        UNLOCK();
        return TRUE;
      } else
      { UNLOCK();
	return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_stream, t);
      }
    }
  } else if ( PL_get_atom(t, &alias) )
  { Symbol symb;

    LOCK();
    if ( (symb=lookupHTable(streamAliases, (void *)alias)) )
    { IOSTREAM *stream;
      unsigned long n = (unsigned long)symb->value;

      if ( n < 6 )			/* standard stream! */
      { stream = LD->IO.streams[n];
      } else
	stream = symb->value;
      
      if ( stream )
      { assert(stream->magic == SIO_MAGIC);
	*s = getStream(stream);
	UNLOCK();
	return TRUE;
      }
    }
    UNLOCK();

    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_stream, t);
  }
      
  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_stream, t);
}


int
PL_unify_stream(term_t t, IOSTREAM *s)
{ int rval;
  stream_context *ctx;
  int i;

  if ( (i=standardStreamIndexFromStream(s)) >= 0 )
    return PL_unify_atom(t, standardStreams[i]);

  LOCK();
  ctx = getStreamContext(s);
  if ( ctx->alias_head )
  { rval = PL_unify_atom(t, ctx->alias_head->name);
  } else
  { term_t a = PL_new_term_ref();
    
    PL_put_pointer(a, s);
    PL_cons_functor(a, FUNCTOR_dstream1, a);

    rval = PL_unify(t, a);
  }
  UNLOCK();

  return rval;
}


bool					/* old FLI name (compatibility) */
PL_open_stream(term_t handle, IOSTREAM *s)
{ return PL_unify_stream(handle, s);
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


bool
getOutputStream(term_t t, IOSTREAM **s)
{ atom_t a;

  if ( t == 0 )
  { *s = getStream(Scurout);
    return TRUE;
  } else if ( PL_get_atom(t, &a) && a == ATOM_user )
  { *s = getStream(Suser_output);
    return TRUE;
  }

  return PL_get_stream_handle(t, s);
}


bool
getInputStream(term_t t, IOSTREAM **s)
{ atom_t a;

  if ( t == 0 )
  { *s = getStream(Scurin);
    return TRUE;
  } else if ( PL_get_atom(t, &a) && a == ATOM_user )
  { *s = getStream(Suser_input);
    return TRUE;
  }

  return PL_get_stream_handle(t, s);
}


bool
streamStatus(IOSTREAM *s)
{ int rval;

  if ( Sferror(s) )
  { atom_t op;
    term_t stream = PL_new_term_ref();

    PL_unify_stream(stream, s);

    if ( s->flags & SIO_INPUT )
    { if ( Sfpasteof(s) )
      { PL_error(NULL, 0, NULL, ERR_PERMISSION,
		 ATOM_input, ATOM_past_end_of_stream, stream);
      }
      op = ATOM_read;
    } else
      op = ATOM_write;

    rval = PL_error(NULL, 0, NULL, ERR_FILE_OPERATION,
		    op, ATOM_stream, stream);
  } else
    rval = TRUE;

  releaseStream(s);

  return rval;
}


		 /*******************************
		 *	     TTY MODES		*
		 *******************************/

ttybuf	ttytab;				/* saved terminal status on entry */
int	ttymode;			/* Current tty mode */

typedef struct input_context * InputContext;
typedef struct output_context * OutputContext;

struct input_context
{ IOSTREAM *    stream;                 /* pushed input */
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
dieIO()
{ if ( GD->io_initialised )
  { pl_noprotocol();
    closeFiles(TRUE);
    PopTty(Sinput, &ttytab);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
closeStream() performs Prolog-level closing. Most important right now is
to to avoid closing the user-streams. If a stream cannot be flushed (due
to a write-error), an exception is  generated   and  the  stream is left
open. This behaviour ensures proper error-handling. How to collect these
resources??

MT: We assume the stream is locked and will unlock it here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
closeStream(IOSTREAM *s)
{ if ( s == Sinput )
  { Sclearerr(s);
    releaseStream(s);
  } else if ( s == Soutput || s == Serror )
  { if ( Sflush(s) < 0 )
      return streamStatus(s);
    releaseStream(s);
  } else
  { if ( Sflush(s) < 0 )
    { streamStatus(s);
      Sclose(s);
      return FALSE;
    }
    Sclose(s);
  }

  succeed;
}


void
closeFiles(int all)
{ TableEnum e;
  Symbol symb;

  e = newTableEnum(streamContext);
  while( (symb=advanceTableEnum(e)) )
  { IOSTREAM *s = symb->name;

    if ( all || !(s->flags & SIO_NOCLOSE) )
      closeStream(getStream(s));
  }
  freeTableEnum(e);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the open OS filedescriptors, so we can close them (see System())
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
openFileDescriptors(unsigned char *buf, int size)
{ TableEnum e;
  Symbol symb;
  int n = 0;

  LOCK();
  e = newTableEnum(streamContext);
  while( (symb=advanceTableEnum(e)) )
  { IOSTREAM *s = symb->name;
    int fd;

    if ( (fd=Sfileno(s)) >= 0 )
    { if ( n > size )
	break;
      buf[n++] = fd;
    }
  }
  freeTableEnum(e);
  UNLOCK();

  return n;
}


void
protocol(const char *str, int n)
{ IOSTREAM *s;

  if ( (s = getStream(Sprotocol)) )
  { while( --n >= 0 )
      Sputc(*str++, s);
    releaseStream(s);			/* we don not check errors */
  }
}


static void
pushInputContext()
{ InputContext c = allocHeap(sizeof(struct input_context));

  c->stream           = Scurin;
  c->term_file        = source_file_name;
  c->term_line        = source_line_no;
  c->previous         = input_context_stack;
  input_context_stack = c;
}


static void
popInputContext()
{ InputContext c = input_context_stack;

  if ( c )
  { Scurin              = c->stream;
    source_file_name    = c->term_file;
    source_line_no      = c->term_line;
    input_context_stack = c->previous;
    freeHeap(c, sizeof(struct input_context));
  } else
    Scurin		= Sinput;
}


static void
pushOutputContext()
{ OutputContext c = allocHeap(sizeof(struct output_context));

  c->stream            = Scurout;
  c->previous          = output_context_stack;
  output_context_stack = c;
}


static void
popOutputContext()
{ OutputContext c = output_context_stack;

  if ( c )
  { if ( c->stream->magic == SIO_MAGIC )
      Scurout = c->stream;
    else
    { Sdprintf("Oops, current stream closed?");
      Scurout = Soutput;
    }
    output_context_stack = c->previous;
    freeHeap(c, sizeof(struct output_context));
  } else
    Scurout              = Soutput;
}


void
PL_write_prompt(int dowrite)
{ IOSTREAM *s = getStream(Suser_output);

  if ( s )
  { if ( dowrite )
      Sfputs(PrologPrompt(), s);

    Sflush(s);
    releaseStream(s);
  }

  LD->prompt.next = FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a single character from Sinput  without   waiting  for a return. The
character should not be echoed.  If   TTY_CONTROL_FEATURE  is false this
function will read the first character and  then skip all character upto
and including the newline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
getSingleChar(IOSTREAM *stream)
{ int c;
  ttybuf buf;
    
  debugstatus.suspendTrace++;
  Slock(stream);
  Sflush(stream);
  PushTty(stream, &buf, TTY_RAW);	/* just donot prompt */
  
  if ( !trueFeature(TTY_CONTROL_FEATURE) )
  { Char c2;

    c2 = Sgetc(stream);
    while( c2 == ' ' || c2 == '\t' )	/* skip blanks */
      c2 = Sgetc(stream);
    c = c2;
    while( c2 != EOF && c2 != '\n' )	/* read upto newline */
      c2 = Sgetc(stream);
  } else
  { if ( stream->position )
    { IOPOS oldpos = *stream->position;
      c = Sgetc(stream);
      *stream->position = oldpos;
    } else
      c = Sgetc(stream);
  }

  if ( c == 4 || c == 26 )		/* should ask the terminal! */
    c = -1;

  PopTty(stream, &buf);
  debugstatus.suspendTrace--;
  Sunlock(stream);

  return c;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
readLine() reads a line from the terminal.  It is used only by the tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef DEL
#define DEL 127
#endif

bool
readLine(IOSTREAM *in, IOSTREAM *out, char *buffer)
{ int c;
  char *buf = &buffer[strlen(buffer)];
  ttybuf tbuf;

  Slock(in);
  Slock(out);

  PushTty(in, &tbuf, TTY_RAW);		/* just donot prompt */

  for(;;)
  { Sflush(out);

    switch( (c=Sgetc(in)) )
    { case '\n':
      case '\r':
      case EOF:
        *buf++ = EOS;
        PopTty(in, &tbuf);
	Sunlock(in);
	Sunlock(out);

	return c == EOF ? FALSE : TRUE;
      case '\b':
      case DEL:
	if ( trueFeature(TTY_CONTROL_FEATURE) && buf > buffer )
	{ Sfputs("\b \b", out);
	  buf--;
	}
      default:
	if ( trueFeature(TTY_CONTROL_FEATURE) )
	  Sputc(c, out);
	*buf++ = c;
    }
  }
}


IOSTREAM *
PL_current_input()
{ return getStream(Scurin);
}


IOSTREAM *
PL_current_output()
{ return getStream(Scurout);
}


static word
openProtocol(term_t f, bool appnd)
{ IOSTREAM *s;
  term_t mode = PL_new_term_ref();

  pl_noprotocol();

  PL_put_atom(mode, appnd ? ATOM_append : ATOM_write);
  if ( (s = openStream(f, mode, 0)) )
  { Sprotocol = s;
    return TRUE;
  }

  return FALSE;
}


word
pl_noprotocol()
{ IOSTREAM *s;

  if ( (s = getStream(Sprotocol)) )
  { closeStream(s);
    Sprotocol = NULL;
  }

  succeed;
}


		 /*******************************
		 *	 STREAM ATTRIBUTES	*
		 *******************************/


foreign_t
pl_set_stream(term_t stream, term_t attr)
{ IOSTREAM *s;
  atom_t aname;
  int arity;

  if ( !PL_get_stream_handle(stream, &s) )
    fail;

  if ( PL_get_name_arity(attr, &aname, &arity) )
  { if ( aname == ATOM_alias && arity == 1 )
    { term_t a = PL_new_term_ref();
      atom_t alias;
      Symbol symb;
      int i;

      _PL_get_arg(1, attr, a);
      if ( !PL_get_atom(a, &alias) )
	return PL_error("set_stream", 2, NULL, ERR_TYPE, ATOM_atom, alias);
      
      if ( (i=standardStreamIndexFromName(alias)) >= 0 )
      { LD->IO.streams[i] = s;
	if ( i == 0 )
	  LD->prompt.next = TRUE;	/* changed standard input: prompt! */
	succeed;
      }

      LOCK();
      if ( (symb = lookupHTable(streamAliases, (void *)alias)) )
	unaliasStream(symb->value, alias);
      aliasStream(s, alias);
      UNLOCK();
      succeed;
    } 
  }

  return PL_error("set_stream", 2, NULL, ERR_TYPE,
		  PL_new_atom("stream_attribute"), attr);
}


		/********************************
		*          STRING I/O           *
		*********************************/

extern IOFUNCTIONS Smemfunctions;

bool
tellString(char **s, int *size)
{ IOSTREAM *stream;
  
  stream = Sopenmem(s, size, "w");
  pushOutputContext();
  Scurout = stream;

  return TRUE;
}


bool
toldString()
{ IOSTREAM *s = getStream(Scurout);

  if ( s && s->functions == &Smemfunctions )
  { Sputc(EOS, s);
    closeStream(s);
    popOutputContext();
  }

  succeed;
}


		/********************************
		*       WAITING FOR INPUT	*
		********************************/

#ifndef HAVE_SELECT

word
pl_wait_for_input(term_t streams, term_t available,
		  term_t timeout)
{ return notImplemented("wait_for_input", 3);
}

#else

word
pl_wait_for_input(term_t Streams, term_t Available,
		  term_t timeout)
{ fd_set fds;
  struct timeval t, *to;
  double time;
  int n, max = 0;
  term_t streammap[256];
  term_t head      = PL_new_term_ref();
  term_t streams   = PL_copy_term_ref(Streams);
  term_t available = PL_copy_term_ref(Available);

  FD_ZERO(&fds);
  while( PL_get_list(streams, head, streams) )
  { IOSTREAM *s;
    int fd;

    if ( !PL_get_stream_handle(head, &s) < 0 )
      fail;
    if ( (fd=Sfileno(s)) < 0 )
    { releaseStream(s);
      return PL_error("wait_for_input", 3, NULL, ERR_DOMAIN,
		      PL_new_atom("file_stream"), head);
    }
    releaseStream(s);
    streammap[fd] = PL_copy_term_ref(head);

    FD_SET(fd, &fds);
    if ( fd > max )
      max = fd;
  }
  if ( !PL_get_nil(streams) )
    return PL_error("wait_for_input", 3, NULL, ERR_TYPE, ATOM_list, Streams);
  if ( !PL_get_float(timeout, &time) )
    return PL_error("wait_for_input", 3, NULL, ERR_TYPE, ATOM_float, timeout);
  
  if ( time > 0.0 )
  { t.tv_sec  = (int)time;
    t.tv_usec = ((int)(time * 1000000) % 1000000);
    to = &t;
  } else
    to = NULL;

  select(max+1, &fds, NULL, NULL, to);

  for(n=0; n <= max; n++)
  { if ( FD_ISSET(n, &fds) )
    { if ( !PL_unify_list(available, head, available) ||
	   !PL_unify(head, streammap[n]) )
	fail;
    }
  }
  PL_unify_nil(available);

  succeed;
}

#endif /* HAVE_SELECT */

		/********************************
		*      PROLOG CONNECTION        *
		*********************************/

int
PL_get_char(term_t c, int *p)
{ int chr;
  char *s;

  if ( PL_get_integer(c, &chr) )
  { if ( chr >= 0 && chr <= 255 )
    { *p = chr;
      return TRUE;
    }
  } else if ( PL_get_atom_chars(c, &s) && s[0] && s[1] == EOS )
  { *p = s[0]&0xff;
    return TRUE;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_character, c);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_unify_char(term_t chr, int c, int how)
    Unify a character.  Try to be as flexible as possible, only binding a
    variable `chr' to a code or one-char-atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_unify_char(term_t chr, int c, int how)
{ int c2;

  if ( PL_is_variable(chr) )
  { switch(how)
    { case CHAR_MODE:
      { atom_t a = (c == -1 ? ATOM_end_of_file : codeToAtom(c));

	return PL_unify_atom(chr, a);
      }
      case CODE_MODE:
      case BYTE_MODE:
      default:
	return PL_unify_integer(chr, c);
    }
  } else if ( PL_get_char(chr, &c2) )
    return c == c2;

  fail;
}


word
pl_put2(term_t stream, term_t chr)
{ IOSTREAM *s;
  int c;

  if ( !PL_get_char(chr, &c) )
    fail;
  if ( !getOutputStream(stream, &s) )
    fail;

  Sputc(c, s);
  
  return streamStatus(s);
}


word
pl_put(term_t chr)
{ return pl_put2(0, chr);
}


word
pl_get2(term_t in, term_t chr)
{ IOSTREAM *s;

  if ( getInputStream(in, &s) )
  { int c;

    for(;;)
    { c = Sgetc(s);

      if ( c == EOF )
      { TRY(PL_unify_integer(chr, -1));
	return streamStatus(s);
      }

      if ( !isBlank(c) )
      { releaseStream(s);
	return PL_unify_integer(chr, c);
      }
    }
  }

  fail;
}


word
pl_get(term_t chr)
{ return pl_get2(0, chr);
}


word
pl_skip2(term_t in, term_t chr)
{ int c;
  int r;
  IOSTREAM *s;

  if ( !PL_get_char(chr, &c) )
    fail;
  if ( !getInputStream(in, &s) )
    fail;
  
  while((r=Sgetc(s)) != c && r != EOF )
    ;

  return streamStatus(s);
}


word
pl_skip(term_t chr)
{ return pl_skip2(0, chr);
}


word
pl_get_single_char(term_t chr)
{ IOSTREAM *s = getStream(Scurin);
  int c = getSingleChar(s);

  if ( c == EOF )
  { PL_unify_integer(chr, -1);
    return streamStatus(s);
  }

  releaseStream(s);

  return PL_unify_integer(chr, c);
}

word
pl_get_byte2(term_t in, term_t chr)
{ IOSTREAM *s;

  if ( getInputStream(in, &s) )
  { int c = Sgetc(s);

    if ( PL_unify_integer(chr, c) )
      return streamStatus(s);

    if ( Sferror(s) )
      return streamStatus(s);

    PL_get_char(chr, &c);		/* set type-error */
  }

  fail;
}


word
pl_get_byte(term_t c)
{ return pl_get_byte2(0, c);
}


word
pl_get_char2(term_t in, term_t chr)
{ IOSTREAM *s;

  if ( getInputStream(in, &s) )
  { int c = Sgetc(s);

    if ( PL_unify_atom(chr, codeToAtom(c)) )
      return streamStatus(s);

    if ( Sferror(s) )
      return streamStatus(s);

    PL_get_char(chr, &c);		/* set type-error */
  }

  fail;
}


word
pl_get_char(term_t c)
{ return pl_get_char2(0, c);
}

word
pl_ttyflush()
{ IOSTREAM *s = getStream(Suser_output);

  Sflush(s);

  return streamStatus(s);
}


word
pl_protocol(term_t file)
{ return openProtocol(file, FALSE);
}


word
pl_protocola(term_t file)
{ return openProtocol(file, TRUE);
}


word
pl_protocolling(term_t file)
{ IOSTREAM *s;

  if ( (s = Sprotocol) )
  { atom_t a;

    if ( (a = fileNameStream(s)) )
      return PL_unify_atom(file, a);
    else
      return PL_unify_stream(file, s);
  }

  fail;
}


word
pl_prompt(term_t old, term_t new)
{ atom_t a;

  if ( PL_unify_atom(old, LD->prompt.current) &&
       PL_get_atom(new, &a) )
  { LD->prompt.current = a;
    succeed;
  }

  fail;
}


void
prompt1(char *prompt)
{ if ( LD->prompt.first )
    remove_string(LD->prompt.first);
  LD->prompt.first = store_string(prompt);
  LD->prompt.first_used = FALSE;
}


word
pl_prompt1(term_t prompt)
{ char *s;

  if ( PL_get_chars(prompt, &s, CVT_ALL) )
  { prompt1(s);
    succeed;
  }

  return PL_error("prompt1", 1, NULL, ERR_TYPE, ATOM_atom, prompt);
}


char *
PrologPrompt()
{ if ( !LD->prompt.first_used && LD->prompt.first )
  { LD->prompt.first_used = TRUE;

    return LD->prompt.first;
  }

  if ( Sinput->position && Sinput->position->linepos == 0 )
    return stringAtom(LD->prompt.current);
  else
    return "";
}


word
pl_tab2(term_t out, term_t spaces)
{ number n;

  if ( valueExpression(spaces, &n) &&
       toIntegerNumber(&n) )
  { int m = n.value.i;
    IOSTREAM *s;

    if ( !getOutputStream(out, &s) )
      fail;

    while(m-- > 0)
    { if ( Sputc(' ', s) < 0 )
	break;
    }

    return streamStatus(s);
  }

  return PL_error("tab", 1, NULL, ERR_TYPE, ATOM_integer, spaces);
}


word
pl_tab(term_t n)
{ return pl_tab2(0, n);
}

		/********************************
		*       STREAM BASED I/O        *
		*********************************/

static const opt_spec open4_options[] = 
{ { ATOM_type,		 OPT_ATOM },
  { ATOM_reposition,     OPT_BOOL },
  { ATOM_alias,	         OPT_ATOM },
  { ATOM_eof_action,     OPT_ATOM },
  { ATOM_close_on_abort, OPT_BOOL },
  { ATOM_buffer,	 OPT_ATOM },
  { NULL_ATOM,	         0 }
};


IOSTREAM *
openStream(term_t file, term_t mode, term_t options)
{ atom_t mname;
  atom_t type           = ATOM_text;
  bool   reposition     = FALSE;
  atom_t alias	        = NULL_ATOM;
  atom_t eof_action     = ATOM_eof_code;
  atom_t buffer         = ATOM_full;
  bool   close_on_abort = TRUE;
  char   how[10];
  char  *h		= how;
  char *path;
  IOSTREAM *s;

  if ( options )
  { if ( !scan_options(options, 0, ATOM_stream_option, open4_options,
		       &type, &reposition, &alias, &eof_action,
		       &close_on_abort, &buffer) )
      fail;
  }

  if ( PL_get_atom(mode, &mname) )
  {      if ( mname == ATOM_write )
      *h++ = 'w';
    else if ( mname == ATOM_append )
      *h++ = 'a';
    else if ( mname == ATOM_update )
      *h++ = 'u';
    else if ( mname == ATOM_read )
      *h++ = 'r';
    else
    { PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_io_mode, mode);
      return NULL;
    }
  } else
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, mode);
    return NULL;
  }

  if ( type == ATOM_binary )
    *h++ = 'b';
  
  *h = EOS;
  if ( PL_get_chars(file, &path, CVT_ATOM|CVT_STRING|CVT_INTEGER) )
  { if ( !(s = Sopen_file(path, how)) )
    { PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
	       ATOM_open, ATOM_file, file);
      return NULL;
    }
    setFileNameStream(s, PL_new_atom(path));
  } 
#ifdef HAVE_POPEN
  else if ( PL_is_functor(file, FUNCTOR_pipe1) )
  { term_t a = PL_new_term_ref();
    char *cmd;

    PL_get_arg(1, file, a);
    if ( !PL_get_chars(a, &cmd, CVT_ATOM|CVT_STRING) )
    { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, a);
      return NULL;
    }

    if ( !(s = Sopen_pipe(cmd, how)) )
    { PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
	       ATOM_open, ATOM_file, file);
      return NULL;
    }
  }
#endif /*HAVE_POPEN*/
  else
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, file);
    return NULL;
  }

  if ( !close_on_abort )
    s->flags |= SIO_NOCLOSE;

  if ( how[0] == 'r' )
  { if ( eof_action != ATOM_eof_code )
    { if ( eof_action == ATOM_reset )
	s->flags |= SIO_NOFEOF;
      else if ( eof_action == ATOM_error )
	s->flags |= SIO_FEOF2ERR;
    }
  } else
  { if ( buffer != ATOM_full )
    { s->flags &= ~SIO_FBUF;
      if ( buffer == ATOM_line )
	s->flags |= SIO_LBUF;
      if ( buffer == ATOM_false )
	s->flags |= SIO_NBUF;
    }
  }

  if ( alias != NULL_ATOM )
    aliasStream(s, alias);

  return s;
}


word
pl_open4(term_t file, term_t mode, term_t stream, term_t options)
{ IOSTREAM *s = openStream(file, mode, options);

  if ( s )
    return PL_unify_stream(stream, s);

  fail;
}


word
pl_open(term_t file, term_t mode, term_t stream)
{ return pl_open4(file, mode, stream, 0);
}

		 /*******************************
		 *	  EDINBURGH I/O		*
		 *******************************/

word
pl_see(term_t f)
{ if ( !pl_set_input(f) )
  { IOSTREAM *s;
    term_t mode = PL_new_term_ref();

    PL_put_atom(mode, ATOM_read);
    if ( !(s = openStream(f, mode, 0)) )
      fail;

    pushInputContext();
    Scurin = s;
  }

  succeed;
}

word
pl_seeing(term_t f)
{ return pl_current_input(f);
}

word
pl_seen()
{ IOSTREAM *s = Scurin;

  popInputContext();

  if ( s->flags & SIO_NOFEOF )
    succeed;

  return closeStream(s);
}

static word
do_tell(term_t f, atom_t m)
{ if ( !pl_set_output(f) )
  { IOSTREAM *s;
    term_t mode = PL_new_term_ref();

    PL_put_atom(mode, m);
    if ( !(s = openStream(f, mode, 0)) )
      fail;

    pushOutputContext();
    Scurout = s;
  }

  succeed;
}

word
pl_tell(term_t f)
{ return do_tell(f, ATOM_write);
}

word
pl_append(term_t f)
{ return do_tell(f, ATOM_append);
}

word
pl_telling(term_t f)
{ return pl_current_output(f);
}

word
pl_told()
{ IOSTREAM *s = Scurout;

  popOutputContext();

  if ( s->flags & SIO_NOFEOF )
    succeed;

  return closeStream(s);
}

		 /*******************************
		 *	   NULL-STREAM		*
		 *******************************/

static int
Swrite_null(void *handle, char *buf, int size)
{ return size;
}


static int
Sread_null(void *handle, char *buf, int size)
{ return 0;
}


static long
Sseek_null(void *handle, long offset, int whence)
{ switch(whence)
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
{ return 0;
}


static const IOFUNCTIONS nullFunctions =
{ Sread_null,
  Swrite_null,
  Sseek_null,
  Sclose_null
};


word
pl_open_null_stream(term_t stream)
{ int sflags = SIO_NBUF|SIO_RECORDPOS;
  IOSTREAM *s = Snew((void *)NULL, sflags, (IOFUNCTIONS *)&nullFunctions);

  return PL_unify_stream(stream, s);
}


word
pl_close(term_t stream)
{ IOSTREAM *s;

  if ( PL_get_stream_handle(stream, &s) )
    return closeStream(s);

  fail;
}

		 /*******************************
		 *	 STREAM-PROPERTY	*
		 *******************************/

static int
stream_file_name_propery(IOSTREAM *s, term_t prop)
{ atom_t name;

  if ( (name = getStreamContext(s)->filename) )
    return PL_unify_atom(prop, name);

  fail;
}


static int
stream_mode_property(IOSTREAM *s, term_t prop)
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


static int
stream_input_prop(IOSTREAM *s)
{ return (s->flags & SIO_INPUT) ? TRUE : FALSE;
}


static int
stream_output_prop(IOSTREAM *s)
{ return (s->flags & SIO_OUTPUT) ? TRUE : FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Incomplete: should be non-deterministic if the stream has multiple aliases!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
stream_alias_prop(IOSTREAM *s, term_t prop)
{ atom_t name;
  stream_context *ctx = getStreamContext(s);
  
  if ( PL_get_atom(prop, &name) )
  { alias *a;

    for( a = ctx->alias_head; a; a = a->next )
    { if ( a->name == name )
	return TRUE;
    }
    
    return FALSE;
  }

  if ( ctx->alias_head )
    return PL_unify_atom(prop, ctx->alias_head->name);

  return FALSE;
}


static int
stream_position_prop(IOSTREAM *s, term_t prop)
{ if ( s->position )
  { return PL_unify_term(prop,
			 PL_FUNCTOR, FUNCTOR_stream_position3,
			   PL_INTEGER, s->position->charno,
			   PL_INTEGER, s->position->lineno,
			   PL_INTEGER, s->position->linepos);
  }

  fail;
}


static int
stream_end_of_stream_prop(IOSTREAM *s, term_t prop)
{ if ( s->flags & SIO_INPUT )
  { atom_t val;

    if ( s->flags & SIO_FEOF2 )
      val = ATOM_past;
    else if ( s->flags & SIO_FEOF )
      val = ATOM_at;
    else
      val = ATOM_not;

    return PL_unify_atom(prop, val);
  }

  return FALSE;
}


static int
stream_eof_action_prop(IOSTREAM *s, term_t prop)
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

static int
stream_reposition_prop(IOSTREAM *s, term_t prop)
{ atom_t val;

  if ( s->functions->seek )
  {
#ifdef HAVE_FSTAT
    int fd = Sfileno(s);
    struct stat buf;

    if ( fstat(fd, &buf) == 0 && (buf.st_mode & S_IFMT) == S_IFREG )
      val = ATOM_true;
    else
      val = ATOM_false;
#else
    val = ATOM_true;
#endif
  } else
    val = ATOM_false;
  
  return PL_unify_atom(prop, val);
}


static int
stream_type_prop(IOSTREAM *s, term_t prop)
{ return PL_unify_atom(prop, s->flags & SIO_TEXT ? ATOM_text : ATOM_binary);
}


static int
stream_file_no_prop(IOSTREAM *s, term_t prop)
{ int fd;

  if ( (fd = Sfileno(s)) >= 0 )
    return PL_unify_integer(prop, fd);

  fail;
}




typedef struct
{ functor_t functor;			/* functor of property */
  int (*function)();			/* function to generate */
} sprop;


static const sprop sprop_list [] =
{ { FUNCTOR_file_name1,	    stream_file_name_propery },
  { FUNCTOR_mode1,	    stream_mode_property },
  { FUNCTOR_input0,	    stream_input_prop },
  { FUNCTOR_output0,	    stream_output_prop },
  { FUNCTOR_alias1,	    stream_alias_prop },
  { FUNCTOR_position1,	    stream_position_prop },
  { FUNCTOR_end_of_stream1, stream_end_of_stream_prop },
  { FUNCTOR_eof_action1,    stream_eof_action_prop },
  { FUNCTOR_reposition1,    stream_reposition_prop },
  { FUNCTOR_type1,    	    stream_type_prop },
  { FUNCTOR_file_no1,	    stream_file_no_prop },
  { 0,			    NULL }
};


typedef struct
{ TableEnum e;				/* Enumerator on stream-table */
  IOSTREAM *s;				/* Stream we are enumerating */
  const sprop *p;			/* Pointer in properties */
} prop_enum;


foreign_t
pl_stream_property(term_t stream, term_t property, word h)
{ IOSTREAM *s;
  prop_enum *pe;
  mark m;
  term_t a1;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      a1 = PL_new_term_ref();
      
      if ( PL_is_variable(stream) )	/* generate */
      {	pe = allocHeap(sizeof(*pe));

	pe->e = newTableEnum(streamContext);
	pe->s = NULL;
	pe->p = sprop_list;

	break;
      } else if ( PL_get_stream_handle(stream, &s) )
      { functor_t f;

	if ( PL_is_variable(property) )	/* generate properties */
	{ pe = allocHeap(sizeof(*pe));

	  pe->e = NULL;
	  pe->s = s;
	  pe->p = sprop_list;
	  releaseStream(s);		/* no locked stream? */

	  break;
	} else if ( PL_get_functor(property, &f) )
	{ const sprop *p = sprop_list;

	  for( ; p->functor; p++ )
	  { if ( f == p->functor )
	    { int rval;

	      switch(arityFunctor(f))
	      { case 0:
		  rval = (*p->function)(s);
		  break;
		case 1:
		{ term_t a1 = PL_new_term_ref();

		  _PL_get_arg(1, property, a1);
		  rval = (*p->function)(s, a1);
		  break;
		}
		default:
		  assert(0);
		  rval = FALSE;
	      }
	      releaseStream(s);
	      return rval;
	    }
	  }
	} else
	  return PL_error(NULL, 0, NULL, ERR_TYPE,
			  ATOM_stream_property, property);
      } else
	fail;				/* bad stream handle */
    case FRG_REDO:
    { pe = ForeignContextPtr(h);
      a1 = PL_new_term_ref();
      
      break;
    }
    case FRG_CUTTED:
    { pe = ForeignContextPtr(h);

      if ( pe->e )
	freeTableEnum(pe->e);

      freeHeap(pe, sizeof(*pe));
      succeed;
    }
    default:
      assert(0);
      fail;
  }


  Mark(m);
  for(;;)
  { if ( pe->s )				/* given stream */
    { mark m2;
  
      if ( PL_is_variable(stream) )
      { if ( !PL_unify_stream(stream, pe->s) )
	  goto enum_e;
      }

      Mark(m2);
      for( ; pe->p->functor ; pe->p++ )
      { if ( PL_unify_functor(property, pe->p->functor) )
	{ int rval;

	  switch(arityFunctor(pe->p->functor))
	  { case 0:
	      rval = (*pe->p->function)(pe->s);
	      break;
	    case 1:
	    { _PL_get_arg(1, property, a1);

	      rval = (*pe->p->function)(pe->s, a1);
	      break;
	    }
	    default:
	      assert(0);
	      rval = FALSE;
	  }
	  if ( rval )
	  { pe->p++;
	    ForeignRedoPtr(pe);
	  }
	}
	Undo(m2);
      }
      pe->s = NULL;
    }
  
  enum_e:
    if ( pe->e )
    { Symbol symb;

      while ( (symb=advanceTableEnum(pe->e)) )
      { Undo(m);
	if ( PL_unify_stream(stream, symb->name) )
	{ pe->s = symb->name;
	  pe->p = sprop_list;
	  break;
	}
      } 
    }

    if ( !pe->s )
    { if ( pe->e )
	freeTableEnum(pe->e);

      freeHeap(pe, sizeof(*pe));
      fail;
    }
  }
}


		 /*******************************
		 *	      FLUSH		*
		 *******************************/


word
pl_flush_output(term_t out)
{ IOSTREAM *s;

  if ( getOutputStream(out, &s) )
  { Sflush(s);
    return streamStatus(s);
  }

  succeed;
}


word
pl_flush()
{ return pl_flush_output(0);
}


static int
getStreamWithPosition(term_t stream, IOSTREAM **sp)
{ IOSTREAM *s;

  if ( PL_get_stream_handle(stream, &s) )
  { if ( !s->position )
    { PL_error(NULL, 0, NULL, ERR_PERMISSION, /* non-ISO */
	       ATOM_property, ATOM_position, stream);
      releaseStream(s);
      return FALSE;
    }

    *sp = s;
    return TRUE;
  }

  return FALSE;
}


word
pl_stream_position(term_t stream, term_t old, term_t new)
{ IOSTREAM *s;
  long oldcharno, charno, linepos, lineno;
  term_t a = PL_new_term_ref();
  functor_t f;

  if ( !(getStreamWithPosition(stream, &s)) )
    fail;

  charno  = s->position->charno;
  lineno  = s->position->lineno;
  linepos = s->position->linepos;
  oldcharno = charno;

  if ( !PL_unify_term(old,
		      PL_FUNCTOR, FUNCTOR_stream_position3,
		        PL_INTEGER, charno,
		        PL_INTEGER, lineno,
		        PL_INTEGER, linepos) )
  { releaseStream(s);
    fail;
  }

  if ( !(PL_get_functor(new, &f) && f == FUNCTOR_stream_position3) ||
       !PL_get_arg(1, new, a) ||
       !PL_get_long(a, &charno) ||
       !PL_get_arg(2, new, a) ||
       !PL_get_long(a, &lineno) ||
       !PL_get_arg(3, new, a) ||
       !PL_get_long(a, &linepos) )
  { releaseStream(s);
    return PL_error("stream_position", 3, NULL,
		    ERR_DOMAIN, ATOM_stream_position, new);
  }

  if ( charno != oldcharno && Sseek(s, charno, 0) < 0 )
  { releaseStream(s);
    return PL_error("stream_position", 3, OsError(),
		    ERR_STREAM_OP, ATOM_position, stream);
  }

  s->position->charno  = charno;
  s->position->lineno  = lineno;
  s->position->linepos = linepos;
  
  releaseStream(s);

  succeed;
}


word
pl_seek(term_t stream, term_t offset, term_t method, term_t newloc)
{ atom_t m;
  int whence = -1;
  long off, new;
  IOSTREAM *s;

  if ( !(PL_get_atom(method, &m)) )
    goto badmethod;

  if ( m == ATOM_bof )
    whence = SIO_SEEK_SET;
  else if ( m == ATOM_current )
    whence = SIO_SEEK_CUR;
  else if ( m == ATOM_eof )
    whence = SIO_SEEK_END;
  else
  { badmethod:
    return PL_error("seek", 4, NULL, ERR_DOMAIN, ATOM_seek_method, method);
  }
  
  if ( !PL_get_long(offset, &off) )
    return PL_error("seek", 4, NULL, ERR_DOMAIN, ATOM_integer, offset);

  if ( PL_get_stream_handle(stream, &s) )
  { if ( (new = Sseek(s, off, whence)) == -1 )
    { PL_error("seek", 4, OsError(), ERR_PERMISSION,
	       ATOM_reposition, ATOM_stream, stream);
      releaseStream(s);
      fail;
    }

    releaseStream(s);
    return PL_unify_integer(newloc, new);
  }

  fail;
}


word
pl_set_input(term_t stream)
{ IOSTREAM *s;

  if ( getInputStream(stream, &s) )
  { Scurin = s;
    return TRUE;
  }

  return FALSE;
}


word
pl_set_output(term_t stream)
{ IOSTREAM *s;

  if ( getOutputStream(stream, &s) )
  { Scurout = s;
    return TRUE;
  }

  return FALSE;
}


word
pl_current_input(term_t stream)
{ return PL_unify_stream(stream, Scurin);
}


word
pl_current_output(term_t stream)
{ return PL_unify_stream(stream, Scurout);
}


word
pl_character_count(term_t stream, term_t count)
{ IOSTREAM *s;

  if ( getStreamWithPosition(stream, &s) )
  { long n = s->position->charno;

    releaseStream(s);
    return PL_unify_integer(count, n);
  }

  fail;
}


word
pl_line_count(term_t stream, term_t count)
{ IOSTREAM *s;

  if ( getStreamWithPosition(stream, &s) )
  { long n = s->position->lineno;

    releaseStream(s);
    return PL_unify_integer(count, n);
  }

  fail;
}


word
pl_line_position(term_t stream, term_t count)
{ IOSTREAM *s;

  if ( getStreamWithPosition(stream, &s) )
  { long n = s->position->linepos;

    releaseStream(s);
    return PL_unify_integer(count, n);
  }

  fail;
}


word
pl_source_location(term_t file, term_t line)
{ char *s;
  char tmp[MAXPATHLEN];

  if ( ReadingSource &&
       (s = AbsoluteFile(stringAtom(source_file_name), tmp)) &&
	PL_unify_atom_chars(file, s) &&
	PL_unify_integer(line, source_line_no) )
    succeed;
  
  fail;
}


word
pl_at_end_of_stream1(term_t stream)
{ IOSTREAM *s;

  if ( getInputStream(stream, &s) )
  { int rval = (Sfeof(s) ? TRUE : FALSE);
    
    releaseStream(s);
    return rval;
  }

  return FALSE;				/* exception */
}


word
pl_at_end_of_stream0()
{ return pl_at_end_of_stream1(0);
}

word
peek(term_t stream, term_t chr, int how)
{ IOSTREAM *s;
  IOPOS pos;
  int c;

  if ( !getInputStream(stream, &s) )
    fail;

  pos = s->posbuf;
  c = Sgetc(s);
  Sungetc(c, s);
  s->posbuf = pos;
  if ( Sferror(s) )
    return streamStatus(s);
  releaseStream(s);

  return PL_unify_char(chr, c, how);
}


word
pl_peek_byte2(term_t stream, term_t chr)
{ return peek(stream, chr, BYTE_MODE);
}


word
pl_peek_byte1(term_t chr)
{ return peek(0, chr, BYTE_MODE);
}

word
pl_peek_code2(term_t stream, term_t chr)
{ return peek(stream, chr, CODE_MODE);
}


word
pl_peek_code1(term_t chr)
{ return peek(0, chr, CODE_MODE);
}

word
pl_peek_char2(term_t stream, term_t chr)
{ return peek(stream, chr, CHAR_MODE);
}


word
pl_peek_char1(term_t chr)
{ return peek(0, chr, CHAR_MODE);
}


		/********************************
		*             FILES             *
		*********************************/

bool
unifyTime(term_t t, long time)
{ return PL_unify_float(t, (double)time);
}


char *
PL_get_filename(term_t n, char *buf, unsigned int size)
{ char *name;
  char tmp[MAXPATHLEN];

  if ( !PL_get_chars(n, &name, CVT_ALL) )
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, n);
    return NULL;
  }
  if ( trueFeature(FILEVARS_FEATURE) )
  { if ( !(name = ExpandOneFile(name, tmp)) )
      return NULL;
  }

  if ( buf )
  { if ( strlen(name) < size )
    { strcpy(buf, name);
      return buf;
    }

    PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
	     ATOM_max_path_length);
    return NULL;
  } else
    return buffer_string(name, 0);
}


word
pl_time_file(term_t name, term_t t)
{ char *fn;

  if ( (fn = PL_get_filename(name, NULL, 0)) )
  { long time;

    if ( (time = LastModifiedFile(fn)) == -1 )
      fail;

    return unifyTime(t, time);
  }

  fail;
}


word
pl_size_file(term_t name, term_t len)
{ char *n;

  if ( (n = PL_get_filename(name, NULL, 0)) )
  { long size;

    if ( (size = SizeFile(n)) < 0 )
      return PL_error("size_file", 2, OsError(), ERR_FILE_OPERATION,
		      ATOM_size, ATOM_file, name);

    return PL_unify_integer(len, size);
  }

  fail;
}


word
pl_size_stream(term_t stream, term_t len)
{ IOSTREAM *s;

  if ( !PL_get_stream_handle(stream, &s) )
    fail;

  return PL_unify_integer(len, Ssize(s));
}


word
pl_access_file(term_t name, term_t mode)
{ char *n;
  int md;
  atom_t m;

  if ( !PL_get_atom(mode, &m) )
    return PL_error("access_file", 2, NULL, ERR_TYPE, ATOM_atom, mode);
  if ( !(n=PL_get_filename(name, NULL, 0)) )
    fail;

  if ( m == ATOM_none )
    succeed;
  
  if      ( m == ATOM_write || m == ATOM_append )
    md = ACCESS_WRITE;
  else if ( m == ATOM_read )
    md = ACCESS_READ;
  else if ( m == ATOM_execute )
    md = ACCESS_EXECUTE;
  else if ( m == ATOM_exist )
    md = ACCESS_EXIST;
  else
    return PL_error("access_file", 2, NULL, ERR_DOMAIN, ATOM_io_mode, mode);

  if ( AccessFile(n, md) )
    succeed;

  if ( md == ACCESS_WRITE && !AccessFile(n, ACCESS_EXIST) )
  { char tmp[MAXPATHLEN];
    char *dir = DirName(n, tmp);

    if ( dir[0] )
    { if ( !ExistsDirectory(dir) )
	fail;
    }
    if ( AccessFile(dir[0] ? dir : ".", md) )
      succeed;
  }

  fail;
}


word
pl_read_link(term_t file, term_t link, term_t to)
{ char *n, *l, *t;
  char buf[MAXPATHLEN];

  if ( !(n = PL_get_filename(file, NULL, 0)) )
    fail;

  if ( (l = ReadLink(n, buf)) &&
       PL_unify_atom_chars(link, l) &&
       (t = DeRefLink(n, buf)) &&
       PL_unify_atom_chars(to, t) )
    succeed;

  fail;
}


word
pl_exists_file(term_t name)
{ char *n;

  if ( !(n = PL_get_filename(name, NULL, 0)) )
    fail;
  
  return ExistsFile(n);
}


word
pl_exists_directory(term_t name)
{ char *n;

  if ( !(n = PL_get_filename(name, NULL, 0)) )
    fail;
  
  return ExistsDirectory(n);
}


word
pl_tmp_file(term_t base, term_t name)
{ char *n;

  if ( !PL_get_chars(base, &n, CVT_ALL) )
    return PL_error("tmp_file", 2, NULL, ERR_TYPE, ATOM_atom, base);

  return PL_unify_atom(name, TemporaryFile(n));
}


word
pl_delete_file(term_t name)
{ char *n;

  if ( !(n = PL_get_filename(name, NULL, 0)) )
    fail;
  
  return RemoveFile(n);
}


word
pl_same_file(term_t file1, term_t file2)
{ char *n1, *n2;
  char name1[MAXPATHLEN];

  if ( (n1 = PL_get_filename(file1, name1, sizeof(name1))) &&
       (n2 = PL_get_filename(file2, NULL, 0)) )
    return SameFile(name1, n2);

  fail;
}


word
pl_rename_file(term_t old, term_t new)
{ char *o, *n;
  char ostore[MAXPATHLEN];

  if ( (o = PL_get_filename(old, ostore, sizeof(ostore))) &&
       (n = PL_get_filename(new, NULL, 0)) )
  { if ( RenameFile(ostore, n) )
      succeed;

    if ( fileerrors )
      return PL_error("rename_file", 2, OsError(), ERR_FILE_OPERATION,
		      ATOM_rename, ATOM_file, old);
    fail;
  }

  fail;
}


word
pl_fileerrors(term_t old, term_t new)
{ return setBoolean(&fileerrors, "fileerrors", old, new);
}


word
pl_absolute_file_name(term_t name, term_t expanded)
{ char *n;
  char tmp[MAXPATHLEN];

  if ( (n = PL_get_filename(name, NULL, 0)) &&
       (n = AbsoluteFile(n, tmp)) )
    return PL_unify_atom_chars(expanded, n);

  fail;
}


word
pl_is_absolute_file_name(term_t name)
{ char *n;

  if ( (n = PL_get_filename(name, NULL, 0)) &&
       IsAbsolutePath(n) )
    succeed;

  fail;
}


word
pl_chdir(term_t dir)
{ char *n;

  if ( (n = PL_get_filename(dir, NULL, 0)) )
  { if ( ChDir(n) )
      succeed;

    if ( fileerrors )
      return PL_error("chdir", 1, NULL, ERR_FILE_OPERATION,
		      ATOM_chdir, ATOM_directory, dir);
    fail;
  }

  fail;
}


word
pl_file_base_name(term_t f, term_t b)
{ char *n;

  if ( !PL_get_chars(f, &n, CVT_ALL) )
    return PL_error("file_base_name", 2, NULL, ERR_TYPE, ATOM_atom, f);

  return PL_unify_atom_chars(b, BaseName(n));
}


word
pl_file_dir_name(term_t f, term_t b)
{ char *n;
  char tmp[MAXPATHLEN];

  if ( !PL_get_chars(f, &n, CVT_ALL) )
    return PL_error("file_dir_name", 2, NULL, ERR_TYPE, ATOM_atom, f);

  return PL_unify_atom_chars(b, DirName(n, tmp));
}


static int
has_extension(const char *name, const char *ext)
{ const char *s = name + strlen(name);

  if ( ext[0] == EOS )
    succeed;

  while(*s != '.' && *s != '/' && s > name)
    s--;
  if ( *s == '.' && s > name && s[-1] != '/' )
  { if ( ext[0] == '.' )
      ext++;
    if ( trueFeature(FILE_CASE_FEATURE) )
      return strcmp(&s[1], ext) == 0;
    else
      return stricmp(&s[1], ext) == 0;
  }

  fail;
}


word
pl_file_name_extension(term_t base, term_t ext, term_t full)
{ char *b = NULL, *e = NULL, *f;
  char buf[MAXPATHLEN];

  if ( PL_get_chars(full, &f, CVT_ALL) )
  { char *s = f + strlen(f);		/* ?base, ?ext, +full */

    while(*s != '.' && *s != '/' && s > f)
      s--;
    if ( *s == '.' )
    { if ( PL_get_chars(ext, &e, CVT_ALL) )
      { if ( e[0] == '.' )
	  e++;
	if ( trueFeature(FILE_CASE_FEATURE) )
	{ TRY(strcmp(&s[1], e) == 0);
	} else
	{ TRY(stricmp(&s[1], e) == 0);
	}
      } else
      { TRY(PL_unify_atom_chars(ext, &s[1]));
      }
      if ( s-f > MAXPATHLEN )
      { maxpath:
	return PL_error("file_name_extension", 3, NULL, ERR_REPRESENTATION,
			ATOM_max_path_length);
      }
      strncpy(buf, f, s-f);
      buf[s-f] = EOS;

      return PL_unify_atom_chars(base, buf);
    }
    if ( PL_unify_atom_chars(ext, "") &&
	 PL_unify(full, base) )
      PL_succeed;

    PL_fail;
  } else if ( !PL_is_variable(full) )
    return PL_error("file_name_extension", 3, NULL, ERR_TYPE,
		    ATOM_atom, full);

  if ( PL_get_chars(base, &b, CVT_ALL|BUF_RING) &&
       PL_get_chars(ext, &e, CVT_ALL) )
  { char *s;

    if ( e[0] == '.' )		/* +Base, +Extension, -full */
      e++;
    if ( has_extension(b, e) )
      return PL_unify(base, full);
    if ( strlen(b) + 1 + strlen(e) + 1 > MAXPATHLEN )
      goto maxpath;
    strcpy(buf, b);
    s = buf + strlen(buf);
    *s++ = '.';
    strcpy(s, e);

    return PL_unify_atom_chars(full, buf);
  }

  if ( !b )
    return PL_error("file_name_extension", 3, NULL, ERR_TYPE,
		    ATOM_atom, base);
  return PL_error("file_name_extension", 3, NULL, ERR_TYPE,
		  ATOM_atom, ext);
}


word
pl_prolog_to_os_filename(term_t pl, term_t os)
{
#ifdef O_XOS
  char *n;
  char buf[MAXPATHLEN];

  if ( PL_get_chars(pl, &n, CVT_ALL) )
  { _xos_os_filename(n, buf);
    return PL_unify_atom_chars(os, buf);
  }
  if ( !PL_is_variable(pl) )
    return PL_error("prolog_to_os_filename", 2, NULL, ERR_TYPE,
		    ATOM_atom, pl);

  if ( PL_get_chars(os, &n, CVT_ALL) )
  { _xos_canonical_filename(n, buf);
    return PL_unify_atom_chars(pl, buf);
  }

  return PL_error("prolog_to_os_filename", 2, NULL, ERR_TYPE,
		  ATOM_atom, os);
#else /*O_XOS*/
  return PL_unify(pl, os);
#endif /*O_XOS*/
}


foreign_t
pl_mark_executable(term_t path)
{ char name[MAXPATHLEN];

  if ( !PL_get_filename(path, name, sizeof(name)) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_source_sink, path);

  return MarkExecutable(name);
}


#if defined(O_XOS) && defined(__WIN32__)
word
pl_make_fat_filemap(term_t dir)
{ char *n;

  if ( (n = PL_get_filename(dir, NULL, 0)) )
  { if ( _xos_make_filemap(n) == 0 )
      succeed;

    if ( fileerrors )
      return PL_error("make_fat_filemap", 1, NULL, ERR_FILE_OPERATION,
		      ATOM_write, ATOM_file, dir);

    fail;
  }
  
  fail;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
copy_stream_data(+StreamIn, +StreamOut, [Len])
	Copy all data from StreamIn to StreamOut.  Should be somewhere else,
	and maybe we need something else to copy resources.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


foreign_t
pl_copy_stream_data3(term_t in, term_t out, term_t len)
{ IOSTREAM *i, *o;
  int c;

  if ( !getInputStream(in, &i) ||
       !getOutputStream(out, &o) )
    return FALSE;

  if ( !len )
  { while ( (c = Sgetc(i)) != EOF )
    { if ( Sputc(c, o) < 0 )
	return streamStatus(o);
    }
  } else
  { long n;

    if ( !PL_get_long(len, &n) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, len);
    
    while ( n-- > 0 && (c = Sgetc(i)) != EOF )
    { if ( Sputc(c, o) < 0 )
	return streamStatus(o);
    }
  }

  return streamStatus(i);
}

foreign_t
pl_copy_stream_data2(term_t in, term_t out)
{ return pl_copy_stream_data3(in, out, 0);
}

