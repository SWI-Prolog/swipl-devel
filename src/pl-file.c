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

#if defined(__WINDOWS__) || defined(__NT__)
#include "windows.h"
#undef TRANSPARENT
#undef FD_SET
#undef FD_ISSET
#undef FD_ZERO
#endif

#include "pl-incl.h"
#include "pl-ctype.h"
#ifdef __WIN32__
#include <console.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#ifdef HAVE_UNISTD_H
#define lock lock_function		/* WATCOM defines function lock() */
#include <unistd.h>
#undef lock
#endif

#define ST_TERMINAL 0			/* terminal based stream */
#define ST_FILE	    1			/* File bound stream */
#define ST_PIPE	    2			/* Pipe bound stream */
#define ST_STRING   3			/* String bound stream */

					/* openStream() flags */
#define OPEN_OPEN   0x1			/* Open for open/[3,4] */
#define OPEN_TEXT   0x2			/* Open in text-mode */

typedef struct plfile *	PlFile;

static struct plfile
{ Atom		name;			/* name of file */
  Atom		stream_name;		/* stream identifier name */
  IOSTREAM *	stream;			/* IOSTREAM package descriptor */
  int		status;			/* F_CLOSED, F_READ, F_WRITE */
  int		type;			/* ST_FILE, ST_PIPE, ST_STRING */
} *fileTable = (PlFile) NULL;		/* Our file table */

int 	Input;				/* current input */
int	Output;				/* current output */

ttybuf	ttytab;				/* saved terminal status on entry */
int	ttymode;			/* Current tty mode */

static Atom prompt_atom;		/* current prompt */
static char *first_prompt;		/* First-line prompt */
static int first_prompt_used;		/* flag */
static int protocolStream = -1;		/* doing protocolling on stream <n> */

static int   maxfiles;			/* maximum file index */

typedef struct input_context * InputContext;
typedef struct output_context * OutputContext;

static struct input_context
{ int	stream;				/* pushed input */
  Atom	term_file;			/* old term_position file */
  int	term_line;			/* old term_position line */
  InputContext previous;		/* previous context */
} *input_context_stack = NULL;

static struct output_context
{ int	stream;				/* pushed input */
  OutputContext previous;		/* previous context */
} *output_context_stack = NULL;

forwards bool	openStream(term_t file, int mode, int flags);
forwards bool	closeStream(int);
forwards bool	unifyStreamName(term_t, int);
forwards bool	unifyStreamNo(term_t, int);
forwards bool	setUnifyStreamNo(term_t, int);
forwards bool	unifyStreamMode(term_t, int);
forwards int	Get0();

#ifdef SIGPIPE
static void
pipeHandler(int sig)
{ warning("Broken pipe\n");
  pl_abort();

  signal(SIGPIPE, SIG_DFL);		/* should abort fail. */
  kill(getpid(), SIGPIPE);		/* Unix has both pipes and kill() */
}
#endif /* SIGPIPE */

void
initIO(void)
{ int n;

  fileerrors = TRUE;
  if ( maxfiles != getdtablesize() )
  { if ( fileTable != (PlFile) NULL )
      freeHeap(fileTable, sizeof(struct plfile) * maxfiles);
    maxfiles = getdtablesize();
    fileTable = allocHeap(sizeof(struct plfile) * maxfiles);
  }

#ifdef __unix__
  if ( !isatty(0) || !isatty(1) )	/* Sinput is not a tty */
    status.notty = TRUE;
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initilise user input, output and error  stream.   How  to do this neatly
without the Unix assumptions?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  for(n=0; n<maxfiles; n++)
  { PlFile f = &fileTable[n];

    switch(n)
    { case 0:
	f->name	       = ATOM_user;
	f->stream_name = ATOM_user_input;
	f->stream      = Sinput;
	f->status      = F_READ;
	f->type	       = ST_TERMINAL;
	break;
      case 1:
	f->name        = ATOM_user;
	f->stream_name = ATOM_user_output;
	f->stream      = Soutput;
	f->status      = F_WRITE;
	f->type	       = ST_TERMINAL;
	break;
      case 2:
	f->name        = ATOM_stderr;
	f->stream_name = ATOM_user_error;
	f->stream      = Serror;
	f->status      = F_WRITE;
	f->type	       = ST_TERMINAL;
	break;
      default:
	f->name        = NULL;
        f->stream      = NULL;
	f->type        = ST_FILE;
	f->status      = F_CLOSED;
    }
  }

  ResetTty();
  Sinput->position  = &Sinput->posbuf;	/* position logging */
  Soutput->position = &Sinput->posbuf;
  Serror->position  = &Sinput->posbuf;

  ttymode = TTY_COOKED;
  PushTty(&ttytab, TTY_SAVE);

  Input = 0;
  Output = 1;

  if ( prompt_atom == (Atom) NULL )
    prompt_atom = ATOM_prompt;
}


void
dieIO()
{ if ( status.io_initialised == TRUE )
  { pl_noprotocol();
    closeFiles();
    PopTty(&ttytab);
  }
}


static bool
closeStream(int n)
{ PlFile f = &fileTable[n];

  if ( f->stream )
  { switch(n)
    { case 0:
	Sclearerr(f->stream);
        break;
      case 1:
      case 2:
	Sflush(f->stream);
        break;
      default:
        Sclose(f->stream);
        f->stream = NULL;
	f->name   = NULL;
	f->status = F_CLOSED;
	break;
    }
  }

  succeed;
}


void
closeFiles(void)
{ int n;
#if O_PCE
  extern int read_nesting;
  read_nesting = 0;
#endif

  for(n=3; n<maxfiles; n++)
  { if ( n != protocolStream )
      closeStream(n);
  }

  Input = 0;
  Output = 1;
}


void
protocol(int c)
{ if ( protocolStream >= 0 )
  { int out;
  
    out = Output;
    Output = protocolStream;
    Put(c);
    Output = out;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
push/popInputContext() maintain the source_location   info  over see(X),
..., seen(X). This is very  hairy.   Note  the common seeing(O), see(N),
..., seen, see(O) construct. To fix this   one, see/1 will only push the
context if it concerns a new stream and seen() will only pop if it is an
open stream.

Should be fixed decently if we redesign all of I/O stream manegement.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
pushInputContext()
{ InputContext c = allocHeap(sizeof(struct input_context));

  c->stream           = Input;
  c->term_file        = source_file_name;
  c->term_line        = source_line_no;
  c->previous         = input_context_stack;
  input_context_stack = c;
}


static void
popInputContext()
{ InputContext c = input_context_stack;

  if ( c )
  { Input               = c->stream;
    source_file_name    = c->term_file;
    source_line_no      = c->term_line;
    input_context_stack = c->previous;
    freeHeap(c, sizeof(struct input_context));
  } else
    Input = 0;
}

static void
pushOutputContext()
{ OutputContext c = allocHeap(sizeof(struct output_context));

  c->stream            = Output;
  c->previous          = output_context_stack;
  output_context_stack = c;
}


static void
popOutputContext()
{ OutputContext c = output_context_stack;

  if ( c )
  { Output               = c->stream;
    output_context_stack = c->previous;
    freeHeap(c, sizeof(struct output_context));
  } else
    Output = 0;
}


int
currentLinePosition()
{ IOSTREAM *stream = fileTable[Output].stream;

  if ( stream && stream->position )
    return stream->position->linepos;

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a single character from the terminal without waiting for  a  return.
The  character  should  not  be  echoed.   If  status.notty is true this
function will read the first character and then skip all character  upto
and including the newline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
getSingleChar(void)
{ int c;
  int OldIn = Input;
  ttybuf buf;
    
  Input = 0;
  debugstatus.suspendTrace++;
  pl_ttyflush();
  PushTty(&buf, TTY_RAW);		/* just donot prompt */
  
  if ( status.notty )
  { Char c2;

    c2 = Get0();
    while( c2 == ' ' || c2 == '\t' )	/* skip blanks */
      c2 = Get0();
    c = c2;
    while( c2 != EOF && c2 != '\n' )	/* read upto newline */
      c2 = Get0();
  } else
    c = Get0();

  PopTty(&buf);
  debugstatus.suspendTrace--;
  Input = OldIn;

  return c;
}


#ifndef DEL
#define DEL 127
#endif

bool
readLine(char *buffer)
{ int oldin = Input;
  int oldout = Output;
  int c;
  char *buf = &buffer[strlen(buffer)];
  ttybuf tbuf;

  Input = 0;
  Output = 1;
  PushTty(&tbuf, TTY_RAW);		/* just donot prompt */

  for(;;)
  { pl_flush();

    switch( (c=Get0()) )
    { case '\n':
      case '\r':
      case EOF:
        *buf++ = EOS;
        Input = oldin;
	Output = oldout;
	PopTty(&tbuf);

	return c == EOF ? FALSE : TRUE;
      case '\b':
      case DEL:
	if ( buf > buffer )
	{ Putf("\b \b");
	  buf--;
	}
      default:
	Put(c);
	*buf++ = c;
    }
  }
}


bool
LockStream()
{ IOSTREAM *s = fileTable[Output].stream;

  return (s && Slock(s) < 0) ? FALSE : TRUE;
}


bool
UnlockStream()
{ IOSTREAM *s = fileTable[Output].stream;

  return (s && Sunlock(s) < 0) ? FALSE : TRUE;
}


bool
Put(int c)
{ IOSTREAM *s = fileTable[Output].stream;

  return (s && Sputc(c, s)) < 0 ? FALSE : TRUE;
}


bool
Puts(const char *str)
{ IOSTREAM *s = fileTable[Output].stream;

  return (s && Sfputs(str, s)) < 0 ? FALSE : TRUE;
}


static int
Get0()
{ IOSTREAM *s = fileTable[Input].stream;
  int c;
  
  if ( s )
  { c = Sgetc(s);
    
    if ( c == EOF && Sfpasteof(s) )
      warning("Attempt to read past end-of-file");
  } else
    c = EOF;

  return c;
}


IOSTREAM *
PL_current_input()
{ return fileTable[Input].stream;
}


IOSTREAM *
PL_current_output()
{ return fileTable[Output].stream;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formated put.  It would be better to define our own formated  write  for
this  which  accepts  both  Prolog data structures (ints, floats, atoms,
etc) and C data structures.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
Putf(char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svfprintf(fileTable[Output].stream, fm, args);
  va_end(args);

  return rval < 0 ? FALSE : TRUE;
}

word
vPutf(char *fm, va_list args)
{ return Svfprintf(fileTable[Output].stream, fm, args) < 0 ? FALSE : TRUE;
}


int
currentInputLine()
{ IOSTREAM *stream = fileTable[Input].stream;

  if ( stream && stream->position )
    return stream->position->lineno;
  else
    return -1;
}


bool
PL_open_stream(term_t handle, IOSTREAM *s)
{ int n;
  PlFile f;

  for(n=3, f=&fileTable[n]; n<maxfiles; n++, f++)
  { if ( !f->stream )
    { f->stream = s;
      f->name   = NULL;
      f->type   = ST_FILE;
      if ( s->flags & SIO_INPUT )
	f->status = F_READ;
      else
	f->status = F_WRITE;

      return PL_unify_integer(handle, n);
    }
  }

  return warning("Out of IO streams");
}


static bool
openStream(term_t file, int mode, int flags)
{ int n;
  IOSTREAM *stream;
  char cmode[3];
  Atom name;
  functor_t f;
  int type;

  DEBUG(2, Sdprintf("openStream file=0x%lx, mode=%d\n", file, mode));

  if ( PL_get_atom(file, &name) )
  { type = ST_FILE;
  } else if ( PL_get_functor(file, &f) && f == FUNCTOR_pipe1)
  {
#ifdef SIGPIPE
    term_t an = PL_new_term_ref();
    type = ST_PIPE;
    
    if ( !PL_get_arg(1, file, an) ||
	 !PL_get_atom(an, &name) )
      return warning("Illegal argument to pipe(Command)");

    signal(SIGPIPE, pipeHandler);
#else
    return warning("Pipes are not supported on this OS");
#endif /*SIGPIPE*/
  } else
    return warning("Illegal stream specification");

  DEBUG(3, Sdprintf("File/command name = %s\n", stringAtom(f)));
  if ( type == ST_FILE )
  { if ( mode == F_READ )
    { if ( name == ATOM_user || name == ATOM_user_input )
      { Input = 0;
	succeed;
      }
    } else
    { if ( name == ATOM_user || name == ATOM_user_output )
      { Output = 1;
        succeed;
      }
      if ( name == ATOM_user_error || name == ATOM_stderr )
      { Output = 2;
	succeed;
      }
    }
  } else if ( type == ST_PIPE && mode == F_APPEND )
    return warning("Cannot open a pipe in `append' mode");
    
  if ( !(flags & OPEN_OPEN) )		/* see/1, tell/1, append/1 */
  { for( n=0; n<maxfiles; n++ )
    { if ( fileTable[n].name == name && fileTable[n].type == type )
      { if ( fileTable[n].status == mode )
	{ switch(mode)
	  { case F_READ:	Input = n; break;
	    case F_WRITE:
	    case F_APPEND:	Output = n; break;
	  }
	  DEBUG(3, Sdprintf("Switched back to already open stream %d\n", n));
	  succeed;
	} else
	{ closeStream(n);
	}
	break;
      }
    }

    if ( mode == F_READ )
      pushInputContext();		/* see/1 to a new file */
  }

  DEBUG(2, Sdprintf("Starting Unix open\n"));
  cmode[0] = (mode == F_READ ? 'r' : mode == F_WRITE ? 'w' : 'a');
  if ( flags & OPEN_TEXT )
    cmode[1] = EOS;
  else
  { cmode[1] = 'b';
    cmode[2] = EOS;
  }

#ifdef HAVE_POPEN
  if ( type == ST_PIPE )
  { if ( !(stream=Sopen_pipe(stringAtom(name), cmode)) )
    { if ( fileerrors )
	warning("Cannot open pipe %s: %s", stringAtom(name), OsError());
      fail;
    }
  } else
#endif /*HAVE_POPEN*/
  { char *fn;

    if ( !(fn = ExpandOneFile(stringAtom(name))) )
      fail;

    if ( !(stream=Sopen_file(fn, cmode)) )
    { if ( fileerrors )
	warning("Cannot open %s: %s", fn, OsError());
      fail;
    }
  }

  for(n=3; n<maxfiles; n++)
    if ( !fileTable[n].stream )
      break;
  if ( n >= maxfiles )
    return warning("Cannot handle more than %d open files", maxfiles);

  fileTable[n].name = name;
  fileTable[n].stream_name = NULL;
  fileTable[n].type = type;
  fileTable[n].stream = stream;
  fileTable[n].status = (mode == F_APPEND ? F_WRITE : mode);

  switch(mode)
  { case F_READ:		Input = n; break;
    case F_WRITE:
    case F_APPEND:		Output = n; break;
  }

  DEBUG(2, Sdprintf("Prolog fileTable[] updated\n"));

  succeed;
}


static bool
unifyStreamName(term_t f, int n)
{ if ( fileTable[n].status == F_CLOSED )
    fail;

#ifdef HAVE_POPEN
  if ( fileTable[n].type == ST_PIPE )
  { term_t a = PL_new_term_ref();

    return (PL_unify_functor(f, FUNCTOR_pipe1) &&
	    PL_get_arg(1, f, a) &&
	    PL_unify_atom(a, fileTable[n].name));
  }
#endif /*HAVE_POPEN*/

  return PL_unify_atom(f, fileTable[n].name);
}


static bool
unifyStreamMode(term_t m, int n)
{ if ( fileTable[n].status == F_CLOSED )
    fail;

  return PL_unify_atom(m, fileTable[n].status == F_READ ? ATOM_read
							: ATOM_write);
}


static bool
unifyStreamNo(term_t stream, int n)
{ Atom name;

  switch( n )
  { case 0:
      name = ATOM_user_input;
      break;
    case 1:	return PL_unify_atom(stream, ATOM_user_output);
      name = ATOM_user_output;
      break;
    case 2:	return PL_unify_atom(stream, ATOM_user_error);
      name = ATOM_user_error;
      break;
    default:
      if ( fileTable[n].stream_name )
	name = fileTable[n].stream_name;
      return PL_unify_integer(stream, n);
  }

  return PL_unify_atom(stream, name);
}


word
pl_told()
{ if ( fileTable[Output].status != F_WRITE )
    succeed;

  closeStream(Output);

  Output = 1;
  succeed;
}  


word
pl_flush()
{ if ( fileTable[Output].stream )
    Sflush(fileTable[Output].stream);

  succeed;
}


word
pl_see(term_t f)
{ return openStream(f, F_READ, OPEN_TEXT);
}


word
pl_seen()
{ if ( fileTable[Input].status != F_READ )
    succeed;

  closeStream(Input);
  popInputContext();

  succeed;
}


static word
openProtocol(term_t f, bool appnd)
{ int out = Output;

  pl_noprotocol();

  if ( openStream(f, appnd ? F_APPEND : F_WRITE, OPEN_TEXT|OPEN_OPEN) )
  { protocolStream = Output;
    Output = out;

    succeed;
  }
  Output = out;

  fail;
}


word
pl_noprotocol()
{ if ( protocolStream >= 0 )
  { closeStream(protocolStream);
    protocolStream = -1;
  }

  succeed;
}


		/********************************
		*          STRING I/O           *
		*********************************/


bool
seeString(char *s)
{ IOSTREAM *stream = Sopen_string(NULL, s, -1, "r");
  PlFile f;
  int n;
  
  for(n=3, f=&fileTable[n]; n<maxfiles; n++, f++)
  { if ( !f->stream )
    { f->stream = stream;
      f->name   = NULL;
      f->status = F_READ;
      f->type   = ST_STRING;

      pushInputContext();
      Input = n;
      succeed;
    }
  }

  return warning("Out of IO streams");
}


bool
seeingString()
{ return fileTable[Input].type == ST_STRING;
}


bool
seenString()
{ PlFile f = &fileTable[Input];

  if ( f->type == ST_STRING && f->stream )
  { Sclose(f->stream);
    f->stream = NULL;
    f->status = F_CLOSED;
    popInputContext();
  }

  succeed;
}


bool
tellString(char *s, int size)
{ IOSTREAM *stream = Sopen_string(NULL, s, size, "w");
  PlFile f;
  int n;
  
  for(n=3, f=&fileTable[n]; n<maxfiles; n++, f++)
  { if ( !f->stream )
    { f->stream = stream;
      f->name   = NULL;
      f->status = F_WRITE;
      f->type   = ST_STRING;

      pushOutputContext();
      Output = n;
      succeed;
    }
  }

  return warning("Out of IO streams");
}


bool
toldString()
{ PlFile f = &fileTable[Output];

  if ( f->type == ST_STRING && f->stream )
  { Sclose(f->stream);
    f->stream = NULL;
    f->status = F_CLOSED;
    popOutputContext();
  }

  succeed;
}


		/********************************
		*        INPUT IOSTREAM NAME        *
		*********************************/

Atom
currentStreamName()			/* only if a file! */
{ PlFile f = &fileTable[Input];

  if ( f->type == ST_FILE || f->type == ST_PIPE )
    return f->name;

  return NULL;
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
  char fdmap[256];
  term_t head      = PL_new_term_ref();
  term_t streams   = PL_copy_term_ref(Streams);
  term_t available = PL_copy_term_ref(Available);

  FD_ZERO(&fds);
  while( PL_get_list(streams, head, streams) )
  { IOSTREAM *s;
    int n, fd;

    if ( (n = streamNo(head, F_READ)) < 0 )
      fail;
    if ( !(s = fileTable[n].stream) || (fd=Sfileno(s)) < 0 )
      fail;
    fdmap[fd] = n;

    FD_SET(fd, &fds);
    if ( fd > max )
      max = fd;
  }
  if ( !PL_get_nil(streams) ||
       !PL_get_float(timeout, &time) )
    return warning("wait_for_input/3: instantiation fault");
  
  if ( time > 0.0 )
  { t.tv_sec  = (int)time;
    t.tv_usec = ((int)(time * 1000000) % 1000000);
    to = &t;
  } else
    to = NULL;

#ifdef hpux
  select(max+1, (int*) &fds, NULL, NULL, to);
#else
  select(max+1, &fds, NULL, NULL, to);
#endif

  for(n=0; n <= max; n++)
  { if ( FD_ISSET(n, &fds) )
    { if ( !PL_unify_list(available, head, available) ||
	   !unifyStreamName(head, fdmap[n]) )
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

word
pl_put(term_t c)
{ int chr;
  char *s;

  if ( PL_get_integer(c, &chr) )
  { if (chr < 0 || chr > 255)
      return warning("put/1: argument is not an ascii character");
    Put(chr);
  } else if ( PL_get_chars(c, &s, CVT_ATOM|CVT_LIST|CVT_STRING) )
  { Puts(s);
  } else
    return warning("put/1: instantiation fault");

  succeed;
}

word
pl_put2(term_t stream, term_t chr)
{ streamOutput(stream, pl_put(chr));
}

word
pl_get(term_t chr)
{ int c;

  do
  { c = Get0();
  } while( c != EOF && isBlank(c) );

  return PL_unify_integer(chr, c);
}


word
pl_skip(term_t chr)
{ int c;
  int r;

  if ( !PL_get_integer(chr, &c) )
    return warning("skip/1: instantiation fault");
  c &= 0xff;

  while((r=Get0()) != c && r != EOF )
    ;

  succeed;
}


word
pl_skip2(term_t stream, term_t chr)
{ streamInput(stream, pl_skip(chr));
}


word
pl_get2(term_t stream, term_t chr)
{ streamInput(stream, pl_get(chr));
}

word
pl_tty()				/* $tty/0 */
{ if ( status.notty )
    fail;
  succeed;
}

word
pl_get_single_char(term_t c)
{ return PL_unify_integer(c, getSingleChar());
}

word
pl_get0(term_t c)
{ return PL_unify_integer(c, Get0());
}

word
pl_get02(term_t stream, term_t c)
{ streamInput(stream, pl_get0(c))
}

word
pl_seeing(term_t f)
{ return unifyStreamName(f, Input);
}

word
pl_telling(term_t f)
{ return unifyStreamName(f, Output);
}

word
pl_tell(term_t f)
{ return openStream(f, F_WRITE, OPEN_TEXT);
}

word
pl_append(term_t f)
{ return openStream(f, F_APPEND, OPEN_TEXT);
}


word
pl_ttyflush()
{ int OldOut = Output;
  bool rval;

  Output = 1;
  rval = pl_flush();
  Output = OldOut;

  return rval;
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
{ if ( protocolStream >= 0 )
    return unifyStreamName(protocolStream, file);

  fail;
}


word
pl_prompt(term_t old, term_t new)
{ Atom a;

  if ( PL_unify_atom(old, prompt_atom) &&
       PL_get_atom(new, &a) )
  { prompt_atom = a;
    succeed;
  }

  fail;
}


void
prompt1(char *prompt)
{ if ( first_prompt )
    remove_string(first_prompt);
  first_prompt = store_string(prompt);
  first_prompt_used = FALSE;
}


word
pl_prompt1(term_t prompt)
{ char *s;

  if ( PL_get_chars(prompt, &s, CVT_ALL) )
  { prompt1(s);
    succeed;
  }

  return warning("prompt1/1: instantiation fault");
}


word
pl_tab(term_t n)
{ word val = evaluate(n);
  int m;

  if ( !isInteger(val) )
    return warning("tab/1: instantiation fault");
  m = (int) valNum(val);

  while(m-- > 0)
    Put(' ');

  succeed;
}


char *
PrologPrompt()
{ if ( !first_prompt_used && first_prompt )
  { first_prompt_used = TRUE;

    return first_prompt;
  }

  if ( Sinput->position && Sinput->position->linepos == 0 )
    return stringAtom(prompt_atom);
  else
    return "";
}


word
pl_tab2(term_t stream, term_t n)
{ streamOutput(stream, pl_tab(n)); /* TBD */
}

		/********************************
		*       STREAM BASED I/O        *
		*********************************/

static bool
setUnifyStreamNo(term_t stream, int n)
{ Atom a;

  if ( PL_get_atom(stream, &a) )
  { register int i;

    for(i = 0; i < maxfiles; i++ )
    { if ( fileTable[i].status != F_CLOSED &&
	   fileTable[i].stream_name == a )
	return warning("Stream name %s already in use", stringAtom(a));
    }
    fileTable[n].stream_name = a;
    succeed;
  }

  return unifyStreamNo(stream, n);
}
      

static opt_spec open4_options[] = 
{ { ATOM_type,       OPT_ATOM },
  { ATOM_reposition, OPT_BOOL },
  { ATOM_alias,	     OPT_ATOM },
  { ATOM_eof_action, OPT_ATOM },
  { NULL,	     0 }
};


word
pl_open4(term_t file, term_t mode,
	 term_t stream, term_t options)
{ int m = -1;
  Atom mname;
  Atom type       = ATOM_text;
  bool reposition = FALSE;
  Atom alias	  = NULL;
  Atom eof_action = ATOM_eof_code;
  int flags = OPEN_OPEN;

  if ( !scan_options(options, 0, open4_options,
		     &type, &reposition, &alias, &eof_action) )
    return warning("open/4: illegal option list");

  if ( alias )
    TRY(PL_unify_atom(stream, alias));
  if ( type == ATOM_text )
    flags |= OPEN_TEXT;
  
  if ( PL_get_atom(mode, &mname) )
  {      if ( mname == ATOM_write )
      m = F_WRITE;
    else if ( mname == ATOM_append )
      m = F_APPEND;
    else if ( mname == ATOM_read )
      m = F_READ;
  }
  if ( m < 0 )
    return warning("open/3: Invalid mode specification");

  if ( m == F_READ )
  { int in = Input;

    if ( openStream(file, m, flags) )
    { if ( setUnifyStreamNo(stream, Input) )
      { if ( eof_action != ATOM_eof_code )
	{ IOSTREAM *s = fileTable[Input].stream;
	  if ( eof_action == ATOM_reset )
	    s->flags |= SIO_NOFEOF;
	  else if ( eof_action == ATOM_error )
	    s->flags |= SIO_FEOF2ERR;
	}
	Input = in;
        succeed;
      }
      closeStream(Input);
      Input = in;

      fail;
    }
    Input = in;
    fail;
  } else
  { int out = Output;
    if ( openStream(file, m, flags) )
    { if ( setUnifyStreamNo(stream, Output) )
      { Output = out;
        succeed;
      }
      closeStream(Output);
      Output = out;
      
      fail;
    }
    Output = out;
    fail;
  }
}


word
pl_open(term_t file, term_t mode, term_t stream)
{ term_t n = PL_new_term_ref();
  PL_put_nil(n);

  return pl_open4(file, mode, stream, n);
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


static IOFUNCTIONS nullFunctions =
{ Sread_null,
  Swrite_null,
  Sseek_null,
  Sclose_null
};


word
pl_open_null_stream(term_t stream)
{ int sflags = SIO_NBUF|SIO_RECORDPOS;
  IOSTREAM *s = Snew((void *)NULL, sflags, &nullFunctions);

  return PL_open_stream(stream, s);
}


int
streamNo(term_t spec, int mode)
{ int n = -1;
  
  if ( !PL_get_integer(spec, &n) )
  { Atom name;

    if ( PL_get_atom(spec, &name) )
    {      if ( name == ATOM_user )
	n = (mode == F_READ ? 0 : 1);
      else if ( name == ATOM_user_input )
	n = 0;
      else if ( name == ATOM_user_output )
        n = 1;
      else if ( name == ATOM_user_error )
        n = 2;
      else
      { int i;

	for(i = 3; i < maxfiles; i++)
	{ if ( fileTable[i].stream_name == name )
	  { n = i;
	    break;
	  }
	}
      }
    }
  }

  if ( n < 0 || n >= maxfiles || fileTable[n].status == F_CLOSED )
  { warning("Illegal I/O stream specification");
    return -1;
  }

  switch(mode)
  { case F_READ|F_WRITE:
      return n;
    case F_READ:
      if ( fileTable[n].status != F_READ )
	return warning("Stream is not open for reading");
      break;
    case F_APPEND:
    case F_WRITE:	
      if ( fileTable[n].status != F_WRITE )
      { warning("Stream is not open for writing");
        return -1;
      }
  }

  return n;
}
  

word
pl_close(term_t stream)
{ int n;

  if ( (n = streamNo(stream, F_READ|F_WRITE)) < 0 )
    fail;

  TRY( closeStream(n) );
  if ( n == Output )
    Output = 1;
  if ( n == Input )
    Input = 0;

  succeed;
}

word
pl_current_stream(term_t file, term_t mode,
		  term_t stream, word h)
{ int n;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      n = 3;
      break;
    case FRG_REDO:
      n = (int) ForeignContext(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }
  
  for( ; n < maxfiles; n++)
  { if ( unifyStreamName(file, n) == FALSE ||
	 unifyStreamMode(mode, n) == FALSE ||
	 unifyStreamNo(stream, n) == FALSE )
      continue;
    if ( ++n < maxfiles )
      ForeignRedo(n);
    succeed;
  }
  
  fail;
}      


word
pl_flush_output(term_t stream)
{ int n;

  if ( (n = streamNo(stream, F_WRITE)) < 0 )
    fail;
  Sflush(fileTable[n].stream);

  succeed;
}


static IOSTREAM *
ioStreamWithPosition(term_t stream)
{ int n;
  IOSTREAM *s;

  if ( (n = streamNo(stream, F_READ|F_WRITE)) < 0 )
    fail;
  s = fileTable[n].stream;
  if ( !s->position )
  { warning("Stream doesn't maintain position");
    return NULL;
  }
  
  return s;
}


word
pl_stream_position(term_t stream, term_t old, term_t new)
{ IOSTREAM *s;
  long oldcharno, charno, linepos, lineno;
  term_t a = PL_new_term_ref();
  functor_t f;

  if ( !(s = ioStreamWithPosition(stream)) )
    fail;

  charno  = s->position->charno;
  lineno  = s->position->lineno;
  linepos = s->position->linepos;
  oldcharno = charno;

  if ( !PL_unify_functor(old, FUNCTOR_stream_position3) ||
       !PL_get_arg(1, old, a) ||
       !PL_unify_integer(a, charno) ||
       !PL_get_arg(2, old, a) ||
       !PL_unify_integer(a, lineno) ||
       !PL_get_arg(3, old, a) ||
       !PL_unify_integer(a, linepos) )
    fail;

  if ( !(PL_get_functor(new, &f) && f == FUNCTOR_stream_position3) ||
       !PL_get_arg(1, new, a) ||
       !PL_get_long(a, &charno) ||
       !PL_get_arg(2, new, a) ||
       !PL_get_long(a, &lineno) ||
       !PL_get_arg(3, new, a) ||
       !PL_get_long(a, &linepos) )
    return warning("stream_position/3: Invalid position specifier");

  if ( charno != oldcharno && Sseek(s, charno, 0) < 0 )
    return warning("Failed to set stream position: %s", OsError());

  s->position->charno  = charno;
  s->position->lineno  = lineno;
  s->position->linepos = linepos;
  
  succeed;
}


word
pl_set_input(term_t stream)
{ int n;

  if ( (n = streamNo(stream, F_READ)) < 0 )
    fail;

  Input = n;
  succeed;
}


word
pl_set_output(term_t stream)
{ int n;

  if ( (n = streamNo(stream, F_WRITE)) < 0 )
    fail;

  Output = n;
  succeed;
}


word
pl_current_input(term_t stream)
{ return unifyStreamNo(stream, Input);
}


word
pl_current_output(term_t stream)
{ return unifyStreamNo(stream, Output);
}

word
pl_character_count(term_t stream, term_t count)
{ IOSTREAM *s = ioStreamWithPosition(stream);

  if ( s )
    return PL_unify_integer(count, s->position->charno);

  fail;
}

word
pl_line_count(term_t stream, term_t count)
{ IOSTREAM *s = ioStreamWithPosition(stream);

  if ( s )
    return PL_unify_integer(count, s->position->lineno);

  fail;
}

word
pl_line_position(term_t stream, term_t count)
{ IOSTREAM *s = ioStreamWithPosition(stream);

  if ( s )
    return PL_unify_integer(count, s->position->linepos);

  fail;
}


word
pl_source_location(term_t file, term_t line)
{ char *s;

  if ( ReadingSource &&
       (s = AbsoluteFile(stringAtom(source_file_name))) &&
	PL_unify_atom_chars(file, s) &&
	PL_unify_integer(line, source_line_no) )
    succeed;
  
  fail;
}


word
pl_at_end_of_stream1(term_t stream)
{ int n;

  if ( (n = streamNo(stream, F_READ)) < 0 )
    fail;

  return Sfeof(fileTable[n].stream) ? TRUE : FALSE;
}


word
pl_at_end_of_stream0()
{ IOSTREAM *s = fileTable[Input].stream;
  
  if ( !s || Sfeof(s) )
    succeed;

  fail;
}


word
pl_peek_byte2(term_t stream, term_t chr)
{ int n;
  IOSTREAM *s;
  int c;

  if ( (n = streamNo(stream, F_READ)) < 0 ||
       !(s = fileTable[n].stream) )
    fail;

  c = Sgetc(s);
  Sungetc(c, s);

  return PL_unify_integer(chr, c);
}


word
pl_peek_byte1(term_t chr)
{ IOSTREAM *s;
  int c;

  if ( !(s = fileTable[Input].stream) )
    fail;

  c = Sgetc(s);
  Sungetc(c, s);

  return PL_unify_integer(chr, c);
}


		/********************************
		*             FILES             *
		*********************************/

bool
unifyTime(term_t t, long time)
{ return PL_unify_float(t, (double)time);
}


char *
get_filename(term_t n, char *buf, unsigned int size)
{ char *name;

  if ( PL_get_chars(n, &name, CVT_ALL) &&
       (name = ExpandOneFile(name)) )
  { if ( buf )
    { if ( strlen(name) < size )
      { strcpy(buf, name);
	return buf;
      }

      warning("File name too long");
    } else
      return name;
  }

  return NULL;
}


word
pl_time_file(term_t name, term_t t)
{ char *fn;

  if ( (fn = get_filename(name, NULL, 0)) )
  { long time;

    if ( (time = LastModifiedFile(fn)) == -1 )
      fail;

    return unifyTime(t, time);
  }

  return warning("time_file/2: instantiation fault");
}


word
pl_size_file(term_t name, term_t len)
{ char *n;

  if ( (n = get_filename(name, NULL, 0)) )
  { long size;

    if ( (size = SizeFile(n)) < 0 )
      return warning("size_file/2: %s", OsError());

    return PL_unify_integer(len, size);
  }

  return warning("exists_file/1: instantiation fault");
}


word
pl_access_file(term_t name, term_t mode)
{ char *n;
  int md;
  Atom m;

  if ( !PL_get_atom(mode, &m) ||
       !(n=get_filename(name, NULL, 0)) )
    return warning("access_file/2: instantiation fault");

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
  { warning("access_file/2: mode: {read,write,append,execute,exist,none}");
    fail;
  }

  if ( AccessFile(n, md) )
    succeed;

  if ( md == ACCESS_WRITE && !AccessFile(n, ACCESS_EXIST) )
  { char *dir = DirName(n);

    if ( AccessFile(*dir == EOS ? "." : dir, md) )
      succeed;
  }

  fail;
}


word
pl_read_link(term_t file, term_t link, term_t to)
{ char *n, *l, *t;

  if ( !(n = get_filename(file, NULL, 0)) )
    return warning("read_link/2: instantiation fault");

  if ( (l = ReadLink(n)) &&
       PL_unify_atom_chars(link, l) &&
       (t = DeRefLink(n)) &&
       PL_unify_atom_chars(to, t) )
    succeed;

  fail;
}


word
pl_exists_file(term_t name)
{ char *n;

  if ( !(n = get_filename(name, NULL, 0)) )
    return warning("exists_file/1: instantiation fault");
  
  return ExistsFile(n);
}


word
pl_exists_directory(term_t name)
{ char *n;

  if ( !(n = get_filename(name, NULL, 0)) )
    return warning("exists_directory/1: instantiation fault");
  
  return ExistsDirectory(n);
}


word
pl_tmp_file(term_t base, term_t name)
{ char *n;

  if ( !PL_get_chars(base, &n, CVT_ALL) )
    return warning("tmp_file/2: instantiation fault");

  return PL_unify_atom(name, TemporaryFile(n));
}


word
pl_delete_file(term_t name)
{ char *n;

  if ( !(n = get_filename(name, NULL, 0)) )
    return warning("delete_file/1: instantiation fault");
  
  return RemoveFile(n);
}


word
pl_same_file(term_t file1, term_t file2)
{ char *n1, *n2;
  char name1[MAXPATHLEN];

  if ( (n1 = get_filename(file1, name1, sizeof(name1))) &&
       (n2 = get_filename(file2, NULL, 0)) )
    return SameFile(name1, n2);

  return warning("same_file/2: instantiation fault");
}


word
pl_rename_file(term_t old, term_t new)
{ char *o, *n;
  char ostore[MAXPATHLEN];

  if ( (o = get_filename(old, ostore, sizeof(ostore))) &&
       (n = get_filename(new, NULL, 0)) )
  { if ( RenameFile(ostore, n) )
      succeed;

    if ( fileerrors )
      warning("rename_file/2: could not rename %s --> %s: %s\n",
	      ostore, n, OsError());
    fail;
  }

  return warning("rename_file/2: instantiation fault");
}


word
pl_fileerrors(term_t old, term_t new)
{ return setBoolean(&fileerrors, "fileerrors", old, new);
}


word
pl_absolute_file_name(term_t name, term_t expanded)
{ char *n;

  if ( (n = get_filename(name, NULL, 0)) &&
       (n = AbsoluteFile(n)) )
    return PL_unify_atom_chars(expanded, n);

  return warning("absolute_file_name/2: instantiation fault");
}


word
pl_is_absolute_file_name(term_t name)
{ char *n;

  if ( (n = get_filename(name, NULL, 0)) &&
       IsAbsolutePath(n) )
    succeed;

  fail;
}


word
pl_chdir(term_t dir)
{ char *n;

  if ( (n = get_filename(dir, NULL, 0)) )
  { if ( ChDir(n) )
      succeed;

    if ( fileerrors )
      warning("chdir/1: cannot change directory to %s: %s", n, OsError());
    fail;
  }

  return warning("chdir/1: instantiation fault");
}


word
pl_file_base_name(term_t f, term_t b)
{ char *n;

  if ( !PL_get_chars(f, &n, CVT_ALL) )
    return warning("file_base_name/2: instantiation fault");

  return PL_unify_atom_chars(b, BaseName(n));
}


word
pl_file_dir_name(term_t f, term_t b)
{ char *n;

  if ( !PL_get_chars(f, &n, CVT_ALL) )
    return warning("file_dir_name/2: instantiation fault");

  return PL_unify_atom_chars(b, DirName(n));
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
    if ( status.case_sensitive_files )
      return strcmp(&s[1], ext) == 0;
    else
      return stricmp(&s[1], ext) == 0;
  }

  fail;
}


word
pl_file_name_extension(term_t base, term_t ext, term_t full)
{ char *b, *e, *f;
  char buf[MAXPATHLEN];

  if ( PL_get_chars(full, &f, CVT_ALL) )
  { char *s = f + strlen(f);		/* ?base, ?ext, +full */

    while(*s != '.' && *s != '/' && s > f)
      s--;
    if ( *s == '.' )
    { if ( PL_get_chars(ext, &e, CVT_ALL) )
      { if ( e[0] == '.' )
	  e++;
	if ( status.case_sensitive_files )
	{ TRY(strcmp(&s[1], e) == 0);
	} else
	{ TRY(stricmp(&s[1], e) == 0);
	}
      } else
      { TRY(PL_unify_atom_chars(ext, &s[1]));
      }
      if ( s-f > MAXPATHLEN )
	return warning("file_extension/2: file too long");
      strncpy(buf, f, s-f);
      buf[s-f] = EOS;

      return PL_unify_atom_chars(base, buf);
    }
    if ( PL_unify_atom_chars(ext, "") &&
	 PL_unify(full, base) )
      PL_succeed;

    PL_fail;
  }

  if ( PL_get_chars(base, &b, CVT_ALL|BUF_RING) &&
       PL_get_chars(ext, &e, CVT_ALL) )
  { char *s;

    if ( e[0] == '.' )		/* +Base, +Extension, -full */
      e++;
    if ( has_extension(b, e) )
      return PL_unify(base, full);
    if ( strlen(b) + 1 + strlen(e) + 1 > MAXPATHLEN )
      return warning("file_extension/2: file too long");
    strcpy(buf, b);
    s = buf + strlen(buf);
    *s++ = '.';
    strcpy(s, e);

    return PL_unify_atom_chars(full, buf);
  }

  return warning("file_extension/2: instantiation fault");
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
  } else if ( PL_get_chars(os, &n, CVT_ALL) )
  { _xos_canonical_filename(n, buf);
    return PL_unify_atom_chars(pl, buf);
  } else
    return warning("prolog_to_os_filename/2: instantiation fault");
#else /*O_XOS*/
  return PL_unify(pl, os);
#endif /*O_XOS*/
}


#if defined(O_XOS) && defined(__WIN32__)
word
pl_make_fat_filemap(term_t dir)
{ char *n;

  if ( (n = get_filename(dir, NULL, 0)) )
  { if ( _xos_make_filemap(n) == 0 )
      succeed;

    if ( fileerrors )
      warning("make_fat_filemap/1: failed: %s", OsError());

    fail;
  }
  
  return warning("make_fat_filemap/1: instantiation fault");
}
#endif
