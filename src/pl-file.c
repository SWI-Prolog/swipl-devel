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
#include "pl-itf.h"

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

forwards bool	openStream(word file, int mode, int fresh);
forwards bool	flush(void);
forwards bool	openProtocol(Atom, bool appnd);
forwards bool	closeProtocol(void);
forwards bool	closeStream(int);
forwards bool	unifyStreamName(Word, int);
forwards bool	unifyStreamNo(Word, int);
forwards bool	setUnifyStreamNo(Word, int);
forwards bool	unifyStreamMode(Word, int);

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
  { closeProtocol();
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

Char
getSingleChar(void)
{ Char c;
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
  { flush();

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
Put(int c)
{ IOSTREAM *s = fileTable[Output].stream;

  return (s && Sputc(c, s)) < 0 ? FALSE : TRUE;
}


int
Get0()
{ IOSTREAM *s = fileTable[Input].stream;
  
  if ( s )
  { int c;

    if ( (c = Sgetc(s)) == EOF && Input == 0 )
      Sclearerr(s);
    
    return c;
  }

  return EOF;
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
PL_open_stream(IOSTREAM *s, Word handle)
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

      return unifyAtomic(handle, consNum(n));
    }
  }

  return warning("Out of IO streams");
}


static bool
openStream(word file, int mode, bool fresh)
{ int n;
  IOSTREAM *stream;
  char *cmode;
  Atom f;
  int type;

  DEBUG(2, Sdprintf("openStream file=0x%lx, mode=%d\n", file, mode));
  if ( isAtom(file) )
  { type = ST_FILE;
    f = (Atom) file;
  } else if (isTerm(file) && functorTerm(file) == FUNCTOR_pipe1)
  {
#ifdef SIGPIPE
    type = ST_PIPE;
    f = (Atom) argTerm(file, 0);
    signal(SIGPIPE, pipeHandler);
#else
    return warning("Pipes are not supported on this OS");
#endif /*SIGPIPE*/
  } else
    return warning("Illegal stream specification");

  DEBUG(3, Sdprintf("File/command name = %s\n", stringAtom(f)));
  if ( type == ST_FILE )
  { if ( mode == F_READ )
    { if ( f == ATOM_user || f == ATOM_user_input )
      { Input = 0;
	succeed;
      }
    } else
    { if ( f == ATOM_user || f == ATOM_user_output )
      { Output = 1;
        succeed;
      }
      if ( f == ATOM_user_error || f == ATOM_stderr )
      { Output = 2;
	succeed;
      }
    }
  } else if ( type == ST_PIPE && mode == F_APPEND )
    return warning("Cannot open a pipe in `append' mode");
    
  if ( !fresh )				/* see/1, tell/1, append/1 */
  { for( n=0; n<maxfiles; n++ )
    { if ( fileTable[n].name == f && fileTable[n].type == type )
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
  cmode = (mode == F_READ ? "r" : mode == F_WRITE ? "w" : "a");

#ifdef HAVE_POPEN
  if ( type == ST_PIPE )
  { if ( !(stream=Sopen_pipe(stringAtom(f), cmode)) )
    { if ( fileerrors )
	warning("Cannot open pipe %s: %s", stringAtom(f), OsError());
      fail;
    }
  } else
#endif /*HAVE_POPEN*/
  { char *name = ExpandOneFile(stringAtom(f));

    if ( name == (char *)NULL )
      fail;

    if ( !(stream=Sopen_file(name, cmode)) )
    { if ( fileerrors )
	warning("Cannot open %s: %s", stringAtom(f), OsError());
      fail;
    }
  }

  for(n=3; n<maxfiles; n++)
    if ( !fileTable[n].stream )
      break;
  if ( n >= maxfiles )
    return warning("Cannot handle more than %d open files", maxfiles);

  fileTable[n].name = f;
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
unifyStreamName(Word f, int n)
{ if ( fileTable[n].status == F_CLOSED )
    fail;
#ifdef HAVE_POPEN
  if ( fileTable[n].type == ST_PIPE )
  { TRY( unifyFunctor(f, FUNCTOR_pipe1) );
    f = argTermP(*f, 0);
  }
#endif /*HAVE_POPEN*/
  return unifyAtomic(f, fileTable[n].name);
}

static bool
unifyStreamMode(Word m, int n)
{ if ( fileTable[n].status == F_CLOSED )
    fail;
  return unifyAtomic(m, fileTable[n].status == F_READ ? ATOM_read
		     				      : ATOM_write);
}

static bool
unifyStreamNo(Word stream, int n)
{ switch( n )
  { case 0:	return unifyAtomic(stream, ATOM_user_input);
    case 1:	return unifyAtomic(stream, ATOM_user_output);
    case 2:	return unifyAtomic(stream, ATOM_user_error);
    default:	if ( fileTable[n].stream_name != NULL )
		  return unifyAtomic(stream, fileTable[n].stream_name);
		return unifyAtomic(stream, consNum(n));
  }
}

bool
told()
{ if ( fileTable[Output].status != F_WRITE )
    succeed;

  closeStream(Output);

  Output = 1;
  succeed;
}  

static bool
flush()
{ if ( fileTable[Output].stream )
    Sflush(fileTable[Output].stream);

  succeed;
}

bool
see(word f)
{ return openStream(f, F_READ, FALSE);
}

bool
seen()
{ if ( fileTable[Input].status != F_READ )
    succeed;

  closeStream(Input);
  popInputContext();

  succeed;
}

static bool
openProtocol(Atom f, bool appnd)
{ int out = Output;

  closeProtocol();

  if ( openStream((word)f, appnd ? F_APPEND : F_WRITE, TRUE) )
  { protocolStream = Output;
    Output = out;

    succeed;
  }
  Output = out;

  fail;
}

static bool
closeProtocol()
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
pl_wait_for_input(streams, available, timeout)
Word streams, available, timeout;
{ return notImplemented("wait_for_input", 3);
}

#else

word
pl_wait_for_input(Word streams, Word available, Word timeout)
{ fd_set fds;
  struct timeval t, *to;
  real time;
  int n, max = 0;
  char fdmap[256];

  FD_ZERO(&fds);
  while( isList(*streams) )
  { Word head = HeadList(streams);
    IOSTREAM *s;
    int n, fd;

    deRef(head);
    if ( (n = streamNo(head, F_READ)) < 0 )
      fail;
    if ( !(s = fileTable[n].stream) || (fd=Sfileno(s)) < 0 )
      fail;
    fdmap[fd] = n;

    FD_SET(fd, &fds);
    if (fd > max) max = fd;
    streams = TailList(streams);
    deRef(streams);
  }
  if ( !isNil(*streams) || wordToReal(*timeout, &time) == FALSE )
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
    { TRY(unifyFunctor(available, FUNCTOR_dot2) );
      TRY(unifyStreamName(HeadList(available), fdmap[n]));
      available = TailList(available);
      deRef(available);
    }
  }
  CLOSELIST(available);

  succeed;
}

#endif /* HAVE_SELECT */

		/********************************
		*      PROLOG CONNECTION        *
		*********************************/

word
pl_put(Word c)
{ Char chr;
  char *s;

  if ( isInteger(*c) )
  { chr = (int) valNum(*c);
    if (chr < 0 || chr > 255)
      return warning("put/1: argument is not an ascii character");
    Put(chr);
    succeed;
  }
  if ( isAtom(*c) )
  { s = stringAtom(*c);
    if (s[0] != '\0' && s[1] == '\0')
    { Put(s[0]);
      succeed;
    }
  }
  if ( isList(*c) )		/* accept put("a"), but also put("hello") */
  { while ( isList(*c) )
    { Word p = HeadList(c);
      deRef(p);
      if ( isInteger(*p) && valNum(*p) >= 0 && valNum(*p) < 256 )
        Put((int)valNum(*p));
      else
        goto error;
      c = TailList(c);
      deRef(c);
    }
    if ( isNil(*c) )
      succeed;
  }

error:
  return warning("put/1: instantiation fault");
}

word
pl_put2(Word stream, Word chr)
{ streamOutput(stream, pl_put(chr));
}

word
pl_get(Word chr)
{ Char c;

  do
  { if ( (c = Get0()) == EOF )
      return unifyAtomic(chr, consNum(c));
  } while( isBlank(c) );

  return unifyAtomic(chr, consNum(c));
}


word
pl_skip(Word chr)
{ int c, r;

  if ( !isInteger(*chr) )
    return warning("skip/1: instantiation fault");
  c = valNum(*chr) & 0xff;

  while((r=Get0()) != c && r != EOF )
    ;

  succeed;
}


word
pl_skip2(Word stream, Word chr)
{ streamInput(stream, pl_skip(chr));
}


word
pl_get2(Word stream, Word chr)
{ streamInput(stream, pl_get(chr));
}

word
pl_tty()				/* $tty/0 */
{ if ( status.notty )
    fail;
  succeed;
}

word
pl_get_single_char(Word c)
{ return unifyAtomic(c, consNum(getSingleChar()));
}

word
pl_get0(Word c)
{ return unifyAtomic(c, consNum(Get0()));
}

word
pl_get02(Word stream, Word c)
{ streamInput(stream, pl_get0(c))
}

word
pl_seeing(Word f)
{ return unifyStreamName(f, Input);
}

word
pl_telling(Word f)
{ return unifyStreamName(f, Output);
}

word
pl_seen(void)
{ return seen();
}

word
pl_told(void)
{ return told();
}

word
pl_see(Word f)
{ return see(*f);
}

word
pl_tell(Word f)
{ return openStream(*f, F_WRITE, FALSE);
}

word
pl_append(Word f)
{ return openStream(*f, F_APPEND, FALSE);
}

word
pl_ttyflush(void)
{ int OldOut = Output;
  bool rval;

  Output = 1;
  rval = flush();
  Output = OldOut;

  return rval;
}

word
pl_flush(void)
{ return flush();
}

word
pl_protocol(Word file)
{ if (!isAtom(*file))
    return warning("protocol/1: argument should be an atom");

  return openProtocol((Atom) *file, FALSE);
}

word
pl_protocola(Word file)
{ if (!isAtom(*file))
    return warning("protocola/1: argument should be an atom");

  return openProtocol((Atom) *file, TRUE);
}

word
pl_noprotocol(void)
{ return closeProtocol();
}

word
pl_protocolling(Word file)
{ if (protocolStream >= 0)
    return unifyAtomic(file, fileTable[protocolStream].name);

  fail;
}

word
pl_prompt(Word old, Word new)
{ TRY( unifyAtomic(old, prompt_atom) )

  if (!isAtom(*new) )
    return warning("prompt/2: instantiation fault");

  prompt_atom = (Atom) *new;

  succeed;
}


void
prompt1(char *prompt)
{ if ( first_prompt )
    remove_string(first_prompt);
  first_prompt = store_string(prompt);
  first_prompt_used = FALSE;
}


word
pl_prompt1(Word prompt)
{ char *s;

  if ( !(s = primitiveToString(*prompt, FALSE)) &&
       !(s = listToString(*prompt)) )
    return warning("prompt1/1: instantiation fault");

  prompt1(s);

  succeed;
}


word
pl_tab(Word n)
{ word val = evaluate(n);
  int m;

  if (!isInteger(val))
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
pl_tab2(Word stream, Word n)
{ streamOutput(stream, pl_tab(n));
}

		/********************************
		*       STREAM BASED I/O        *
		*********************************/

static bool
setUnifyStreamNo(Word stream, int n)
{ if ( isAtom(*stream) )
  { register int i;

    for(i = 0; i < maxfiles; i++ )
    { if ( fileTable[i].status != F_CLOSED &&
	   fileTable[i].stream_name == (Atom)*stream )
	return warning("Stream name %s already in use", stringAtom(*stream));
    }
    fileTable[n].stream_name = (Atom) *stream;
    succeed;
  }

  return unifyStreamNo(stream, n);
}
      
word
pl_open(Word file, Word mode, Word stream)
{ int m;

  if ( *mode == (word) ATOM_write )
    m = F_WRITE;
  else if ( *mode == (word) ATOM_append )
    m = F_APPEND;
  else if ( *mode == (word) ATOM_read )
    m = F_READ;
  else
    return warning("open/3: Invalid mode specification");

  if ( m == F_READ )
  { int in = Input;
    if ( openStream(*file, m, TRUE) )
    { if ( setUnifyStreamNo(stream, Input) )
      { Input = in;
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
    if ( openStream(*file, m, TRUE) )
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

#ifndef DEVNULL
#define DEVNULL "/dev/null"
#endif

word
pl_open_null_stream(Word stream)
{ static word mode = (word) ATOM_write;
  word file = (word) lookupAtom(DEVNULL);

  return pl_open(&file, &mode, stream);
}

int
streamNo(Word spec, int mode)
{ int n = -1;
  
  if ( isInteger(*spec) )
  { n = (int) valNum(*spec);
  } else if ( isAtom(*spec) )
  { Atom k = (Atom) *spec;

    if ( k == ATOM_user )
      n = (mode == F_READ ? 0 : 1);
    else if ( k == ATOM_user_input )
      n = 0;
    else if ( k == ATOM_user_output )
      n = 1;
    else if ( k == ATOM_user_error )
      n = 2;
    else
    { register int i;

      for(i = 3; i < maxfiles; i++)
      { if ( fileTable[i].stream_name == k )
        { n = i;
          break;
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
pl_close(Word stream)
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
pl_current_stream(Word file, Word mode, Word stream, word h)
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
pl_flush_output(Word stream)
{ int n;

  if ( (n = streamNo(stream, F_WRITE)) < 0 )
    fail;
  Sflush(fileTable[n].stream);

  succeed;
}


static IOSTREAM *
ioStreamWithPosition(Word stream)
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
pl_stream_position(Word stream, Word old, Word new)
{ IOSTREAM *s;
  long oldcharno, charno, linepos, lineno;

  if ( !(s = ioStreamWithPosition(stream)) )
    fail;
  charno  = s->position->charno;
  lineno  = s->position->lineno;
  linepos = s->position->linepos;
  oldcharno = charno;

  TRY( unifyFunctor(old, FUNCTOR_stream_position3) );
  TRY( unifyAtomic(argTermP(*old, 0), consNum(charno)) );
  TRY( unifyAtomic(argTermP(*old, 1), consNum(lineno)) );
  TRY( unifyAtomic(argTermP(*old, 2), consNum(linepos)) );

  deRef(new);
  if ( !isTerm(*new) ||
       functorTerm(*new) != FUNCTOR_stream_position3 ||
       !isInteger(argTerm(*new, 0)) ||
       !isInteger(argTerm(*new, 1)) ||
       !isInteger(argTerm(*new, 2)) )
    return warning("stream_position/3: Invalid position specifier");

  charno = valNum(argTerm(*new, 0));
  lineno = valNum(argTerm(*new, 1));
  linepos= valNum(argTerm(*new, 2));

  if ( charno != oldcharno && Sseek(s, charno, 0) < 0 )
    return warning("Failed to set stream position: %s", OsError());

  s->position->charno  = charno;
  s->position->lineno  = lineno;
  s->position->linepos = linepos;
  
  succeed;
}

word
pl_set_input(Word stream)
{ int n;

  if ( (n = streamNo(stream, F_READ)) < 0 )
    fail;

  Input = n;
  succeed;
}

word
pl_set_output(Word stream)
{ int n;

  if ( (n = streamNo(stream, F_WRITE)) < 0 )
    fail;

  Output = n;
  succeed;
}

word
pl_current_input(Word stream)
{ return unifyStreamNo(stream, Input);
}

word
pl_current_output(Word stream)
{ return unifyStreamNo(stream, Output);
}

word
pl_character_count(Word stream, Word count)
{ IOSTREAM *s = ioStreamWithPosition(stream);

  if ( s )
    return unifyAtomic(count, consNum(s->position->charno));

  fail;
}

word
pl_line_count(Word stream, Word count)
{ IOSTREAM *s = ioStreamWithPosition(stream);

  if ( s )
    return unifyAtomic(count, consNum(s->position->lineno));

  fail;
}

word
pl_line_position(Word stream, Word count)
{ IOSTREAM *s = ioStreamWithPosition(stream);

  if ( s )
    return unifyAtomic(count, consNum(s->position->linepos));

  fail;
}


word
pl_source_location(Word file, Word line)
{ if ( ReadingSource )
  { char *s = AbsoluteFile(stringAtom(source_file_name));

    if ( s != NULL )
    { TRY( unifyAtomic(file, lookupAtom(s)) );
      TRY( unifyAtomic(line, consNum(source_line_no)) );
      
      succeed;
    }
  }
  
  fail;
}


		/********************************
		*             FILES             *
		*********************************/

bool
unifyTime(Word t, long int time)
{ return unifyAtomic(t, globalReal((real)time));
}


word
pl_time_file(Word name, Word t)
{ char *n;
  long time;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("time_file/2: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;

  if ( (time = LastModifiedFile(n)) == -1 )
    fail;

  return unifyTime(t, time);
}


word
pl_size_file(Word name, Word len)
{ char *n;
  long size;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("exists_file/1: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  if ( (size = SizeFile(n)) < 0 )
    return warning("size_file/2: %s", OsError());

  return unifyAtomic(len, consNum(size));
}


word
pl_access_file(Word name, Word mode)
{ char *n;
  int md;
  Atom m = (Atom) *mode;

  if ( m == ATOM_none )
    succeed;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("access_file/2: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  if      ( m == ATOM_write || m == ATOM_append )
    md = ACCESS_WRITE;
  else if ( m == ATOM_read )
    md = ACCESS_READ;
  else if ( m == ATOM_execute )
    md = ACCESS_EXECUTE;
  else if ( m == ATOM_exist )
    md = ACCESS_EXIST;
  else
    return warning("access_file/2: mode is one of {read,write,append,execute,exist,none}");

  return AccessFile(n, md);
}


word
pl_read_link(Word file, Word link, Word to)
{ char *n, *l, *t;

  if ( (n = primitiveToString(*file, FALSE)) == (char *)NULL )
    return warning("read_link/2: instantiation fault");
  if ( (l = ReadLink(n)) )
    TRY(unifyAtomic(link, lookupAtom(l)));
  if ( (t = DeRefLink(n)) )
    return unifyAtomic(to, lookupAtom(t));

  succeed;
}


word
pl_exists_file(Word name)
{ char *n;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("exists_file/1: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  return ExistsFile(n);
}


word
pl_exists_directory(Word name)
{ char *n;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("exists_directory/1: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  return ExistsDirectory(n);
}


word
pl_tmp_file(Word base, Word name)
{ char *n;

  if ( (n = primitiveToString(*base, FALSE)) == (char *)NULL )
    return warning("tmp_file/2: instantiation fault");

  return unifyAtomic(name, TemporaryFile(n));
}


word
pl_delete_file(Word name)
{ char *n;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("delete_file/1: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  return RemoveFile(n);
}


word
pl_same_file(Word file1, Word file2)
{ char *n1, *n2;

  initAllocLocal();
  if ( (n1 = primitiveToString(*file1, TRUE)) == NULL ||
       (n2 = primitiveToString(*file2, TRUE)) == NULL )
    return warning("same_file/2: instantiation fault");

  if ( (n1 = ExpandOneFile(n1)) == NULL )
    fail;
  n1 = store_string_local(n1);
  if ( (n2 = ExpandOneFile(n2)) == NULL )
    fail;
  stopAllocLocal();

  return SameFile(n1, n2);
}


word
pl_rename_file(Word old, Word new)
{ char *o, *n;

  initAllocLocal();
  o = primitiveToString(*old, TRUE);
  n = primitiveToString(*new, TRUE);
  if ( o == (char *) NULL || n == (char *) NULL )
  { stopAllocLocal();
    return warning("rename_file/2: instantiation fault");
  }
  
  if ( (o = ExpandOneFile(o)) == (char *)NULL )
    fail;
  o = store_string_local(o);
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  n = store_string_local(n);
  stopAllocLocal();

  if ( RenameFile(o, n) )
    succeed;
  else
  { if ( fileerrors )
      warning("rename_file/2: could not rename %s --> %s: %s\n",
	      o, n, OsError());
    fail;
  }
}


word
pl_fileerrors(Word old, Word new)
{ TRY(unifyAtomic(old, (fileerrors ? ATOM_on : ATOM_off)) );

  if ( *new == (word) ATOM_on )       fileerrors = TRUE;
  else if ( *new == (word) ATOM_off ) fileerrors = FALSE;
  else                                fail;

  succeed;
}


word
pl_absolute_file_name(Word name, Word expanded)
{ char *s = primitiveToString(*name, FALSE);

  if ( s == (char *) NULL || (s = AbsoluteFile(s)) == (char *) NULL)
    return warning("Invalid file specification");

  return unifyAtomic(expanded, lookupAtom(s));
}


word
pl_is_absolute_file_name(Word name)
{ char *s = primitiveToString(*name, FALSE);
  char pls[MAXPATHLEN];

  if ( s &&
       (s = ExpandOneFile(PrologPath(s, pls))) &&
       IsAbsolutePath(s) )
    succeed;

  fail;
}


word
pl_chdir(Word dir)
{ char *s = primitiveToString(*dir, FALSE);
  char pls[MAXPATHLEN];

  if ( s == (char *)NULL )
    return warning("chdir/1: instantiation fault");
  if ( (s = ExpandOneFile(PrologPath(s, pls))) == (char *)NULL )
    fail;
  
  if ( ChDir(s) )
    succeed;

  return warning("chdir/1: cannot change directory to %s: %s", s, OsError());
}


word
pl_file_base_name(Word f, Word b)
{ if (!isAtom(*f))
    return warning("file_base_name/2: instantiation fault");

  return unifyAtomic(b, lookupAtom(BaseName(stringAtom(*f))));
}


word
pl_file_dir_name(Word f, Word b)
{ if (!isAtom(*f))
    return warning("file_dir_name/2: instantiation fault");

  return unifyAtomic(b, lookupAtom(DirName(stringAtom(*f))));
}


word
pl_prolog_to_os_filename(Word pl, Word os)
{
#ifdef O_XOS
  char buf[MAXPATHLEN];

  if ( isAtom(*pl) )
  { _xos_os_filename(stringAtom(*pl), buf);
    return unifyAtomic(os, lookupAtom(buf));
  } else if ( isAtom(*os) )
  { _xos_canonical_filename(stringAtom(*os), buf);
    return unifyAtomic(pl, lookupAtom(buf));
  } else
    return warning("prolog_to_os_filename/2: instantiation fault");
#else /*O_XOS*/
  return pl_unify(pl, os);
#endif /*O_XOS*/
}


#if defined(O_XOS) && defined(__WIN32__)
word
pl_make_fat_filemap(Word dir)
{ char *s = primitiveToString(*dir, FALSE);
  char pls[MAXPATHLEN];

  if ( s == (char *)NULL )
    return warning("make_fat_filemap/1: instantiation fault");
  if ( (s = ExpandOneFile(PrologPath(s, pls))) == (char *)NULL )
    fail;
  
  if ( _xos_make_filemap(s) < 0 )
    return warning("make_fat_filemap/1: failed: %s", OsError());
  
  succeed;
}
#endif
