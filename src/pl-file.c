/*  $Id$

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

#include "pl-incl.h"
#include "pl-ctype.h"
#if unix
#include <sys/time.h>
#include <sys/file.h>
#endif

#define MAXSTRINGNEST	20		/* tellString --- Told nesting */

#if AIX || hpux
#define file prolog_file
#define File PrologFile
#endif

typedef struct file *	File;

static struct file
{ Atom		name;			/* name of file */
  Atom		stream_name;		/* stream identifier name */
  FILE *	fd;			/* Unix file descriptor */
  int		lineno;			/* current line no */
  long		charno;			/* character count */
  int		linepos;		/* position in line */
  int		status;			/* opened, how ? */
  bool		pipe;			/* opened as a pipe ? */
  bool		isatty;			/* Stream connects to a terminal */
} *fileTable = (File) NULL;		/* Our file table */

int 	Input;				/* current input */
int	Output;				/* current output */

ttybuf	ttytab;				/* saved terminal status on entry */
int	ttymode;			/* Current tty mode */

static Atom prompt_atom;		/* current prompt */
int    protocolStream = -1;		/* doing protocolling on stream <n> */

static struct
{ char *string;
  long  left;
} outStringStack[MAXSTRINGNEST];	/* maximum depth to nest string i/o */
int outStringDepth = 0;			/* depth of nesting */
static char *inString;			/* string for reading */

static int   maxfiles;			/* maximum file index */
static int   ttyLinePos;			/* current column on tty */
static int   ttyLineNo;			/* terminal line number count */
static int   ttyCharNo;			/* terminal character count */
static bool  fileerrors = TRUE;		/* give warning on open errors? */
#if O_FOLD
static int   fold = O_FOLD;		/* default line folding */
#else
static int   fold = -1;			/* line folding */
#endif

forwards void	stopHandler P((void));
forwards void	pipeHandler P((void));
forwards void	protocol P((Char c, int mode));
forwards bool	openStream P((word file, int mode, int fresh));
forwards bool	flush P((void));
forwards bool	openProtocol P((Atom, bool appnd));
forwards bool	closeProtocol P((void));
forwards bool	closeStream P((int));
forwards void	updateCounts P((Char, File));
forwards bool	unifyStreamName P((Word, int));
forwards bool	unifyStreamNo P((Word, int));
forwards bool	setUnifyStreamNo P((Word, int));
forwards bool	unifyStreamMode P((Word, int));

#ifdef SIGSTOP
#define JOBCONTROL 1
#endif

#if JOBCONTROL
/*  Signal handler for SIGTSTP (user typing  ^Z).  Restores the  terminal
    flags  to when Prolog was started and restores them to their current
    setting after the continue.

 ** Sun Jun 19 16:32:30 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static
void
stopHandler()
{ ttybuf tab;

  if (novice == TRUE)
  { warning("Job control (^Z) disabled");
    return;
  }

  PushTty(&tab, TTY_SAVE);
  PopTty(&ttytab);
  kill(getpid(), SIGSTOP);		/* Who has SISSTOP probably also */
  PopTty(&tab);				/* has kill() and getpid() */
}
#endif JOBCONTROL

#if PIPE
static void
pipeHandler()
{ Putf("Broken pipe\n");
  pl_abort();

  signal(SIGPIPE, SIG_DFL);		/* should abort fail. */
  kill(getpid(), SIGPIPE);		/* Unix has both pipes and kill() */
}
#endif PIPE

void
initIO()
{ int n;

  if ( maxfiles != GetDTableSize() )
  { if ( fileTable != (File) NULL )
      freeHeap(fileTable, (File) allocHeap(sizeof(struct file) * maxfiles));
    maxfiles = GetDTableSize();
    fileTable = (File) allocHeap(sizeof(struct file) * maxfiles);
  }
  inString = (char *) NULL;
  outStringDepth = 0;

  for(n=0; n<maxfiles; n++)
  { fileTable[n].name = (Atom) NULL;
    fileTable[n].status = F_CLOSED;
    fileTable[n].fd = (FILE *) NULL;
    fileTable[n].lineno = 1;
    fileTable[n].linepos = 0;
    fileTable[n].charno = 0L;
    fileTable[n].pipe = FALSE;
  }

  ttyCharNo = ttyLinePos = 0;
  ttyLineNo = 1;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initilise user input, output and error stream.  How to do this neat without
the Unix assumptions?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  for(n=0; n<3; n++)
  { if ( OpenStream(n) )
    { fileTable[n].isatty = IsaTty(n);

      switch(n)
      { case 0:
	  if ( fileTable[n].isatty == FALSE )
	    status.notty = TRUE;
	  fileTable[n].name = ATOM_user;
	  fileTable[n].stream_name = ATOM_user_input;
	  fileTable[n].status = F_READ;
	  fileTable[n].fd = stdin;
#if hpux || sun
	  stdin->_ptr = stdin->_base;
	  stdin->_cnt = 0;		/* clear input after dump */
	  clearerr(stdin);
#endif
	  break;
	case 1:
	  fileTable[n].name = ATOM_user;
	  fileTable[n].stream_name = ATOM_user_output;
	  fileTable[n].status = F_WRITE;
	  fileTable[n].fd = stdout;
	  break;
	case 2:
	  fileTable[n].name = ATOM_stderr;
	  fileTable[n].stream_name = ATOM_user_error;
	  fileTable[n].status = F_WRITE;
	  fileTable[n].fd = stderr;
	  break;
      }
    } else
      warning("Stream %d is not open", n);
  }

  ttymode = TTY_COOKED;			/* initial tty mode */
  PushTty(&ttytab, TTY_COOKED);
  ResetTty();
#if JOBCONTROL
  signal(SIGTSTP, stopHandler);
#endif

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
closeStream(n)
int n;
{ if ( n < 3 || fileTable[n].status == F_CLOSED )
    succeed;

#if PIPE
  if (fileTable[n].pipe == TRUE)
    Pclose(fileTable[n].fd);
  else
#endif PIPE
    Fclose(fileTable[n].fd);
  fileTable[n].status = F_CLOSED;
  fileTable[n].name = fileTable[n].stream_name = (Atom) NULL;
  fileTable[n].pipe = FALSE;

  succeed;
}

void
closeFiles()
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

static void
protocol(c, mode)
register Char c;
int mode;
{ if ( mode == F_READ && ttymode >= TTY_RAW ) /* Non-echo mode: do not log */
    return;

  ttyCharNo++;

  switch(c)
  { case '\n':	ttyLinePos = 0;
		ttyLineNo++;
		break;
    case '\t':	ttyLinePos |= 7;
		ttyLinePos++;
		break;
    case '\b':	if (ttyLinePos > 0)
		  ttyLinePos--;
		break;
    case EOF:	return;
    default:	ttyLinePos++;
		break;
  }

  if ( protocolStream >= 0 )
  { int out;
  
    out = Output;
    Output = protocolStream;
    Put(c);
    Output = out;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read() first checks the input stream and calls  getc(fd)  directly  when
reading from a file.  This procedure checks whether this is possible and
returns the file descriptor.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
newLineInput()
{ fileTable[Input].lineno++;
  fileTable[Input].linepos = 0;
}

FILE *
checkInput(stream)
int stream;
{ if ( inString ||
       fileTable[stream].status != F_READ ||
       stream == 0)
    return (FILE *) NULL;

  return fileTable[Input].fd;
}
       
static void
updateCounts(c, f)
register Char c;
register File f;
{ f->charno++;
  switch(c)
  { case '\n':  f->lineno++;
		f->linepos = 0;
		break;
    case '\t':  f->linepos |= 7;
		f->linepos++;
		break;
    case '\b':  if ( f->linepos > 0 )
		  f->linepos--;
		break;
    default:	f->linepos++;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a character from the current input stream, which is either a file or
a string.  Reading from strings is used to implement predicates such  as
atom_to_term/2.   This  function  is  of  type `Char' (an int) to ensure
portable transfer of EOF.

This function is normally called via the macro Get0().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Char
get_character()
{ Char c;

  if ( inString )
  { if ( *inString == EOS )
      return EOF;
    return (Char) *inString++;
  }

  if (fileTable[Input].status != F_READ)
  { warning("No current input stream");
    return (Char) EOF;
  }

  if ( fileTable[Input].isatty )
  { if ( ttyLinePos != 0 )
      pl_ttyflush();
    c = (Input == 0 ? GetChar() : Getc(fileTable[Input].fd));
    protocol(c, F_READ);
  } else
  { c = Getc(fileTable[Input].fd);
    updateCounts(c, &fileTable[Input]);
  }
      
  return c;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a single character from the terminal without waiting for  a  return.
The  character  should  not  be  echoed.   If  status.notty is true this
function will read the first character and then skip all character  upto
and including the newline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Char
getSingleChar()
{ Char c;
  int OldIn = Input;

  Input = 0;
  debugstatus.suspendTrace++;
  
  if ( status.notty )
  { Char c2;

    c2 = Get0();
    while( c2 == ' ' || c2 == '\t' )	/* skip blanks */
      c2 = Get0();
    c = c2;
    while( c2 != EOF && c2 != '\n' )	/* read upto newline */
      c2 = Get0();
  } else
  { ttybuf buf;
    
    PushTty(&buf, TTY_RAW);		/* switch to raw mode */
    c = Get0();
    PopTty(&buf);			/* restore tty */
  }

  Input = OldIn;
  debugstatus.suspendTrace--;

  return c;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The central character output function.  Normally called  via  the  macro
Put(c) which includes automatic casting of the argument to `Char', so no
problems  arise  on  machines with different argument passing for `char'
and `int'
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
put_character(c)
Char c;
{ if ( outStringDepth > 0 )
  { if ( outStringStack[outStringDepth].left-- <= 0 )
      fail;
    *outStringStack[outStringDepth].string++ = (char) c;

    succeed;
  }

  if ( fileTable[Output].status != F_WRITE )
    return warning("No current output stream");

  Putc(c, fileTable[Output].fd);

  if ( fileTable[Output].isatty )
  { protocol(c, F_WRITE);
    if ( fold > 0 && ttyLinePos > fold )
      Put('\n');
    if ( ttyLinePos == 0 )			/* just put a newline */
    { Fflush(fileTable[Output].fd);
    }
  } else
  { updateCounts(c, &fileTable[Output]);
  }

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formated put.  It would be better to define our own formated  write  for
this  which  accepts  both  Prolog data structures (ints, floats, atoms,
etc) and C data structures.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*VARARGS1*/
word
#if ANSI && !AIX
Putf(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vPutf(fm, args);
  va_end(args);

  succeed;
}

#else

Putf(va_alist)
va_dcl
{ va_list args;
  char *fm;

  va_start(args);
  fm = va_arg(args, char *);
  vPutf(fm, args);
  va_end(args);

  succeed;
}
#endif

word
vPutf(fm, args)
char *fm;
va_list args;
{ char tmp[10240];
  char *s;

  vsprintf(tmp, fm, args);

  for(s=tmp; *s; s++)
    TRY(Put(*s) );

  succeed;
}

bool
readLine(buf, stream)
char *buf;
int stream;
{ int oldin = Input;
  Char c;

  Input = stream;
  while( (c=Get0()) != EOF && c != '\n' && c != '\r' )
    *buf++ = c;

  *buf++ = EOS;
  Input = oldin;

  return c == EOF ? FALSE : TRUE;
}

int
currentInputLine()
{ return fileTable[Input].isatty ? ttyLineNo : fileTable[Input].lineno;
}

static bool
openStream(file, mode, fresh)
word file;
int mode;
bool fresh;
{ int n;
  FILE *fd;
  char *cmode;
  Atom f;
  bool pipe;

  DEBUG(2, printf("openStream file=0x%lx, mode=%d\n", file, mode));
  if (isAtom(file))
  { pipe = FALSE;
    f = (Atom) file;
  } else if (isTerm(file) && functorTerm(file) == FUNCTOR_pipe1)
  {
#if PIPE
    pipe = TRUE;
    f = (Atom) argTerm(file, 0);
    signal(SIGPIPE, pipeHandler);
#else
    return warning("Pipes are not supported on this OS");
#endif PIPE
  } else
    return warning("Illegal stream specification");

  DEBUG(3, printf("File/command name = %s\n", stringAtom(f)));
  if ( pipe == FALSE )
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
  } else
  { if ( mode == F_APPEND )
      return warning("Cannot open a pipe in `append' mode");
  }
    
  if ( !fresh )
  { for(n=0; n<maxfiles; n++)
    { if (fileTable[n].name == f && fileTable[n].pipe == pipe)
      { if (fileTable[n].status == mode)
	{ switch(mode)
	  { case F_READ:	Input = n; break;
	    case F_WRITE:
	    case F_APPEND:	Output = n; break;
	  }
	  DEBUG(3, printf("Switched back to already open stream %d\n", n));
	  succeed;
	} else
	{ closeStream(n);
	}
	break;
      }
    }
  }

  DEBUG(2, printf("Starting Unix open\n"));
  cmode = (mode == F_READ ? "r" : mode == F_WRITE ? "w" : "a");

#if PIPE
  if (pipe)
  { if ((fd=Popen(stringAtom(f), cmode)) == (FILE *) NULL)
    { if (fileerrors)
	warning("Cannot open pipe %s: %s", stringAtom(f), OsError());
      fail;
    }
  } else
#endif
  { char *name = ExpandOneFile(stringAtom(f));

    if ( name == (char *)NULL )
      fail;

    if ((fd=Fopen(name, cmode)) == (FILE *) NULL)
    { if (fileerrors)
	warning("Cannot open %s: %s", stringAtom(f), OsError());
      fail;
    }
  }

  DEBUG(2, printf("Unix open succeeded in fd=%d\n", n));

  n = fileno(fd);
  fileTable[n].name = f;
  fileTable[n].stream_name = NULL;
  fileTable[n].pipe = pipe;
  fileTable[n].fd = fd;
  fileTable[n].status = (mode == F_APPEND ? F_WRITE : mode);
  fileTable[n].lineno = 1;
  fileTable[n].charno = fileTable[n].linepos = 0;
  fileTable[n].isatty = IsaTty(n);

  switch(mode)
  { case F_READ:		Input = n; break;
    case F_WRITE:
    case F_APPEND:		Output = n; break;
  }

  DEBUG(2, printf("Prolog fileTable[] updated\n"));

  succeed;
}

static bool
unifyStreamName(f, n)
Word f;
int n;
{ if ( fileTable[n].status == F_CLOSED )
    fail;
#if PIPE
  if (fileTable[n].pipe)
  { TRY( unifyFunctor(f, FUNCTOR_pipe1) );
    f = argTermP(*f, 0);
  }
#endif
  return unifyAtomic(f, fileTable[n].name);
}

static bool
unifyStreamMode(m, n)
Word m;
int n;
{ if ( fileTable[n].status == F_CLOSED )
    fail;
  return unifyAtomic(m, fileTable[n].status == F_READ ? ATOM_read : ATOM_write);
}

static bool
unifyStreamNo(stream, n)
Word stream;
int n;
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
{ if ( fileTable[Output].status == F_WRITE )
    Fflush(fileTable[Output].fd);

  succeed;
}

bool
see(f)
word f;
{ return openStream(f, F_READ, FALSE);
}

bool
seen()
{ if ( fileTable[Input].status != F_READ )
    succeed;

  closeStream(Input);

  Input = 0;  

  succeed;
}

static bool
openProtocol(f, appnd)
Atom f;
bool appnd;
{ int out = Output;

  closeProtocol();

  if ( openStream((word)f, appnd ? F_APPEND : F_WRITE, TRUE) == TRUE )
  { protocolStream = Output;
    Output = out;

    succeed;
  }
  Output = out;

  fail;
}

static bool
closeProtocol()
{ if (protocolStream >= 0)
  { closeStream(protocolStream);
    protocolStream = -1;
  }

  succeed;
}

void
prompt(always)
bool always;
{ if ( Input == 0 &&
       inString == (char *) NULL &&
       fileTable[Input].isatty &&
       (always || ttyLinePos == 0))
  { int oldOut = Output;

    Output = 1;
    Putf("%s", stringAtom(prompt_atom));
    flush();
    Output = oldOut;
  }
}


		/********************************
		*          STRING I/O           *
		*********************************/

bool
seeString(s)
char *s;
{ inString = s;

  succeed;
}

bool
seeingString()
{ return inString == (char *)NULL ? FALSE : TRUE;
}

bool
seenString()
{ inString = (char *) NULL;

  succeed;
}

bool
tellString(s, n)
char *s;
long n;
{ outStringDepth++;
  if ( outStringDepth >= MAXSTRINGNEST )
  { warning("Exeeded maximum string based i/o nesting");
    pl_abort();
  }
  outStringStack[outStringDepth].string = s;
  outStringStack[outStringDepth].left = n - 1;		/* 1 for the EOS */

  succeed;
}

bool
toldString()
{ if ( outStringDepth > 0 )
  { *outStringStack[outStringDepth].string = EOS;
    outStringDepth--;
  }

  succeed;
}

		/********************************
		*        INPUT FILE NAME        *
		*********************************/

Atom
currentStreamName()
{ if ( inString )
    return NULL;

  return fileTable[Input].name;
}

		/********************************
		*       WAITING FOR INPUT	*
		********************************/

#if unix

word
pl_wait_for_input(streams, available, timeout)
Word streams, available, timeout;
{ fd_set fds;
  struct timeval t, *to;
  real time;
  int n;
  extern int select();

  FD_ZERO(&fds);
  while( isList(*streams) )
  { int fd = streamNo(HeadList(streams), F_READ);

    if ( fd < 0 )
      fail;
    FD_SET(fd, &fds);
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

  select(maxfiles, &fds, NULL, NULL, to);

  for(n=0; n<maxfiles; n++)
  { if ( FD_ISSET(n, &fds) )
    { TRY(unifyFunctor(available, FUNCTOR_dot2) );
      TRY(unifyStreamName(HeadList(available), n));
      available = TailList(available);
      deRef(available);
    }
  }
  CLOSELIST(available);

  succeed;
}

#else

word
pl_wait_for_input(streams, available, timeout)
Word streams, available, timeout;
{ return notImplemented("wait_for_input", 3);
}

#endif unix

		/********************************
		*      PROLOG CONNECTION        *
		*********************************/

word
pl_tty_fold(old, new)
Word old, new;
{ TRY( unifyAtomic(old, consNum(fold)) );
  if ( !isInteger(*new) )
    return warning("tty_fold/2: instantiation fault");
  fold = (int) valNum(*new);

  succeed;
}


word
pl_put(c)
Word c;
{ Char chr;
  char *s;

  if (isInteger(*c))
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
pl_put2(stream, chr)
Word stream, chr;
{ streamOutput(stream, pl_put(chr));
}

word
pl_get(chr)
Word chr;
{ Char c;

  do
  { if ( (c = Get0()) == EOF )
      return unifyAtomic(chr, consNum(c));
  } while( isBlank(c) );

  return unifyAtomic(chr, consNum(c));
}

word
pl_get2(stream, chr)
Word stream, chr;
{ streamInput(stream, pl_get(chr));
}

word
pl_tty()				/* $tty/0 */
{ if ( status.notty )
    fail;
  succeed;
}

word
pl_get_single_char(c)
Word c;
{ return unifyAtomic(c, consNum(getSingleChar()));
}

word
pl_get0(c)
Word c;
{ return unifyAtomic(c, consNum(Get0()));
}

word
pl_get02(stream, c)
Word stream, c;
{ streamInput(stream, pl_get0(c))
}

word
pl_seeing(f)
Word f;
{ return unifyStreamName(f, Input);
}

word
pl_telling(f)
Word f;
{ return unifyStreamName(f, Output);
}

word
pl_seen()
{ return seen();
}

word
pl_told()
{ return told();
}

word
pl_see(f)
Word f;
{ return see(*f);
}

word
pl_tell(f)
Word f;
{ return openStream(*f, F_WRITE, FALSE);
}

word
pl_append(f)
Word f;
{ return openStream(*f, F_APPEND, FALSE);
}

word
pl_ttyflush()
{ int OldOut = Output;
  bool rval;

  Output = 1;
  rval = flush();
  Output = OldOut;

  return rval;
}

word
pl_flush()
{ return flush();
}

word
pl_protocol(file)
Word file;
{ if (!isAtom(*file))
    return warning("protocol/1: argument should be an atom");

  return openProtocol((Atom) *file, FALSE);
}

word
pl_protocola(file)
Word file;
{ if (!isAtom(*file))
    return warning("protocola/1: argument should be an atom");

  return openProtocol((Atom) *file, TRUE);
}

word
pl_noprotocol()
{ return closeProtocol();
}

word
pl_protocolling(file)
Word file;
{ if (protocolStream >= 0)
    return unifyAtomic(file, fileTable[protocolStream].name);

  fail;
}

word
pl_prompt(old, new)
Word old, new;
{ TRY( unifyAtomic(old, prompt_atom) )

  if (!isAtom(*new) )
    return warning("prompt/2: instantiation fault");

  prompt_atom = (Atom) *new;

  succeed;
}

word
pl_tab(n)
Word n;
{ word val = evaluate(n);
  int m;

  if (!isInteger(val))
    return warning("tab/1: instantiation fault");
  m = (int) valNum(val);

  while(m-- > 0)
    Put(' ');

  succeed;
}

word
pl_tab2(stream, n)
Word stream, n;
{ streamOutput(stream, pl_tab(n));
}

		/********************************
		*       STREAM BASED I/O        *
		*********************************/

static bool
setUnifyStreamNo(stream, n)
Word stream;
int n;
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
pl_open(file, mode, stream)
Word file, mode, stream;
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
    { if ( setUnifyStreamNo(stream, Input) == TRUE )
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
    { if ( setUnifyStreamNo(stream, Output) == TRUE )
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
pl_open_null_stream(stream)
Word stream;
{ static word mode = (word) ATOM_write;
  static word file = (word) ATOM_devnull;

  return pl_open(&file, &mode, stream);
}

int
streamNo(spec, mode)
Word spec;
int mode;
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
pl_close(stream)
Word stream;
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
pl_current_stream(file, mode, stream, h)
Word file, mode, stream;
word h;
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
pl_flush_output(stream)
Word stream;
{ int n;

  if ( (n = streamNo(stream, F_WRITE)) < 0 )
    fail;
  Fflush(fileTable[n].fd);

  succeed;
}

word
pl_stream_position(stream, old, new)
Word stream, old, new;
{ int n;
  long oldcharno, charno, linepos, lineno;
  extern int fseek();

  if ( (n = streamNo(stream, F_READ|F_WRITE)) < 0 )
    fail;

  TRY( unifyFunctor(old, FUNCTOR_stream_position3) );
  if ( fileTable[n].isatty )
  { charno  = ttyCharNo;
    lineno  = ttyLineNo;
    linepos = ttyLinePos;
  } else
  { charno  = fileTable[n].charno;
    lineno  = fileTable[n].lineno;
    linepos = fileTable[n].linepos;
  }
  oldcharno = charno;
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

  if ( charno != oldcharno && fseek(fileTable[n].fd, charno, 0) < 0 )
    return warning("Failed to set stream position: %s", OsError());
  fileTable[n].charno = charno;
  fileTable[n].lineno = (int) lineno;
  fileTable[n].linepos = (int) linepos;
  
  succeed;
}

word
pl_set_input(stream)
Word stream;
{ int n;

  if ( (n = streamNo(stream, F_READ)) < 0 )
    fail;

  Input = n;
  succeed;
}

word
pl_set_output(stream)
Word stream;
{ int n;

  if ( (n = streamNo(stream, F_WRITE)) < 0 )
    fail;

  Output = n;
  succeed;
}

word
pl_current_input(stream)
Word stream;
{ return unifyStreamNo(stream, Input);
}

word
pl_current_output(stream)
Word stream;
{ return unifyStreamNo(stream, Output);
}

word
pl_character_count(stream, count)
Word stream, count;
{ int n;
  long c;

  if ( (n = streamNo(stream, F_WRITE|F_READ)) < 0 )
    fail;
  c = fileTable[n].isatty ? ttyCharNo : fileTable[n].charno;

  return unifyAtomic(count, consNum(c));
}

word
pl_line_count(stream, count)
Word stream, count;
{ int n;
  long c;

  if ( (n = streamNo(stream, F_WRITE|F_READ)) < 0 )
    fail;
  c = fileTable[n].isatty ? ttyLineNo : fileTable[n].lineno;

  return unifyAtomic(count, consNum(c));
}

word
pl_line_position(stream, count)
Word stream, count;
{ int n;
  long c;

  if ( (n = streamNo(stream, F_WRITE|F_READ)) < 0 )
    fail;
  c = fileTable[n].isatty ? ttyLinePos : fileTable[n].linepos;

  return unifyAtomic(count, consNum(c));
}


word
pl_source_location(file, line)
Word file, line;
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
unifyTime(t, time)
Word t;
long time;
{ return unifyAtomic(t, globalReal((real)time));
}


word
pl_time_file(name, t)
Word name, t;
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
pl_size_file(name, len)
Word name, len;
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
pl_access_file(name, mode)
Word name, mode;
{ char *n;
  int md;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("access_file/2: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  if      ( *mode == (word) ATOM_write )
    md = ACCESS_WRITE;
  else if ( *mode == (word) ATOM_read )
    md = ACCESS_READ;
  else if ( *mode == (word) ATOM_execute )
    md = ACCESS_EXECUTE;
  else
    return warning("access_file/2: mode is one of {read, write, execute}");

  return AccessFile(n, md);
}


word
pl_exists_file(name)
Word name;
{ char *n;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("exists_file/1: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  return ExistsFile(n);
}


word
pl_exists_directory(name)
Word name;
{ char *n;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("exists_directory/1: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  return ExistsDirectory(n);
}


word
pl_tmp_file(base, name)
Word base, name;
{ char *n;

  if ( (n = primitiveToString(*base, FALSE)) == (char *)NULL )
    return warning("tmp_file/2: instantiation fault");

  return unifyAtomic(name, TemporaryFile(n));
}


word
pl_delete_file(name)
Word name;
{ char *n;

  if ( (n = primitiveToString(*name, FALSE)) == (char *)NULL )
    return warning("delete_file/1: instantiation fault");
  if ( (n = ExpandOneFile(n)) == (char *)NULL )
    fail;
  
  return DeleteFile(n);
}


word
pl_same_file(file1, file2)
Word file1, file2;
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
pl_rename_file(old, new)
Word old, new;
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

  return RenameFile(o, n);
}


word
pl_fileerrors(old, new)
Word old, new;
{ TRY(unifyAtomic(old, (fileerrors ? ATOM_on : ATOM_off)) );

  if ( *new == (word) ATOM_on )       fileerrors = TRUE;
  else if ( *new == (word) ATOM_off ) fileerrors = FALSE;
  else                                fail;

  succeed;
}


word
pl_absolute_file_name(name, expanded)
Word name, expanded;
{ char *s = primitiveToString(*name, FALSE);

  if ( s == (char *) NULL || (s = AbsoluteFile(s)) == (char *) NULL)
    return warning("Invalid file specification");

  return unifyAtomic(expanded, lookupAtom(s));
}


word
pl_chdir(dir)
Word dir;
{ char *s = primitiveToString(*dir, FALSE);

  if ( s == (char *)NULL )
    return warning("chdir/1: instantiation fault");
  if ( (s = ExpandOneFile(s)) == (char *)NULL )
    fail;
  
  if ( ChDir(s) )
    succeed;

  return warning("chdir/1: cannot change directory to %s", s);
}


word
pl_file_base_name(f, b)
Word f, b;
{ if (!isAtom(*f))
    return warning("file_base_name/2: instantiation fault");

  return unifyAtomic(b, lookupAtom(BaseName(stringAtom(*f))));
}


word
pl_file_dir_name(f, b)
Word f, b;
{ if (!isAtom(*f))
    return warning("file_dir_name/2: instantiation fault");

  return unifyAtomic(b, lookupAtom(DirName(stringAtom(*f))));
}

