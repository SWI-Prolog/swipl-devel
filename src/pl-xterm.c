/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2016, University of Amsterdam
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

#if defined(__linux__) || defined(__GLIBC__) || defined(__GNU__) || defined(__CYGWIN__)
#define _XOPEN_SOURCE 600
#endif

/* #define O_DEBUG 1 */
#include "pl-incl.h"
#if defined(HAVE_GRANTPT) && defined(O_PLMT)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open an alternative  xterm-console.  Used   to  support  multi-threading
user-interaction. Currently only implemented using the Unix-98 /dev/ptmx
style of pseudo terminals (Solaris 2.x,  Linux   2.2  and many more). As
multi-threading asks for a modern Unix   version  anyhow, this should be
ok.

In principle, asking for  HAVE_GRANTPT  should   do,  but  I  don't want
portability problems for users of   the  single-threaded version.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#ifdef HAVE_SYS_STROPTS_H
#include <sys/stropts.h>	/* needed for ioctl(fd, I_PUSH, "..") */
#endif
#include <termios.h>
#include <signal.h>

typedef struct
{ int fd;				/* associated file */
  int pid;				/* PID of xterm */
  int count;
} xterm;


static ssize_t
Xterm_read(void *handle, char *buffer, size_t count)
{ GET_LD
  xterm *xt = handle;
  ssize_t size;

  if ( LD->prompt.next && ttymode != TTY_RAW )
    PL_write_prompt(TRUE);
  else
    Sflush(Suser_output);

  do
  { size = read(xt->fd, buffer, count);

    if ( size < 0 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	break;
      }

      continue;
    }
  } while(0);

  if ( size == 0 )			/* end-of-file */
  { LD->prompt.next = TRUE;
  } else if ( size > 0 && buffer[size-1] == '\n' )
    LD->prompt.next = TRUE;

  return size;
}


static ssize_t
Xterm_write(void *handle, char *buffer, size_t count)
{ xterm *xt = handle;
  ssize_t size;

  do
  { size = write(xt->fd, buffer, count);

    if ( size < 0 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	break;
      }

      continue;
    }
  } while(0);

  return size;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Normally we kill the xterm if all   file-descriptors  are closed. If the
thread  cannot  be  killed  however,   dieIO()    will   kill  only  the
file-descriptors that are not in use and may   fail to kill all of them.
Therefore we kill on the first occasion.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
Xterm_close(void *handle)
{ GET_LD
  xterm *xt = handle;

  DEBUG(1, Sdprintf("Closing xterm-handle (count = %d)\n", xt->count));

  if ( xt->pid &&
       ((GD->cleaning != CLN_NORMAL) ||
	(LD && LD->thread.info->status != PL_THREAD_RUNNING)) )
  { kill(xt->pid, SIGKILL);
    xt->pid = 0;
  }

  if ( --xt->count == 0 )
  { close(xt->fd);
    if ( xt->pid )
      kill(xt->pid, SIGKILL);
    freeHeap(xt, sizeof(*xt));
  }

  return 0;
}


static int
Xterm_control(void *handle, int action, void *arg)
{ xterm *xt = handle;

  switch(action)
  { case SIO_GETFILENO:
    { int *rval = arg;

      *rval = xt->fd;
      return 0;
    }
    case SIO_SETENCODING:
      return 0;
    default:
      return -1;
  }
}


static IOFUNCTIONS SXtermfunctions =
{ Xterm_read,
  Xterm_write,
  NULL,
  Xterm_close,
  Xterm_control
};


static int
unifyXtermStream(term_t t, xterm *xt, int flags)
{ IOSTREAM *s;
  int defflags = (SIO_NOCLOSE|SIO_TEXT|SIO_RECORDPOS);

  if ( (s=Snew(xt, (defflags|flags), &SXtermfunctions)) )
  { s->encoding = initEncoding();
    return PL_unify_stream(t, s);
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Start the Xterm window. This window  runs   in  a  separate process. How
should this process be related to us?  Should it be a new session?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXARGV 100
#define RESARGC 10

foreign_t
pl_open_xterm(term_t title, term_t in, term_t out, term_t err, term_t argv)
{ GET_LD
  int master, slave, pid;
  char *slavename;
  struct termios termio;
  xterm *xt;
  char *titlechars;
  char *xterm_argv[MAXARGV];
  int xterm_argc = 0;
  int xterm_malloced_argc;
  term_t tail = PL_copy_term_ref(argv);
  term_t head = PL_new_term_ref();
  int i;

  if ( !PL_get_chars(title, &titlechars, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  xterm_argv[xterm_argc++] = "xterm";
  while(PL_get_list_ex(tail, head, tail))
  { if ( xterm_argc >= MAXARGV-RESARGC )
      return PL_representation_error("xterm_argc");
    if ( !PL_get_chars(head, &xterm_argv[xterm_argc++],
		       CVT_ALL|BUF_MALLOC|CVT_EXCEPTION) )
      return FALSE;
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;
  xterm_malloced_argc = xterm_argc;

#ifdef HAVE_POSIX_OPENPT
  if ( (master = posix_openpt(O_RDWR)) < 0 )
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "posix_openpt");
#else
  if ( (master = open("/dev/ptmx", O_RDWR)) < 0 )
  { GET_LD
    term_t file = PL_new_term_ref();

    PL_put_atom_chars(file, "/dev/ptmx");
    return PL_error(NULL, 0, NULL, ERR_FILE_OPERATION,
		    ATOM_open, ATOM_file, file);
  }
#endif

  grantpt(master);
  unlockpt(master);
  slavename = ptsname(master);
  slave = open(slavename, O_RDWR);
#ifdef HAVE_SYS_STROPTS_H
  ioctl(slave, I_PUSH, "ptem");
  ioctl(slave, I_PUSH, "ldterm");
#endif

  if ( tcgetattr(slave, &termio) )
    perror("tcgetattr");
  termio.c_lflag &= ~ECHO;
  termio.c_lflag |= (ICANON|IEXTEN);
  termio.c_cc[VERASE] = 8;
  if ( tcsetattr(slave, TCSANOW, &termio) )
    perror("tcsetattr");

  if ( (pid = fork()) == 0 )
  { char arg[64];
    char *cc;


    signal(SIGINT, SIG_IGN);		/* Don't stop on user interaction */
					/* from toplevel */
    cc = slavename+strlen(slavename)-2;
    if ( strchr(cc, '/' ) )
      sprintf(arg, "-S%s/%d", BaseName(slavename), master);
    else
      sprintf(arg, "-S%c%c%d", cc[0], cc[1], master);

    xterm_argv[xterm_argc++] = arg;
    xterm_argv[xterm_argc++] = "-T";
    xterm_argv[xterm_argc++] = titlechars;
    xterm_argv[xterm_argc]   = NULL;		/* Up to RESARGC */

    execvp("xterm", xterm_argv);
    perror("execlp");
  }

  for(i=1; i<xterm_malloced_argc; i++)
    PL_free(xterm_argv[i]);

  for (;;)			/* read 1st line containing window-id */
  { char c;

    if ( read(slave, &c, 1) < 0 )
      break;
    if ( c == '\n' )
      break;
  }
  termio.c_lflag |= ECHO;
  DEBUG(1, Sdprintf("%s: Erase = %d\n", slavename, termio.c_cc[VERASE]));
  if ( tcsetattr(slave, TCSADRAIN, &termio) == -1 )
    perror("tcsetattr");

  xt = allocHeapOrHalt(sizeof(*xt));
  xt->pid   = pid;
  xt->fd    = slave;
  xt->count = 3;			/* opened 3 times */

  return (unifyXtermStream(in,  xt, SIO_INPUT|SIO_LBUF|SIO_NOFEOF) &&
	  unifyXtermStream(out, xt, SIO_OUTPUT|SIO_LBUF) &&
	  unifyXtermStream(err, xt, SIO_OUTPUT|SIO_NBUF));
}

#else /*HAVE_GRANTPT*/

foreign_t
pl_open_xterm(term_t title, term_t in, term_t out, term_t err, term_t argv)
{ return notImplemented("open_xterm", 4);
}

#endif /*HAVE_GRANTPT*/
