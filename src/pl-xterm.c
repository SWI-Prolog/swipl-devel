/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifdef __linux__
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

foreign_t
pl_open_xterm(term_t title, term_t in, term_t out, term_t err)
{ int master, slave, pid;
  char *slavename;
  struct termios termio;
  xterm *xt;
  char *titlechars;

  if ( !PL_get_chars(title, &titlechars, CVT_ALL) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_text, title);

#ifdef HAVE_POSIX_OPENPT
  if ( (master = posix_openpt(O_RDWR)) < 0 )
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "posix_openpt");
#else
  if ( (master = open("/dev/ptmx", O_RDWR)) < 0 )
  { term_t file = PL_new_term_ref();

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
    execlp("xterm", "xterm", arg, "-T", titlechars,
	   "-xrm", "*backarrowKeyIsErase: false",
	   "-xrm", "*backarrowKey: false",
	   NULL);
    perror("execlp");
  }

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
pl_open_xterm(term_t title, term_t in, term_t out, term_t err)
{ return notImplemented("open_xterm", 4);
}

#endif /*HAVE_GRANTPT*/
