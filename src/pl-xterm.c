/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

#ifdef __linux__
#define _XOPEN_SOURCE
#endif

#include "pl-incl.h"
#if defined(HAVE_GRANTPT) && defined(O_PLMT)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open an alternative  xterm-console.  Used   to  support  multi-threading
user-interaction. Currently only implemented using the Unix-98 /dev/ptmx
style of pseudo terminals (Solaris 2.x,  Linux   2.2  and many more). As
multi-threading asks for a modern Unix   version  anyhow, this should be
ok.

In principle, asking for  HAVE_GRANTPT  should   do,  but  I  don't want
portability problems for users of the single-threaded version.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stropts.h>
#include <termios.h>
#include <signal.h>
 
typedef struct
{ int fd;				/* associated file */
  int pid;				/* PID of xterm */
  int count;
} xterm;


static int
Xterm_read(void *handle, char *buffer, int count)
{ xterm *xt = handle;
  int size;

  if ( LD->prompt.next && ttymode != TTY_RAW )
  { Sfputs(PrologPrompt(), Suser_output);
    
    LD->prompt.next = FALSE;
  }
  Sflush(Suser_output);

  size = read(xt->fd, buffer, count);

  if ( size == 0 )			/* end-of-file */
  { LD->prompt.next = TRUE;
  } else if ( size > 0 && buffer[size-1] == '\n' )
    LD->prompt.next = TRUE;


  return size;
}


static int
Xterm_write(void *handle, char *buffer, int count)
{ xterm *xt = handle;

  protocol(buffer, count);

  return write(xt->fd, buffer, count);
}


static int
Xterm_close(void *handle)
{ xterm *xt = handle;

  if ( --xt->count == 0 )
  { close(xt->fd);
    kill(xt->pid, SIGKILL);
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
    return PL_error("open_xterm", 3, NULL, ERR_TYPE, ATOM_text, title);

  master = open("/dev/ptmx", O_RDWR);
  grantpt(master);                             
  unlockpt(master);
  slavename = ptsname(master);
  slave = open(slavename, O_RDWR);
  ioctl(slave, I_PUSH, "ptem"); 
  ioctl(slave, I_PUSH, "ldterm");
 
  tcgetattr(slave, &termio);
  termio.c_lflag &= ~ECHO;
  termio.c_lflag |= (ICANON|IEXTEN);
  tcsetattr(slave, TCSANOW, &termio);
 
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
  Sdprintf("%s: Erase = %d\n", slavename, termio.c_cc[VERASE]);
  if ( tcsetattr(slave, TCSADRAIN, &termio) == -1 )
    perror("tcsetattr");
 
  xt = allocHeap(sizeof(*xt));
  xt->pid   = pid;
  xt->fd    = slave;
  xt->count = 3;			/* opened 3 times */

  PL_unify_stream(in,  Snew(xt,
			    SIO_INPUT|SIO_LBUF|SIO_NOFEOF,
			    &SXtermfunctions));
  PL_unify_stream(out, Snew(xt,
			    SIO_OUTPUT|SIO_LBUF,
			    &SXtermfunctions));
  PL_unify_stream(err, Snew(xt,
			    SIO_OUTPUT|SIO_NBUF,
			    &SXtermfunctions));

  succeed;
}
 
#else /*HAVE_GRANTPT*/

foreign_t
pl_open_xterm(term_t title, term_t in, term_t out, term_t err)
{ return notImplemented("open_xterm", 4);
}

#endif /*HAVE_GRANTPT*/
