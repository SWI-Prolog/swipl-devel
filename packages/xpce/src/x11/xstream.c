/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <md.h>				/* get HAVE_'s */
#if defined(HAVE_SOCKET) && defined(HAVE_SHUTDOWN)
#include <sys/types.h>
#include <sys/socket.h>			/* avoid send() conflict */
#endif

#include <h/kernel.h>
#include <h/unix.h>
#include "include.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <errno.h>

#define OsError() getOsErrorPce(PCE)

static XtInputId
getXtInputIdStream(Stream s)
{ return (XtInputId) s->ws_ref;
}


static void
setXtInputIdStream(Stream s, XtInputId id)
{ s->ws_ref = (WsRef) id;
}


#ifndef SHUT_RD
#define SHUT_RD 0
#define SHUT_WR 1
#endif

void
ws_close_input_stream(Stream s)
{ if ( s->rdstream )
  { fclose(s->rdstream);
    s->rdstream = NULL;
  }

  if ( s->rdfd >= 0 )
  {
#ifdef HAVE_SHUTDOWN
    if ( instanceOfObject(s, ClassSocket) )
      shutdown(s->rdfd, SHUT_RD);
    else
#endif
      close(s->rdfd);
    s->rdfd = -1;
  }

  ws_no_input_stream(s);
}


void
ws_close_output_stream(Stream s)
{ if ( s->wrfd >= 0 )
  {
#ifdef HAVE_SHUTDOWN
    if ( instanceOfObject(s, ClassSocket) )
      shutdown(s->wrfd, SHUT_WR);
/*    else 		Seems we must do both to free the descriptor */
#endif
      close(s->wrfd);
    s->wrfd = -1;
  }
}


void
ws_close_stream(Stream s)
{
}


static void
ws_handle_stream_data(XtPointer xp, int *source, XtInputId *id)
{ Stream s = (Stream)xp;

  pceMTLock(LOCK_PCE);
  assert(isProperObject(s));
  DEBUG(NAME_stream, Cprintf("handleInputStream(%s)\n", pp(s)));
  handleInputStream(s);
  pceMTUnlock(LOCK_PCE);
}


void
ws_input_stream(Stream s)
{ if ( s->rdfd >= 0 )
  { XtInputId id;

    id = XtAppAddInput(pceXtAppContext(NULL),
		       s->rdfd,
		       (XtPointer)(XtInputReadMask),
		       ws_handle_stream_data, s);

    setXtInputIdStream(s, id);

    DEBUG(NAME_stream,
	  Cprintf("Registered %s for asynchronous input\n", pp(s)));
  }
}


void
ws_no_input_stream(Stream s)
{ XtInputId id;

  if ( (id = getXtInputIdStream(s)) )
  { XtRemoveInput(id);
    setXtInputIdStream(s, 0);

    DEBUG(NAME_stream,
	  Cprintf("Un-registered %s for asynchronous input\n", pp(s)));
  }
}


static void
ws_accept(XtPointer xp, int *source, XtInputId *id)
{ acceptSocket((Socket) xp);
}


void
ws_listen_socket(Socket s)
{ XtInputId id;

  id = XtAppAddInput(pceXtAppContext(NULL),
		     s->rdfd,
		     (XtPointer)(XtInputReadMask),
		     ws_accept, s);

  setXtInputIdStream((Stream) s, id);
}


status
ws_write_stream_data(Stream s, void *data, int len)
{ if ( s->wrfd < 0 )
    return errorPce(s, NAME_notOpen);
  if ( write(s->wrfd, data, len) != len )
    return errorPce(s, NAME_ioError, OsError());

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ws_read_stream_data(Stream s, void *data, int len, Int timeout)
    Read input from stream s into data, which is len bytes long.  If timeout
    is not DEFAULT, the read waits at most timeout milliseconds before doing
    the system read.

Return values: -2: timeout
	       -1: error
	        0: end-of-file
	       >0: bytes read
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


int
ws_read_stream_data(Stream s, void *data, int len, Real timeout)
{ if ( s->rdfd < 0 )
  { errno = EINVAL;
    return -1;
  }
  
  if ( notDefault(timeout) )
  { fd_set readfds;
    struct timeval to;
    double v = valReal(timeout);

    to.tv_sec  = (long)v;
    to.tv_usec = (long)(v * 1000000.0) % 1000000;

    FD_ZERO(&readfds);
    FD_SET(s->rdfd, &readfds);
    if ( select(s->rdfd+1, &readfds, NULL, NULL, &to) == 0 )
      return -2;
  }

  return read(s->rdfd, data, len);
}


void
ws_done_process(Process p)
{
}
