/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <md.h>				/* get HAVE_'s */
#if defined(HAVE_SOCKET) && defined(HAVE_SHUTDOWN)
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
{ XtInputId id;

  if ( s->rdstream )
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

  if ( (id = getXtInputIdStream(s)) )
  { XtRemoveInput(id);
    setXtInputIdStream(s, 0);
  }
}


void
ws_close_output_stream(Stream s)
{ if ( s->wrfd >= 0 )
  {
#ifdef HAVE_SHUTDOWN
    if ( instanceOfObject(s, ClassSocket) )
      shutdown(s->wrfd, SHUT_WR);
    else
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
{ handleInputStream((Stream) xp);
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


int
ws_read_stream_data(Stream s, void *data, int len)
{ if ( s->rdfd < 0 )
    return -1;
  
  return read(s->rdfd, data, len);
}



StringObj
ws_read_line_stream(Stream s, Int timeout)
{ if ( s->rdfd >= 0 )
  { char buf[LINESIZE];

    if ( !s->rdstream )
      s->rdstream = fdopen(s->rdfd, "r");

    if ( notDefault(timeout) )
    { fd_set readfds;
      struct timeval to;

      to.tv_sec = valInt(timeout) / 1000;
      to.tv_usec = (valInt(timeout) % 1000) * 1000;

      FD_ZERO(&readfds);
      FD_SET(fileno(s->rdstream), &readfds);
      if ( select(32, &readfds, NULL, NULL, &to) == 0 )
	fail;
    }

    if (fgets(buf, LINESIZE, s->rdstream) == NULL)	/* eof */
    { closeInputStream(s);
      fail;
    }

    answer(CtoString(buf));
  }

  errorPce(s, NAME_notOpen);
  fail;
}


void
ws_done_process(Process p)
{
}
