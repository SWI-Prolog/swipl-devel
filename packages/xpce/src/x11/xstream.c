/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>
#include "include.h"

static XtInputId
getXtInputIdStream(Stream s)
{ return (XtInputId) s->ws_ref;
}


static void
setXtInputIdStream(Stream s, XtInputId id)
{ s->ws_ref = (WsRef) id;
}


void
ws_close_stream(Stream s)
{ XtInputId id;

  if ( (id = getXtInputIdStream(s)) )
  { XtRemoveInput(id);
    setXtInputIdStream(s, 0);
  }
}


static void
ws_handle_stream_data(XtPointer xp, int *source, XtInputId *id)
{ handleInputStream((Stream) xp);
}


void
ws_input_stream(Stream s)
{ XtInputId id;

  id = XtAppAddInput(pceXtAppContext(NULL),
		     s->rdfd,
		     (XtPointer)(XtInputReadMask),
		     ws_handle_stream_data, s);

  setXtInputIdStream(s, id);
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
