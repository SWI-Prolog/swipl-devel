/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/interface.h>
#include <h/graphics.h>


status
initialiseHost(Host h, Name which)
{ assign(h, system, which);
  assign(h, callBack, ON);
  assign(h, language, NAME_prolog);
  assign(h, messages, newObject(ClassChain, 0));

  succeed;
}


static status
callHostv(Host host, Name selector, int argc, Any *argv)
{ status rval;
  int n;

  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) )
      addCodeReference(argv[n]);

  rval = hostSend(host, selector, argc, argv);
  
  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) && !isFreedObj(argv[n]) )
      delCodeReference(argv[n]);

  return rval;
}


static Any
getCallHostv(Host host, Name selector, int argc, Any *argv)
{ Any rval;
  int n;

  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) )
      addCodeReference(argv[n]);

  rval = hostGet(host, selector, argc, argv);

  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) && !isFreedObj(argv[n]) )
      delCodeReference(argv[n]);

  return rval;
}


static status
catchAllHostv(Host h, Name selector, int argc, Any *argv)
{ if ( h->callBack == ON )
  { status rval;

    if ( !(rval = callHostv(h, selector, argc, argv)) &&
    	 PCE->last_error == NAME_noBehaviour )
      assign(PCE, last_error, NIL);

    return rval;
  } else
  { ArgVector(av, argc+2);
    int ac;

    av[0] = h; av[1] = (Any) selector;
    for(ac=2; ac <= argc; ac++)
      av[ac] = argv[ac-2];

    return appendChain(h->messages, newObjectv(ClassMessage, ac, av));
  }
}


static Any
getCatchAllHostv(Host h, Name selector, int argc, Any *argv)
{ Any rval;

  if ( h->callBack == OFF )
  { errorPce(h, NAME_noCallBack);
    fail;
  }
  
  if ( !(rval = getCallHostv(h, selector, argc, argv)) &&
        PCE->last_error == NAME_noBehaviour )
    assign(PCE, last_error, NIL);

  answer(rval);
}


/* (AA)	If Host returns a message from the queue it can not be
 *	deleted as it would be garbage collected immediately.
 *	So, we set a flag to indicate we should delete the message
 *	the next time we enter getMessageHost() again.
 */
int	HostDeleteHeadFirst = FALSE;

static Message
getMessageHost(Host h)
{ Message msg;
  Bool oldBack = h->callBack;

  assign(h, callBack, OFF);

  if (HostDeleteHeadFirst)
    deleteHeadChain(h->messages);
  HostDeleteHeadFirst = FALSE;

  for (;;)
  { if ((msg = getHeadChain(h->messages)) != FAIL)
    { HostDeleteHeadFirst = TRUE;
      assign(h, callBack, oldBack);
      answer(msg);
    }
    dispatchDisplayManager(TheDisplayManager(), DEFAULT, DEFAULT);
  }
}


status
makeClassHost(Class class)
{ sourceClass(class, makeClassHost, __FILE__, "$Revision$");

  localClass(class, NAME_language, NAME_host, "{prolog,lisp,c}", NAME_both,
	     "Host language pce is connected to");
  localClass(class, NAME_system, NAME_host, "name", NAME_both,
	     "Identifier name of host language");
  localClass(class, NAME_callBack, NAME_callback, "bool", NAME_both,
	     "Queue messages or invoke asynchronously");
  localClass(class, NAME_messages, NAME_callback, "chain*", NAME_get,
	     "Message queue");

  termClass(class, "host", 1, NAME_system);
  saveStyleClass(class, NAME_external);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "name=name",
	     "Create host from name",
	     initialiseHost);
  sendMethod(class, NAME_catchAll, NAME_callback, 2, "name", "any ...",
	     "Call procedure on host",
	     catchAllHostv);
  sendMethod(class, NAME_call, NAME_callback, 2, "name", "unchecked ...",
	     "Invoke a host defined send_method",
	     callHostv);

  getMethod(class, NAME_call, NAME_callback, "unchecked", 2,
	    "name", "unchecked ...",
	    "Invoke a host defined get_method",
	    getCallHostv);
  getMethod(class, NAME_catchAll, NAME_callback, "any", 2, "name", "any ...",
	    "Call function on host",
	    getCatchAllHostv);
  getMethod(class, NAME_message, NAME_callback, "message", 0,
	    "Read next message from queue",
	    getMessageHost);

  HOST = globalObject(NAME_host, ClassHost, NAME_unknown, 0);
  protectObject(HOST);

  succeed;
}


Host
HostObject()
{ if ( HOST )
    return HOST;

  return findGlobal(NAME_host);
}
