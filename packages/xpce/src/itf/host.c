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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_name_any_XXX[] =
        { "name", "any ..." };
static char *T_name_unchecked_XXX[] =
        { "name", "unchecked ..." };

/* Instance Variables */

static vardecl var_host[] =
{ IV(NAME_language, "{prolog,lisp,c}", IV_BOTH,
     NAME_host, "Host language pce is connected to"),
  IV(NAME_system, "name", IV_BOTH,
     NAME_host, "Identifier name of host language"),
  IV(NAME_callBack, "bool", IV_BOTH,
     NAME_callback, "Queue messages or invoke asynchronously"),
  IV(NAME_messages, "chain*", IV_GET,
     NAME_callback, "Message queue")
};

/* Send Methods */

static senddecl send_host[] =
{ SM(NAME_initialise, 1, "name=name", initialiseHost,
     DEFAULT, "Create host from name"),
  SM(NAME_call, 2, T_name_unchecked_XXX, callHostv,
     NAME_callback, "Invoke a host defined send_method"),
  SM(NAME_catchAll, 2, T_name_any_XXX, catchAllHostv,
     NAME_callback, "Call procedure on host")
};

/* Get Methods */

static getdecl get_host[] =
{ GM(NAME_call, 2, "unchecked", T_name_unchecked_XXX, getCallHostv,
     NAME_callback, "Invoke a host defined get_method"),
  GM(NAME_catchAll, 2, "any", T_name_any_XXX, getCatchAllHostv,
     NAME_callback, "Call function on host"),
  GM(NAME_message, 0, "message", NULL, getMessageHost,
     NAME_callback, "Read next message from queue")
};

/* Resources */

#define rc_host NULL
/*
static resourcedecl rc_host[] =
{ 
};
*/

/* Class Declaration */

static Name host_termnames[] = { NAME_system };

ClassDecl(host_decls,
          var_host, send_host, get_host, rc_host,
          1, host_termnames,
          "$Rev$");

status
makeClassHost(Class class)
{ declareClass(class, &host_decls);
  saveStyleClass(class, NAME_external);

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
