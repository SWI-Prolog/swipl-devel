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

#include <h/kernel.h>
#include <h/interface.h>
#include <h/graphics.h>


status
initialiseHost(Host h, Name which)
{ assign(h, system, which);
  assign(h, callBack, ON);
  assign(h, language, NAME_prolog);
  assign(h, messages, newObject(ClassChain, EAV));

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
    for(ac=0; ac < argc; ac++)
      av[ac+2] = argv[ac];

    return appendChain(h->messages, newObjectv(ClassMessage, argc+2, av));
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


static Message
getMessageHost(Host h)
{ Message msg;
  Bool oldBack = h->callBack;

  assign(h, callBack, OFF);

  for (;;)
  { if ( (msg = getHeadChain(h->messages)) )
    { assign(h, callBack, oldBack);

      addCodeReference(msg);
      deleteHeadChain(h->messages);
      delCodeReference(msg);
      pushAnswerObject(msg);

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
        { "name", "any|host_data ..." };
static char *T_name_unchecked_XXX[] =
        { "name", "unchecked ..." };
static char *T_sendCall[] =
	{ "name|host_data", "unchecked ..." };

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
  SM(NAME_call, 2, T_sendCall, callHostv,
     NAME_callback, "Invoke a host defined send operation"),
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
static classvardecl rc_host[] =
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

  HOST = globalObject(NAME_host, ClassHost, NAME_unknown, EAV);
  protectObject(HOST);

  succeed;
}


Host
HostObject()
{ if ( HOST )
    return HOST;

  return findGlobal(NAME_host);
}
