/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Chain.h>
#include <pce/Class.h>
#include <pce/Create.h>
#include <pce/Message.h>
#include <pce/Obtain.h>

static PceArg PcNappend("append");
static PceArg PcNdelete("delete");
static PceArg PcNfor_all("for_all");
static PceArg PcNforward_vector("forward_vector");
static PceArg PcNintersection("intersection");
static PceArg PcNsubtract("subtract");
static PceArg PcNvalue("value");


static PceVariable *d_listener_messages;

static PceStatus
subscribeDispatcher(PceReceiver d, PceArg me, PceArg id, PceArg action)
{ PceArg ch;
  PceArg lm = d.fetch(d_listener_messages);

  if ( (ch = d.get(PcNvalue, id)) )
    ch.send(PcNappend, action);
  else
    d.send(PcNvalue, id, PceChain(action));

  if ( (ch = lm.get(PcNvalue, me)) )
    ch.send(PcNappend, action);
  else
    lm.send(PcNvalue, me, PceChain(action));

  return TRUE;
}


static PceStatus
unsubscribeDispatcher(PceReceiver d, PceArg me, PceArg id)
{ PceArg lm = d.fetch(d_listener_messages);
  PceArg msgs;

  if ( (msgs = lm.get(PcNvalue, me)) )
  { if ( id == TheDefault )
    { d.send(PcNfor_all, PceMessage(PceObtain(TheArg1, PcNvalue),
				   PcNsubtract, msgs));
      lm.send(PcNdelete, me);
    } else
    { PceArg messages = d.get(PcNvalue, id);
      PceArg common   = messages.get(PcNintersection, msgs);

      messages.send(PcNsubtract, common);
      msgs.send(PcNsubtract, common);
    }
  }

  return TRUE;
}


static PceStatus
dispatchDispatcher(PceReceiver d, PceArg id, PceArg args)
{ PceChain ch = d.get(PcNvalue, id);

  if ( ch )
  { PceCell c = ch.head();

    for( ; c; c++)
      c.value().send(PcNforward_vector, args);
  }

  return TRUE;
}


PceStatus
makeClassDispatcher(PceClass cl)
{ d_listener_messages =
    cl.defvar("listener_messages", "code", "Messages to send",
	      "sheet", "get", PceCreate("sheet"));

  cl.defsendmethod("subscribe", "register", "Subscribe myself",
		   subscribeDispatcher, "me=any", "id=name", "action=code");
  cl.defsendmethod("unsubscribe", "register", "Unsubscribe myself",
		   unsubscribeDispatcher, "me=any", "id=[name]");
  cl.defsendmethod("dispatch", "dispatching", "Dispatch id & args",
		   dispatchDispatcher, "id=name", "context=any ...");

  return TRUE;
}

PceClass ClassDispatcher("dispatcher", "sheet", "Dispatch messages",
			 makeClassDispatcher);

PceStatus
pceInitApplication(int argc, char *argv[])
{ return TRUE;
}
