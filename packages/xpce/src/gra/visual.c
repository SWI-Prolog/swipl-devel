/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
containedInVisual(VisualObj v, VisualObj super)
{ while( v != FAIL && notNil(v) )
  { if ( v == super )
      succeed;
    v = get(v, NAME_containedIn, 0);
  }

  fail;
}


static VisualObj
getContainedInVisual(VisualObj v)
{ fail;
}


static Chain
getContainsVisual(VisualObj v)
{ fail;
}


static VisualObj
getContainerVisual(VisualObj v, Any cond)
{ while(v)
  { if ( instanceOfObject(cond, ClassClass) &&
	 instanceOfObject(v, (Class)cond) )
      return v;
    if ( instanceOfObject(cond, ClassCode) &&
	 forwardCodev(cond, 1, (Any *)&v) )
      return v;

    v = getv(v, NAME_containedIn, 0, NULL);
  }

  fail;
}


static VisualObj
getMasterVisual(VisualObj v)
{ answer(v);
}


status
resetVisual(VisualObj v)
{ Chain ch = get(v, NAME_contains, 0);

  if ( ch != FAIL )
  { Cell cell;
    
    for_cell(cell, ch)
      send(cell->value, NAME_reset, 0);

    doneObject(ch);
  }

  succeed;
}


static void
collectSubsVisual(VisualObj v, Chain ch, int root)
{ if ( root || !onFlag(v, F_PROTECTED|F_LOCKED|F_FREED|F_FREEING) )
  { Chain subs = getv(v, NAME_contains, 0, NULL);

    appendChain(ch, v);
    if ( subs )
    { Cell cell;
      for_cell(cell, subs)
	collectSubsVisual(cell->value, ch, FALSE);
    }
  }
}


status
destroyVisual(VisualObj v)
{ if ( !onFlag(v, F_PROTECTED) )
  { Chain subs = newObject(ClassChain, 0);
    VisualObj sub;

    collectSubsVisual(v, subs, TRUE);
    for_chain(subs, sub,
	      { DEBUG(NAME_destroy, printf("%s ->free\n", pp(sub)));
		sendv(sub, NAME_free, 0, NULL);
	      });
    freeObject(subs);

    succeed;
  }

  fail;
}



static Any
getFrameVisual(VisualObj v)
{ for(;;)
  { if ( instanceOfObject(v, ClassFrame) )
      answer(v);
    if ( !instanceOfObject(v, ClassVisual) ||
         !(v = get(v, NAME_containedIn, 0)) )
      fail;
  }
}


Any
getReportToVisual(VisualObj v)
{ return getv(v, NAME_containedIn, 0, NULL);
}


status
reportVisual(VisualObj v, Name kind, CharArray fmt, int argc, Any *argv)
{ VisualObj super;
  status rval = FAIL;
  
  if ( (super = getv(v, NAME_reportTo, 0, NULL)) )
  { ArgVector(av, argc + 2);

    av[0] = kind;
    av[1] = fmt;
    copyArgs(argc, argv, &av[2]);

    if ( isNil(REPORTEE->value) )
    { withLocalVars(assignVar(REPORTEE, v, NAME_local);
		    rval = sendv(super, NAME_report, argc+2, av));
    } else
      rval = sendv(super, NAME_report, argc+2, av);
  }

  return rval;
}


status
alertReporteeVisual(Any v)
{ Any obj = (isNil(REPORTEE->value) ? v : REPORTEE->value);

  while( obj && !isNil(obj) && !hasSendMethodObject(obj, NAME_alert) )
    obj = getv(obj, NAME_containedIn, 0, NULL);

  if ( obj && !isNil(obj) )
    send(obj, NAME_alert, 0);

  succeed;
}


status
makeClassVisual(Class class)
{ sourceClass(class, makeClassVisual, __FILE__, "$Revision$");

  termClass(class, "visual", 0);

  sendMethod(class, NAME_containedIn, NAME_organisation, 1, "visual",
	     "Test if I'm contained in argument",
	     containedInVisual);
  sendMethod(class, NAME_reset, NAME_abort, 0,
	     "Send a ->reset to all contained objects",
	     resetVisual);
  sendMethod(class, NAME_destroy, NAME_oms, 0,
	     "Destroy consists-of tree of visual objects",
	     destroyVisual);
  sendMethod(class, NAME_report, NAME_report, 3,
	     "kind={status,inform,progress,done,warning,error}",
	     "format=[char_array]", "argument=any ...",
	     "Report message (send to <-contained_in)",
	     reportVisual);

  getMethod(class, NAME_containedIn, NAME_organisation, "visual", 0,
	    "Visual I'm contained in (parent)",
	    getContainedInVisual);
  getMethod(class, NAME_contains, NAME_organisation, "chain", 0,
	    "Chain with visuals I manage",
	    getContainsVisual);
  getMethod(class, NAME_container, NAME_organisation, "condition=visual",
	    1, "class|code", 
	    "Innermost visual that satisfies condition",
	    getContainerVisual);
  getMethod(class, NAME_master, NAME_event, "visual", 0,
	    "Principal visual I'm part of (self)",
	    getMasterVisual);
  getMethod(class, NAME_frame, NAME_organisation, "frame", 0,
	    "Frame I'm part of (when existing)",
	    getFrameVisual);
  getMethod(class, NAME_reportTo, NAME_report, "visual", 0,
	    "Object for ->report (equivalent to <-containe_in",
	    getReportToVisual);

  succeed;
}

