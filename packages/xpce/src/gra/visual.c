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
	      { DEBUG(NAME_destroy, Cprintf("%s ->free\n", pp(sub)));
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
    { Chain visited = answerObject(ClassChain, v, 0);

      withLocalVars(assignVar(REPORTEE, visited, NAME_local);
		    rval = sendv(super, NAME_report, argc+2, av));

      doneObject(visited);
    } else
    { appendChain(REPORTEE->value, v);
      rval = sendv(super, NAME_report, argc+2, av);
    }
  }

  return rval;
}


status
alertReporteeVisual(Any v)
{ Any obj = (isNil(REPORTEE->value) ? v : getHeadChain(REPORTEE->value));

  while( obj && !isNil(obj) && !hasSendMethodObject(obj, NAME_alert) )
    obj = getv(obj, NAME_containedIn, 0, NULL);

  if ( obj && !isNil(obj) )
    send(obj, NAME_alert, 0);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

/* Instance Variables */

#define var_visual NULL
/*
vardecl var_visual[] =
{ 
};
*/

/* Send Methods */

static senddecl send_visual[] =
{ SM(NAME_reset, 0, NULL, resetVisual,
     NAME_abort, "Send a ->reset to all contained objects"),
  SM(NAME_destroy, 0, NULL, destroyVisual,
     NAME_oms, "Destroy consists-of tree of visual objects"),
  SM(NAME_containedIn, 1, "visual", containedInVisual,
     NAME_organisation, "Test if I'm contained in argument"),
  SM(NAME_report, 3, T_report, reportVisual,
     NAME_report, "Report message (send to <-contained_in)")
};

/* Get Methods */

static getdecl get_visual[] =
{ GM(NAME_master, 0, "visual", NULL, getMasterVisual,
     NAME_event, "Principal visual I'm part of (self)"),
  GM(NAME_containedIn, 0, "visual", NULL, getContainedInVisual,
     NAME_organisation, "Visual I'm contained in (parent)"),
  GM(NAME_container, 1, "condition=visual", "class|code", getContainerVisual,
     NAME_organisation, "Innermost visual that satisfies condition"),
  GM(NAME_contains, 0, "chain", NULL, getContainsVisual,
     NAME_organisation, "Chain with visuals I manage"),
  GM(NAME_frame, 0, "frame", NULL, getFrameVisual,
     NAME_organisation, "Frame I'm part of (when existing)"),
  GM(NAME_reportTo, 0, "visual", NULL, getReportToVisual,
     NAME_report, "Object for ->report (equivalent to <-containe_in")
};

/* Resources */

#define rc_visual NULL
/*
static classvardecl rc_visual[] =
{ 
};
*/

/* Class Declaration */


ClassDecl(visual_decls,
          var_visual, send_visual, get_visual, rc_visual,
          0, NULL,
          "$Rev$");

status
makeClassVisual(Class class)
{ return declareClass(class, &visual_decls);

  succeed;
}

