/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status arrowsJoint(Joint, Name);

status
initialiseJoint(Joint jt, Int x, Int y, Int w, Int h, Name arrows)
{ initialiseGraphical(jt, x, y, w, h);

  if ( notDefault(arrows) )
    arrowsJoint(jt, arrows);

  succeed;
}


status
copyJoint(Joint jt1, Joint jt2)
{ copyGraphical(jt1, jt2);

  return setArrowsJoint(jt1, jt2->first_arrow, jt2->second_arrow);
}


		/********************************
		*            ARROWS		*
		********************************/

static Arrow
getDefaultArrowJoint(Joint jt)
{ answer(newObject(ClassArrow, 0));
}


status
setArrowsJoint(Joint jt, Arrow first, Arrow second)
{ if ( isDefault(first)  ) first  = jt->first_arrow;
  if ( isDefault(second) ) second = jt->second_arrow;

  if ( jt->first_arrow == first && jt->second_arrow == second )
    succeed;

  CHANGING_GRAPHICAL(jt,
	assign(jt, first_arrow, first);
	assign(jt, second_arrow, second);
	changedEntireImageGraphical(jt));

  succeed;
}


static status
firstArrowJoint(Joint jt, Arrow arrow)
{ return setArrowsJoint(jt, arrow, DEFAULT);
}


static status
secondArrowJoint(Joint jt, Arrow arrow)
{ return setArrowsJoint(jt, DEFAULT, arrow);
}


static Arrow
initArrowJoint(Joint jt)
{ Any rval;

  if ( !(rval = qadGetv(jt, NAME_defaultArrow, 0, NULL)) )
    rval = NIL;

  return rval;
}


static status
arrowsJoint(Joint jt, Name arrows)
{ Arrow first, second;

  if ( equalName(arrows, NAME_none) )
  { first  = NIL;
    second = NIL;
  } else if ( arrows == NAME_first )
  { first = (notNil(jt->first_arrow) ? jt->first_arrow : initArrowJoint(jt));
    second = NIL;
  } else if ( arrows == NAME_second )
  { first = NIL;
    second = (notNil(jt->second_arrow) ? jt->second_arrow
				       : initArrowJoint(jt));
  } else if ( arrows == NAME_both )
  { first = (notNil(jt->first_arrow) ? jt->first_arrow : initArrowJoint(jt));
    second = (notNil(jt->second_arrow) ? jt->second_arrow
	      			       : initArrowJoint(jt));
  } else
    fail;

  return setArrowsJoint(jt, first, second);
}


static Name
getArrowsJoint(Joint jt)
{ if ( notNil(jt->first_arrow) )
  { if ( notNil(jt->second_arrow) )
      return NAME_both;
    else
      return NAME_first;
  } else
  { if ( notNil(jt->second_arrow) )
      return NAME_second;
    else
      return NAME_none;
  }
}


status
makeClassJoint(Class class)
{ sourceClass(class, makeClassJoint, __FILE__, "$Revision$");

  localClass(class, NAME_firstArrow, NAME_appearance, "arrow*", NAME_get,
	     "Arrow on start-point");
  localClass(class, NAME_secondArrow, NAME_appearance, "arrow*", NAME_get,
	     "Arrow on end-point");

  termClass(class, "joint", 1, NAME_arrows);

  storeMethod(class, NAME_firstArrow, firstArrowJoint);
  storeMethod(class, NAME_secondArrow, secondArrowJoint);

  sendMethod(class, NAME_initialise, DEFAULT,
	     5, "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	        "arrows=[{none,first,second,both}]",
	     "Create joint with bounding-box and arrows",
	     initialiseJoint);

  sendMethod(class, NAME_arrows, NAME_appearance, 1,
	     "arrows={none,first,second,both}",
	     "Default arrows on {none,first,second,both}",
	     arrowsJoint);

  getMethod(class, NAME_arrows, NAME_appearance,
	    "arrows={none,first,second,both}", 0,
	    "Which arrows are defined",
	    getArrowsJoint);
  getMethod(class, NAME_defaultArrow, NAME_appearance,
	    "arrow", 0,
	    "Create default arrow for ->arrows",
	    getDefaultArrowJoint);

  succeed;
}
