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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "x=[int]", "y=[int]", "width=[int]",
	  "height=[int]", "arrows=[{none,first,second,both}]" };

/* Instance Variables */

static vardecl var_joint[] =
{ SV(NAME_firstArrow, "arrow*", IV_GET|IV_STORE, firstArrowJoint,
     NAME_appearance, "Arrow on start-point"),
  SV(NAME_secondArrow, "arrow*", IV_GET|IV_STORE, secondArrowJoint,
     NAME_appearance, "Arrow on end-point")
};

/* Send Methods */

static senddecl send_joint[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseJoint,
     DEFAULT, "Create joint with bounding-box and arrows"),
  SM(NAME_arrows, 1, "arrows={none,first,second,both}", arrowsJoint,
     NAME_appearance, "Default arrows on {none,first,second,both}")
};

/* Get Methods */

static getdecl get_joint[] =
{ GM(NAME_arrows, 0, "arrows={none,first,second,both}", NULL, getArrowsJoint,
     NAME_appearance, "Which arrows are defined"),
  GM(NAME_defaultArrow, 0, "arrow", NULL, getDefaultArrowJoint,
     NAME_appearance, "Create default arrow for ->arrows")
};

/* Resources */

#define rc_joint NULL
/*
static resourcedecl rc_joint[] =
{ 
};
*/

/* Class Declaration */

static Name joint_termnames[] = { NAME_arrows };

ClassDecl(joint_decls,
          var_joint, send_joint, get_joint, rc_joint,
          1, joint_termnames,
          "$Rev$");


status
makeClassJoint(Class class)
{ return declareClass(class, &joint_decls);
}
