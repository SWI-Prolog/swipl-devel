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

static Graphical
getDefaultArrowJoint(Joint jt)
{ answer(newObject(ClassArrow, EAV));
}


status
setArrowsJoint(Joint jt, Graphical first, Graphical second)
{ if ( isDefault(first)  ) first  = jt->first_arrow;
  if ( isDefault(second) ) second = jt->second_arrow;

  if ( jt->first_arrow == first && jt->second_arrow == second )
    succeed;

  CHANGING_GRAPHICAL(jt,
		     assign(jt, first_arrow, first);
		     assign(jt, second_arrow, second);
		     requestComputeGraphical(jt, DEFAULT);
		     changedEntireImageGraphical(jt));

  succeed;
}


static status
firstArrowJoint(Joint jt, Graphical arrow)
{ return setArrowsJoint(jt, arrow, DEFAULT);
}


static status
secondArrowJoint(Joint jt, Graphical arrow)
{ return setArrowsJoint(jt, DEFAULT, arrow);
}


static Graphical
initArrowJoint(Joint jt)
{ Any rval;

  if ( !(rval = qadGetv(jt, NAME_defaultArrow, 0, NULL)) )
    rval = NIL;

  return rval;
}


static status
arrowsJoint(Joint jt, Name arrows)
{ Graphical first, second;

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
		 *	      SELECTED		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The selection blobs of joints often exceed the area.  Enlarge it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
selectedJoint(Joint jt, Bool selected)
{ if ( jt->selected != selected )
  { CHANGING_GRAPHICAL(jt,
		       assign(jt, selected, selected);
		       requestComputeGraphical(jt, DEFAULT);
		       changedEntireImageGraphical(jt));
  }

  succeed;
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
{ SV(NAME_firstArrow, "graphical*", IV_GET|IV_STORE, firstArrowJoint,
     NAME_appearance, "Arrow on start-point"),
  SV(NAME_secondArrow, "graphical*", IV_GET|IV_STORE, secondArrowJoint,
     NAME_appearance, "Arrow on end-point")
};

/* Send Methods */

static senddecl send_joint[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseJoint,
     DEFAULT, "Create joint with bounding-box and arrows"),
  SM(NAME_arrows, 1, "arrows={none,first,second,both}", arrowsJoint,
     NAME_appearance, "Default arrows on {none,first,second,both}"),
  SM(NAME_selected, 1, "bool", selectedJoint,
     NAME_selection, "If @on, I'm selected")
};

/* Get Methods */

static getdecl get_joint[] =
{ GM(NAME_arrows, 0, "arrows={none,first,second,both}", NULL, getArrowsJoint,
     NAME_appearance, "Which arrows are defined"),
  GM(NAME_defaultArrow, 0, "graphical", NULL, getDefaultArrowJoint,
     NAME_appearance, "Create default arrow for ->arrows")
};

/* Resources */

#define rc_joint NULL
/*
static classvardecl rc_joint[] =
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
