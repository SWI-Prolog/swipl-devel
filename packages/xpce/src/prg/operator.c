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
#include <h/lang.h>

static status	kindOperator(Operator o, Name kind);

static status
initialiseOperator(Operator o, Name name, Int priority, Name kind)
{ assign(o, name, name);
  assign(o, priority, priority);

  return kindOperator(o, kind);
}


static status
kindOperator(Operator o, Name kind)
{ int lp, rp, p = valInt(o->priority);

  if ( kind == NAME_xf )
    lp = p-1, rp = 0;
  else if ( kind == NAME_yf )
    lp = p, rp = 0;
  else if ( kind == NAME_fx )
    lp = 0, rp = p-1;
  else if ( kind == NAME_fy )
    lp = 0, rp = p;
  else if ( kind == NAME_xfx )
    lp = rp = p-1;
  else if ( kind == NAME_yfy )
    lp = rp = p;
  else if ( kind == NAME_xfy )
    lp = p-1, rp = p;
  else /* if ( kind == NAME_yfx ) */
    lp = p, rp = p-1;
    
  assign(o, left_priority, toInt(lp));
  assign(o, right_priority, toInt(rp));

  succeed;
}


static Name
getKindOperator(Operator o)
{ Int lp = o->left_priority;
  Int rp = o->right_priority;
  Int p  = o->priority;

  if ( lp == ZERO )
    answer(rp == p ? NAME_fy : NAME_fx );
  if ( rp == ZERO )
    answer(lp == p ? NAME_yf : NAME_xf);
  if ( rp == p )
    answer(lp == p ? NAME_yfy : NAME_xfy);
  else
    answer(lp == p ? NAME_yfx : NAME_xfx);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=name", "priority=0..1200", "kind={xf,yf,xfx,xfy,yfx,yfy,fy,fx}" };

/* Instance Variables */

static vardecl var_operator[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Name of the operator"),
  IV(NAME_priority, "int", IV_GET,
     NAME_internal, "Priority of the operator"),
  IV(NAME_leftPriority, "int", IV_GET,
     NAME_internal, "Max priority of left-hand operant"),
  IV(NAME_rightPriority, "int", IV_GET,
     NAME_internal, "Max priority of right-hand operant")
};

/* Send Methods */

static senddecl send_operator[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseOperator,
     DEFAULT, "Initialise"),
  SM(NAME_kind, 1, "kind={xf,yf,xfx,xfy,yfx,yfy,fy,fx}", kindOperator,
     NAME_syntax, "Define associativity of the operator")
};

/* Get Methods */

static getdecl get_operator[] =
{ GM(NAME_kind, 0, "kind={xf,yf,xfx,xfy,yfx,yfy,fy,fx}", NULL, getKindOperator,
     NAME_syntax, "associativity of the operator")
};

/* Resources */

#define rc_operator NULL
/*
static classvardecl rc_operator[] =
{ 
};
*/

/* Class Declaration */

static Name operator_termnames[] = { NAME_name, NAME_priority, NAME_kind };

ClassDecl(operator_decls,
          var_operator, send_operator, get_operator, rc_operator,
          3, operator_termnames,
          "$Rev$");

status
makeClassOperator(Class class)
{ return declareClass(class, &operator_decls);
}
