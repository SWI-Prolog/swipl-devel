/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
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


status
makeClassOperator(Class class)
{ sourceClass(class, makeClassOperator, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name", NAME_get,
	     "Name of the operator");
  localClass(class, NAME_priority, NAME_internal, "int", NAME_get,
	     "Priority of the operator");
  localClass(class, NAME_leftPriority, NAME_internal, "int", NAME_get,
	     "Max priority of left-hand operant");
  localClass(class, NAME_rightPriority, NAME_internal, "int", NAME_get,
	     "Max priority of right-hand operant");

  termClass(class, "operator", 3, NAME_name, NAME_priority, NAME_kind);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "name=name", "priority=0..1200",
	     "kind={xf,yf,xfx,xfy,yfx,yfy,fy,fx}",
	     "Initialise",
	     initialiseOperator);
  sendMethod(class, NAME_kind, NAME_syntax, 1,
	     "kind={xf,yf,xfx,xfy,yfx,yfy,fy,fx}",
	     "Define associativity of the operator",
	     kindOperator);

  getMethod(class, NAME_kind, NAME_syntax,
	    "kind={xf,yf,xfx,xfy,yfx,yfy,fy,fx}", 0,
	    "associativity of the operator",
	    getKindOperator);

  succeed;
}
