/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/arith.h>

static status
initialiseSpatial(Spatial s,
		  Equation xFrom, Equation yFrom,
		  Equation xTo, Equation yTo,
		  Equation wTo, Equation hTo)
{ assign(s, xFrom, isDefault(xFrom) ? (Equation) NIL : xFrom);
  assign(s, yFrom, isDefault(yFrom) ? (Equation) NIL : yFrom);
  assign(s, xTo, isDefault(xTo) ? (Equation) NIL : xTo);
  assign(s, yTo, isDefault(yTo) ? (Equation) NIL : yTo);
  assign(s, wTo, isDefault(wTo) ? (Equation) NIL : wTo);
  assign(s, hTo, isDefault(hTo) ? (Equation) NIL : hTo);

  succeed;
}


static inline Int
_getVar(Equation e, Var var, va_list args) /* Var, Int, Var, Int, ... */
{ int argc, i;
  Var vars[FWD_PCE_MAX_ARGS];
  Any vals[FWD_PCE_MAX_ARGS];
  Any savd[FWD_PCE_MAX_ARGS];
  Int rval;

  for(argc = 0; (vars[argc] = va_arg(args, Var)) != NULL; argc++)
  { assert(argc <= FWD_PCE_MAX_ARGS);
    assert(instanceOfObject(vars[argc], ClassVar));

    vals[argc] = va_arg(args, Expression);
    assert(vals[argc] != NULL);
  }

  for(i=0; i<argc; i++)
  { savd[i] = vars[i]->value;
    setVar(vars[i], vals[i]);
  }

  rval = getVarEquation(e, var);

  for(i=0; i<argc; i++)
    setVar(vars[i], savd[i]);

  return rval;
}


static Int
getVar(Equation e, Var var, ...)
{ va_list args;
  Int rval;

  va_start(args, var);
  rval = _getVar(e, var, args);
  va_end(args);

  return rval;
}


#define CALC(var, rel, how, def) TRY(var = (notNil(rel) ? how : def))

static status
forwardsSpatial(Spatial s, Any from, Any to)
{ Int xref, yref;
  Int tX, tY, tW, tH;
  Area f, t;

  TRY(f = get(from, NAME_area, 0));
  TRY(t = get(to, NAME_area, 0));

  CALC(xref,s->xFrom,getVar(s->xFrom,VarXref,VarX,f->x,VarW,f->w,0),f->x);
  CALC(yref,s->yFrom,getVar(s->yFrom,VarYref,VarY,f->y,VarH,f->h,0),f->y);
  CALC(tW, s->wTo, getVar(s->wTo, VarW2, VarW, f->w, 0), t->w);
  CALC(tH, s->hTo, getVar(s->hTo, VarH2, VarH, f->h, 0), t->h);
  CALC(tX, s->xTo,getVar(s->xTo, VarX, VarXref, xref, VarW, tW, 0), t->x);
  CALC(tY, s->yTo,getVar(s->yTo, VarY, VarYref, yref, VarH, tH, 0), t->y);

  DEBUG(NAME_spatial,
	Cprintf("%s->f: (%s,%s) -- %ld,%ld,%ld,%ld ==> (%ld, %ld, %ld, %ld)\n",
		pp(s), pp(from), pp(to),
		valInt(f->x), valInt(f->y), valInt(f->w), valInt(f->h),
		valInt(tX), valInt(tY), valInt(tW), valInt(tH)));

  if (t->x != tX || t->y != tY || t->w != tW || t->h != tH)
    return send(to, NAME_set, tX, tY, tW, tH, 0);

  succeed;
}


static status
backwardsSpatial(Spatial s, Any from, Any to)
{ Int xref, yref;
  Int fW, fH, fX, fY;

  Area f, t;

  TRY(f = get(from, NAME_area, 0));
  TRY(t = get(to, NAME_area, 0));

  CALC(xref, s->xTo, getVar(s->xTo,VarXref,VarX,t->x,VarW,t->w,0), t->x);
  CALC(yref, s->yTo, getVar(s->yTo,VarYref,VarY,t->y,VarH,t->h,0), t->y);
  CALC(fW, s->wTo, getVar(s->wTo,VarW,VarW2,t->w,0), f->w);
  CALC(fH, s->hTo, getVar(s->hTo,VarH,VarH2,t->h,0), f->h);
  CALC(fX, s->xTo, getVar(s->xFrom,VarX,VarXref,xref,VarW,f->w,0), f->x);
  CALC(fY, s->yTo, getVar(s->yFrom,VarY,VarYref,yref,VarH,f->h,0), f->y);

  DEBUG(NAME_spatial,
	Cprintf("%s->b: (%s,%s) -- %ld,%ld,%ld,%ld ==> (%ld, %ld, %ld, %ld)\n",
		pp(s), pp(from), pp(to),
		valInt(t->x), valInt(t->y), valInt(t->w), valInt(t->h),
		valInt(fX), valInt(fY), valInt(fW), valInt(fH)));

  if (f->x != fX || f->y != fY || f->w != fW || f->h != fH)
    return send(from, NAME_set, fX, fY, fW, fH, 0);

  succeed;
}


static status
createSpatial(Spatial s, Any from, Any to)
{ if (isNil(from) || isNil(to))
    succeed;
  return forwardsSpatial(s, from, to);
}


status
makeClassSpatial(Class class)
{ sourceClass(class, makeClassSpatial, __FILE__, "$Revision$");

  localClass(class, NAME_xFrom, NAME_position, "=*", NAME_both,
	     "X of reference at `from' (XYHW -> xref)");
  localClass(class, NAME_yFrom, NAME_position, "=*", NAME_both,
	     "Y of reference at `from' (XYHW -> yref)");
  localClass(class, NAME_xTo, NAME_position, "=*", NAME_both,
	     "X of reference at `to' (XYHW -> xref)");
  localClass(class, NAME_yTo, NAME_position, "=*", NAME_both,
	     "Y of reference at `to' (XYHW -> yref)");
  localClass(class, NAME_wTo, NAME_dimension, "=*", NAME_both,
	     "Equation between `w' and `w2'");
  localClass(class, NAME_hTo, NAME_dimension, "=*", NAME_both,
	     "Equation between `h' and `h2'");

  termClass(class, "spatial", 6, NAME_xFrom, NAME_yFrom, 
			         NAME_xTo, NAME_yTo, 
			         NAME_wTo, NAME_hTo);
  sendMethod(class, NAME_initialise, DEFAULT, 6,
	     "x1=[=]*", "y1=[=]*", "x2=[=]*", "y2=[=]*",
	     "width=[=]*", "height=[=]*",
	     "Create from equations x1, y1, x2, y2, w, h",
	     initialiseSpatial);

  sendMethod(class, NAME_create, DEFAULT, 2,
	     "from=graphical*", "to=graphical*",
	     "Establish spatial relation",
	     createSpatial);
  sendMethod(class, NAME_forwards, DEFAULT, 2,
	     "from=graphical", "to=graphical",
	     "Maintain after `to' has changed",
	     forwardsSpatial);
  sendMethod(class, NAME_backwards, DEFAULT, 2,
	     "from=graphical", "to=graphical",
	     "Maintain after `from' has changed",
	     backwardsSpatial);

  succeed;
}

