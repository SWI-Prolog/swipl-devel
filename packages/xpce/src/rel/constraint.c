/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status	forwardCreateConstraint(Constraint c);
static status	fromConstraint(Constraint c, Any obj);
static status	toConstraint(Constraint c, Any obj);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE (Jan Wielemaker, 16/11/90):

Using this implementation technique we cannot  handle constraints with
the same from and  to.   The  problem  is that  we  do not know  which
whether  the constraint  is  to be   executed forwards  or   backards.
Consider the following calls:

	?- new(@b, box(40,50)),
	   new(@c, constraint(@b, @b, identity(width, height))).

to construct a square.

We now hope that   sending   ->width will  also update  ->height;  and
sending ->height will update ->width.  To realise this, the constraint
must  know whether to invoke  the relation with  direction forwards or
backwards.  In  this case the   constraint would have  to monitor  the
changes to the object resulting from  sending the original message and
analyse the relation.   Constraints are   not supposed to  know  about
their relation and thus  cannot perform this task.  Furthermore,  what
do we do after the user sends

	?- send(@b, size, size(30, 60)).

Turn it into a square of 30x30 or 60x60?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseConstraint(Constraint c, Any from, Any to,
		     Relation relation, Name only)
{ Name lock;

  if ( from == to )
    return errorPce(c, NAME_cannotConstraintSelf);

  assign(c, from, from);
  assign(c, to, to);
  assign(c, relation, relation);

  if ( isDefault(only) )
    lock = NAME_none;
  else if ( equalName(only, NAME_forwards) )
    lock = NAME_backwards;
  else
    lock = NAME_forwards;

  assign(c, locked, lock);
  constraintObject(from, c);
  constraintObject(to, c);
  forwardCreateConstraint(c);

  succeed;
}


static status
unlinkConstraint(Constraint c)
{ toConstraint(c, NIL);
  fromConstraint(c, NIL);

  succeed;
}


status
lockConstraint(Constraint c, Any obj)
{ if (c->locked == NAME_none)
  { assign(c, locked, obj == c->from ? NAME_back : NAME_front);
    succeed;
  }

  fail;
}


status
unlockConstraint(Constraint c, Any obj)
{ if (c->locked == (obj == c->from ? NAME_back : NAME_front))
    assign(c, locked, NAME_none);

  succeed;
}


static status
relationConstraint(Constraint c, Relation relation)
{ if (c->relation == relation)
    succeed;
  
  assign(c, relation, relation);
  forwardCreateConstraint(c);

  succeed;
}


static status
forwardCreateConstraint(Constraint c)
{ if ( notNil(c->from) && notNil(c->to) )
    send(c->relation, NAME_create, c->from, c->to, 0);

  succeed;
}


status
executeConstraint(Constraint c, Any obj)
{ if ( isNil(c->from) || isNil(c->to) )
    fail;

  if ( obj == c->from && (equalName(c->locked, NAME_forwards) ||
			  equalName(c->locked, NAME_front)) )
    fail;
  if ( obj == c->to   && (equalName(c->locked, NAME_backwards) ||
			  equalName(c->locked, NAME_back)) )
    fail;

  return send(c->relation, 
	      obj == c->from ? NAME_forwards : NAME_backwards, 
	      c->from, c->to, 0);
}


static status
fromConstraint(Constraint c, Any obj)
{ Instance old_from = c->from;

  if (old_from == obj)
    succeed;

  assign(c, from, obj);
  deleteConstraintObject(old_from, c);
  if (isNil(obj))
    succeed;

  constraintObject(c->from, c);
  forwardCreateConstraint(c);

  succeed;
}


static status
toConstraint(Constraint c, Any obj)
{ Instance old_to = c->to;

  if (old_to == obj)
    succeed;

  assign(c, to, obj);
  if ( notNil(old_to) )
    deleteConstraintObject(old_to, c);

  if (isNil(obj))
    succeed;

  constraintObject(c->to, c);
  forwardCreateConstraint(c);

  succeed;
}


status
makeClassConstraint(Class class)
{ sourceClass(class, makeClassConstraint, __FILE__, "$Revision$");

  localClass(class, NAME_from, NAME_client, "object", NAME_get,
	     "`From' side of the constraint");
  localClass(class, NAME_to, NAME_client, "object", NAME_get,
	     "`To' side of the constraint");
  localClass(class, NAME_relation, NAME_relation, "relation", NAME_get,
	     "Relation that describes the constraint");
  localClass(class, NAME_locked, NAME_internal,
	     "{none,forwards,front,backwards,back}", NAME_none,
	     "Avoid looping of propagation");

  termClass(class, "constraint",
	    3, NAME_from, NAME_to, NAME_relation, NAME_locked);
  cloneStyleClass(class, NAME_relation);

  storeMethod(class, NAME_from, fromConstraint);
  storeMethod(class, NAME_relation, relationConstraint);
  storeMethod(class, NAME_to, toConstraint);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "from=object", "to=object", "relation=relation",
	     "propagate=[{forwards,backwards}]",
	     "Create from objects, relation and direction",
	     initialiseConstraint);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Remove from to- and from-object",
	     unlinkConstraint);

  succeed;
}

