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
    updateConstraintsObject(c->from);

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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "from=object", "to=object", "relation=relation", "propagate=[{forwards,backwards}]" };

/* Instance Variables */

static vardecl var_constraint[] =
{ SV(NAME_from, "object", IV_GET|IV_STORE, fromConstraint,
     NAME_client, "`From' side of the constraint"),
  SV(NAME_to, "object", IV_GET|IV_STORE, toConstraint,
     NAME_client, "`To' side of the constraint"),
  SV(NAME_relation, "relation", IV_GET|IV_STORE, relationConstraint,
     NAME_relation, "Relation that describes the constraint"),
  IV(NAME_locked, "{none,forwards,front,backwards,back}", IV_NONE,
     NAME_internal, "Avoid looping of propagation")
};

/* Send Methods */

static senddecl send_constraint[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseConstraint,
     DEFAULT, "Create from objects, relation and direction"),
  SM(NAME_unlink, 0, NULL, unlinkConstraint,
     DEFAULT, "Remove from to- and from-object")
};

/* Get Methods */

#define get_constraint NULL
/*
static getdecl get_constraint[] =
{ 
};
*/

/* Resources */

#define rc_constraint NULL
/*
static classvardecl rc_constraint[] =
{ 
};
*/

/* Class Declaration */

static Name constraint_termnames[] = { NAME_from, NAME_to, NAME_relation, NAME_locked };

ClassDecl(constraint_decls,
          var_constraint, send_constraint, get_constraint, rc_constraint,
          3, constraint_termnames,
          "$Rev$");

status
makeClassConstraint(Class class)
{ declareClass(class, &constraint_decls);
  cloneStyleClass(class, NAME_relation);

  succeed;
}

