/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD:	handle destruction of these objects.  Not that important as they
	are generally not destroyed and only a bit of memory is wasted
	if they are.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define XREF_TABLESIZE 		256
#define HashValue(obj)		(((ulong)(obj) & (XREF_TABLESIZE-1)))

static Xref XrefTable[XREF_TABLESIZE];


WsRef
getXrefObject(Any obj, DisplayObj d)
{ int v = HashValue(obj);
  Xref r;

  XrefsResolved++;

  for( r = XrefTable[v]; r != NULL; r = r->next)
    if ( r->object == obj && r->display == d )
    { DEBUG(NAME_getXref, printf("getXrefObject(%s, %s) --> 0x%lx\n",
				 pp(obj), pp(d), (ulong) r->xref));
      return r->xref;
    }

  if ( openDisplay(d) == SUCCEED )
  { if ( send(obj, NAME_Xopen, d, 0) == SUCCEED )
    { for( r = XrefTable[v]; r != NULL; r = r->next)
	if ( r->object == obj && r->display == d )
	{ DEBUG(NAME_getXref, printf("getXrefObject(%s, %s) --> 0x%lx\n",
				     pp(obj), pp(d), (ulong) r->xref));
	  return r->xref;
	}
    }
  }

  XrefsResolved--;

  errorPce(obj, NAME_xOpen, d);

  return NULL;
}


WsRef
getExistingXrefObject(Any obj, DisplayObj d)
{ int v = HashValue(obj);
  Xref r;


  for( r = XrefTable[v]; r != NULL; r = r->next)
    if ( r->object == obj && r->display == d )
    { XrefsResolved++;
      return r->xref;
    }

  return NULL;
}


status
registerXrefObject(Any obj, DisplayObj d, WsRef xref)
{ Xref *R = &XrefTable[HashValue(obj)];
  Xref r, new;

  DEBUG(NAME_xref, printf("registerXrefObject(%s, %s, 0x%lx)\n",
			  pp(obj), pp(d), (ulong) xref));

  for( r = *R; r != NULL; r = r->next)
    if ( r->object == obj && r->display == d )
    { r->xref = xref;
      succeed;
    }

  new = alloc(sizeof(struct xref));
  new->object = obj;
  new->display = d;
  new->xref = xref;
  new->next = *R;
  *R = new;

  succeed;
}


Xref
unregisterXrefObject(Any obj, DisplayObj d)
{ Xref *R = &XrefTable[HashValue(obj)];
  Xref r = *R;
  static struct xref old;

  for( ; r != NULL; R = &r->next, r = *R )
    if ( r->object == obj && (r->display == d || isDefault(d)) )
    { *R = r->next;

      DEBUG(NAME_xref, printf("unregisterXrefObject(%s, %s)\n",
			      pp(obj), pp(r->display)));
      old = *r;
      unalloc(sizeof(struct xref), r);
      return &old;
    }

  fail;
}


#if KEEP
static void
unregisterAllXrefsObject(Any obj)
{ Xref *R = &XrefTable[HashValue(obj)];
  Xref r = *R;
  WsRef old;

  for( ; r != NULL; R = &r->next, r = *R )
  { if ( r->object == obj )
    { *R = r->next;
      R = &r->next;

      DEBUG(NAME_xref, printf("unregisterXrefObject(%s, %s)\n",
			      pp(obj), pp(r->display)));
      old = r->xref;
      unalloc(sizeof(struct xref), r);

    } else
      R = &r->next;
  }
}
#endif

