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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD:	handle destruction of these objects.  Not that important as they
	are generally not destroyed and only a bit of memory is wasted
	if they are.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define XREF_TABLESIZE 		256
#define HashValue(obj)		(((unsigned long)(obj) & (XREF_TABLESIZE-1)))

static Xref XrefTable[XREF_TABLESIZE];


WsRef
getXrefObject(Any obj, DisplayObj d)
{ int v = HashValue(obj);
  Xref r;

  XrefsResolved++;

  for( r = XrefTable[v]; r != NULL; r = r->next)
    if ( r->object == obj && r->display == d )
    { DEBUG(NAME_getXref, Cprintf("getXrefObject(%s, %s) --> 0x%lx\n",
				  pp(obj), pp(d), (unsigned long) r->xref));
      return r->xref;
    }

  if ( openDisplay(d) == SUCCEED )
  { if ( send(obj, NAME_Xopen, d, EAV) == SUCCEED )
    { for( r = XrefTable[v]; r != NULL; r = r->next)
	if ( r->object == obj && r->display == d )
	{ DEBUG(NAME_getXref, Cprintf("getXrefObject(%s, %s) --> 0x%lx\n",
				      pp(obj), pp(d), (unsigned long) r->xref));
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

  DEBUG(NAME_xref, Cprintf("registerXrefObject(%s, %s, 0x%lx)\n",
			   pp(obj), pp(d), (unsigned long) xref));

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
  { if ( r->object == obj && (r->display == d || isDefault(d)) )
    { *R = r->next;

      DEBUG(NAME_xref, Cprintf("unregisterXrefObject(%s, %s)\n",
			       pp(obj), pp(r->display)));
      old = *r;
      unalloc(sizeof(struct xref), r);
      return &old;
    }
  }

  fail;
}


void
closeAllXrefs()
{ int i;  

  for(i=0; i<XREF_TABLESIZE; i++)
  { Xref r = XrefTable[i];
    Xref nr;

    for(; r; r = nr)
    { nr = r->next;

      send(r->object, NAME_Xclose, r->display, EAV);
    }
  }
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

      DEBUG(NAME_xref, Cprintf("unregisterXrefObject(%s, %s)\n",
			       pp(obj), pp(r->display)));
      old = r->xref;
      unalloc(sizeof(struct xref), r);

    } else
      R = &r->next;
  }
}
#endif

