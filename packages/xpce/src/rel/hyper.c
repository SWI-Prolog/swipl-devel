/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>


static status
initialiseHyper(Hyper h, Any from, Any to, Name fname, Name bname)
{ initialiseProgramObject(h);

  if ( isDefault(bname) )
    bname = fname;
  
  assign(h, from, from);
  assign(h, to, to);
  assign(h, forward_name, fname);
  assign(h, backward_name, bname);

  if ( send(from, NAME_attachHyper, h, to,   0) &&
       send(to,   NAME_attachHyper, h, from, 0) )
    succeed;

  fail;
}


static status
unlinkHyper(Hyper h)
{ if ( !onFlag(h->to, F_FREED|F_FREEING) )
    deleteHyperObject(h->to, h);
  if ( !onFlag(h->from, F_FREED|F_FREEING) )
    deleteHyperObject(h->from, h);

  succeed;
}


static status
unlinkFromHyper(Hyper h)
{ return freeObject(h);
}


static status
unlinkToHyper(Hyper h)
{ return freeObject(h);
}

		 /*******************************
		 *	    LOAD/SAVE		*
		 *******************************/

static status
SaveRelationHyper(Hyper h, FileObj f)
{ if ( isSavedObject(h->from) && isSavedObject(h->to) )
  { storeCharFile(f, 's');
    return storeObject(h, f);
  }

  succeed;
}


static status
loadHyper(Hyper h, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(h, fd, def));
  
  if ( restoreVersion >= 13 )
  { attachHyperObject(h->from, h, h->to);
    attachHyperObject(h->to, h, h->from);
  }

  succeed;
}


status
makeClassHyper(Class class)
{ sourceClass(class, makeClassHyper, __FILE__, "$Revision$");

  setLoadStoreFunctionClass(class, loadHyper, storeSlotsObject);

  localClass(class, NAME_from, NAME_client, "object", NAME_get,
	     "From side of hyper link");
  localClass(class, NAME_to, NAME_client, "object", NAME_get,
	     "To side of hyper link");
  localClass(class, NAME_forwardName, NAME_name, "name", NAME_both,
	     "Name as visible from <-from");
  localClass(class, NAME_backwardName, NAME_name, "name", NAME_both,
	     "Name as visible from <-to");

  termClass(class, "hyper",
	    4, NAME_from, NAME_to, NAME_forwardName, NAME_backwardName);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "from=object", "to=object", "forward=name", "backward=[name]",
	     "Create named link between objects",
	     initialiseHyper);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink hyper from objects",
	     unlinkHyper);
  sendMethod(class, NAME_SaveRelation, NAME_file, 1, "file",
	     "Consider saving relation (->save_in_file)",
	     SaveRelationHyper);
  sendMethod(class, NAME_unlinkFrom, NAME_internal, 0,
	     "<-from side is being unlinked",
	     unlinkFromHyper);
  sendMethod(class, NAME_unlinkTo, NAME_internal, 0,
	     "<-to side is being unlinked",
	     unlinkToHyper);

  succeed;
}
