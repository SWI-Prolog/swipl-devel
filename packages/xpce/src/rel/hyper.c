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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "from=object", "to=object", "forward=name", "backward=[name]" };

/* Instance Variables */

static vardecl var_hyper[] =
{ IV(NAME_from, "object", IV_GET,
     NAME_client, "From side of hyper link"),
  IV(NAME_to, "object", IV_GET,
     NAME_client, "To side of hyper link"),
  IV(NAME_forwardName, "name", IV_BOTH,
     NAME_name, "Name as visible from <-from"),
  IV(NAME_backwardName, "name", IV_BOTH,
     NAME_name, "Name as visible from <-to")
};

/* Send Methods */

static senddecl send_hyper[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseHyper,
     DEFAULT, "Create named link between objects"),
  SM(NAME_unlink, 0, NULL, unlinkHyper,
     DEFAULT, "Unlink hyper from objects"),
  SM(NAME_SaveRelation, 1, "file", SaveRelationHyper,
     NAME_file, "Consider saving relation (->save_in_file)"),
  SM(NAME_unlinkFrom, 0, NULL, unlinkFromHyper,
     NAME_internal, "<-from side is being unlinked"),
  SM(NAME_unlinkTo, 0, NULL, unlinkToHyper,
     NAME_internal, "<-to side is being unlinked")
};

/* Get Methods */

#define get_hyper NULL
/*
static getdecl get_hyper[] =
{ 
};
*/

/* Resources */

#define rc_hyper NULL
/*
static resourcedecl rc_hyper[] =
{ 
};
*/

/* Class Declaration */

static Name hyper_termnames[] = { NAME_from, NAME_to, NAME_forwardName, NAME_backwardName };

ClassDecl(hyper_decls,
          var_hyper, send_hyper, get_hyper, rc_hyper,
          4, hyper_termnames,
          "$Rev$");

status
makeClassHyper(Class class)
{ declareClass(class, &hyper_decls);
  setLoadStoreFunctionClass(class, loadHyper, storeSlotsObject);

  succeed;
}
