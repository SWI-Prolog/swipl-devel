/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static Chain TheApplications;

static status
initialiseApplication(Application app, Name name)
{ assign(app, name,    name);
  assign(app, members, newObject(ClassChain, 0));
  assign(app, kind,    NAME_user);
/*assign(app, modal,   NIL);*/

  appendChain(TheApplications, app);

  succeed;
}


static status
unlinkApplication(Application app)
{ if ( notNil(app->members) )
  { FrameObj fr;

    for_chain(app->members, fr, send(fr, NAME_destroy, 0));
  }

  deleteChain(TheApplications, app);

  succeed;
}


static status
appendApplication(Application app, FrameObj fr)
{ if ( fr->application != app )
  { if ( notNil(fr->application) )
      send(fr->application, NAME_delete, fr, 0);

    assign(fr, application, app);
    appendChain(app->members, fr);
    if ( fr->modal == NAME_application )
      assign(app, modal, fr);
  }

  succeed;
}


static status
deleteApplication(Application app, FrameObj fr)
{ if ( onFlag(app, F_FREED|F_FREEING) )
    succeed;

  if ( fr->application == app )
  { deleteChain(app->members, fr);
    assign(fr, application, NIL);
    if ( app->modal == fr )
      app->modal = NIL;
    succeed;
  }

  fail;
}


static FrameObj
getMemberApplication(Application app, Name name)
{ Cell cell;

  for_cell(cell, app->members)
  { FrameObj fr = cell->value;

    if ( fr->name == name )
      answer(fr);
  }
  
  fail;
}


static Chain
getContainsApplication(Application app)
{ answer(app->members);
}


static status
resetApplication(Application app)
{ assign(app, modal, NIL);

  succeed;
}


void
resetApplications()
{ if ( TheApplications )
  { Application app;

    for_chain(TheApplications, app, send(app, NAME_reset, 0));
  }
}


static status
modalApplication(Application app, FrameObj fr)
{ if ( fr->application != app )
  { TRY(send(fr, NAME_application, app, 0));
  }

  assign(app, modal, fr);

  succeed;
}


/* Type declaractions */

/* Instance Variables */

static vardecl var_application[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_name, "Identifier name of the application"),
  IV(NAME_members, "chain", IV_GET,
     NAME_organisation, "Chain holding member frames"),
  IV(NAME_kind, "{user,service}", IV_BOTH,
     NAME_debugging, "If @off, events cannot be debugged"),
  SV(NAME_modal, "frame*", IV_GET, modalApplication,
     NAME_event, "Frame for modal operation")
};

/* Send Methods */

static senddecl send_application[] =
{ SM(NAME_initialise, 1, "name", initialiseApplication,
     DEFAULT, "Create named application"),
  SM(NAME_unlink, 0, NULL, unlinkApplication,
     DEFAULT, "->destroy frames and remove from @applications"),
  SM(NAME_append, 1, "frame", appendApplication,
     NAME_organisation, "Add frame to the application"),
  SM(NAME_delete, 1, "frame", deleteApplication,
     NAME_organisation, "Remove frame from the application"),
  SM(NAME_reset, 0, NULL, resetApplication,
     DEFAULT, "Reset <-modal to @nil")
};

/* Get Methods */

static getdecl get_application[] =
{ GM(NAME_member, 1, "frame", "name", getMemberApplication,
     NAME_organisation, "Member frame with given name"),
  GM(NAME_contains,  0, "chain", NULL, getContainsApplication,
     NAME_organisation, "Chain with frames I manage")
};

/* Resources */

#define rc_application NULL
/*
static resourcedecl rc_application[] =
{ 
};
*/

/* Class Declaration */

static Name application_termnames[] = { NAME_name };

ClassDecl(application_decls,
          var_application, send_application, get_application, rc_application,
          1, application_termnames,
          "$Rev$");

status
makeClassApplication(Class class)
{ declareClass(class, &application_decls);

  TheApplications = globalObject(NAME_applications, ClassChain, 0);

  succeed;
}
