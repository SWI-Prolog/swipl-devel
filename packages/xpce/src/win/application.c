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

static Chain TheApplications;

static status
initialiseApplication(Application app, Name name)
{ assign(app, name,    name);
  assign(app, members, newObject(ClassChain, EAV));
  assign(app, kind,    NAME_user);
  assign(app, modal,   newObject(ClassChain, EAV));

  appendChain(TheApplications, app);

  succeed;
}


static status
unlinkApplication(Application app)
{ if ( notNil(app->members) )
  { FrameObj fr;

    for_chain(app->members, fr, send(fr, NAME_destroy, EAV));
  }

  deleteChain(TheApplications, app);

  succeed;
}


static status
appendApplication(Application app, FrameObj fr)
{ if ( fr->application != app )
  { if ( notNil(fr->application) )
      send(fr->application, NAME_delete, fr, EAV);

    assign(fr, application, app);
    appendChain(app->members, fr);
    if ( fr->modal == NAME_application )
      send(app, NAME_modal, fr, EAV);
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
    deleteChain(app->modal, fr);
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

    for_chain(TheApplications, app, send(app, NAME_reset, EAV));
  }
}


static status
modalApplication(Application app, FrameObj fr)
{ if ( notNil(fr) )
  { if ( fr->application != app )
    { TRY(send(fr, NAME_application, app, EAV));
    }

    prependChain(app->modal, fr);
  }

  succeed;
}


static FrameObj
getModalApplication(Application app)
{ if ( instanceOfObject(app->modal, ClassChain) )
    answer(getHeadChain(app->modal));

  fail;
}


/* Type declaractions */

/* Instance Variables */

static vardecl var_application[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_name, "Identifier name of the application"),
  IV(NAME_members, "chain", IV_GET,
     NAME_organisation, "Chain holding member frames"),
  IV(NAME_kind, "{user,service}", IV_BOTH,
     NAME_debugging, "If service, events cannot be debugged"),
  IV(NAME_modal, "chain", IV_NONE,
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
     DEFAULT, "Reset <-modal to @nil"),
  SM(NAME_modal, 1, "frame", modalApplication,
     NAME_event, "Make frame modal to application")
};

/* Get Methods */

static getdecl get_application[] =
{ GM(NAME_member, 1, "frame", "name", getMemberApplication,
     NAME_organisation, "Member frame with given name"),
  GM(NAME_contains,  0, "chain", NULL, getContainsApplication,
     NAME_organisation, "Chain with frames I manage"),
  GM(NAME_modal,  0, "frame", NULL, getModalApplication,
     NAME_event, "Frame that is modal to the application")
};

/* Resources */

#define rc_application NULL
/*
static classvardecl rc_application[] =
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

  TheApplications = globalObject(NAME_applications, ClassChain, EAV);

  succeed;
}
