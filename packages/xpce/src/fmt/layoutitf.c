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


status
initialiseLayoutInterface(Any obj, Graphical image)
{ LayoutInterface itf = obj;

  assign(itf,   image,            image);

  return qadSendv(image, NAME_layoutInterface, 1, &obj);
}


status
unlinkLayoutInterface(Any obj)
{ LayoutInterface itf = obj;

  if ( notNil(itf->image) && !isFreedObj(itf->image) )
  { Any nil = NIL;

    return qadSendv(itf->image, NAME_layoutInterface, 1, &nil);
  }

  succeed;
}


static LayoutInterface
getConvertLayoutInterface(Any context, Graphical image)
{ fail;
}


status
changedAreaLayoutInterface(LayoutInterface itf)
{ if ( notNil(itf->layout_manager) &&
       itf->layout_manager->request_compute != NAME_computing )
    return requestComputeLayoutManager(itf->layout_manager, DEFAULT);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

/* Instance Variables */

static vardecl var_layoutitf[] =
{ IV(NAME_layoutManager, "layout_manager*", IV_GET,
     NAME_organisation, "My manager"),
  IV(NAME_image, "graphical", IV_GET,
     NAME_appearance, "Image managed")
};
  
/* Send Methods */

static senddecl send_layoutitf[] =
{ SM(NAME_initialise, 1, "graphical", initialiseLayoutInterface,
     DEFAULT, "Initialise abstract instance"),
  SM(NAME_unlink, 0, NULL, unlinkLayoutInterface,
     DEFAULT, "Unlink from <-image")
};

/* Get Methods */

static getdecl get_layoutitf[] =
{ GM(NAME_convert, 1, "layout_interface", "graphical",
     getConvertLayoutInterface,
     DEFAULT, "Convert graphical object")
};

/* Resources */

#define rc_layoutitf NULL
/*
static classvardecl rc_layoutitf[] =
{
};
*/

/* Class Declaration */

static Name layoutitf_termnames[] = { NAME_image };

ClassDecl(layoutitf_decls,
          var_layoutitf, send_layoutitf, get_layoutitf, rc_layoutitf,
          1, layoutitf_termnames,
          "$Rev$");

status
makeClassLayoutInterface(Class class)
{ return declareClass(class, &layoutitf_decls);
}

