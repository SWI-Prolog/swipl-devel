/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>


status
initialiseLayoutInterface(Any obj, Graphical image)
{ LayoutInterface itf = obj;

  assign(itf,   image,            image);
  assign(image, layout_interface, itf);

  succeed;
}


status
unlinkLayoutInterface(Any obj)
{ LayoutInterface itf = obj;

  if ( notNil(itf->image) && !isFreedObj(itf->image) )
    assign(itf->image, layout_interface, NIL);

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
static resourcedecl rc_layoutitf[] =
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

