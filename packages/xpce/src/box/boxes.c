/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

#define GLOBALS_HERE 1
#include "boxes.h"

static struct class_definition boxdefs[] =
{ { NAME_rubber, NAME_object, makeClassRubber,
    &ClassRubber, "Ease to get bigger or smaller" },
  { NAME_hbox, NAME_object, makeClassHBox,
    &ClassHBox, "Typesetting: horizontal box" },
  { NAME_tbox, NAME_hbox, makeClassTBox,
    &ClassTBox, "Typesetting: horizontal box holding text" },
  { NAME_grbox, NAME_hbox, makeClassGrBox,
    &ClassGrBox, "Typesetting: horizontal box holding graphical" },
  { NAME_parbox, NAME_device, makeClassParBox,
    &ClassParBox, "Typesetting: paragraph" },
  { NAME_lbox, NAME_device, makeClassLBox,
    &ClassLBox, "Typesetting: list-box" },
  { NULL, NULL, NULL, NULL, NULL }
};


void
initBoxes()
{ defineClasses(boxdefs);
}
