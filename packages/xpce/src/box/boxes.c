/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
