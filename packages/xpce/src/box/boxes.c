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
