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

#ifndef BOXES_H_INCLUDED
#define BOXES_H_INCLUDED

typedef struct hbox	*HBox;
typedef struct tbox	*TBox;
typedef struct grbox	*GrBox;
typedef struct parbox	*ParBox;
typedef struct rubber	*Rubber;
typedef struct lbox     *LBox;

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

#include "proto.h"

#ifdef GLOBALS_HERE
#undef GLOBAL
#define GLOBAL 
#endif

GLOBAL Class ClassHBox;
GLOBAL Class ClassRubber;
GLOBAL Class ClassTBox;
GLOBAL Class ClassGrBox;
GLOBAL Class ClassParBox;
GLOBAL Class ClassLBox;

#define ABSTRACT_HBOX \
  Int		width;			/* total width */ \
  Int		ascent;			/* height above baseline */ \
  Int		descent;		/* depth below baseline */ \
  Rubber	rubber;			/* h/v stretchability */


NewClass(hbox)
  ABSTRACT_HBOX
End;


NewClass(tbox)
  ABSTRACT_HBOX
  CharArray	text;			/* represented text */
  Style		style;			/* used style parameters */
End;


NewClass(grbox)
  ABSTRACT_HBOX
  Graphical	graphical;		/* Held in-line graphical */
  Any		alignment;		/* top, bottom, center, left, right */
End;


NewClass(rubber)
  Int		stretch;		/* Get bigger */
  Int		shrink;			/* Get smaller */
  Int		level;			/* hfil/hfill/hfilll (1/2/3) */
  Int		natural;		/* Natural desired size */
  Int		minimum;		/* Minimum size */
  Int		maximum;		/* Maximum size */
  Name		linebreak;		/* @nil, allow, force */
End;


NewClass(parbox)
  ABSTRACT_DEVICE			/* graphical device */
  Int		line_width;		/* Max width of a line */
  Vector	content;		/* Contained hboxes */
  Name		alignment;		/* left,right,center,justify */
  Bool		auto_crop;		/* Crop content */
End;


NewClass(lbox)
  ABSTRACT_DEVICE
  Int		left_margin;		/* width of left margin */
  Int		right_margin;		/* with of right margin */
  Int		top_sep;		/* separation above  */
  Int		item_sep;		/* separation between items */
  Int		label_sep;		/* label to item distance */
  Int		label_width;		/* Width of label box */
End;


#endif /*BOXES_H_INCLUDED*/
