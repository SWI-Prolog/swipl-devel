/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

#ifndef NEW_TEXT_H_INCLUDED
#define NEW_TEXT_H_INCLUDED

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

typedef struct hbox	*HBox;
typedef struct tbox	*TBox;
typedef struct grbox	*GrBox;
typedef struct parbox	*ParBox;
typedef struct rubber	*Rubber;
typedef struct lbox     *LBox;

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
  Any		baseline;		/* Location of the baseline */
  Any		alignment;		/* left, right (in paragraph) */
End;


NewClass(rubber)
  Int		stretch;		/* Get bigger */
  Int		shrink;			/* Get smaller */
  Int		level;			/* hfil/hfill/hfilll (1/2/3) */
  Name		linebreak;		/* @nil, allow, force */
End;


NewClass(parbox)
  ABSTRACT_DEVICE			/* graphical device */
  Int		line_width;		/* Max width of a line */
  Chain		content;		/* Contained hboxes */
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


#endif /*NEW_TEXT_H_INCLUDED*/
