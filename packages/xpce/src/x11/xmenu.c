/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/interface.h>
#include "include.h"

		 /*******************************
		 *	    SCROLLBAR		*
		 *******************************/

status
ws_draw_scrollbar_arrow(ScrollBar s,
			int x, int y, int w, int h,
			Name which, int up)
{ fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the height of the scrollbar image.   If  the width matches the
image width, we return the image height. Otherwise -1 to indicate to use
the generic method.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ws_arrow_height_scrollbar(ScrollBar s)
{ return -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Draw the scrollbar thumb. The colours are   hard to find, but we'll give
it a try ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_draw_sb_thumb(int x, int y, int w, int h)
{ fail;
}


		 /*******************************
		 *	      BUTTON		*
		 *******************************/

Any
ws_3d_grey()
{ static Colour c;

  if ( !c )
  { c = newObject(ClassColour, CtoKeyword("grey60"), 0);
  }

  return c;
}


status
ws_draw_button_face(Button b, int x, int y, int w, int h,
		    int up, int defb, int focus)
{ fail;
}

		 /*******************************
		 *	      TEXTITEM		*
		 *******************************/

int
ws_combo_box_width()
{ return -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ws_entry_field() is used by classes  that   need  to  create an editable
field of specified dimensions. If the field   happens to be not editable
now, this is indicated by `editable'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_entry_field(int x, int y, int w, int h, int flags)
{ fail;
}

		 /*******************************
		 *	       MENU		*
		 *******************************/

status
ws_draw_checkbox(int x, int y, int w, int h, int b, int flags)
{ fail;
}


status
ws_checkbox_size(int flags, int *w, int *h)
{ *w = 0;
  *h = 0;

  fail;
}

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

int
ws_message_box(Any msg, int flags)
{ return MBX_NOTHANDLED;
}
