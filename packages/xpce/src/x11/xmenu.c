/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>
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

Colour
ws_3d_grey()
{ static Colour c;

  if ( !c )
  { c = newObject(ClassColour, CtoKeyword("grey60"), EAV);
  }

  return c;
}


status
ws_draw_button_face(DialogItem di, int x, int y, int w, int h,
		    int up, int defb, int focus)
{ fail;
}

		 /*******************************
		 *	      TEXTITEM		*
		 *******************************/

static Elevation noedit_elevation;
static Elevation edit_elevation;
static Elevation button_elevation;


static void
init_entry_resources()
{ static int done = FALSE;

  if ( !done )
  { done = TRUE;

    noedit_elevation = globalObject(NIL, ClassElevation, NIL,
				    toInt(-1), EAV);
    edit_elevation   = globalObject(NIL, ClassElevation, NIL,
				    toInt(-1), WHITE_COLOUR, EAV);
    button_elevation = getClassVariableValueClass(ClassButton,
						  NAME_elevation);
  }
}


int
ws_combo_box_width()
{ return 14;
}


int
ws_stepper_width()
{ return ws_combo_box_width();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ws_entry_field() is used by classes  that   need  to  create an editable
field of specified dimensions. If the field   happens to be not editable
now, this is indicated by `editable'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_entry_field(int x, int y, int w, int h, int flags)
{  init_entry_resources();

  if ( !(flags & TEXTFIELD_EDITABLE) )
  { r_3d_box(x, y, w, h, 0, noedit_elevation, TRUE);
  } else
  { r_3d_box(x, y, w, h, 0, edit_elevation, TRUE);

    if ( flags & TEXTFIELD_COMBO )
    { int iw = valInt(SCROLL_DOWN_IMAGE->size->w);
      int ih = valInt(SCROLL_DOWN_IMAGE->size->h);
      int iy = y+2 + (h-4-valInt(SCROLL_DOWN_IMAGE->size->h))/2;
      int cw = ws_combo_box_width();
      int up = !(flags & TEXTFIELD_COMBO_DOWN);

      r_3d_box(x+w-cw-2, y+2, cw, h-4, 0, button_elevation, up);
      r_image(SCROLL_DOWN_IMAGE, 0, 0, x+w-cw+(cw-iw)/2-2, iy, iw, ih, ON);
    }
    if ( flags & TEXTFIELD_STEPPER )
    { int cw = ws_stepper_width();
      int bh = (h-4)/2;
      int b1up, b2up;

      b1up = !(flags & TEXTFIELD_INCREMENT);
      b2up = !(flags & TEXTFIELD_DECREMENT);

      r_3d_box(x+w-cw-2, y+2,    cw, bh, 0, button_elevation, b1up);
      r_3d_box(x+w-cw-2, y+2+bh, cw, bh, 0, button_elevation, b2up);

      { int iw = valInt(INT_ITEM_IMAGE->size->w)/2;
	int ih = valInt(INT_ITEM_IMAGE->size->h);
	int ix = x+w-2-(cw+iw)/2;
	int dy = (bh-ih+1)/2;

	r_image(INT_ITEM_IMAGE, 0,  0, ix, y+2+dy,      iw, ih, ON);
	r_image(INT_ITEM_IMAGE, iw, 0, ix, y+h-2-dy-ih, iw, ih, ON);
      }
    }
  }

  succeed;
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
{ Name label, reply;
  Any d, l, i = NIL;
  FrameObj fr;

  if ( instanceOfObject(EVENT->value, ClassEvent) )
    fr = getFrameWindow(((EventObj)EVENT->value)->window, OFF);
  else
    fr = NULL;

  if ( flags & MBX_INFORM )
  { label = NAME_information;
  } else if ( flags & MBX_CONFIRM )
  { label = NAME_confirm;
  } else if ( flags & MBX_ERROR )
  { label = NAME_error;
  } else
    return MBX_NOTHANDLED;

  d = answerObject(ClassDialog, getLabelNameCharArray((CharArray)label), EAV);
  if ( fr )
  { send(d, NAME_transientFor, fr, EAV);

    if ( notNil(fr->application) )
      send(d, NAME_modal, NAME_application, EAV);
    else
      send(d, NAME_modal, NAME_transient, EAV);
  } else
    send(d, NAME_kind, NAME_transient, EAV);

  if ( flags & MBX_ERROR )
    send(d, NAME_append,
	 i=newObject(ClassLabel, NAME_icon, EXCLAMATION_IMAGE, EAV), EAV);
  send(d, NAME_append,
       l=newObject(ClassLabel, NAME_label, msg, EAV), NAME_right, EAV);
  send(l, NAME_length, ZERO, EAV);
  if ( flags & MBX_ERROR )
  { send(l, NAME_reference, newObject(ClassPoint, EAV), EAV);
    send(i, NAME_reference, newObject(ClassPoint, EAV), EAV);
  }
  send(d, NAME_append,
       newObject(ClassGraphical, ZERO, ZERO, ONE, ONE, EAV), EAV);
  send(d, NAME_append,
       newObject(ClassButton, NAME_ok,
		 newObject(ClassMessage, d, NAME_return,
			   NAME_ok, EAV), EAV), EAV);
  if ( flags & MBX_CONFIRM )
    send(d, NAME_append,
	 newObject(ClassButton, NAME_cancel,
		   newObject(ClassMessage, d, NAME_return,
			     NAME_cancel, EAV), EAV), EAV);

  if ( fr )
    reply = get(d, NAME_confirmCentered,
		get(fr->area, NAME_center, EAV),
		EAV);
  else
    reply = get(d, NAME_confirmCentered, EAV);

  send(d, NAME_destroy, EAV);

  if ( reply == NAME_ok )
    return MBX_OK;
  else
    return MBX_CANCEL;
}
