/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

		 /*******************************
		 *	    SCROLLBAR		*
		 *******************************/

typedef struct 
{ Name		which;			/* up, left, down, right */
  int		status;			/* up/down */
  char *	imgname;		/* name of the image */
  Image		img;			/* associated image */
} sb_image_def;


static sb_image_def sbimages[] =
{ { NAME_up,	TRUE,	"win_uparrow",  NULL },
  { NAME_down,	TRUE,	"win_dnarrow",  NULL },
  { NAME_left,	TRUE,	"win_lfarrow",  NULL },
  { NAME_right,	TRUE,	"win_rgarrow",  NULL },
  { NAME_up,	FALSE,	"win_uparrowd", NULL },
  { NAME_down,	FALSE,	"win_dnarrowd", NULL },
  { NAME_left,	FALSE,	"win_lfarrowd", NULL },
  { NAME_right,	FALSE,	"win_rgarrowd", NULL },
  { NULL,	FALSE,  NULL,		NULL }
};


static Image
sb_image(Name which, int up)
{ sb_image_def *sd;

  for(sd = sbimages; sd->which; sd++)
  { if ( sd->which == which && up == sd->status )
    { if ( !sd->img )
	sd->img = newObject(ClassImage, CtoKeyword(sd->imgname), 0);

      return sd->img;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Draw the arrow of a scrollbar. Windows   defines images for this, and we
will use these, unless the size doesn't  match (a scrollbar created with
non-default width. If this function return   failure,  the general thing
will be called.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_draw_scrollbar_arrow(ScrollBar s,
			int x, int y, int w, int h,
			Name which, int up)
{ if ( s->look == NAME_win )
  { Image img = sb_image(which, up ? TRUE : FALSE);

    if ( img && valInt(img->size->w) == w && valInt(img->size->h) == w )
    { r_image(img, 0, 0, x, y, w, h, OFF);
      succeed;
    }
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the height of the scrollbar image.   If  the width matches the
image width, we return the image height. Otherwise -1 to indicate to use
the generic method.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ws_arrow_height_scrollbar(ScrollBar s)
{ if ( s->orientation == NAME_vertical )
  { Image img = sb_image(NAME_up, TRUE);

    if ( img && img->size->w == s->area->w )
      return valInt(img->size->h);
  } else
  { Image img = sb_image(NAME_left, TRUE);

    if ( img && img->size->h == s->area->h )
      return valInt(img->size->w);
  }

  return -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Draw the scrollbar thumb. The colours are   hard to find, but we'll give
it a try ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static COLORREF thumb_colours[5];

static void
init_sb_thumb_colours()
{ static int done = FALSE;

  if ( !done )
  { Image img = sb_image(NAME_up, TRUE);
    int w = valInt(img->size->w);
    int h = valInt(img->size->h);

    d_image(img, 0, 0, w, h);
    thumb_colours[0] = r_get_pixel(0,0); /* top-colour */
    thumb_colours[1] = r_get_pixel(1,1);
    thumb_colours[2] = r_get_pixel(2,2);
    thumb_colours[3] = r_get_pixel(w-2,h-2);
    thumb_colours[4] = r_get_pixel(w-1,h-1);
    d_done();
    done = TRUE;

    DEBUG(NAME_scrollBar,
	  Cprintf("Thumb colours: 0x%06x, 0x%06x, 0x%06x, 0x%06x, 0x%06x\n",
		  thumb_colours[0],
		  thumb_colours[1],
		  thumb_colours[2],
		  thumb_colours[3],
		  thumb_colours[4]));
  }
}


status
ws_draw_sb_thumb(int x, int y, int w, int h)
{ init_sb_thumb_colours();

  r_3d_rectangle(x, y, w, h, 2, thumb_colours);

  succeed;
}


		 /*******************************
		 *	      BUTTON		*
		 *******************************/

static COLORREF button_colours[5];
static Colour   button_face;

static void
init_button_colours()
{ int done = FALSE;

  if ( !done )
  { init_sb_thumb_colours();
    
    button_colours[1] = thumb_colours[0]; /* just look at it !? */
    button_colours[0] = thumb_colours[1];
    button_colours[2] = thumb_colours[2];
    button_colours[3] = thumb_colours[3];
    button_colours[4] = thumb_colours[4];

    button_face = newObject(ClassColour, CtoKeyword("win_btnface"), 0);

    done = TRUE;
  }
}


COLORREF
ws_3d_grey_rgb()
{ init_sb_thumb_colours();

  return thumb_colours[3];
}


Colour
ws_3d_grey()
{ static Colour c = NULL;

  if ( !c )
    c = newObject(ClassColour, CtoKeyword("_win_3d_grey"), 0);

  return c;
}


status
ws_draw_button_face(Button b, int x, int y, int w, int h,
		    int up, int defb, int focus)
{ if ( b->look != NAME_win )
    fail;

  init_button_colours();
  r_thickness(1);
    
  if ( defb )
    r_box(x-1, y-1, w+2, h+2, 0, NIL);

  if ( up )
    r_3d_rectangle(x, y, w, h, 2, button_colours);
  else
    r_box(x, y, w, h, 0, button_face);

  if ( focus )
  { r_dash(NAME_dotted);
    r_box(x+3, y+3, w-6, h-6, 0, NIL);
  }

  succeed;
}

		 /*******************************
		 *	      TEXTITEM		*
		 *******************************/

static Elevation elevation1;		/* Generic hight-1 elevation */
static Colour    WinBackground;		/* Generic window background */
static COLORREF  edit_colours[5];	/* elevation for entry-field */
static Image	 WinCombo;		/* @win_combo image */
static COLORREF  down_colours[3];	/* elevation for down-button */

static void
init_entry_resources()
{ static int done = FALSE;

  if ( !done )
  { init_button_colours();
    elevation1    = globalObject(NIL, ClassElevation, ONE, 0);
    WinBackground = newObject(ClassColour, CtoKeyword("win_window"), 0);
    WinCombo      = newObject(ClassImage, CtoKeyword("win_combo"), 0);

    edit_colours[0] = thumb_colours[3];
    edit_colours[1] = thumb_colours[4];
    edit_colours[2] = cref_colour(WinBackground);
    edit_colours[3] = thumb_colours[0];
    edit_colours[4] = thumb_colours[1];

    down_colours[0] = thumb_colours[3];
    down_colours[1] = thumb_colours[2];
    down_colours[2] = thumb_colours[3];
  }
}


int
ws_combo_box_width()
{ init_entry_resources();

  return valInt(WinCombo->size->w) + 4;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ws_entry_field() is used by classes  that   need  to  create an editable
field of specified dimensions. If the field   happens to be not editable
now, this is indicated by `editable'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_entry_field(int x, int y, int w, int h, int flags)
{ init_entry_resources();

  if ( !(flags & TEXTFIELD_EDITABLE) )
    r_3d_box(x, y, w, h, 0, elevation1, FALSE);
  else
  { r_3d_rectangle(x, y, w, h, 2, edit_colours);

    if ( flags & TEXTFIELD_COMBO )
    { int iw = valInt(WinCombo->size->w);
      int ih = valInt(WinCombo->size->h);
      int iy = y+2 + (h-4-valInt(WinCombo->size->h))/2;
      int cw = iw + 4;

      if ( flags & TEXTFIELD_COMBO_DOWN )
	r_3d_rectangle(x+w-cw-2, y+2, cw, h-4, 1, down_colours);
      else
	r_3d_rectangle(x+w-cw-2, y+2, cw, h-4, 2, thumb_colours);

      r_image(WinCombo, 0, 0, x+w-cw, iy, iw, ih, OFF);
    }
  }

  succeed;
}

		 /*******************************
		 *	       MENU		*
		 *******************************/

typedef struct
{ unsigned char x;			/* X-index in image */
  unsigned char y;			/* Y-index in image */
} checkbox_location;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: Update this table if the CHECKBOX_* constant, change!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

					/* multiple, active, selected  */
static checkbox_location checkboxes[] =
{ { 2, 1 },				/* 000 */
  { 3, 1 },				/* 001 */
  { 0, 1 },				/* 010 */
  { 1, 1 },				/* 011 */
  { 2, 0 },				/* 100 */
  { 3, 0 },				/* 101 */
  { 0, 0 },				/* 110 */
  { 1, 0 }				/* 111 */
};

static Image CheckBoxes;		/* win_checkboxes */
static Image BtnCorners;		/* win_btncorners */

#define USE_MASKBLT			/* for now, but may nor work on 3.x */

static void
init_checkboxes()
{ static int done = FALSE;

  if ( !done )
  { Image img;
    BitmapObj bm;

    done = TRUE;

    CheckBoxes = newObject(ClassImage, CtoKeyword("win_checkboxes"), 0);
    img = newObject(ClassImage, CtoKeyword("win_btncorners"), 0);
    bm = answerObject(ClassBitmap, img, 0);
    BtnCorners = globalObject(CtoName("_checkbox_mask"), ClassImage, NIL,
			      img->size->w, img->size->h, 0);
    send(BtnCorners, NAME_drawIn, bm, 0);
#ifdef USE_MASKBLT
    send(BtnCorners, NAME_invert, 0);
#endif
    doneObject(bm);
  }
}


status
ws_draw_checkbox(int x, int y, int w, int h, int b, int flags)
{ init_checkboxes();

  if ( CheckBoxes )
  { int sx = checkboxes[flags & 0x7].x;
    int sy = checkboxes[flags & 0x7].y;
    int iw = valInt(CheckBoxes->size->w)/4;
    int ih = valInt(CheckBoxes->size->h)/3;
    int by = y + (h-ih)/2;

    sx *= iw;
    sy *= ih;

    if ( !(flags & CHECKBOX_MULTIPLE) )
    { HDC   hdc   = d_current_hdc();
      HDC bmhdc   = CreateCompatibleDC(hdc);
      HBITMAP msk = (HBITMAP) getXrefObject(BtnCorners, CurrentDisplay(NIL));
      HBITMAP bm  = (HBITMAP) getXrefObject(CheckBoxes, CurrentDisplay(NIL));
      HBITMAP obm;

#ifdef USE_MASKBLT
      obm = ZSelectObject(bmhdc, bm);
      MaskBlt(hdc,			/* destination */
	      x+b, by, iw, ih,		/* destination rectangle */
	      bmhdc,			/* source */
	      sx, sy,			/* source rectangle */
	      msk,			/* mask */
	      0, 0,			/* mask rectangle */
	      MAKEROP4(SRCCOPY, SRCAND)); /* operations */
#else
      obm = ZSelectObject(bmhdc, msk);
      BitBlt(hdc, x+b, by, iw, ih, bmhdc, 0, 0, SRCAND);
      ZSelectObject(bmhdc, bm);
      BitBlt(hdc, x+b, by, iw, ih, bmhdc, sx, sy, SRCPAINT);
#endif

      ZSelectObject(bmhdc, obm);
      DeleteDC(bmhdc);
    } else
    { r_image(CheckBoxes, sx, sy, x+b, by, iw, ih, OFF);
    }

    succeed;
  }

  fail;
}


status
ws_checkbox_size(int flags, int *w, int *h)
{ init_checkboxes();

  if ( CheckBoxes )
  { *w = valInt(CheckBoxes->size->w)/4;
    *h = valInt(CheckBoxes->size->h)/3;

    succeed;
  }

  fail;
}

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

int
ws_message_box(Any msg, int flags)
{ Name n = msg;
  char *s = strName(n);
  UINT style = MB_TASKMODAL;
  char *title;

  if ( flags & MBX_INFORM )
  { style |= MB_OK;
    title = "Information";
  } else if ( flags & MBX_CONFIRM )
  { style |= MB_OKCANCEL;
    title = "Confirm";
  } else if ( flags & MBX_ERROR )
  { style |= MB_OKCANCEL|MB_ICONEXCLAMATION;
    title = "Error";
  } else
    return MBX_NOTHANDLED;

  switch(MessageBox(NULL, s, title, style))
  { case IDOK:
      return MBX_OK;
    default:
      return MBX_CANCEL;
  }
}
