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
#include "include.h"

#define X11LastEventTime() ((Time)LastEventTime())


static XrmOptionDescRec opTable[] =
{ {"-xrm",	NULL,	XrmoptionResArg, NULL }
};


void
ws_flush_display(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XFlush(r->display_xref);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Seems this can get into a loop. Why? For   now  we will jump out after a
while. Seems processing 1000 messages is   more  then enough to fullfill
the aim of this function.

Maybe  we  should  somehow   check   the    state   of   the  connecting
file-descriptor to see whether the server is lost?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_synchronise_display(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;
  static int retry=0;
  int i;

  XFlush(r->display_xref);
  XSync(r->display_xref, False);

  
  for(i=1000; (XtAppPending(pceXtAppContext(NULL)) & XtIMAll) && --i > 0; )
    XtAppProcessEvent(pceXtAppContext(NULL), XtIMAll);

  if ( i == 0 )
  { Cprintf("ws_synchronise_display(): looping??\n");

    if ( ++retry == 10 )
    { Cprintf("Trouble, trying to abort\n");
      hostAction(HOST_ABORT);
    } else if ( retry == 20 )
    { Cprintf("Serious trouble, calling exit()\n");
      exit(1);
    }
  } else
    retry = 0;				/* seems to work again */
}


void
ws_bell_display(DisplayObj d, int volume)
{ DisplayWsXref r = d->ws_ref;

  XBell(r->display_xref, volume);
}


void
ws_get_size_display(DisplayObj d, int *w, int *h)
{ DisplayWsXref r = d->ws_ref;
  int screen;

  screen = XDefaultScreen(r->display_xref);
  *w = XDisplayWidth(r->display_xref,  screen);
  *h = XDisplayHeight(r->display_xref, screen);
}


Name
ws_get_visual_type_display(DisplayObj d)
{ if ( ws_depth_display(d) == 1 )
    return NAME_monochrome;
  else
  { DisplayWsXref r = d->ws_ref;
    Visual *v = XDefaultVisual(r->display_xref,
			       DefaultScreen(r->display_xref));
    int vclass = v->class;
  
    switch(vclass)
    { case StaticGray:	return NAME_staticGrey;
      case GrayScale:	return NAME_greyScale;
      case StaticColor:	return NAME_staticColour;
      case PseudoColor:	return NAME_pseudoColour;
      case TrueColor:	return NAME_trueColour;
      case DirectColor:	return NAME_directColour;
      default:		return (Name) toInt(vclass);
    }
  }
}


int
ws_depth_display(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  return r->depth;
}


int
ws_resolution_display(DisplayObj d, int *rx, int *ry)
{ Cprintf("No support to get the screen-resolution for X11 yet\n");

  fail;
}


void
ws_activate_screen_saver(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XForceScreenSaver(r->display_xref, ScreenSaverActive);
}


void
ws_deactivate_screen_saver(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XForceScreenSaver(r->display_xref, ScreenSaverReset);
}


void
ws_init_display(DisplayObj d)
{ DisplayWsXref ref = alloc(sizeof(display_ws_ref));

  memset(ref, 0, sizeof(*ref));
  ref->depth	        = 1;
  ref->black_pixel      = 1L;
  ref->foreground_pixel = ref->black_pixel;
  ref->background_pixel = ref->white_pixel;

  d->ws_ref = ref;
}


status
ws_legal_display_name(char *s)
{ char host[LINESIZE];
  int display, screen;

  if ( sscanf(s, "%[a-zA-Z0-9.]:%d.%d", host, &display, &screen) >= 2 )
    succeed;

  fail;
}


status
ws_opened_display(DisplayObj d)
{ return display_x11_ref(d, display_xref) == NULL ? FAIL : SUCCEED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We'd like to keep the XPCE resource  file `Pce' in XPCE's homedirectory,
so we can have multiple versions of this   file. For this reason we push
$XAPPLRESDIR.

I've diagnosed that actually, the primary path searched is not the first
element of this path, but the  last!?  If   anyone  can  point me to the
proper  semantics  of  $XAPPLRESDIR.   grep  through  /usr/man/man3/X*.3
doesn't give any hints ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_open_display(DisplayObj d)
{ DisplayWsXref ref = d->ws_ref;
  char *address;
  Display *display;

  address = isDefault(d->address) ? NULL : strName(d->address);
  display = XtOpenDisplay(pceXtAppContext(NULL),
			  address, "xpce",
			  "Pce",	/* resource class (not used) */
			  opTable, XtNumber(opTable),
			  &PCEargc, PCEargv);

  if ( !display )
  { char problem[LINESIZE];
    char *theaddress = XDisplayName(address);

    if ( isDefault(d->address) && !getenv("DISPLAY") )
      sprintf(problem, "no DISPLAY environment variable");
    else if ( !ws_legal_display_name(theaddress) )
      sprintf(problem, "malformed address: %s", theaddress);
    else
      strcpy(problem, "No permission to contact X-server?");

    errorPce(d, NAME_noXServer,
	     CtoName(theaddress), CtoString(problem), 0);
    return;
  } else
  { int screen = DefaultScreen(display);

    ref->display_xref = display;
    ref->colour_map   = DefaultColormap(display, screen);
    ref->white_pixel  = WhitePixel(display, screen);
    ref->black_pixel  = BlackPixel(display, screen);
    ref->depth        = DefaultDepth(display, screen);
  }
  
  { Arg args[5];
    Cardinal n = 0;

    XtSetArg(args[n], XtNmappedWhenManaged, False); n++;
    XtSetArg(args[n], XtNwidth,      	    64);    n++;
    XtSetArg(args[n], XtNheight,      	    64);    n++;

    ref->shell_xref = XtAppCreateShell("xpce",
				       "Pce", /* Resource Class */
				       applicationShellWidgetClass,
				       display,
				       args, n);
  }

  if ( !ref->shell_xref )
  { errorPce(d, NAME_noMainWindow);
    return;
  }

  XtRealizeWidget(ref->shell_xref);	/* Need a window for GC */

  ref->root_bitmap	= XCreatePixmap(display, XtWindow(ref->shell_xref),
					8, 4, 1);
}


void
ws_quit_display(DisplayObj d)
{ Cprintf("Cannot close display yet\n");
}


static DrawContext
new_draw_context(DisplayObj d, Drawable drawable, Name kind)
{ DrawContext ctx = alloc(sizeof(struct draw_context));
  DisplayWsXref r = d->ws_ref;
  Display *display = r->display_xref;
  XGCValues values;
  unsigned long black, white;
  Name vclass = ws_get_visual_type_display(d);

# define GCALL (GCFunction|GCForeground|GCBackground|GCGraphicsExposures)

  ctx->kind = kind;

  if ( kind == NAME_bitmap )
  { values.foreground = 1;
    values.background = 0;
    black = 1;
    white = 0;
    ctx->depth = 1;
  } else
  { values.foreground = r->foreground_pixel;
    values.background = r->background_pixel;
    black = r->black_pixel;
    white = r->white_pixel;
    ctx->depth = r->depth;
  }

  values.graphics_exposures = False;

  values.function   = GXinvert;			/* On true- and direct-color */
  if ( vclass == NAME_trueColour ||		/* displays, invert value */
       vclass == NAME_directColour )		/* On mapped displays */
    values.plane_mask = ~0L;			/* 0 and 1 are normally  */
  else						/* blank and white */
    values.plane_mask = 1;
  ctx->complementGC = XCreateGC(display, drawable, GCALL|GCPlaneMask, &values);

  values.function   = GXcopy;
  values.fill_rule  = EvenOddRule;
  values.arc_mode   = ArcPieSlice;
  ctx->fillGC	    = XCreateGC(display, drawable,
				GCALL|GCFillRule|GCArcMode, &values);

  values.fill_style = FillOpaqueStippled;
  ctx->bitmapGC	    = XCreateGC(display, drawable,
				GCALL|GCFillStyle|GCFillRule, &values);

  values.function   = (black == 0L ? GXor : GXand);
  ctx->andGC	    = XCreateGC(display, drawable,
				GCALL|GCFillRule|GCArcMode, &values);

  values.function   = GXcopy;
  ctx->workGC	    = XCreateGC(display, drawable, GCALL, &values);
  ctx->copyGC	    = XCreateGC(display, drawable, GCALL, &values);
  ctx->opGC	    = XCreateGC(display, drawable, GCALL, &values);

  values.foreground = values.background;
  ctx->clearGC	    = XCreateGC(display, drawable, GCALL, &values);
  
  values.foreground = black;
  ctx->shadowGC	    = XCreateGC(display, drawable, GCALL, &values);
  values.foreground = white;
  ctx->reliefGC	    = XCreateGC(display, drawable, GCALL, &values);

#undef GCALL

  ctx->pen	        = -1;
  ctx->dash	        = NAME_none;
  ctx->fill	        = NIL;
  ctx->arcmode		= NAME_pieSlice;
  ctx->and_pattern      = NIL;
  ctx->font	        = NIL;
  ctx->font_info	= NULL;
  ctx->char_widths	= NULL;
  ctx->colour	        = NIL;
  ctx->background       = NIL;
  ctx->foreground_pixel = 0L;
  ctx->background_pixel = 0L;
  ctx->subwindow_mode   = OFF;
  ctx->invert_mode      = OFF;

  return ctx;
}


status
ws_init_graphics_display(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  if ( r->pixmap_context == NULL )
  { r->bitmap_context   = new_draw_context(d,
					   r->root_bitmap,
					   NAME_bitmap);
    r->pixmap_context   = new_draw_context(d,
					   XtWindow(r->shell_xref),
					   NAME_pixmap);

  }
  
  succeed;
}


void
ws_foreground_display(DisplayObj d, Colour c)
{ DisplayWsXref r = d->ws_ref;

  r->foreground_pixel = getPixelColour(c, d);
}


void
ws_background_display(DisplayObj d, Colour c)
{ DisplayWsXref r = d->ws_ref;

  r->background_pixel = getPixelColour(c, d);
}


void
ws_draw_in_display(DisplayObj d, Graphical gr, Bool invert, Bool subtoo)
{ d_screen(d);
  if ( invert == ON ) r_invert_mode(ON);
  if ( subtoo == ON ) r_subwindow_mode(ON);
  RedrawArea(gr, gr->area);
  r_invert_mode(OFF);
  r_subwindow_mode(OFF);
  d_done();
}


void
ws_grab_server(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XGrabServer(r->display_xref);
}


void
ws_ungrab_server(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XUngrabServer(r->display_xref);
}


Int
ws_display_connection_number(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  if ( r && r->display_xref )
    return toInt(ConnectionNumber(r->display_xref));
  else
    fail;
}


status
ws_events_queued_display(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  if ( r && r->display_xref )
  { XSync(r->display_xref, False);
    if ( XtAppPending(pceXtAppContext(NULL)) & XtIMAll )
      succeed;
  }

  fail;
}

		 /*******************************
		 *	    CUT-BUFFER		*
		 *******************************/

status
ws_set_cutbuffer(DisplayObj d, int n, String s)
{ DisplayWsXref r = d->ws_ref;

  if ( n == 0 )
    XStoreBytes(r->display_xref, s->s_text, str_datasize(s));
  else
    XStoreBuffer(r->display_xref, s->s_text, str_datasize(s), n);

  succeed;
}


StringObj
ws_get_cutbuffer(DisplayObj d, int n)
{ DisplayWsXref r = d->ws_ref;
  char *data;
  int size;
  string str;
  StringObj rval;

  if ( n == 0 )
    data = XFetchBytes(r->display_xref, &size);
  else
    data = XFetchBuffer(r->display_xref, &size, valInt(n));

  str_inithdr(&str, ENC_ASCII);
  str.size = size;
  str.s_text = (unsigned char *)data;
  rval = StringToString(&str);

  XFree(data);
  answer(rval);
}

		 /*******************************
		 *	     SELECTION		*
		 *******************************/

static int	 selection_complete;
static StringObj selection_value;
static Name	 selection_error;

static Atom
nameToSelectionAtom(DisplayObj d, Name name)
{ if ( name == NAME_primary )	return XA_PRIMARY;
  if ( name == NAME_secondary ) return XA_SECONDARY;
  if ( name == NAME_string )	return XA_STRING;
  
  return DisplayAtom(d, getv(name, NAME_upcase, 0, NULL));
}


static Name
atomToSelectionName(DisplayObj d, Atom a)
{ if ( a == XA_PRIMARY )   return NAME_primary;
  if ( a == XA_SECONDARY ) return NAME_secondary;
  if ( a == XA_STRING )    return NAME_string;

  { Name xname = CtoName(DisplayAtomToString(d, a));
    Name lname = getv(xname, NAME_downcase, 0, NULL);
    
    return CtoKeyword(strName(lname));
  }

  fail;
}


unsigned long
ws_get_selection_timeout()
{ return XtAppGetSelectionTimeout(pceXtAppContext(NULL));
}


void
ws_set_selection_timeout(unsigned long time)
{ XtAppSetSelectionTimeout(pceXtAppContext(NULL), time);
}


static void
collect_selection_display(Widget w, XtPointer xtp,
			  Atom *selection, Atom *type,
			  XtPointer value, unsigned long *len, int *format)
{ if ( *type == XA_STRING )
  { string s;

    if ( *format == 8 )
    { str_inithdr(&s, ENC_ASCII);
      s.size = *len;
      s.s_text = (unsigned char *)value;
    } else if ( *format == 16 )
    { str_inithdr(&s, ENC_UNICODE);
      s.size = *len / 2;
      s.s_text = (unsigned char *)value;
    } else
    { selection_error = CtoName("Bad format");
      selection_complete = TRUE;
      return;
    }

    selection_value = StringToString(&s);
    XtFree(value);
  } else if ( *type == XT_CONVERT_FAIL )
    selection_error = NAME_timeout;
  else
    selection_error = CtoName("Bad type");

  selection_complete = TRUE;
}


Any
ws_get_selection(DisplayObj d, Name which, Name target)
{ DisplayWsXref r = d->ws_ref;

  selection_complete = FALSE;
  selection_error = NIL;
  XtGetSelectionValue(r->shell_xref,
		      nameToSelectionAtom(d, which),
		      nameToSelectionAtom(d, target),
		      collect_selection_display,
		      d,
		      X11LastEventTime());

  while(!selection_complete)
    dispatchDisplayManager(d->display_manager, DEFAULT, toInt(50));

  if ( notNil(selection_error) )
  { errorPce(d, NAME_getSelection, which, selection_error);
    fail;
  }

  answer(selection_value);
}


static DisplayObj
widgetToDisplay(Widget w)
{ DisplayManager dm = TheDisplayManager();
  Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj d = cell->value;
    DisplayWsXref r = d->ws_ref;

    if ( r->shell_xref == w )
      return d;
  }

  fail;
}


static Boolean
convert_selection_display(Widget w,
			  Atom *selection, Atom *target, Atom *type_return,
			  XtPointer *value, unsigned long *len, int *format)
{ DisplayObj d = widgetToDisplay(w);
  Hyper h;
  Function msg;
  Name which = atomToSelectionName(d, *selection);
  Name hypername = getAppendName(which, NAME_selectionOwner);

  if ( d &&
       (h = getFindHyperObject(d, hypername, DEFAULT)) &&
       (msg = getAttributeObject(h, NAME_convertFunction)) &&
       (msg = checkType(msg, TypeFunction, NIL)) )
  { CharArray ca;
    Name tname = atomToSelectionName(d, *target);

    if ( (ca = getForwardReceiverFunction(msg, h->to, which, tname, EAV)) &&
	 (ca = checkType(ca, TypeCharArray, NIL)) )
    { String s = &ca->data;
      int data = str_datasize(s);
      char *buf = XtMalloc(data);

      memcpy(buf, s->s_text, data);
      *value = buf;
      *len = data;
      *format = (isstr8(s) ? sizeof(char8) : sizeof(char16)) * 8;
      *type_return = XA_STRING;

      return True;
    }
  }

  return False;
}


static void
loose_selection_widget(Widget w, Atom *selection)
{ DisplayObj d = widgetToDisplay(w);

  DEBUG(NAME_selection,
	Cprintf("%s: Loosing %s selection",
		pp(d), pp(atomToSelectionName(d, *selection))));
    
  if ( d )
    looseSelectionDisplay(d, atomToSelectionName(d, *selection));
}


void
ws_disown_selection(DisplayObj d, Name selection)
{ DisplayWsXref r = d->ws_ref;

  XtDisownSelection(r->shell_xref,
		    nameToSelectionAtom(d, selection),
		    X11LastEventTime());
}


status
ws_own_selection(DisplayObj d, Name selection, Name type)
{ DisplayWsXref r = d->ws_ref;

  if ( XtOwnSelection(r->shell_xref, nameToSelectionAtom(d, selection),
		      X11LastEventTime(),
		      convert_selection_display,
		      loose_selection_widget,
		      NULL) )
    succeed;

  fail;
}


Name
ws_window_manager(DisplayObj d)
{
#if O_MOTIF
  DisplayWsXref r = d->ws_ref;

  if ( XmIsMotifWMRunning(r->shell_xref) )
    return NAME_mwm;
#endif

  fail;
}


void
ws_synchronous(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XSynchronize(r->display_xref, True);
}


void
ws_asynchronous(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XSynchronize(r->display_xref, False);
}

		 /*******************************
		 *	    POSTSCRIPT		*
		 *******************************/

static int
psdepthXImage(XImage *im)
{ if ( im->depth < 3 )			/* 1, 2 */
    return im->depth;
  if ( im->depth < 8 )
    return 4;
  return 8;
}


status
ws_postscript_display(DisplayObj d)
{ XWindowAttributes atts;
  XImage *im;
  int iw, ih;
  Window root;
  DisplayWsXref r;

  openDisplay(d);
  r = d->ws_ref;

  XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);
  root = atts.root;
  XGetWindowAttributes(r->display_xref, root, &atts);

  iw = atts.width; ih = atts.height;
  im = XGetImage(r->display_xref, atts.root,
		 0, 0, iw, ih, AllPlanes, ZPixmap);

  ps_output("0 0 ~D ~D ~D greymap\n", iw, ih, psdepthXImage(im));
  postscriptXImage(im, 0, 0, iw, ih, r->display_xref, r->colour_map, 0);
  ps_output("\n");

  XDestroyImage(im);
  succeed;
}


Image
ws_grab_image_display(DisplayObj d, int x, int y, int width, int height)
{ XWindowAttributes atts;
  XImage *im = NULL;
  Image i = NULL;
  Window root;
  DisplayWsXref r;

  openDisplay(d);
  r = d->ws_ref;

				/* display attributes */
  XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);
  root = atts.root;
  XGetWindowAttributes(r->display_xref, root, &atts);

  if ( x < 0 )			/* clip to display size */
  { width += x;
    x = 0;
  }
  if ( y < 0 )
  { height += y;
    y = 0;
  }
  if ( x + width > atts.width )
    width = atts.width - x;
  if ( y + height > atts.height )
    height = atts.height - y;

  if ( width <= 0 || height <= 0 )
    fail;

  i = answerObject(ClassImage, NIL,
		   toInt(width), toInt(height), NAME_pixmap, EAV);
  im = XGetImage(r->display_xref, atts.root,
		 x, y, width, height, AllPlanes, ZPixmap);
    
  if ( i && im )
  { setXImageImage(i, im);
    assign(i, depth, toInt(im->depth));
  } else
  { if ( im )
      XDestroyImage(im);
    if ( i )
    { freeObject(i);
      i = NULL;
    }
  }

  answer(i);
}
