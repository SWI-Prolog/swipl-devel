/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
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


void
ws_synchronise_display(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;

  XSync(r->display_xref, False);

  while( XtAppPending(pceXtAppContext(NULL)) & XtIMAll )
    XtAppProcessEvent(pceXtAppContext(NULL), XtIMAll);
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

  ref->display_xref     = NULL;
  ref->shell_xref       = NULL;
  ref->root_bitmap      = 0;
  ref->depth	        = 1;
  ref->white_pixel      = 0L;
  ref->black_pixel      = 1L;
  ref->colour_map       = 0;
  ref->pixmap_context   = NULL;
  ref->bitmap_context   = NULL;
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
  CharArray home = get(PCE, NAME_home, 0);
  char *oldxapplresdir = getenv("XAPPLRESDIR");
  char applresdir[512];

  if ( home )
  { char *s;

    strcpy(applresdir, "XAPPLRESDIR=");
    if ( oldxapplresdir )
    { strcat(applresdir, oldxapplresdir);
      strcat(applresdir, ":");
    }
    strcat(applresdir, strName(home));
    s = &applresdir[strlen(applresdir)];
    if ( s[-1] == '/' && s > applresdir )
      s--;
    *s = EOS;

					/* does or doesn't putenv copy? */
					/* Read the SunOs 4.1 manual */
					/* ... and tell me if you understand */
    putenv(save_string(applresdir));
  }
  
  address = isDefault(d->address) ? NULL : strName(d->address);
  display = XtOpenDisplay(pceXtAppContext(NULL),
			  address, "xpce",
			  strName(d->resource_class),
			  opTable, XtNumber(opTable),
			  &PCEargc, PCEargv);

/*
  if ( resdirpushed )
  { sprintf(applresdir, "XAPPLRESDIR=%s", oldxapplresdir);
    putenv(save_string(applresdir));
  }
*/
    
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
				       strName(d->resource_class),
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
  ulong black, white;

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

  values.function   = GXinvert;
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
  RedrawAreaGraphical(gr, gr->area);
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

  str.size = size;
  str.encoding = ENC_ASCII;
  str.b16 = FALSE;
  str.pad = 0;
  str.s_text = data;
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


ulong
ws_get_selection_timeout()
{ return XtAppGetSelectionTimeout(pceXtAppContext(NULL));
}


void
ws_set_selection_timeout(ulong time)
{ XtAppSetSelectionTimeout(pceXtAppContext(NULL), time);
}


static void
collect_selection_display(Widget w, XtPointer xtp,
			  Atom *selection, Atom *type,
			  XtPointer value, unsigned long *len, int *format)
{ if ( *type == XA_STRING )
  { string s;

    if ( *format == 8 )
    { s.encoding = ENC_ASCII;
      s.size = *len;
      s.s_text = value;
      s.b16 = FALSE;
    } else if ( *format == 16 )
    { s.encoding = ENC_ASCII;
      s.size = *len / 2;
      s.s_text = value;
      s.b16 = TRUE;
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

    if ( (ca = getForwardReceiverFunction(msg, h->to, which, tname, 0)) &&
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
ws_own_selection(DisplayObj d, Name selection)
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
		 0, 0, iw, ih, AllPlanes, XYPixmap);

  ps_output("0 0 ~D ~D bitmap\n\n", iw, ih);
  postscriptXImage(im, iw, ih, r->background_pixel);
  ps_output("\n");

  XDestroyImage(im);
  succeed;
}


		 /*******************************
		 *	     RESOURCES		*
		 *******************************/

StringObj
ws_get_resource_value(DisplayObj d,
		      Name cc, Name cn,
		      Name rc, Name rn)
{ DisplayWsXref r = d->ws_ref;
  char *val;				/* actually X11 String ... */
  XtResource res;
  
  res.resource_name   = resourceName(rn);
  res.resource_class  = strName(rc);
  res.resource_type   = XtRString;
  res.resource_size   = sizeof(String);
  res.resource_offset = 0;
  res.default_type    = XtRString;
  res.default_addr    = NULL;

  XtGetSubresources(r->shell_xref, &val,
		    strName(cn), strName(cc), &res, 1, NULL, 0);
  if ( val )
    answer(CtoString(val));

  fail;
}
