/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>
#include "include.h"
#include <X11/keysym.h>

#undef roundup
#define roundup(v, n)		((((v)+(n)-1)/(n))*(n))
#define rescale(v, o, n)	((v) * (n) / (o))
#define XBRIGHT ((1L<<16)-1)


XtAppContext	ThePceXtAppContext;	/* X toolkit application context */

		 /*******************************
		 *       X11 APP CONTEXT	*
		 *******************************/

static int
x_error_handler(Display *display, XErrorEvent *error)
{ char msg[1024];
  char request[100];
  char buf[100];

  XGetErrorText(display, error->error_code, msg, 1024);
  sprintf(buf, "%d", error->request_code);
  XGetErrorDatabaseText(display, "XRequest", buf,
			"Unknown request", request, 100);
  Cprintf("X error of failed request: %s\n", msg);
  Cprintf("Major opcode of failed request: %d (%s)\n",
	  error->request_code, request);
  Cprintf("Minor opcode of failed request: %d\n", error->minor_code);
  Cprintf("Resource id in failed request:  0x%x\n",
	  (unsigned int) error->resourceid);
  Cprintf("Serial number of failed request: %ld\n", error->serial);
  
  errorPce(NIL, NAME_xError);

  return 0;				/* what to return here? */
}


XtAppContext
pceXtAppContext(XtAppContext ctx)
{ if ( ThePceXtAppContext == NULL )
  { if ( ctx != NULL )
    { ThePceXtAppContext = ctx;
      XSetErrorHandler(x_error_handler);
    } else
    { XtToolkitInitialize();
      XSetErrorHandler(x_error_handler);

      if ( (ThePceXtAppContext = XtCreateApplicationContext()) == NULL )
      { errorPce(TheDisplayManager(), NAME_noApplicationContext);
	fail;
      }
    }
  }

  return ThePceXtAppContext;
}

		 /*******************************
		 *	 WIDGET REFERENCE	*
		 *******************************/

Widget
widgetWindow(PceWindow sw)
{ return (Widget) sw->ws_ref;
}


Widget
widgetFrame(FrameObj fr)
{ return fr->ws_ref ? ((frame_ws_ref *)fr->ws_ref)->widget : NULL;
}

void
setXImageImage(Image image, XImage *i)
{ image->ws_ref = i;
}


		 /*******************************
		 *	     X11 ATOMS		*
		 *******************************/

#if O_MOTIF
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>			/* was X11? */
#include <Xm/Protocols.h>		/* idem */

#define XInternAtom(d, nm, v) XmInternAtom(d, nm, v)
#define XGetAtomName(d, atom) XmAtomToName(d, atom)
#endif

Atom
DisplayAtom(DisplayObj d, Name name)
{ DisplayWsXref r = d->ws_ref;

  return XInternAtom(r->display_xref, strName(name), False);
}


char *
DisplayAtomToString(DisplayObj d, Atom atom)
{ DisplayWsXref r = d->ws_ref;

  return XGetAtomName(r->display_xref, atom);
}


Atom
FrameAtom(FrameObj fr, Name name)
{ return DisplayAtom(fr->display, name);
}


char *
FrameAtomToString(FrameObj fr, Atom a)
{ return DisplayAtomToString(fr->display, a);
}


Atom
WmProtocols(FrameObj fr)
{ return FrameAtom(fr, CtoName("WM_PROTOCOLS"));
}

		 /*******************************
		 *	  FRAME THINGS		*
		 *******************************/

Window
getWMFrameFrame(FrameObj fr)
{ Widget wdg;

  if ( (wdg = widgetFrame(fr)) )
  { Window w = XtWindow(wdg);
    DisplayWsXref r = fr->display->ws_ref;

    if ( equalName(fr->kind, NAME_popup) )
      return w;
    else
    { Window root, parent, *children;
      unsigned int nchildren;
      int m = 5;

      while(--m >= 0)			/* avoid a loop */
      { if ( !XQueryTree(r->display_xref, w, &root, &parent,
			 &children, &nchildren) )
	  return w;
	XFree((char *) children);	/* declared char * ???? */
	DEBUG(NAME_frame, Cprintf("w = %ld; root = %ld; parent = %ld\n",
				  w, root, parent));
	if ( parent == root )
	  return w;

	w = parent;
      }
    }
  }

  return 0;
}

		 /*******************************
		 *	     POSTSCRIPT		*
		 *******************************/

#define putByte(b) { ps_put_char(print[(b >> 4) & 0xf]); \
		     ps_put_char(print[b & 0xf]); \
 		     if ( (++bytes % 32) == 0 ) ps_put_char('\n'); \
		     bits = 8; c = 0; \
		   }

status
postscriptXImage(XImage *im,
		 int fx, int fy, int w, int h,
		 Display *disp,
		 Colormap cmap,
		 int depth)
{ static char print[] = { '0', '1', '2', '3', '4', '5', '6', '7',
			  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
  int x, y, w8, psbright;
  int bits, bytes;
  int c;
  unsigned char *psmap;
  int psmap_alloced = 0;
  XColor **cinfo;

  if ( depth == 0 )			/* PostScript depth is 1, 2, 4, or 8 */
  { depth = im->depth;

    if ( depth == 3 )
      depth = 2;
    else if ( depth > 4 && depth < 8 )
      depth = 4;
    else if ( depth > 8 )
      depth = 8;
  }

  if ( im->format == XYBitmap )
  { psmap = (unsigned char *)"\1\0";
    psbright = 1;
  } else
  { int entries	= 1<<im->depth;

    if ( im->depth > 16 )
      return errorPce(NIL, NAME_maxDepth, toInt(16));

    psbright = (1<<depth) - 1;
    psmap = (unsigned char *)pceMalloc(entries * sizeof(unsigned char));
    psmap_alloced++;
    if ( (cinfo = makeSparceCInfo(disp, cmap, im, NULL)) )
    { XColor **xc = cinfo;
      int i, psscale = (1<<depth)-1;

      for(i=0; i<entries; i++, xc++)
      { if ( *xc )
	{ int val = intensityXColor(*xc);

	  psmap[i] = rescale(val, XBRIGHT, psscale);
	} else
	  psmap[i] = 0;
      }
      freeSparceCInfo(cinfo, im->depth);
    }
  }


  w8 = roundup(w, 8);
  for(bytes = c = 0, bits = 8, y = fy; y < h; y++)
  { for(x = fx; x < w8; x++)
    { int pixval;

      bits -= depth;
      pixval = (x < w ? psmap[XGetPixel(im, x, y)] : psbright);
      c |= pixval << bits;
      if ( bits == 0 )
        putByte(c);
    }
  }

  if ( psmap_alloced )
    pceFree(psmap);

  succeed;
}


		 /*******************************
		 *       COLOUR HANDLING	*
		 *******************************/

int
intensityXColor(XColor *c)
{ unsigned int r = c->red;
  unsigned int g = c->green;
  unsigned int b = c->blue;

  return (r*20 + g*32 + b*18)/(20+32+18);
}


ulong
getPixelColour(Colour c, DisplayObj d)
{ XColor *color = (XColor *) getXrefObject(c, d);

  return color ? color->pixel : 0L;
}


void
x11_set_gc_foreground(DisplayObj d, Any fg, int gcs, GC *gc)
{ XGCValues values;
  ulong mask;
  DisplayWsXref r = d->ws_ref;

  if ( instanceOfObject(fg, ClassColour) )
  { ulong pixel = getPixelColour(fg, d);
	
    values.foreground = pixel;
    values.fill_style = FillSolid;
    mask	      = (GCForeground|GCFillStyle);
  } else
  { Pixmap pm   = (Pixmap) getXrefObject(fg, d);
    
    values.tile       = pm;
    values.fill_style = FillTiled;
    mask	      = (GCTile|GCFillStyle);
  }

  for(; gcs > 0; gcs--, gc++)
    XChangeGC(r->display_xref, *gc, mask, &values);
}


static int
distanceColours(Name vt, XColor *c1, XColor *c2)
{ if ( vt == NAME_greyScale )
  { int i1 = intensityXColor(c1);
    int i2 = intensityXColor(c2);

    return abs(i1 - i2);
  } else
  { int dr = ((int)c1->red - (int)c2->red) / 4;
    int dg = ((int)c1->green - (int)c2->green) / 4;
    int db = ((int)c1->blue - (int)c2->blue) / 4;

    return (int)sqrt((double)(dr*dr + dg*dg + db*db)) * 4;
  }
}


status
allocNearestColour(Display *display, Colormap map, int depth, Name vt,
		   XColor *c)
{ XColor *colors;
  int entries = 1<<depth;

  if ( (colors = alloc(entries * sizeof(XColor))) )
  { int i, j;
      
    for(i=0; i<entries; i++)
      colors[i].pixel = i;

    DEBUG(NAME_colour, Cprintf("Looking for %d %d %d\n",
			       c->red, c->green, c->blue));

    if ( isDefault(vt) )		/* TBD */
    { Visual *v = XDefaultVisual(display, DefaultScreen(display));
      int vclass = v->class;

      switch(vclass)
      { case StaticGray: vt = NAME_staticGrey;
        case GrayScale:	 vt = NAME_greyScale;
      }
    }

    XQueryColors(display, map, colors, entries);

    for(j=0; j<entries; j++)
    { XColor *cb = NULL;
      int badness = 1000000;
      XColor *e = colors;

      for(i=0; i<entries; i++, e++)
      { if ( e->flags != 0xff )		/* tried this one */
	{ int d = distanceColours(vt, c, e);

	  if ( d < badness )
	  { cb = e;
	    badness = d;
	  }
	}
      }

      assert(cb);
      
      DEBUG(NAME_colour, Cprintf("Mapped colour %d %d %d --> %d %d %d\n",
				 c->red, c->green, c->blue,
				 cb->red, cb->green, cb->blue));

      *c = *cb;
      if ( XAllocColor(display, map, c) )
      { unalloc(entries * sizeof(XColor), colors);
	succeed;
      } else
      {	cb->flags = 0xff;		/* don't try this one anymore! */
	DEBUG(NAME_colour, Cprintf("Can't allocate, trying another one\n"));
      }
    }
  } 

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XColor **makeSparceCInfo(Display *d, Colormap cm, XImage *i, int *nc)
	returns an array of pointers to XColor structures for all pixels
	occurring in `i'.  The total number of colours found is stored
	in `nc' is this pointer is non-NULL.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

XColor **
makeSparceCInfo(Display *disp, Colormap cmap, XImage *img, int *ncolours)
{ int entries = 1L<<img->depth;
  XColor **info = (XColor **)pceMalloc(sizeof(XColor *) * entries);
  XColor *data;
  int width = img->width;
  int height = img->height;
  int x, y, i;
  int colours = 0;

  DEBUG(NAME_pnm, Cprintf("makeSparceCInfo(): depth = %d, %d entries\n",
			  img->depth, entries));

  for(i=0; i<entries; i++)
    info[i] = NULL;
      
  for(y=0; y<height; y++)
  { for(x=0; x<width; x++)
    { ulong pixel = XGetPixel(img, x, y);

      if ( pixel > entries )
      { Cprintf("PNM: Illegal pixel value at %d,%d: %d\n", x, y, pixel);
	pixel = 0;
      }
      if ( !info[pixel] )
      { info[pixel] = (XColor *) 1;	/* flag it */
	colours++;
      }
    }
  }

  if ( ncolours )
    *ncolours = colours;
  data = (XColor *)pceMalloc(sizeof(XColor) * colours);
  colours=0;
  for(i=0; i<entries; i++)
  { if ( info[i] )
    { info[i] = &data[colours++];
      info[i]->pixel = i;
    }
  }
  assert(ncolours ? *ncolours == colours : TRUE);

  XQueryColors(disp, cmap, data, colours);

  return info;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void greySparceCInfo(XColor **cinfo, int depth)
	Translates a color-info array returned by makeSparceCInfo() into
	a grey-map.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
greySparceCInfo(XColor **cinfo, int depth)
{ int i;
  int entries = 1 << depth;

  for(i=0; i<entries; i++)
  { if ( cinfo[i] )
    { XColor *c = cinfo[i];
      int value = intensityXColor(c);
      
      c->red = c->green = c->blue = value;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void	freeSparceCInfo(XColor **table, int depth)
	Deallocates a structure returned by makeSparceCInfo().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
freeSparceCInfo(XColor **table, int depth)
{ int entries = 1<<depth;
  int i;

  for(i=0; i<entries; i++)
  { if ( table[i] )
    { pceFree(table[i]);
      break;
    }
  }

  pceFree(table);
}


		/********************************
		*      X-EVENT TRANSLATION	*
		********************************/

static Any
keycode_to_name(XEvent *event)
{ char buf[256];
  int bytes;
  KeySym sym;

  bytes = XLookupString((XKeyEvent *) event, buf, 256, &sym, NULL);

  switch(sym)				/* special ones */
  { case XK_BackSpace:
      if ( event->xkey.state & Mod1Mask )
	return toInt(8+META_OFFSET);
      return NAME_backspace;
  }

  if ( bytes == 1 )
  { int c = buf[0] & 0xff;

    if ( event->xkey.state & Mod1Mask )	/* meta depressed */
      c += META_OFFSET;

    return toInt(c);
  }

  switch(sym)
  { case XK_F1:		return NAME_keyTop_1;
    case XK_F2:		return NAME_keyTop_2;
    case XK_F3:		return NAME_keyTop_3;
    case XK_F4:		return NAME_keyTop_4;
    case XK_F5:		return NAME_keyTop_5;
    case XK_F6:		return NAME_keyTop_6;
    case XK_F7:		return NAME_keyTop_7;
    case XK_F8:		return NAME_keyTop_8;
    case XK_F9:		return NAME_keyTop_9;
    case XK_F10:	return NAME_keyTop_10;

    case XK_L1:		return NAME_keyLeft_1;
    case XK_L2:		return NAME_keyLeft_2;
    case XK_L3:		return NAME_keyLeft_3;
    case XK_L4:		return NAME_keyLeft_4;
    case XK_L5:		return NAME_keyLeft_5;
    case XK_L6:		return NAME_keyLeft_6;
    case XK_L7:		return NAME_keyLeft_7;
    case XK_L8:		return NAME_keyLeft_8;
    case XK_L9:		return NAME_keyLeft_9;
    case XK_L10:	return NAME_keyLeft_10;

    case XK_R1:		return NAME_keyRight_1;
    case XK_R2:		return NAME_keyRight_2;
    case XK_R3:		return NAME_keyRight_3;
    case XK_R4:		return NAME_keyRight_4;
    case XK_R5:		return NAME_keyRight_5;
    case XK_R6:		return NAME_keyRight_6;
    case XK_R7:		return NAME_keyRight_7;
    case XK_R8:		return NAME_keyRight_8;
    case XK_R9:		return NAME_keyRight_9;
    case XK_R10:	return NAME_keyRight_10;
    case XK_R11:	return NAME_keyRight_11;
    case XK_R12:	return NAME_keyRight_12;
    case XK_R13:	return NAME_keyRight_13;
    case XK_R14:	return NAME_keyRight_14;
    case XK_R15:	return NAME_keyRight_14;

/* Cursor motion */

#ifndef XK_Page_Up
#define XK_Page_Up XK_Prior
#endif
#ifndef XK_Page_Down
#define XK_Page_Down XK_Next
#endif

    case XK_Home:	return NAME_cursorHome;
    case XK_Left:	return NAME_cursorLeft;
    case XK_Up:		return NAME_cursorUp;
    case XK_Right:	return NAME_cursorRight;
    case XK_Down:	return NAME_cursorDown;
    case XK_Page_Up:	return NAME_pageUp;
    case XK_Page_Down:	return NAME_pageDown;
    case XK_End:	return NAME_end;
    case XK_Begin:	return NAME_begin;

/* Misc Functions */

    case XK_Select:	return NAME_select;
    case XK_Print:	return NAME_print;
    case XK_Execute:	return NAME_execute;
    case XK_Insert:	return NAME_insert;
    case XK_Undo:	return NAME_undo;
    case XK_Redo:	return NAME_redo;
    case XK_Menu:	return NAME_menu;
    case XK_Find:	return NAME_find;
    case XK_Cancel:	return NAME_cancel;
    case XK_Help:	return NAME_help;
    case XK_Break:	return NAME_break;
  }

  DEBUG(NAME_keysym, Cprintf("sym = 0x%X\n", (unsigned int)sym));

  fail;
}


static Int
state_to_buttons(unsigned int state, Name name)
{ int r = 0;

  if ( state & Button1Mask )	r |= BUTTON_ms_left;
  if ( state & Button2Mask )	r |= BUTTON_ms_middle;
  if ( state & Button3Mask )	r |= BUTTON_ms_right;
  if ( state & ShiftMask )	r |= BUTTON_shift;
  if ( state & ControlMask )	r |= BUTTON_control;
  if ( state & Mod1Mask )	r |= BUTTON_meta;

  if ( equalName(name, NAME_msLeftDown) )
    r |= BUTTON_ms_left;
  else if ( equalName(name, NAME_msMiddleDown) )
    r |= BUTTON_ms_middle;
  else if ( equalName(name, NAME_msRightDown) )
    r |= BUTTON_ms_right;
  else if ( equalName(name, NAME_msLeftUp) )
    r &= ~BUTTON_ms_left;
  else if ( equalName(name, NAME_msMiddleUp) )
    r &= ~BUTTON_ms_middle;
  else if ( equalName(name, NAME_msRightUp) )
    r &= ~BUTTON_ms_right;

  return toInt(r);
}


static Name
button_to_name(int press, unsigned int button)
{ switch( button )
  { case Button1:	return press ? NAME_msLeftDown   : NAME_msLeftUp;
    case Button2:	return press ? NAME_msMiddleDown : NAME_msMiddleUp;
    case Button3:	return press ? NAME_msRightDown  : NAME_msRightUp;
  }

  fail;
}



EventObj
CtoEvent(Any window, XEvent *event)	/* window or frame */
{ Time time;
  int state = 0;
  int x;
  int y;
  Any name;

  switch( event->xany.type )
  { case KeyPress:			/* Key pressed */
    { x     = event->xkey.x;
      y     = event->xkey.y;
      state = event->xkey.state;
      time  = event->xkey.time;
      name  = keycode_to_name(event);
      if ( name == FAIL )
        fail;

      break;
    }
    case ButtonPress:			/* Button pressed/released */
    case ButtonRelease:
    { x     = event->xbutton.x;
      y     = event->xbutton.y;
      state = event->xbutton.state;
      time  = event->xbutton.time;
      name  = button_to_name(event->xany.type == ButtonPress,
				   event->xbutton.button);
      if ( name == FAIL )
        fail;

      break;
    }
    case MotionNotify:			/* Pointer motion events */
    { x     = event->xmotion.x;
      y     = event->xmotion.y;
      state = event->xmotion.state;
      time  = event->xmotion.time;

      if ( state & Button1Mask )
        name = NAME_msLeftDrag;
      else if ( state & Button2Mask )
        name = NAME_msMiddleDrag;
      else if ( state & Button3Mask )
        name = NAME_msRightDrag;
      else
        name = NAME_locMove;

      break;
    }
    case EnterNotify:
    case LeaveNotify:
    { x     = event->xcrossing.x;
      y     = event->xcrossing.y;
      state = event->xcrossing.state;
      time  = event->xcrossing.time;

#     define AnyButtonMask (Button1Mask|Button2Mask|Button3Mask)

      if ( event->xany.type == EnterNotify )
      { name = (state & AnyButtonMask ? NAME_areaResume
		 		      : NAME_areaEnter);
      } else
      { name = (state & AnyButtonMask ? NAME_areaCancel
			    	      : NAME_areaExit);
      }

      break;
    }
   default:				/* unknown event: do not convert */
      fail;
  }

  setLastEventTime(time);

  return answerObject(ClassEvent,
		      name, 
		      window,
		      toInt(x), toInt(y),
		      state_to_buttons(state, name),
		      0);		   
}

		 /*******************************
		 *      COMPATIBILITY HACKS	*
		 *******************************/

#if XT_REVISION <= 3

Status XSetWMProtocols (dpy, w, protocols, count)
    Display *dpy;
    Window w;
    Atom *protocols;
    int count;
{   static Atom a = None;

    if ( a == None )
	a = XInternAtom (dpy, "WM_PROTOCOLS", False);

    XChangeProperty (dpy, w, a, XA_ATOM, 32,
		     PropModeReplace, (unsigned char *) protocols, count);
    return True;
}

#endif

#if !defined(HAVE_XTPOPUPSPRINGLOADED) && !defined(sun)
					/* sun's X11R5 fails on AC_HAVE_FUNC */
					/* for X11 functions */

void XtPopupSpringLoaded (widget)
    Widget widget;
{
    _XtPopup(widget, XtGrabExclusive, True);
}

#endif




