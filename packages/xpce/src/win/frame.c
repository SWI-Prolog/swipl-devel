/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <unistd.h>

TileObj		getTileFrame P((FrameObj));
forwards int	get_position_from_center_frame P((FrameObj, Point,
						  int *, int *));
static void	ensure_on_display P((FrameObj, int *, int *));
static status	closedFrame P((FrameObj, Bool));
static status	openFrame(FrameObj fr, Point pos, Bool grab, Bool normalise);
static status	doneMessageFrame(FrameObj fr, Code msg);
static status	geometryFrame(FrameObj fr, Name spec);
static status	setFrame(FrameObj fr, Int x, Int y, Int w, Int h);
static status	flushFrame(FrameObj fr);
static status	kindFrame(FrameObj fr, Name kind);
static status	informTransientsFramev(FrameObj fr, Name selector,
				       int argc, Any *argv);

static status
initialiseFrame(FrameObj fr, Name label, Name kind, DisplayObj display)
{ if ( isDefault(kind) )
    kind = NAME_toplevel;
  if ( isDefault(display) )
    display = CurrentDisplay(NIL);
  if ( isDefault(label) )
    label = CtoName("Untitled");

  assign(fr, label,         label);
  assign(fr, display,       display);	/* Host on which to open frame */
  assign(fr, geometry,	    DEFAULT);	/* resources */
  assign(fr, icon_label,    DEFAULT);
  assign(fr, icon_image,    DEFAULT);
  assign(fr, border,	    DEFAULT);
  assign(fr, confirm_done,  DEFAULT);
  assign(fr, area,	    newObject(ClassArea, 0));
  assign(fr, members,	    newObject(ClassChain, 0));
  assign(fr, destroying,    OFF);
  assign(fr, kind,	    kind);
  assign(fr, status,	    NAME_unmapped);
  assign(fr, can_delete,    ON);
  assign(fr, has_returned,  OFF);
  assign(fr, wm_protocols,  newObject(ClassSheet, 0));
  assign(fr, wm_protocols_attached, OFF);

  doneMessageFrame(fr, newObject(ClassMessage, RECEIVER, NAME_wmDelete, 0));

  fr->ws_ref = NULL;			/* Window System Reference */

  succeed;
}


static status
unlinkFrame(FrameObj fr)
{ if ( fr->destroying == OFF )
  { FrameObj sfr;
    PceWindow sw;

    assign(fr, destroying, ON);

    if ( notNil(fr->transients) )
      for_chain(fr->transients, sfr, send(sfr, NAME_free, 0));
    if ( notNil(fr->transient_for) && notNil(fr->transient_for->transients) )
      deleteChain(fr->transient_for->transients, fr);

    ws_uncreate_frame(fr);
    deleteChain(fr->display->frames, fr);

    for_chain(fr->members, sw, freeObject(sw));
  }

  succeed;
}


static FrameObj
getConvertFrame(Class class, PceWindow sw)
{ answer(getFrameWindow(sw));
}

		 /*******************************
		 *	     SAVE-LOAD		*
		 *******************************/

static status
storeFrame(FrameObj fr, FileObj file)
{ return storeSlotsObject(fr, file);
}


static status
loadFrame(FrameObj fr, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(fr, fd, def));
  assign(fr, wm_protocols_attached, OFF);

  if ( fr->status == NAME_open )
  { assign(fr, status, NAME_unmapped);
    restoreMessage(newObject(ClassMessage, fr, NAME_open,
			     get(fr->area, NAME_position, 0), 0));
  }

  succeed;
}


		/********************************
		*          OPEN/CREATE		*
		********************************/

Any
getConfirmFrame(FrameObj fr, Point pos, Bool grab, Bool normalise)
{ Any rval;

  TRY( openFrame(fr, pos, grab, normalise) );
  busyCursorDisplay(fr->display, NIL, DEFAULT);

  assign(fr, has_returned, OFF);
  synchroniseDisplay(fr->display);
  while( fr->has_returned == OFF )
  { if ( dispatchDisplay(fr->display) )
    { char buf[LINESIZE];

      printf("Confirmer running; discarding input ... "); fflush(stdout);
      read(fileno(stdin), buf, LINESIZE);
      printf("ok\n");
    }
  }

  rval = fr->return_value;
  if ( isObject(rval) )
  { addCodeReference(rval);
    assign(fr, return_value, NIL);
    delCodeReference(rval);
    pushAnswerObject(rval);
  } else
    assign(fr, return_value, NIL);
  
  answer(rval);
}


Any
getConfirmCenteredFrame(FrameObj fr, Point pos, Bool grab)
{ int x, y;
  Point p2;
  Any rval;

  TRY( send(fr, NAME_create, 0) );

  get_position_from_center_frame(fr, pos, &x, &y);
  ensure_on_display(fr, &x, &y);
  p2 = tempObject(ClassPoint, toInt(x), toInt(y), 0);

  rval = getConfirmFrame(fr, p2, grab, OFF);
  considerPreserveObject(p2);
  return rval;
}


static status
returnFrame(FrameObj fr, Any obj)
{ assign(fr, return_value, obj);
  assign(fr, has_returned, ON);

  succeed;
}


static status
openFrame(FrameObj fr, Point pos, Bool grab, Bool normalise)
{ if ( !createdFrame(fr) )
    TRY( send(fr, NAME_create, 0) );
  
  if ( notDefault(pos) )
  { Int x = pos->x, y = pos->y, w = DEFAULT, h = DEFAULT;

    if ( normalise == ON )
    { Size s = getSizeDisplay(fr->display);
      int dw = valInt(s->w), dh = valInt(s->h);
      int fw = valInt(fr->area->w), fh = valInt(fr->area->h);

      if ( valInt(x) + fw > dw ) x = toInt(dw - fw);
      if ( valInt(y) + fh > dh ) y = toInt(dh - fh);
      if ( valInt(x) < 0 )       x = ZERO;
      if ( valInt(y) < 0 )	 y = ZERO;
    }

    setFrame(fr, x, y, w, h);  
  }

  statusFrame(fr, NAME_open);
  
  succeed;
}


static status
openCenteredFrame(FrameObj fr, Point pos, Bool grab)
{ int x, y;
  int rval;
  Point p2;

  TRY( send(fr, NAME_create, 0) );

  get_position_from_center_frame(fr, pos, &x, &y);
  ensure_on_display(fr, &x, &y);
  p2 = answerObject(ClassPoint, toInt(x), toInt(y), 0);
  rval = openFrame(fr, p2, grab, OFF);
  doneObject(p2);

  return rval;
}


status
resizeFrame(FrameObj fr)
{ Area a = fr->area;
  TileObj t = getTileFrame(fr);

  if ( t != FAIL )
    send(t, NAME_set, ZERO, ZERO, a->w, a->h, 0);

  succeed;
}


		/********************************
		*         WM_PROTOCOLS		*
		********************************/

static status
attachWmProtocolsFrame(FrameObj fr)
{ ws_attach_wm_prototols_frame(fr);

  succeed;
}


static status
wmProtocolFrame(FrameObj fr, Name name, Code msg)
{ valueSheet(fr->wm_protocols, name, msg);
  if ( fr->wm_protocols_attached == ON )
    attachWmProtocolsFrame(fr);

  succeed;
}


static status
deleteWmProtocolFrame(FrameObj fr, Name name)
{ if ( isAttributeSheet(fr->wm_protocols, name) == SUCCEED )
  { deleteSheet(fr->wm_protocols, name);
    if ( fr->wm_protocols_attached == ON )
      attachWmProtocolsFrame(fr);
  }

  succeed;
}


static status
wmDeleteFrame(FrameObj fr)
{ if ( fr->can_delete == OFF )
    fail;
  
  if ( fr->confirm_done == ON )
  { TRY(send(fr->display, NAME_confirm,
	     CtoName("Delete window ``%s''"), fr->label, 0));
  }
  
  return send(fr, NAME_destroy, 0);    
}


static status
doneMessageFrame(FrameObj fr, Code msg)
{ return wmProtocolFrame(fr, CtoName("WM_DELETE_WINDOW"), msg);
}


static status
saveMessageFrame(FrameObj fr, Code msg)
{ return wmProtocolFrame(fr, CtoName("WM_SAVE_YOURSELF"), msg);
}


static status
mappedFrame(FrameObj fr, Bool val)
{ Any stat = (val == ON ? NAME_open : NAME_hidden);
  informTransientsFramev(fr, NAME_status, 1, &stat);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To create a frame:

  1) Give all children the change to request a size.
  2) Fit the the windows in the frame and give them the size they should
     do with.
  3) Create the shell widget
  4) Create the children
  5) Manage the children
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
createdFrame(FrameObj fr)
{ return ws_created_frame(fr);
}


static status
createFrame(FrameObj fr)
{ Cell cell;

  if ( createdFrame(fr) )
    succeed;

  obtainResourcesObject(fr);
  TRY(openDisplay(fr->display));
  appendChain(fr->display->frames, fr);

  TRY(send(fr, NAME_fit, 0));

  ws_create_frame(fr);

  for_cell(cell, fr->members)
    send(cell->value, NAME_create, 0);

  ws_realise_frame(fr);
  assign(fr, status, NAME_hidden);

  attachWmProtocolsFrame(fr);

  if ( isName(fr->geometry) )
    geometryFrame(fr, fr->geometry);

  for_cell(cell, fr->members)
  { updateCursorWindow(cell->value);
    qadSendv(cell->value, NAME_resize, 0, NULL);
  }
  
  succeed;
}


status
fitFrame(FrameObj fr)
{ TileObj t;
  Cell cell;
  Int border;

  for_cell(cell, fr->members)
    send(cell->value, NAME_ComputeDesiredSize, 0);

  TRY(t = getTileFrame(fr));
  enforceTile(t);
  border = mul(t->border, TWO);

  return setFrame(fr, DEFAULT, DEFAULT,
		  add(t->idealWidth, border),
		  add(t->idealHeight, border));
}


status
statusFrame(FrameObj fr, Name stat)
{ if ( stat != NAME_unmapped && !createdFrame(fr) )
    TRY(send(fr, NAME_create, 0));

  if ( fr->status != stat )
  { assign(fr, status, stat);
    if ( stat == NAME_open )
    { ws_show_frame(fr, OFF);		/* TBD: grab arg does not work anyway */
      resizeFrame(fr);
      flushFrame(fr);
    } else if ( stat == NAME_iconic )
    { ws_iconify_frame(fr);
    } else if ( stat == NAME_hidden )
    { ws_unshow_frame(fr);
    } else /*unmapped*/
    {
    }
  }
  
  succeed;
}


static status
waitFrame(FrameObj fr)
{ if ( fr->status == NAME_unmapped )
    TRY(send(fr, NAME_open, 0));

  while( fr->status == NAME_hidden )
  { if ( dispatchDisplay(fr->display) )
    { char buf[LINESIZE];

      printf("Waiting for %s to open; discarding input ... ", pp(fr));
      fflush(stdout);
      read(fileno(stdin), buf, LINESIZE);
      printf("ok\n");
    }
  }

  if ( fr->status == NAME_open )
    succeed;

  fail;					/* error? */
}


static status
showFrame(FrameObj fr, Bool val)
{ return statusFrame(fr, val == ON ? NAME_open : NAME_hidden);
}


static Bool
getShowFrame(FrameObj fr)
{ answer(fr->status == NAME_open ? ON : OFF);
}


		/********************************
		*           HIDE/EXPOSE		*
		********************************/

status
exposeFrame(FrameObj fr)
{ statusFrame(fr, NAME_open);

  ws_raise_frame(fr);

  succeed;
}


status
hideFrame(FrameObj fr)
{ ws_lower_frame(fr);

  succeed;
}


static status
exposedFrame(FrameObj fr)
{ informTransientsFramev(fr, NAME_expose, 0, NULL);

  succeed;
}


static status
hiddenFrame(FrameObj fr)
{ informTransientsFramev(fr, NAME_hide, 0, NULL);

  succeed;
}

		/********************************
		*       AREA MANAGEMENENT	*
		********************************/

static int
get_position_from_center_frame(FrameObj fr, Point pos, int *x, int *y)
{ if ( isDefault(pos) )
  { Size sz = getSizeDisplay(fr->display);

    *x = valInt(sz->w) / 2;
    *y = valInt(sz->h) / 2;
  } else
  { *x = valInt(pos->x);
    *y = valInt(pos->y);
  }

  *x -= valInt(fr->area->w) / 2;
  *y -= valInt(fr->area->h) / 2;
  
  succeed;
}


static void
ensure_on_display(FrameObj fr, int *x, int *y)
{ Size sz = getSizeDisplay(fr->display);

  if ( *x + valInt(fr->area->w) > valInt(sz->w) )
    *x -= *x + valInt(fr->area->w) - valInt(sz->w);
  if ( *y + valInt(fr->area->w) > valInt(sz->w) )
    *y -= *y + valInt(fr->area->w) - valInt(sz->w);
  if ( *x < 0 )
    *x = 0;
  if ( *y < 0 )
    *y = 0;
}


static Size
getSizeFrame(FrameObj fr)
{ answer(getSizeArea(fr->area));
}


static Point
getPositionFrame(FrameObj fr)
{ answer(getPositionArea(fr->area));
}


static Area
getBoundingBoxFrame(FrameObj fr)
{ int x, y, w, h;

  if ( ws_frame_bb(fr, &x, &y, &w, &h) )
    answer(answerObject(ClassArea, ZERO, ZERO, toInt(w), toInt(h), 0));

  fail;
}



static Name
getGeometryFrame(FrameObj fr)
{ int x, y, w, h;

  if ( ws_frame_bb(fr, &x, &y, &w, &h) )
  { int dw, dh;
    int xn=0, yn=0;
    Size size;
    char buf[100];

    size = getSizeDisplay(fr->display);
    dw = valInt(size->w);
    dh = valInt(size->h);

    if ( x > dw - (x+w) )
    { x = dw - (x+w);
      xn++;
    }
    if ( y > dh - (y+h) )
    { y = dh - (y+h);
      yn++;
    }

    sprintf(buf, "%dx%d%s%d%s%d",
	    w, h, xn ? "-" : "+", x, yn ? "-" : "+", y);

    answer(CtoName(buf));
  }

  fail;
}


static Image
getImageFrame(FrameObj fr)
{ if ( createdFrame(fr) )
    answer(ws_image_of_frame(fr));

  errorPce(fr, NAME_mustBeCreatedBefore, NAME_image);
  fail;
}


static status
geometryFrame(FrameObj fr, Name spec)
{ assign(fr, geometry, spec);

  ws_x_geometry_frame(fr, spec);

  succeed;
}


static status
setFrame(FrameObj fr, Int x, Int y, Int w, Int h)
{ Area a = fr->area;
  Int ow = a->w;
  Int oh = a->h;

  setArea(a, x, y, w, h);

  if ( createdFrame(fr) )
  { ws_geometry_frame(fr, a->x, a->y, a->w, a->h);

    if ( ow != a->w || oh != a->h )
      resizeFrame(fr);
  }

  succeed;
}


static status
xFrame(FrameObj fr, Int x)
{ return setFrame(fr, x, DEFAULT, DEFAULT, DEFAULT);
}


static status
yFrame(FrameObj fr, Int y)
{ return setFrame(fr, DEFAULT, y, DEFAULT, DEFAULT);
}


static status
widthFrame(FrameObj fr, Int w)
{ return setFrame(fr, DEFAULT, DEFAULT, w, DEFAULT);
}


static status
heightFrame(FrameObj fr, Int h)
{ return setFrame(fr, DEFAULT, DEFAULT, DEFAULT, h);
}


static status
sizeFrame(FrameObj fr, Size sz)
{ return setFrame(fr, DEFAULT, DEFAULT, sz->w, sz->h);
}


static status
positionFrame(FrameObj fr, Point pos)
{ return setFrame(fr, pos->x, pos->y, DEFAULT, DEFAULT);
}


static status
centerFrame(FrameObj fr, Point pos)
{ int x, y;

  get_position_from_center_frame(fr, pos, &x, &y);
  return setFrame(fr, toInt(x), toInt(y), DEFAULT, DEFAULT);
}


static status
areaFrame(FrameObj fr, Area area)
{ return setFrame(fr, area->x, area->y, area->w, area->h);
}


static status
showLabelFrame(FrameObj fr, Bool val)
{ return kindFrame(fr, val == ON ? NAME_toplevel : NAME_transient);
}


static status
borderFrame(FrameObj fr, Int width)
{ assign(fr, border, width);

  ws_border_frame(fr, valInt(width));
  succeed;
}

		 /*******************************
		 *	     CURSORS		*
		 *******************************/

status
busyCursorFrame(FrameObj fr, CursorObj c, Bool block_events)
{ if ( createdFrame(fr) )
    ws_busy_cursor_frame(fr, c);

  succeed;
}


static status
resetFrame(FrameObj fr)
{ busyCursorFrame(fr, NIL, DEFAULT);

  return resetVisual((VisualObj) fr);
}



		/********************************
		*             ICONS		*
		********************************/

static status
iconFrame(FrameObj fr, Image image, Name label)
{ assign(fr, icon_image, image);
  if ( notDefault(label) )
    assign(fr, icon_label, label);
  ws_set_icon_frame(fr);

  succeed;
}


Name
getIconLabelFrame(FrameObj fr)
{ answer(notNil(fr->icon_label) ? fr->icon_label : fr->label);
}


static status
iconLabelFrame(FrameObj fr, Name label)
{ assign(fr, icon_label, label);
  ws_set_icon_label_frame(fr);

  succeed;
}


static status
iconPositionFrame(FrameObj fr, Point pos)
{ assign(fr, icon_position, pos);

  if ( notNil(pos) )
    ws_set_icon_position_frame(fr, valInt(pos->x), valInt(pos->y));

  succeed;
}


static Point
getIconPositionFrame(FrameObj fr)
{ int x, y;

  if ( ws_get_icon_position_frame(fr, &x, &y) )
    answerObject(ClassPoint, toInt(x), toInt(y));

  answer(fr->icon_position);
}


static status
closedFrame(FrameObj fr, Bool val)
{ return statusFrame(fr, val == ON ? NAME_iconic : NAME_open);
}


static Bool
getClosedFrame(FrameObj fr)
{ answer(fr->status == NAME_open ? OFF : ON);	/* dubious */
}


		/********************************
		*          MISCELENEOUS		*
		********************************/

static status
flushFrame(FrameObj fr)
{ return flushDisplay(fr->display);
}


static status
synchroniseFrame(FrameObj fr)
{ return synchroniseDisplay(fr->display);
}


static status
bellFrame(FrameObj fr, Int volume)
{ return bellDisplay(fr->display, volume);
}


TileObj
getTileFrame(FrameObj fr)
{ if ( notNil(fr->members->head) )
  { PceWindow sw = getHeadChain(fr->members);
  
    return getRootTile(sw->tile);
  }

  fail;
}


static status
labelFrame(FrameObj fr, Name label, Name icon)
{ assign(fr, label, label);

  ws_set_label_frame(fr);
  
  if ( notDefault(icon) )
    iconLabelFrame(fr, icon);

  succeed;
}


static status
appendFrame(FrameObj fr, PceWindow sw)
{ return frameWindow(sw, fr);
}


static Chain
getMembersFrame(FrameObj fr)
{ Chain rval = answerObject(ClassChain, 0);
  Cell cell;

  for_cell(cell, fr->members)
  { if ( instanceOfObject(cell->value, ClassWindowDecorator) )
    { WindowDecorator wd = cell->value;

      appendChain(rval, wd->window);
    } else
      appendChain(rval, cell->value);
  }

  answer(rval);
}


status
AppendFrame(FrameObj fr, PceWindow sw)
{ appendChain(fr->members, sw);

  if ( createdFrame(fr) )
  { TRY(send(sw, NAME_create, 0));

    ws_manage_window(sw);
    send(fr, NAME_fit, 0);
  }

  succeed;
}


status
DeleteFrame(FrameObj fr, PceWindow sw)
{ if ( instanceOfObject(sw->device, ClassWindowDecorator) )
    return DeleteFrame(fr, (PceWindow) sw->device);

  deleteChain(fr->members, sw);
  assign(sw, frame, NIL);

  if ( createdFrame(fr) )
  { ws_unmanage_window(sw);
    TRY(send(sw, NAME_uncreate, 0));
    unrelateTile(sw->tile);
    send(fr, NAME_fit, 0);
  }

  succeed;
}


static status
deleteFrame(FrameObj fr, PceWindow sw)
{ DeleteFrame(fr, sw);
  frameWindow(sw, newObject(ClassFrame, DEFAULT, DEFAULT, fr->display, 0));

  succeed;
}


static PceWindow
getMemberFrame(FrameObj fr, Name name)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow w;

    if ( (w=getUserWindow(cell->value))->name == name )
      answer(w);
  }
  
  fail;
}


static FrameObj
getFrameFrame(FrameObj fr)
{ answer(fr);
}


PceWindow
getInputFocusFrame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;

    if ( sw->input_focus == ON )
      answer(sw);
  }
  
  fail;
}


status
inputFocusFrame(FrameObj fr, PceWindow sw)
{ if ( getFrameWindow(sw) != fr )
    return errorPce(fr, NAME_notPart, sw);

  if ( sw->input_focus != OFF && sw->displayed == ON )
  { Cell cell;

    DEBUG(NAME_focus, printf("Setting input focus of %s to %s\n",
			     pp(fr), pp(sw)));

    ws_grab_keyboard_window(sw, ON);
     
    for_cell(cell, fr->members)
    { PceWindow w = cell->value;

      assign(sw, input_focus, w == sw ? ON : OFF);
    }
  }

  succeed;
}


static status
typedFrame(FrameObj fr, EventId id)
{ PceWindow sw;

  for_chain(fr->members, sw,
	    if ( send(sw, NAME_typed, id, 0) )
	      succeed;);

  fail;
}


		/********************************
		*           FRAME KINDS		*
		********************************/

static status
kindFrame(FrameObj fr, Name kind)
{ if ( fr->kind != kind )
  { if ( createdFrame(fr) )
      return errorPce(fr, NAME_noChangeAfterOpen);

    assign(fr, kind, kind);
  }

  succeed;
}


static status
transientForFrame(FrameObj fr, FrameObj fr2)
{ if ( fr->transient_for != fr2 )
  { if ( !createdFrame(fr) )
      kindFrame(fr, NAME_transient);

    if ( notNil(fr->transient_for) && notNil(fr->transient_for->transients) )
      deleteChain(fr->transient_for->transients, fr);

    assign(fr, transient_for, fr2);

    if ( notNil(fr2) )
    { if ( isNil(fr2->transients) )
	assign(fr2, transients, newObject(ClassChain, 0));
      addChain(fr2->transients, fr);

      if ( fr->kind == NAME_transient )
	ws_transient_frame(fr, fr2);
    }
  }

  succeed;
}


static status
informTransientsFramev(FrameObj fr, Name selector, int argc, Any *argv)
{ FrameObj sfr;

  if ( notNil(fr->transients) )
    for_chain(fr->transients, sfr, sendv(sfr, selector, argc, argv));

  succeed;
}


		/********************************
		*            CATCH-ALL		*
		********************************/

static Any
getCatchAllFramev(FrameObj fr, Name name)
{ Name base;

  if ( (base = getDeleteSuffixName(name, NAME_Member)) )
    answer(getMemberFrame(fr, base));

  fail;
}


extern postscriptFrame(FrameObj fr);

		/********************************
		*             VISUAL		*
		********************************/

static status
reportFrame(FrameObj fr, Name kind, CharArray fmt, int argc, Any *argv)
{ Any window, reporter;
  ArgVector(av, argc + 2);

  av[0] = kind;
  av[1] = fmt;
  copyArgs(argc, argv, &av[2]);

  if ( (reporter = getv(fr, NAME_reportTo, 0, NULL)) &&
       reporter != fr->display )
    return sendv(reporter, NAME_report, argc+2, av);

  for_chain(fr->members, window,
	    if ( !parentGoal(VmiSend, window, NAME_report) &&
	         sendv(window, NAME_report, argc+2, av) )
	      succeed);

  if ( notNil(fr->transient_for) &&
       sendv(fr->transient_for, NAME_report, argc+2, av) )
    succeed;

  return reportVisual((VisualObj)fr, kind, fmt, argc, argv);
}


static DisplayObj
getContainedInFrame(FrameObj fr)
{ answer(fr->display);
}


static Chain
getContainsFrame(FrameObj fr)
{ answer(fr->members);
}


status
makeClassFrame(Class class)
{ sourceClass(class, makeClassFrame, __FILE__, "$Revision$");

  setLoadStoreFunctionClass(class, loadFrame, storeFrame);

  localClass(class, NAME_label, NAME_label, "name", NAME_get,
	     "Label of the frame");
  localClass(class, NAME_iconLabel, NAME_icon, "name*", NAME_none,
	     "Label in the iconic representation");
  localClass(class, NAME_iconImage, NAME_icon, "image*", NAME_get,
	     "Image used for the iconic representation");
  localClass(class, NAME_iconPosition, NAME_icon, "point*", NAME_get,
	     "Position of the iconic image");
  localClass(class, NAME_display, NAME_organisation, "display", NAME_both,
	     "Display the frame resides on");
  localClass(class, NAME_border, NAME_appearance, "[int]", NAME_get,
	     "Width of border");
  localClass(class, NAME_area, NAME_area, "area", NAME_get,
	     "Area of the opened frame on the display");
  localClass(class, NAME_geometry, NAME_area, "name*", NAME_none,
	     "X-window geometry specification");
  localClass(class, NAME_members, NAME_organisation, "chain", NAME_none,
	     "Windows in the frame");
  localClass(class, NAME_destroying, NAME_internal, "bool", NAME_none,
	     "Handle destruction gracefully");
  localClass(class, NAME_kind, NAME_appearance,
	     "{toplevel,transient,popup}", NAME_get,
	     "Tool, support or popup");
  localClass(class, NAME_transientFor, NAME_transient, "frame*", NAME_get,
	     "Frame I'm transient for (i.e. support for)");
  localClass(class, NAME_transients, NAME_transient, "chain*", NAME_get,
	     "Back pointer for transient frames");
  localClass(class, NAME_returnValue, NAME_modal, "any", NAME_none,
	     "Bin for value of ->return");
  localClass(class, NAME_hasReturned, NAME_modal, "bool", NAME_none,
	     "->return has been executed");
  localClass(class, NAME_status, NAME_visibility,
	     "{unmapped,hidden,iconic,open}", NAME_get,
	     "Current visibility of the frame");
  localClass(class, NAME_canDelete, NAME_permission, "bool", NAME_both,
	     "Frame can be deleted by user");
  localClass(class, NAME_confirmDone, NAME_permission, "bool", NAME_both,
	     "Ask confirmation on user-delete");
  localClass(class, NAME_wmProtocols, NAME_windowManager, "sheet", NAME_get,
	     "Protocol-name --> message");
  localClass(class, NAME_wmProtocolsAttached, NAME_internal, "bool", NAME_get,
	     "Have we registered the protocols");
  localClass(class, NAME_wsRef, NAME_windowSystem, "alien:WsRef", NAME_none,
	     "Window-System reference");

  termClass(class, "frame", 3, NAME_label, NAME_kind, NAME_display);

  storeMethod(class, NAME_kind, kindFrame);
  storeMethod(class, NAME_status, statusFrame);
  storeMethod(class, NAME_transientFor, transientForFrame);
  storeMethod(class, NAME_geometry, geometryFrame);
  storeMethod(class, NAME_area, areaFrame);
  storeMethod(class, NAME_iconLabel, iconLabelFrame);
  storeMethod(class, NAME_iconPosition, iconPositionFrame);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "label=[name]", "kind=[{toplevel,transient,popup}]",
	     "display=[display]",
	     "Create from label, kind and display",
	     initialiseFrame);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy windows and related X-window",
	     unlinkFrame);
  sendMethod(class, NAME_bell, NAME_report, 1, "volume=[int]",
	     "Ring the bell on display",
	     bellFrame);
  sendMethod(class, NAME_create, NAME_open, 0,
	     "Establish X counterpart",
	     createFrame);
  sendMethod(class, NAME_open, NAME_open, 3,
	     "position=[point]", "grab=[bool]", "normalise=[bool]",
	     "->create and map on the display",
	     openFrame);
  sendMethod(class, NAME_openCentered, NAME_open, 2,
	     "center=[point]", "grab=[bool]",
	     "Open centered around point",
	     openCenteredFrame);
  sendMethod(class, NAME_closed, NAME_icon, 1, "open=bool",
	     "Open/iconify frame",
	     closedFrame);
  sendMethod(class, NAME_set, NAME_area, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Set XYWH of frame on display",
	     setFrame);
  sendMethod(class, NAME_x, NAME_area, 1, "x=int",
	     "Set X-coordinate of frame",
	     xFrame);
  sendMethod(class, NAME_y, NAME_area, 1, "x=int",
	     "Set Y-coordinate of frame",
	     yFrame);
  sendMethod(class, NAME_width, NAME_area, 1, "width=int",
	     "Set width of frame",
	     widthFrame);
  sendMethod(class, NAME_height, NAME_area, 1, "height=int",
	     "Set height of frame",
	     heightFrame);
  sendMethod(class, NAME_size, NAME_area, 1, "size=size",
	     "Resize the frame",
	     sizeFrame);
  sendMethod(class, NAME_position, NAME_area, 1, "position=point",
	     "Move the frame on the display",
	     positionFrame);
  sendMethod(class, NAME_move, NAME_area, 1, "position=point",
	     "Move the frame on the display",
	     positionFrame);
  sendMethod(class, NAME_center, NAME_area, 1, "center=[point]",
	     "Move the frame to make point its center",
	     centerFrame);
  sendMethod(class, NAME_append, NAME_organisation, 1, "subwindow=window",
	     "Append a window to the frame",
	     appendFrame);
  sendMethod(class, NAME_delete, NAME_organisation, 1, "member:window",
	     "Delete window from the frame",
	     deleteFrame);
  sendMethod(class, NAME_flush, NAME_animate, 0,
	     "Flush X-server",
	     flushFrame);
  sendMethod(class, NAME_synchronise, NAME_animate, 0,
	     "->flush and process pending events",
	     synchroniseFrame);
  sendMethod(class, NAME_show, NAME_visibility, 1, "show=bool",
	     "(Un)show the frame on the display",
	     showFrame);
  sendMethod(class, NAME_busyCursor, NAME_event, 2,
	     "cursor=[cursor]*", "block_input=[bool]",
	     "Define (temporary) cursor for all windows in the frame",
	     busyCursorFrame);
  sendMethod(class, NAME_reset, DEFAULT, 0,
	     "Remove ->busy_cursor",
	     resetFrame);
  sendMethod(class, NAME_border, NAME_appearance, 1, "thickness=int",
	     "X-border width",
	     borderFrame);
  sendMethod(class, NAME_showLabel, NAME_appearance, 1, "show=bool",
	     "If @off, sets <->kind to `transient'",
	     showLabelFrame);
  sendMethod(class, NAME_label, NAME_label, 2,
	     "label=name", "icon_label=[name]",
	     "Set label of the frame",
	     labelFrame);
  sendMethod(class, NAME_icon, NAME_icon, 2,
	     "image=image", "icon_label=[name]",
	     "Set image and icon_label",
	     iconFrame);
  sendMethod(class, NAME_expose, NAME_stacking, 0,
	     "Put frame above all others on the display",
	     exposeFrame);
  sendMethod(class, NAME_hide, NAME_stacking, 0,
	     "Put frame below all others on the display",
	     hideFrame);
  sendMethod(class, NAME_exposed, NAME_stacking, 0,
	     "Inform transient windows to expose",
	     exposedFrame);
  sendMethod(class, NAME_hidden, NAME_stacking, 0,
	     "Inform transient windows to hide",
	     hiddenFrame);
  sendMethod(class, NAME_return, NAME_modal, 1, "any",
	     "Return after a <-confirm",
	     returnFrame);
  sendMethod(class, NAME_Postscript, NAME_postscript, 0,
	     "Create PostScript for interior",
	     postscriptFrame);
  sendMethod(class, NAME_wmProtocol, NAME_windowManager, 2,
	     "protocol=name", "action=code",
	     "Register window manager protocol",
	     wmProtocolFrame);
  sendMethod(class, NAME_deleteWmProtocol, NAME_windowManager, 1,
	     "protocol=name",
	     "Delete window manager protocol",
	     deleteWmProtocolFrame);
  sendMethod(class, NAME_doneMessage, NAME_windowManager, 1, "action=code",
	     "Trap window manager WM_DELETE_WINDOW",
	     doneMessageFrame);
  sendMethod(class, NAME_saveMessage, NAME_windowManager, 1, "action=code",
	     "Trap window manager WM_SAVE_YOURSELF",
	     saveMessageFrame);
  sendMethod(class, NAME_wmDelete, NAME_windowManager, 0,
	     "Default handling for WM_DELETE_WINDOW",
	     wmDeleteFrame);
  sendMethod(class, NAME_fit, NAME_layout, 0,
	     "Recompute windows and resize the frame",
	     fitFrame);
  sendMethod(class, NAME_resize, NAME_layout, 0,
	     "Recompute layout of sub-windows",
	     resizeFrame);
  sendMethod(class, NAME_inputFocus, NAME_focus, 1, "window",
	     "Forward keyboard input to this window",
	     inputFocusFrame);
  sendMethod(class, NAME_mapped, NAME_open, 1, "bool",
	     "Inform transients using ->show",
	     mappedFrame);
  sendMethod(class, NAME_wait, NAME_open, 0,
	     "Wait till <-status is `open'",
	     waitFrame);
  sendMethod(class, NAME_typed, NAME_accelerator, 1, "event_id",
	     "Dispatch over available windows",
	     typedFrame);
  sendMethod(class, NAME_report, NAME_report, 3,
	     "{status,inform,progress,done,warning,error}",
	     "format=[char_array]", "argument=any ...",
	     "Report message (send to <-members)",
	     reportFrame);
  
  getMethod(class, NAME_size, NAME_area, "size", 0,
	    "Size on the display",
	    getSizeFrame);
  getMethod(class, NAME_position, NAME_area, "point", 0,
	    "Position on the display",
	    getPositionFrame);
  getMethod(class, NAME_closed, NAME_icon, "bool", 0,
	    "Open (@off) or iconic (@on)",
	    getClosedFrame);
  getMethod(class, NAME_show, NAME_visibility, "bool", 0,
	    "@on iff <-status = open; @off otherwise",
	    getShowFrame);
  getMethod(class, NAME_image, NAME_conversion, "image", 1,
	    "[{bitmap,pixmap}]",
	    "Image with the pixels of the frame",
	    getImageFrame);
  getMethod(class, NAME_confirm, NAME_modal, "return_value=any", 3,
	    "position=[point]", "grab=[bool]", "normalise=[bool]",
	    "Start sub-eventloop until ->return",
	    getConfirmFrame);
  getMethod(class, NAME_confirmCentered, NAME_modal, "return_value=any", 2,
	    "center=[point]", "grab=[bool]",
	    "As <-confirm, but centered around point",
	    getConfirmCenteredFrame);
  getMethod(class, NAME_member, NAME_organisation, "window", 1, "name",
	    "Find member window by name",
	    getMemberFrame);
  getMethod(class, NAME_members, NAME_organisation, "chain", 0,
	    "New chain holding all member windows",
	    getMembersFrame);
  getMethod(class, NAME_tile, NAME_layout, "tile", 0,
	    "Find tile managing object",
	    getTileFrame);
  getMethod(class, NAME_boundingBox, NAME_postscript, "area", 0,
	    "Bounding for PostScript",
	    getBoundingBoxFrame);
  getMethod(class, NAME_postscript, NAME_postscript, "postscript=string", 2,
	    "landscape=[bool]", "scale_in=[area]",
	    "Get PostScript representation of frame",
	    getPostscriptObject);
  getMethod(class, NAME_frame, NAME_organisation, "frame", 0,
	    "Returns itself",
	    getFrameFrame);
  getMethod(class, NAME_containedIn, DEFAULT, "display", 0,
	    "Display that contains me",
	    getContainedInFrame);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "Chain with windows contained",
	    getContainsFrame);
  getMethod(class, NAME_convert, DEFAULT, "frame", 1, "window",
	    "Frame of the window",
	    getConvertFrame);
  getMethod(class, NAME_inputFocus, NAME_event, "window", 0,
	    "Window with <-input_focus == @on",
	    getInputFocusFrame);
  getMethod(class, NAME_iconLabel, NAME_icon, "name", 0,
	    "Name of the icon",
	    getIconLabelFrame);
  getMethod(class, NAME_geometry, NAME_area, "name", 0,
	    "X-geometry specification",
	    getGeometryFrame);
  getMethod(class, NAME_iconPosition, NAME_icon, "point*", 0,
	    "(Current) position of the icon",
	    getIconPositionFrame);
  getMethod(class, NAME_catchAll, NAME_organisation, "window", 1,
	    "window_name=name",
	    "Get named window",
	    getCatchAllFramev);

  attach_resource(class, "confirm_done", "bool", "@on",
		  "Show confirmer on `Delete'");
  attach_resource(class, "icon_image", "image*", "pce.bm",
		  "Image displayed for an icon");
  attach_resource(class, "icon_label", "name*", "@nil",
		  "Label displayed in the icon");
  attach_resource(class, "geometry", "name*", "@nil",
		  "Position/size of the frame");
  attach_resource(class, "busy_cursor", "cursor*", "watch",
		  "Default cursor displayed by ->busy_cursor");

  succeed;
}

