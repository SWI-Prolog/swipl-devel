/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

TileObj		getTileFrame(FrameObj);
forwards int	get_position_from_center_frame(FrameObj, Point, int *, int *);
static void	ensure_on_display(FrameObj, int *, int *);
static status	closedFrame(FrameObj, Bool);
static status	openFrame(FrameObj fr, Point pos, Bool grab, Bool normalise);
static status	doneMessageFrame(FrameObj fr, Code msg);
static status	geometryFrame(FrameObj fr, Name spec);
static status	setFrame(FrameObj fr, Int x, Int y, Int w, Int h);
static status	flushFrame(FrameObj fr);
static status	kindFrame(FrameObj fr, Name kind);
static status	informTransientsFramev(FrameObj fr, Name selector,
				       int argc, Any *argv);
static status	grabPointerFrame(FrameObj fr, Bool grab, CursorObj cursor);
static status	cursorFrame(FrameObj fr, CursorObj cursor);
static status   statusFrame(FrameObj fr, Name stat);

#define isOpenFrameStatus(s) ((s) == NAME_window || (s) == NAME_fullScreen)

static status
initialiseFrame(FrameObj fr, Name label, Name kind,
		DisplayObj display,
		Application app)
{ if ( isDefault(kind) )
    kind = NAME_toplevel;
  if ( isDefault(display) )
    display = CurrentDisplay(NIL);
  if ( isDefault(label) )
    label = CtoName("Untitled");
  if ( isDefault(app) )
    app = NIL;

  assign(fr, name,	    getClassNameObject(fr));
  assign(fr, label,         label);
  assign(fr, display,       display);	/* Host on which to open frame */
  assign(fr, geometry,	    DEFAULT);	/* resources */
  assign(fr, icon_label,    DEFAULT);
  assign(fr, icon_image,    DEFAULT);
  assign(fr, border,	    DEFAULT);
  assign(fr, background,    DEFAULT);
  assign(fr, confirm_done,  DEFAULT);
  assign(fr, colour_map,    DEFAULT);
  assign(fr, area,	    newObject(ClassArea, 0));
  assign(fr, members,	    newObject(ClassChain, 0));
  assign(fr, kind,	    kind);
  assign(fr, status,	    NAME_unmapped);
  assign(fr, can_delete,    ON);
  assign(fr, input_focus,   OFF);
  assign(fr, wm_protocols,  newObject(ClassSheet, 0));
  assign(fr, wm_protocols_attached, OFF);

  doneMessageFrame(fr, newObject(ClassMessage, RECEIVER, NAME_wmDelete, 0));

  fr->ws_ref = NULL;			/* Window System Reference */

  if ( notNil(app) )
    send(app, NAME_append, fr, 0);

  succeed;
}


static status
unlinkFrame(FrameObj fr)
{ if ( fr->status != NAME_unlinking )
  { FrameObj sfr;
    PceWindow sw;
    Cell cell;

    assign(fr, status, NAME_unlinking);

    for_cell(cell, fr->members)		/* suppress any updates */
    { PceWindow sw = cell->value;

      assign(sw, displayed, OFF);
    }

    if ( notNil(fr->transients) )
      for_chain(fr->transients, sfr, send(sfr, NAME_free, 0));
    if ( notNil(fr->transient_for) && notNil(fr->transient_for->transients) )
      deleteChain(fr->transient_for->transients, fr);

    ws_uncreate_frame(fr);
    deleteChain(fr->display->frames, fr);
    if ( notNil(fr->application) && notNil(fr->application->members) )
      deleteChain(fr->application->members, fr);

    for_chain(fr->members, sw, freeObject(sw));

    unlinkedWindowEvent(fr);
  }

  succeed;
}


static FrameObj
getConvertFrame(Class class, PceWindow sw)
{ answer(getFrameWindow(sw, DEFAULT));
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
  assign(fr, input_focus, OFF);

  if ( isOpenFrameStatus(fr->status) )
  { assign(fr, status, NAME_unmapped);
    restoreMessage(newObject(ClassMessage, fr, NAME_open,
			     get(fr->area, NAME_position, 0), 0));
  }

  succeed;
}


static status
convertOldSlotFrame(FrameObj fr, Name var, Any value)
{ if ( var == NAME_show )
    assign(fr, status, value == ON ? NAME_open : NAME_hidden);

  succeed;
}


static status
initialiseNewSlotFrame(FrameObj fr, Variable var)
{ if ( var->name == NAME_background )
    assign(fr, background, getResourceValueObject(fr, NAME_background));

  succeed;
}

		/********************************
		*          OPEN/CREATE		*
		********************************/

static Constant ConstantNotReturned;

Any
getConfirmFrame(FrameObj fr, Point pos, Bool grab, Bool normalise)
{ Any rval;

  TRY( openFrame(fr, pos, grab, normalise) );
  busyCursorDisplay(fr->display, NIL, DEFAULT);

  assign(fr, return_value, ConstantNotReturned);
  synchroniseDisplay(fr->display);
  while( offFlag(fr, F_FREED|F_FREEING) &&
	 fr->return_value == ConstantNotReturned )
  { if ( dispatchDisplay(fr->display) )
      ws_discard_input("Confirmer running");
  }

  if ( onFlag(fr, F_FREED|F_FREEING) )
    fail;

  rval = fr->return_value;
  if ( isObject(rval) )
  { addCodeReference(rval);
    assign(fr, return_value, ConstantNotReturned);
    delCodeReference(rval);
    pushAnswerObject(rval);
  } else
    assign(fr, return_value, ConstantNotReturned);
  
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

  if ( !isOpenFrameStatus(fr->status) )
    statusFrame(fr, NAME_window);
  
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

  if ( t )
    send(t, NAME_layout, ZERO, ZERO, a->w, a->h, 0);

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
{ Any stat = (val == ON ? NAME_window : NAME_hidden);
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


static status
uncreateFrame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
    send(cell->value, NAME_uncreate, 0);

  ws_uncreate_frame(fr);
  succeed;
}


status
fitFrame(FrameObj fr)
{ TileObj t;
  Cell cell;
  Int border;

  TRY(t = getTileFrame(fr));
  enforceTile(t, OFF);

  for_cell(cell, fr->members)
    send(cell->value, NAME_ComputeDesiredSize, 0);

  enforceTile(t, ON);
  border = mul(t->border, TWO);

  assign(fr->area, w, ZERO);		/* ensure ->resize */

  return setFrame(fr, DEFAULT, DEFAULT,
		  add(t->idealWidth, border),
		  add(t->idealHeight, border));
}


static status
statusFrame(FrameObj fr, Name stat)
{ if ( stat != NAME_unmapped && !createdFrame(fr) )
    TRY(send(fr, NAME_create, 0));

  if ( stat == NAME_open )
    stat = NAME_window;

  if ( fr->status != stat )
  { int opened = (isOpenFrameStatus(stat) && !isOpenFrameStatus(fr->status));

    ws_status_frame(fr, stat);
    assign(fr, status, stat);

    if ( opened )
    { resizeFrame(fr);
      flushFrame(fr);
    }
  }
  
  succeed;
}


static status
frame_is_upto_date(FrameObj fr)
{ Cell cell;

  if ( fr->status == NAME_hidden )
    fail;
  
  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;
    
    if ( ChangedWindows && memberChain(ChangedWindows, sw) )
      fail;
  }

  succeed;
}


static status
waitFrame(FrameObj fr)
{ if ( fr->status == NAME_unmapped )
    TRY(send(fr, NAME_open, 0));

  while( !frame_is_upto_date(fr) )
  { if ( dispatchDisplay(fr->display) )
      ws_discard_input("Waiting for frame to open");
  }

  if ( isOpenFrameStatus(fr->status) )
    succeed;

  fail;					/* error? */
}


static status
showFrame(FrameObj fr, Bool val)
{ if ( val == ON )
  { if ( isOpenFrameStatus(fr->status) )
      succeed;
    else
      return statusFrame(fr, NAME_window);
  } else
    return statusFrame(fr, NAME_hidden);
}


static Bool
getShowFrame(FrameObj fr)
{ answer(isOpenFrameStatus(fr->status) ? ON : OFF);
}


		/********************************
		*           HIDE/EXPOSE		*
		********************************/

status
exposeFrame(FrameObj fr)
{ showFrame(fr, ON);
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
{ moveAfterChain(fr->display->frames, fr, DEFAULT);
  informTransientsFramev(fr, NAME_expose, 0, NULL);

  succeed;
}


static status
hiddenFrame(FrameObj fr)
{ moveAfterChain(fr->display->frames, fr, getTailChain(fr->display->frames));
  informTransientsFramev(fr, NAME_hide, 0, NULL);

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
{ if ( fr->border != width )
  { assign(fr, border, width);

    if ( ws_created_frame(fr) )
      ws_border_frame(fr, valInt(width));
  }

  succeed;
}


static status
backgroundFrame(FrameObj fr, Any bg)
{ if ( fr->background != bg )
  { assign(fr, background, bg);

    if ( ws_created_frame(fr) )
      ws_frame_background(fr, bg);
  }

  succeed;
}


static void
forwardColourMapChange(Device d)
{ Cell cell;

  if ( instanceOfObject(d, ClassWindow) )
    redrawWindow((PceWindow)d, DEFAULT);

  for_cell(cell, d->graphicals)
  { if ( instanceOfObject(cell->value, ClassDevice) )
      forwardColourMapChange(cell->value);
  }
}


status
forwardColourMapChangeFrame(FrameObj fr)
{ if ( !isFreedObj(fr) || isFreeingObj(fr) )
  { Cell cell;

    for_cell(cell, fr->members)
    { forwardColourMapChange(cell->value);
    }
  }

  succeed;
}


static status
colourMapFrame(FrameObj fr, ColourMap cm)
{ assign(fr, colour_map, cm);

  return forwardColourMapChangeFrame(fr);
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
{ if ( val == ON )
  { if ( isOpenFrameStatus(fr->status) )
      succeed;
    else
      return statusFrame(fr, NAME_window);
  } else
    return statusFrame(fr, NAME_iconic);
}


static Bool
getClosedFrame(FrameObj fr)
{ answer(fr->status == NAME_iconic ? ON : OFF);
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

    if ( getResourceValueObject(fr, NAME_fitAfterAppend) == ON )
      send(fr, NAME_fit, 0);
    else
      send(fr, NAME_resize, 0);

    if ( isOpenFrameStatus(fr->status) )
      send(sw, NAME_displayed, ON, 0);
  }

  succeed;
}


status
DeleteFrame(FrameObj fr, PceWindow sw)
{ if ( instanceOfObject(sw->device, ClassWindowDecorator) )
    return DeleteFrame(fr, (PceWindow) sw->device);

  if ( sw->frame != fr )
    return errorPce(fr, NAME_noMember, sw);

  deleteChain(fr->members, sw);
  assign(sw, frame, NIL);		/* may kill the frame */

  if ( !isFreedObj(fr) && createdFrame(fr) )
  { ws_unmanage_window(sw);
    TRY(send(sw, NAME_uncreate, 0));
    unrelateTile(sw->tile);
    if ( getResourceValueObject(fr, NAME_fitAfterAppend) == ON )
      send(fr, NAME_fit, 0);
    else
      send(fr, NAME_resize, 0);
  }

  succeed;
}


static status
deleteFrame(FrameObj fr, PceWindow sw)
{ if ( valInt(fr->members->size) <= 1 )
    fail;				/* cannot delete last (yet) */

  return DeleteFrame(fr, sw);
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


static PceWindow
getPointerWindowFrame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator)sw;
	
      sw = dw->window;
      if ( sw->has_pointer == ON )
	answer(sw);
    }

    if ( sw->has_pointer == ON )
      answer(sw);
  }

  fail;
}


static status
applicationFrame(FrameObj fr, Application app)
{ if ( fr->application != app )
  { if ( notNil(app) )
      return send(app, NAME_append, fr, 0);
    else if ( notNil(fr->application) )
      return send(fr->application, NAME_delete, fr, 0);
  }

  succeed;
}


		 /*******************************
		 *	   EVENT HANDLING	*
		 *******************************/

static status
keyboardFocusFrame(FrameObj fr, PceWindow sw)
{ if ( getHyperedObject(fr, NAME_keyboardFocus, DEFAULT) != sw )
    freeHypersObject(fr, NAME_keyboardFocus, DEFAULT);

  if ( instanceOfObject(sw, ClassWindow) )
  { newObject(ClassHyper, fr, sw, NAME_keyboardFocus, NAME_KeyboardFocus, 0);
    if ( fr->input_focus == ON )
      send(fr, NAME_inputWindow, sw, 0);
  } else if ( fr->input_focus == ON )
  { PceWindow iw = getPointerWindowFrame(fr);

    send(fr, NAME_inputWindow, iw, 0);
  }
  
  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the window  for  redirecting  keyboard   strokes.  If  there  is an
explicit focus, this is easy.  Otherwise,  use   the  window  that has a
keyboard-focus or the window that has a focus (in this order).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PceWindow
getKeyboardFocusFrame(FrameObj fr)
{ PceWindow sw;
  Cell cell;

  if ( (sw = getHyperedObject(fr, NAME_keyboardFocus, DEFAULT)) )
    answer(sw);

  if ( getSizeChain(fr->members) == ONE )
  { sw = getHeadChain(fr->members);

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator)sw;
      answer(dw->window);
    }
  }

  for_cell(cell, fr->members)
  { PceWindow sw2 = cell->value;

    if ( instanceOfObject(sw2, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator)sw2;
      sw2 = dw->window;
    }

    if ( notNil(sw2->focus) )
      answer(sw2);
  }

  answer(sw);
}


static status
inputWindowFrame(FrameObj fr, PceWindow iw)
{ Cell cell;

  if ( fr->input_focus == ON )
  { for_cell(cell, fr->members)
    { PceWindow sw = cell->value;

      inputFocusWindow(sw, sw == iw ? ON : OFF);

      if ( instanceOfObject(sw, ClassWindowDecorator) )
      { WindowDecorator dw = (WindowDecorator)sw;
	
	sw = dw->window;
	inputFocusWindow(sw, sw == iw ? ON : OFF);
      }
    }
  }

  succeed;
}


static status
inputFocusFrame(FrameObj fr, Bool val)
{ if ( fr->input_focus != val )
  { Cell cell;

    assign(fr, input_focus, val);
    if ( val == ON )
    { PceWindow iw;

      if ( (iw = getKeyboardFocusFrame(fr)) ||
	   (iw = ws_window_holding_point_frame(fr)) )
	inputWindowFrame(fr, iw);
    } else
    { for_cell(cell, fr->members)
      { PceWindow sw = cell->value;

	inputFocusWindow(cell->value, OFF);

	if ( instanceOfObject(sw, ClassWindowDecorator) )
	{ WindowDecorator dw = (WindowDecorator)sw;
	
	  sw = dw->window;
	  inputFocusWindow(sw, OFF);
	}
      }
    }
  }

  succeed;
}


static void
drawFeedbackLines(FrameObj fr)
{ Graphical gr = getAttributeObject(fr, NAME_ResizingFeedback);
  
  if ( gr && instanceOfObject(gr, ClassGraphical) )
    drawInDisplay(fr->display, gr, DEFAULT, ON, ON);
}


static status
cancelResizeTileFrame(FrameObj fr)
{ deleteAttributeObject(fr, NAME_ResizingFeedback);
  deleteAttributeObject(fr, NAME_ResizingTile);
  grabPointerFrame(fr, OFF, DEFAULT);
  grabServerDisplay(fr->display, OFF);

  succeed;
}


static status
resizeTileEventFrame(FrameObj fr, EventObj ev)
{ TileObj t;

  if ( (t=getAttributeObject(fr, NAME_ResizingTile)) &&
       instanceOfObject(t, ClassTile) )
  { if ( isDragEvent(ev) )
    { Device dev = getAttributeObject(fr, NAME_ResizingFeedback);
      Graphical l1 = getHeadChain(dev->graphicals);
      Graphical l2 = getTailChain(dev->graphicals);

      drawFeedbackLines(fr);		/* erase them */
      
      if ( t->super->orientation == NAME_vertical )
      { int y = valInt(getYEvent(ev, fr->display));

	if ( y < valInt(fr->area->y) ||
	     y > valInt(fr->area->y) + valInt(fr->area->h) )
	  return cancelResizeTileFrame(fr);

	yGraphical(l1, toInt(y-1));
	yGraphical(l2, toInt(y+1));
      } else
      { int x = valInt(getXEvent(ev, fr->display));
	
	if ( x < valInt(fr->area->x) ||
	     x > valInt(fr->area->x) + valInt(fr->area->w) )
	  return cancelResizeTileFrame(fr);

	xGraphical(l1, toInt(x-1));
	xGraphical(l2, toInt(x+1));
      }
      drawFeedbackLines(fr);
      succeed;
    } else if ( isUpEvent(ev) )
    { drawFeedbackLines(fr);
      cancelResizeTileFrame(fr);

      if ( t->super->orientation == NAME_vertical )
	send(t, NAME_height,
	     toInt(valInt(getYEvent(ev, fr)) - valInt(t->area->y) - 1), 0);
      else
	send(t, NAME_width,
	     toInt(valInt(getXEvent(ev, fr)) - valInt(t->area->x) - 1), 0);

      succeed;
    } else
      fail;
  } else
  { TileObj rzt;			/* resize-tile */

    t = getTileFrame(fr);
    if ( (rzt = getSubTileToResizeTile(t, getPositionEvent(ev, fr))) )
    { Name rcname = (rzt->super->orientation == NAME_horizontal
				? NAME_horizontalResizeCursor
				: NAME_verticalResizeCursor);
      CursorObj c = getResourceValueObject(fr, rcname);

      if ( !c )
	fail;

      cursorFrame(fr, c);
      if ( isDownEvent(ev) )
      { int x1, y1, x2, y2;
	int dx, dy;
	Device dev;

	attributeObject(fr, NAME_ResizingTile, rzt);

	if ( rzt->super->orientation == NAME_vertical )
	{ int xo = valInt(fr->area->x);

	  x1 = xo + valInt(rzt->area->x);
	  x2 = x1 + valInt(rzt->area->w);
	  y1 = y2 = valInt(getYEvent(ev, fr->display));
	  dx = 0; dy = 1;
	} else
	{ int yo = valInt(fr->area->y);

	  y1 = yo + valInt(rzt->area->y);
	  y2 = y1 + valInt(rzt->area->h);
	  x1 = x2 = valInt(getXEvent(ev, fr->display));
	  dx = 1; dy = 0;
	}
	attributeObject(fr, NAME_ResizingFeedback,
			dev = newObject(ClassDevice, 0));
	displayDevice(dev, newObject(ClassLine,
				     toInt(x1-dx), toInt(y1-dy),
				     toInt(x2-dx), toInt(y2-dy), 0),
		      DEFAULT);
	displayDevice(dev, newObject(ClassLine,
				     toInt(x1+dx), toInt(y1+dy),
				     toInt(x2+dx), toInt(y2+dy), 0),
		      DEFAULT);
	drawFeedbackLines(fr);
	grabPointerFrame(fr, ON, c);
	grabServerDisplay(fr->display, ON);
      }      

      succeed;
    } else
    { cursorFrame(fr, DEFAULT);

      fail;
    }
  }
}


status
blockedByModalFrame(FrameObj fr, EventObj ev)
{ FrameObj bfr;

  if ( notNil(fr->application) &&
       notNil(fr->application->modal) &&
       isOpenFrameStatus(fr->application->modal->status) &&
       fr->application->modal != fr )
  { bfr = fr->application->modal;
    goto blocked;
  } else
  { if ( notNil(fr->transients) )
    { Cell cell;

      for_cell(cell, fr->transients)
      { FrameObj fr2 = cell->value;
      
	if ( fr2->modal == NAME_transient &&
	     isOpenFrameStatus(fr2->status) )
	{ bfr = fr2;
	  goto blocked;
	}
      }
    }
  }

  fail;

blocked:
  if ( isAEvent(ev, NAME_button) )
  { send(bfr, NAME_expose, 0);
    if ( isUpEvent(ev) )		/* avoid multiple bells */
      send(fr, NAME_bell, 0);
  }    

  succeed;
}


status
eventFrame(FrameObj fr, EventObj ev)
{ if ( blockedByModalFrame(fr, ev) )
    fail;

  if ( isAEvent(ev, NAME_keyboard ) )
  { PceWindow sw;

    if ( (sw = getKeyboardFocusFrame(fr)) )
      return postEvent(ev, (Graphical) sw, DEFAULT);

    return send(fr, NAME_typed, ev->id, 0);
  }

  return resizeTileEventFrame(fr, ev);
}


static status
cursorFrame(FrameObj fr, CursorObj cursor)
{ ws_frame_cursor(fr, cursor);

  succeed;
}


static status
grabPointerFrame(FrameObj fr, Bool grab, CursorObj cursor)
{ ws_grab_frame_pointer(fr, grab, cursor);

  succeed;
}


static status
modalFrame(FrameObj fr, Name how)
{ assign(fr, modal, how);

  if ( notNil(fr->application) &&
       fr->application->modal == fr &&
       how != NAME_application )
  { assign(fr->application, modal, NIL);
  } else
  { if ( how == NAME_application && notNil(fr->application) )
      assign(fr->application, modal, fr);
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_report[] =
        { "{status,inform,progress,done,warning,error}", "format=[char_array]", "argument=any ..." };
static char *T_centerADpointD_grabADboolD[] =
        { "center=[point]", "grab=[bool]" };
static char *T_busyCursor[] =
        { "cursor=[cursor]*", "block_input=[bool]" };
static char *T_icon[] =
        { "image=image", "icon_label=[name]" };
static char *T_initialise[] =
        { "label=[name]",
	  "kind=[{toplevel,transient,popup}]",
	  "display=[display]",
	  "application=[application]"};
static char *T_label[] =
        { "label=name", "icon_label=[name]" };
static char *T_postscript[] =
        { "landscape=[bool]", "scale_in=[area]" };
static char *T_positionADpointD_grabADboolD_normaliseADboolD[] =
        { "position=[point]", "grab=[bool]", "normalise=[bool]" };
static char *T_wmProtocol[] =
        { "protocol=name", "action=code" };
static char *T_convertOldSlot[] =
        { "slot=name", "value=any" };
static char *T_set[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_grab_pointer[] =
	{ "grab=bool", "cursor=[cursor]" };

/* Instance Variables */

static vardecl var_frame[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_name, "Name of the frame"),
  IV(NAME_label, "name", IV_GET,
     NAME_label, "Label of the frame"),
  SV(NAME_iconLabel, "name*", IV_NONE|IV_STORE, iconLabelFrame,
     NAME_icon, "Label in the iconic representation"),
  IV(NAME_iconImage, "image*", IV_GET,
     NAME_icon, "Image used for the iconic representation"),
  SV(NAME_iconPosition, "point*", IV_GET|IV_STORE, iconPositionFrame,
     NAME_icon, "Position of the iconic image"),
  SV(NAME_application, "application*", IV_GET|IV_STORE, applicationFrame,
     NAME_organisation, "Application the frame belongs too"),
  IV(NAME_display, "display", IV_BOTH,
     NAME_organisation, "Display the frame resides on"),
  IV(NAME_border, "[int]", IV_GET,
     NAME_appearance, "Width of border"),
  SV(NAME_background, "colour|pixmap", IV_GET|IV_STORE, backgroundFrame,
     NAME_appearance, "Background of the frame"),
  SV(NAME_colourMap, "[colour_map]*", IV_GET|IV_STORE, colourMapFrame,
     NAME_appearance, "Colourmap for the window's frame"),
  SV(NAME_area, "area", IV_GET|IV_STORE, areaFrame,
     NAME_area, "Area of the opened frame on the display"),
  SV(NAME_geometry, "name*", IV_NONE|IV_STORE, geometryFrame,
     NAME_area, "X-window geometry specification"),
  IV(NAME_members, "chain", IV_NONE,
     NAME_organisation, "Windows in the frame"),
  SV(NAME_kind, "{toplevel,transient,popup}", IV_GET|IV_STORE, kindFrame,
     NAME_appearance, "Tool, support or popup"),
  SV(NAME_transientFor, "frame*", IV_GET|IV_STORE, transientForFrame,
     NAME_transient, "Frame I'm transient for (i.e. support for)"),
  IV(NAME_transients, "chain*", IV_GET,
     NAME_transient, "Back pointer for transient frames"),
  SV(NAME_modal, "{application,transient}*", IV_GET|IV_STORE, modalFrame,
     NAME_modal, "Operate as modal window"),
  IV(NAME_returnValue, "any", IV_NONE,
     NAME_modal, "Bin for value of ->return"),
  SV(NAME_inputFocus, "bool", IV_GET|IV_STORE, inputFocusFrame,
     NAME_event, "Frame has focus for keyboard events"),
  IV(NAME_status, "{unlinking,unmapped,hidden,iconic,window,full_screen}", IV_GET,
     NAME_visibility, "Current visibility of the frame"),
  IV(NAME_canDelete, "bool", IV_BOTH,
     NAME_permission, "Frame can be deleted by user"),
  IV(NAME_confirmDone, "bool", IV_BOTH,
     NAME_permission, "Ask confirmation on user-delete"),
  IV(NAME_wmProtocols, "sheet", IV_GET,
     NAME_windowManager, "Protocol-name --> message"),
  IV(NAME_wmProtocolsAttached, "bool", IV_GET,
     NAME_internal, "Have we registered the protocols"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_windowSystem, "Window-System reference")
};

/* Send Methods */

static senddecl send_frame[] =
{ SM(NAME_convertOldSlot, 2, T_convertOldSlot, convertOldSlotFrame,
     DEFAULT, "Convert old `show' slot"),
  SM(NAME_initialise, 4, T_initialise, initialiseFrame,
     DEFAULT, "Create from label, kind and display"),
  SM(NAME_initialiseNewSlot, 1, "var=variable", initialiseNewSlotFrame,
     DEFAULT, "Initialise <-background"),
  SM(NAME_reset, 0, NULL, resetFrame,
     DEFAULT, "Remove ->busy_cursor"),
  SM(NAME_unlink, 0, NULL, unlinkFrame,
     DEFAULT, "Destroy windows and related X-window"),
  SM(NAME_status, 1, "{unmapped,hidden,iconic,window,full_screen,open}",
     statusFrame, DEFAULT, "Current visibility of the frame"),
  SM(NAME_typed, 1, "event_id", typedFrame,
     NAME_accelerator, "Dispatch over available windows"),
  SM(NAME_flush, 0, NULL, flushFrame,
     NAME_animate, "Flush X-server"),
  SM(NAME_synchronise, 0, NULL, synchroniseFrame,
     NAME_animate, "->flush and process pending events"),
  SM(NAME_border, 1, "thickness=int", borderFrame,
     NAME_appearance, "X-border width"),
  SM(NAME_showLabel, 1, "show=bool", showLabelFrame,
     NAME_appearance, "If @off, sets <->kind to `transient'"),
  SM(NAME_center, 1, "center=[point]", centerFrame,
     NAME_area, "Move the frame to make point its center"),
  SM(NAME_height, 1, "height=int", heightFrame,
     NAME_area, "Set height of frame"),
  SM(NAME_move, 1, "position=point", positionFrame,
     NAME_area, "Move the frame on the display"),
  SM(NAME_position, 1, "position=point", positionFrame,
     NAME_area, "Move the frame on the display"),
  SM(NAME_set, 4, T_set, setFrame,
     NAME_area, "Set XYWH of frame on display"),
  SM(NAME_size, 1, "size=size", sizeFrame,
     NAME_area, "Resize the frame"),
  SM(NAME_width, 1, "width=int", widthFrame,
     NAME_area, "Set width of frame"),
  SM(NAME_x, 1, "x=int", xFrame,
     NAME_area, "Set X-coordinate of frame"),
  SM(NAME_y, 1, "x=int", yFrame,
     NAME_area, "Set Y-coordinate of frame"),
  SM(NAME_busyCursor, 2, T_busyCursor, busyCursorFrame,
     NAME_event, "Define (temporary) cursor for all windows in the frame"),
  SM(NAME_inputWindow, 1, "window", inputWindowFrame,
     NAME_focus, "Input is directed to this window"),
  SM(NAME_keyboardFocus, 1, "[window]*", keyboardFocusFrame,
     NAME_focus, "Redirect (default) keyboard input here"),
  SM(NAME_closed, 1, "open=bool", closedFrame,
     NAME_icon, "Open/iconify frame"),
  SM(NAME_icon, 2, T_icon, iconFrame,
     NAME_icon, "Set image and icon_label"),
  SM(NAME_label, 2, T_label, labelFrame,
     NAME_label, "Set label of the frame"),
  SM(NAME_fit, 0, NULL, fitFrame,
     NAME_layout, "Recompute windows and resize the frame"),
  SM(NAME_resize, 0, NULL, resizeFrame,
     NAME_layout, "Recompute layout of sub-windows"),
  SM(NAME_return, 1, "any", returnFrame,
     NAME_modal, "Return after a <-confirm"),
  SM(NAME_create, 0, NULL, createFrame,
     NAME_open, "Establish window-system counterpart"),
  SM(NAME_mapped, 1, "bool", mappedFrame,
     NAME_open, "Inform transients using ->show"),
  SM(NAME_open, 3, T_positionADpointD_grabADboolD_normaliseADboolD, openFrame,
     NAME_open, "->create and map on the display"),
  SM(NAME_openCentered, 2, T_centerADpointD_grabADboolD, openCenteredFrame,
     NAME_open, "Open centered around point"),
  SM(NAME_uncreate, 0, NULL, uncreateFrame,
     NAME_open, "Destroy window-system counterpart"),
  SM(NAME_wait, 0, NULL, waitFrame,
     NAME_open, "Wait till <-status is `open'"),
  SM(NAME_append, 1, "subwindow=window", appendFrame,
     NAME_organisation, "Append a window to the frame"),
  SM(NAME_delete, 1, "member:window", deleteFrame,
     NAME_organisation, "Delete window from the frame"),
  SM(NAME_Postscript, 0, NULL, postscriptFrame,
     NAME_postscript, "Create PostScript for interior"),
  SM(NAME_bell, 1, "volume=[int]", bellFrame,
     NAME_report, "Ring the bell on display"),
  SM(NAME_report, 3, T_report, reportFrame,
     NAME_report, "Report message (send to <-members)"),
  SM(NAME_expose, 0, NULL, exposeFrame,
     NAME_stacking, "Put frame above all others on the display"),
  SM(NAME_exposed, 0, NULL, exposedFrame,
     NAME_stacking, "Inform transient windows to expose"),
  SM(NAME_hidden, 0, NULL, hiddenFrame,
     NAME_stacking, "Inform transient windows to hide"),
  SM(NAME_hide, 0, NULL, hideFrame,
     NAME_stacking, "Put frame below all others on the display"),
  SM(NAME_show, 1, "show=bool", showFrame,
     NAME_visibility, "(Un)show the frame on the display"),
  SM(NAME_deleteWmProtocol, 1, "protocol=name", deleteWmProtocolFrame,
     NAME_windowManager, "Delete window manager protocol"),
  SM(NAME_doneMessage, 1, "action=code", doneMessageFrame,
     NAME_windowManager, "Trap window manager WM_DELETE_WINDOW"),
  SM(NAME_saveMessage, 1, "action=code", saveMessageFrame,
     NAME_windowManager, "Trap window manager WM_SAVE_YOURSELF"),
  SM(NAME_wmDelete, 0, NULL, wmDeleteFrame,
     NAME_windowManager, "Default handling for WM_DELETE_WINDOW"),
  SM(NAME_wmProtocol, 2, T_wmProtocol, wmProtocolFrame,
     NAME_windowManager, "Register window manager protocol"),
  SM(NAME_event, 1, "event", eventFrame,
     NAME_event, "Handle event on frame-background"),
  SM(NAME_cursor, 1, "[cursor]", cursorFrame,
     NAME_event, "Define the cursor for the frame-background"),
  SM(NAME_grabPointer, 2, T_grab_pointer, grabPointerFrame,
     NAME_event, "Grap all pointer-events")
};

/* Get Methods */

static getdecl get_frame[] =
{ GM(NAME_containedIn, 0, "display", NULL, getContainedInFrame,
     DEFAULT, "Display that contains me"),
  GM(NAME_contains, 0, "chain", NULL, getContainsFrame,
     DEFAULT, "Chain with windows contained"),
  GM(NAME_convert, 1, "frame", "window", getConvertFrame,
     DEFAULT, "Frame of the window"),
  GM(NAME_geometry, 0, "name", NULL, getGeometryFrame,
     NAME_area, "X-geometry specification"),
  GM(NAME_position, 0, "point", NULL, getPositionFrame,
     NAME_area, "Position on the display"),
  GM(NAME_size, 0, "size", NULL, getSizeFrame,
     NAME_area, "Size on the display"),
  GM(NAME_image, 1, "image", "[{bitmap,pixmap}]", getImageFrame,
     NAME_conversion, "Image with the pixels of the frame"),
  GM(NAME_keyboardFocus, 0, "window", NULL, getKeyboardFocusFrame,
     NAME_focus, "Window for default keyboard input"),
  GM(NAME_closed, 0, "bool", NULL, getClosedFrame,
     NAME_icon, "Open (@off) or iconic (@on)"),
  GM(NAME_iconLabel, 0, "name", NULL, getIconLabelFrame,
     NAME_icon, "Name of the icon"),
  GM(NAME_iconPosition, 0, "point*", NULL, getIconPositionFrame,
     NAME_icon, "(Current) position of the icon"),
  GM(NAME_tile, 0, "tile", NULL, getTileFrame,
     NAME_layout, "Find tile managing object"),
  GM(NAME_confirm, 3, "return_value=any", T_positionADpointD_grabADboolD_normaliseADboolD, getConfirmFrame,
     NAME_modal, "Start sub-eventloop until ->return"),
  GM(NAME_confirmCentered, 2, "return_value=any", T_centerADpointD_grabADboolD, getConfirmCenteredFrame,
     NAME_modal, "As <-confirm, but centered around point"),
  GM(NAME_catchAll, 1, "window", "window_name=name", getCatchAllFramev,
     NAME_organisation, "Get named window"),
  GM(NAME_frame, 0, "frame", NULL, getFrameFrame,
     NAME_organisation, "Returns itself"),
  GM(NAME_member, 1, "window", "name", getMemberFrame,
     NAME_organisation, "Find member window by name"),
  GM(NAME_members, 0, "chain", NULL, getMembersFrame,
     NAME_organisation, "New chain holding all member windows"),
  GM(NAME_boundingBox, 0, "area", NULL, getBoundingBoxFrame,
     NAME_postscript, "Bounding for PostScript"),
  GM(NAME_postscript, 2, "postscript=string", T_postscript, getPostscriptObject,
     NAME_postscript, "Get PostScript representation of frame"),
  GM(NAME_show, 0, "bool", NULL, getShowFrame,
     NAME_visibility, "@on iff <-status = open; @off otherwise")
};

/* Resources */

static resourcedecl rc_frame[] =
{ RC(NAME_background, "colour|pixmap", "white",
     "Default background colour"),
  RC(NAME_busyCursor, "cursor*", "watch",
     "Default cursor displayed by ->busy_cursor"),
  RC(NAME_confirmDone, "bool", "@on",
     "Show confirmer on `Delete'"),
  RC(NAME_geometry, "name*", "@nil",
     "Position/size of the frame"),
  RC(NAME_iconImage, "image*", "@pce_image",
     "Image displayed for an icon"),
  RC(NAME_iconLabel, "name*", "@nil",
     "Label displayed in the icon"),
  RC(NAME_horizontalResizeCursor, "cursor", "sb_h_double_arrow",
     "Cursor for horizontally resizing tile"),
  RC(NAME_verticalResizeCursor, "cursor", "sb_v_double_arrow",
     "Cursor for vertically resizing tile"),
  RC(NAME_fitAfterAppend, "bool", "@off",
     "Automatically ->fit the frame after a subwindow was added")
};

/* Class Declaration */

static Name frame_termnames[] = { NAME_label, NAME_kind, NAME_display };

ClassDecl(frame_decls,
          var_frame, send_frame, get_frame, rc_frame,
          3, frame_termnames,
          "$Rev$");


status
makeClassFrame(Class class)
{ declareClass(class, &frame_decls);
  setLoadStoreFunctionClass(class, loadFrame, storeFrame);

  ConstantNotReturned = globalObject(NAME_NotReturned, ClassConstant,
				     NAME_NotReturned,
				     CtoString("Used for `frame <-confirm'"),
				     0);
  succeed;
}

