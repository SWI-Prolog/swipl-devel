/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

static void	compute_slider(Slider, int *, int *, int *, int *,
			       int *, int *, int *, int *, int *);
static status	applySlider(Slider, Bool);
static status	restoreSlider(Slider s);
static Type	getTypeSlider(Slider s);
static status	displayedValueSlider(Slider s, Any val);

#define SLIDER_HEIGHT 20
#define VALUE_GAP 20
#define BAR_WIDTH 5

#define OL_BAR_HEIGHT 5
#define OL_BOX_WIDTH 10

static status
initialiseSlider(Slider s, Name name, Any low, Any high, Any def, Message msg)
{ createDialogItem(s, name);

  assign(s, label_font,    DEFAULT);
  assign(s, label_width,   DEFAULT);
  assign(s, value_font,    DEFAULT);
  assign(s, show_label,    ON);
  assign(s, show_value,    ON);
  assign(s, format,	   DEFAULT);
  assign(s, low,	   low);
  assign(s, high,	   high);
  assign(s, message,	   msg);
  assign(s, width,	   toInt(200));
  assign(s, drag,	   OFF);

  assign(s, default_value, def);
  if ( !restoreSlider(s) )
  { assign(s, selection, s->low);
    displayedValueSlider(s, s->low);
  }

  return requestComputeGraphical(s, DEFAULT);
}


static float
convert_value(Any val)
{ return isInteger(val) ? (float)valInt(val) : ((Real)(val))->value;
}


static void
format_value(Slider s, char *buf, Any val)
{ int deffmt = isDefault(s->format);

  if ( isInteger(val) )
    sprintf(buf, deffmt ? "%ld" : strName(s->format), valInt(val));
  else
    sprintf(buf, deffmt ? "%f"  : strName(s->format), ((Real)val)->value);
}


static status
RedrawAreaSlider(Slider s, Area a)
{ int x, y, w, h;
  int ny, vx, vy, lx, ly, sx, sy, hx, hy;
  int vv;
  int bw = (s->look == NAME_x ? BAR_WIDTH : OL_BOX_WIDTH);
  float lv = convert_value(s->low);
  float hv = convert_value(s->high);
  float dv = convert_value(s->displayed_value);

  if ( dv < lv )
    dv = lv;
  else if ( dv > hv )
    dv = hv;

  if ( hv > lv )
    vv = rfloat(((float) (valInt(s->width) - bw) * (dv - lv)) / (hv - lv));
  else
    vv = 0;

  initialiseDeviceGraphical(s, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  r_thickness(valInt(s->pen));
  r_dash(s->texture);

  compute_slider(s, &ny, &vx, &vy, &lx, &ly, &sx, &sy, &hx, &hy);
  r_clear(x, y, w, h);

  if ( s->show_label == ON )
  { int ex = valInt(getExFont(s->label_font));

    str_string(&s->label->data, s->label_font, x, y+ny, vx-ex, 0,
	       s->label_format, NAME_top);
  }
      
  if ( s->look == NAME_motif )
  { int by = y+sy+(SLIDER_HEIGHT-OL_BAR_HEIGHT)/2;
    int ex  = x + sx + valInt(s->width);
    Elevation z = getResourceValueObject(s, NAME_elevation);

    r_3d_box(x+sx, by, vv, OL_BAR_HEIGHT, 0, z, FALSE);
    r_3d_box(x+sx+vv+bw, by, ex-(x+sx+vv+bw), OL_BAR_HEIGHT, 0, z, FALSE);
    r_3d_box(x+sx+vv, y+sy, bw, SLIDER_HEIGHT, 0, z, TRUE);
  } else if ( s->look == NAME_openLook )
  { int by = y+sy+(SLIDER_HEIGHT-OL_BAR_HEIGHT)/2;
    int ly2 = by+OL_BAR_HEIGHT-1;
    int ex  = x + sx + valInt(s->width);

    r_fill(x+sx, by+1, 1, OL_BAR_HEIGHT-2, BLACK_IMAGE);
    r_fill(x+sx+1, by, vv-2, OL_BAR_HEIGHT, BLACK_IMAGE);
    r_line(x+sx+1+vv+bw, by, ex-2, by);
    r_line(x+sx+1+vv+bw, ly2, ex-2, ly2);
    r_line(ex-1, by+1, ex-1, ly2-1);
    r_shadow_box(x+sx+vv, y+sy, bw, SLIDER_HEIGHT, 0, 1, NIL);
  } else
  { r_fill(x+sx, y+sy, vv, SLIDER_HEIGHT, GREY50_IMAGE);
    r_box(x+sx, y+sy, valInt(s->width), SLIDER_HEIGHT, 0, NIL);
    r_fill(x+sx+vv, y+sy, bw, SLIDER_HEIGHT, BLACK_IMAGE);
  }

  if ( s->show_value == ON )
  { char buf[100];
    string str;

    buf[0] = '[';
    format_value(s, &buf[1], s->displayed_value);
    strcat(buf, "]");
    str_set_ascii(&str, buf);
    str_string(&str, s->value_font, x+vx, y+vy, 0, 0, NAME_left, NAME_top);
    format_value(s, buf, s->low);
    str_set_ascii(&str, buf);
    str_string(&str, s->value_font, x+lx, y+ly, 0, 0, NAME_left, NAME_top);
    format_value(s, buf, s->high);
    str_set_ascii(&str, buf);
    str_string(&str, s->value_font, x+hx, y+hy, 0, 0, NAME_left, NAME_top);
  }

  return RedrawAreaGraphical(s, a);
}


static void
compute_label_slider(Slider s, int *lw, int *lh)
{ if ( s->show_label == ON )
  { str_size(&s->label->data, s->label_font, lw, lh);
    *lw += valInt(getExFont(s->label_font));
    if ( notDefault(s->label_width) )
      *lw = max(valInt(s->label_width), *lw);
  } else
  { *lw = *lh = 0;
  }
}


static void
compute_slider(Slider s, int *ny, int *vx, int *vy, int *lx, int *ly, int *sx, int *sy, int *hx, int *hy)
{ int hv = (s->show_value == ON ? valInt(getHeightFont(s->value_font)) : 0);
  int hl, hm;

  compute_label_slider(s, vx, &hl);
  hm = max(hl, max(hv, SLIDER_HEIGHT));

  *ny = (hm - hl) / 2;
  *sy = (hm - SLIDER_HEIGHT) / 2;
  *vy = *ly = *hy = (hm - hv) / 2;

  if ( s->show_value == ON )
  { int shw, slw, sh, cw = 4, tw;
    char buf[100];
    string str;

    buf[0] = '[';
    format_value(s, &buf[1], s->high);
    strcat(buf, "]");
    str_set_ascii(&str, buf);
    str_size(&str, s->value_font, &shw, &sh);
    format_value(s, buf, s->low);
    str_set_ascii(&str, buf);
    str_size(&str, s->value_font, &slw, &sh);
    if ( convert_value(s->low) < 0.0 &&
	 (tw = (c_width('-', s->value_font) + slw)) > shw )
      shw = tw;

    *lx = *vx + shw + cw;
    *sx = *lx + slw + cw;
    *hx = *sx + valInt(s->width) + cw;
  } else
  { *lx = *sx = *vx;
    *hx = *sx + valInt(s->width);
  }
}


static status
computeSlider(Slider s)
{ if ( notNil(s->request_compute) )
  { int ny, vx, vy, lx, ly, sx, sy, hx, hy;
    int w, h;
    int sw, sh;

    obtainResourcesObject(s);
    compute_slider(s, &ny, &vx, &vy, &lx, &ly, &sx, &sy, &hx, &hy);
    h = SLIDER_HEIGHT;
    h = max(h, ly+valInt(getHeightFont(s->label_font)));
    h = max(h, vy+valInt(getHeightFont(s->value_font)));

    if ( s->show_value == ON )
    { char buf[100];
      string str;

      sprintf(buf, "%ld", valInt(s->high));
      str_set_ascii(&str, buf);
      str_size(&str, s->value_font, &sw, &sh);
      w = hx + sw;
    } else
      w = hx;

    CHANGING_GRAPHICAL(s,
	  assign(s->area, w, toInt(w));
	  assign(s->area, h, toInt(h)));

    assign(s, request_compute, NIL);
  }

  succeed;
}


static Point
getReferenceSlider(Slider s)
{ Point ref;

  if ( !(ref = getReferenceDialogItem(s)) )
    ref = answerObject(ClassPoint, ZERO, getAscentFont(s->label_font));
  
  answer(ref);
}


static Int
getLabelWidthSlider(Slider s)
{ int lw, lh;

  compute_label_slider(s, &lw, &lh);
  answer(toInt(lw));
}


static status
labelWidthSlider(Slider s, Int w)
{ if ( s->show_label == ON && s->label_width != w )
  { assign(s, label_width, w); 
    CHANGING_GRAPHICAL(s,
	requestComputeGraphical(s, DEFAULT));
  }

  succeed;
}


		/********************************
		*        EVENT HANDLING		*
		********************************/

static status
displayedValueSlider(Slider s, Any val)
{ if ( s->displayed_value != val )
  { assign(s, displayed_value, val);

    changedDialogItem(s);    
  }

  succeed;
}


static status
eventSlider(Slider s, EventObj ev)
{ if ( eventDialogItem(s, ev) )
    succeed;

  if ( s->active == OFF )
    fail;

  if ( isAEvent(ev, NAME_msLeftDown) )
    return send(s, NAME_focus, 0);

  if ( isAEvent(ev, NAME_msLeft) &&
       hasModifierEvent(ev, findGlobal(NAME_ModifierAllUp)) )
  { int ny, vx, vy, lx, ly, sx, sy, hx, hy;
    int se;
    int ex;
    Int X, Y;

    get_xy_event(ev, s, ON, &X, &Y);
    ex = valInt(X);
    compute_slider(s, &ny, &vx, &vy, &lx, &ly, &sx, &sy, &hx, &hy);
    se = sx + valInt(s->width);

    if ( ex > sx - 10 && ex < se + 10 )
    { Any val;

      if ( ex < sx ) ex = sx;
      if ( ex > se ) ex = se;

      if ( isInteger(s->low) && isInteger(s->high) )
      { val = toInt(((ex - sx) * (valInt(s->high) - valInt(s->low)) /
		     (se - sx)) + valInt(s->low));
      } else
      { float l = convert_value(s->low);
	float h = convert_value(s->high);
	
	val = CtoReal(((float)(ex - sx) * (h - l) / (float) (se - sx)) + l);
      }

      send(s, NAME_displayedValue, val, 0);
      if ( isUpEvent(ev) && !send(s->device, NAME_modifiedItem, s, ON, 0) )
	applySlider(s, ON);		/* TBD: or ->modified_item! */
      else if ( s->drag == ON && instanceOfObject(s->message, ClassCode) )
	forwardReceiverCode(s->message, s, s->displayed_value, 0);
    }
  } else if ( isAEvent(ev, NAME_areaCancel) )
    sendv(s, NAME_displayedValue, 1, (Any *) &s->selection);

  fail;
}


		/********************************
		*          ATTRIBUTES		*
		********************************/


static status
labelFontSlider(Slider s, FontObj font)
{ return assignGraphical(s, NAME_labelFont, font);
}


static status
valueFontSlider(Slider s, FontObj font)
{ return assignGraphical(s, NAME_valueFont, font);
}


static status
showLabelSlider(Slider s, Bool val)
{ return assignGraphical(s, NAME_showLabel, val);
}


static status
showValueSlider(Slider s, Bool val)
{ return assignGraphical(s, NAME_showValue, val);
}


static status
formatSlider(Slider s, Name val)
{ return assignGraphical(s, NAME_format, val);
}


static status
lowSlider(Slider s, Any val)
{ return assignGraphical(s, NAME_low, val);
}


static status
selectionSlider(Slider s, Any val)
{ Any v;
  Type t = getTypeSlider(s);

  if ( (v = checkType(val, t, s)) )
  { assign(s, selection, val);
    displayedValueSlider(s, val);
  
    succeed;
  }

  return errorPce(t, NAME_unexpectedType, val);
}


static status
highSlider(Slider s, Any val)
{ return assignGraphical(s, NAME_high, val);
}


static status
widthSlider(Slider s, Int val)
{ return assignGraphical(s, NAME_width, val);
}


static Int
getWidthSlider(Slider s)
{ answer(s->width);
}


		/********************************
		*         COMMUNICATION		*
		********************************/

static Any
getSelectionSlider(Slider s)
{ assign(s, selection, s->displayed_value);

  answer(s->selection);
}


static Bool
getModifiedSlider(Slider s)
{ answer(s->selection == s->displayed_value ? OFF : ON);
}


static status
modifiedSlider(Slider s, Bool val)
{ if ( val == OFF )
    displayedValueSlider(s, s->selection);

  succeed;
}


static Type
getTypeSlider(Slider s)
{ if ( isInteger(s->low) && isInteger(s->high) )
    answer(TypeInt);
  
  answer(TypeReal);
}


static Any
getDefaultSlider(Slider s)
{ answer(checkType(s->default_value, getTypeSlider(s), s));
}


static status
defaultSlider(Slider s, Any val)
{ if ( s->default_value != val )
  { assign(s, default_value, val);

    return restoreSlider(s);
  }

  succeed;
}


static status
restoreSlider(Slider s)
{ Any val;

  if ( (val = getDefaultSlider(s)) )
    return selectionSlider(s, val);

  fail;
}


static status
applySlider(Slider s, Bool always)
{ Any val;

  if ( instanceOfObject(s->message, ClassCode) &&
       (always == ON || getModifiedSlider(s) == ON) &&
       (val = getSelectionSlider(s)) )
  { forwardReceiverCode(s->message, s, val, 0);
    succeed;
  }

  fail;
}


status
makeClassSlider(Class class)
{ sourceClass(class, makeClassSlider, __FILE__, "$Revision$");

  localClass(class, NAME_selection, NAME_selection, "int|real", NAME_get,
	     "Current selection");
  localClass(class, NAME_default, NAME_apply, "int|real|function", NAME_none,
	     "The default selection or function to get it");
  localClass(class, NAME_displayedValue, NAME_selection, "int|real", NAME_get,
	     "Currently displayed value");
  localClass(class, NAME_labelFont, NAME_appearance, "font", NAME_get,
	     "Font for label");
  localClass(class, NAME_valueFont, NAME_appearance, "font", NAME_get,
	     "Font for current selection");
  localClass(class, NAME_showLabel, NAME_appearance, "bool", NAME_get,
	     "Whether label is shown");
  localClass(class, NAME_showValue, NAME_appearance, "bool", NAME_get,
	     "Whether selection is shown");
  localClass(class, NAME_format, NAME_appearance, "[name]", NAME_get,
	     "Format for the printed values");
  localClass(class, NAME_low, NAME_selection, "int|real", NAME_get,
	     "Minimum of range");
  localClass(class, NAME_high, NAME_selection, "int|real", NAME_get,
	     "Maximum of range");
  localClass(class, NAME_width, NAME_area, "int", NAME_none,
	     "Length of the bar");
  localClass(class, NAME_drag, NAME_event, "bool", NAME_both,
	     "Send messages while dragging");

  termClass(class, "slider", 5,
	    NAME_label, NAME_low, NAME_high, NAME_selection, NAME_message);
  setRedrawFunctionClass(class, RedrawAreaSlider);

  storeMethod(class, NAME_labelFont, 	  labelFontSlider);
  storeMethod(class, NAME_valueFont, 	  valueFontSlider);
  storeMethod(class, NAME_showLabel, 	  showLabelSlider);
  storeMethod(class, NAME_showValue, 	  showValueSlider);
  storeMethod(class, NAME_format,	  formatSlider);
  storeMethod(class, NAME_low,       	  lowSlider);
  storeMethod(class, NAME_high,      	  highSlider);
  storeMethod(class, NAME_selection, 	  selectionSlider);
  storeMethod(class, NAME_width,     	  widthSlider);
  storeMethod(class, NAME_displayedValue, displayedValueSlider);

  sendMethod(class, NAME_initialise, DEFAULT, 5,
	     "name","low=int|real", "high=int|real", "value=int|real|function",
	     "message=[code]*",
	     "Create from label, low, high, default and message",
	     initialiseSlider);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Compute desired size",
	     computeSlider);
  sendMethod(class, NAME_event, DEFAULT, 1, "event",
	     "Process an event",
	     eventSlider);
  sendMethod(class, NAME_labelWidth, NAME_layout, 1, "pixels=[int]",
	     "Set width of label in pixels",
	     labelWidthSlider);
  sendMethod(class, NAME_restore, NAME_apply, 0,
	     "Set ->selection to <-default",
	     restoreSlider);
  sendMethod(class, NAME_default, NAME_apply, 1, "value=int|real|function",
	     "Set variable -default and ->selection",
	     defaultSlider);
  sendMethod(class, NAME_apply, NAME_apply, 1, "always=[bool]",
	     "->execute if <-modified or @on",
	     applySlider);
  sendMethod(class, NAME_modified, NAME_apply, 1, "bool",
	     "Reset modified flag",
	     modifiedSlider);

  getMethod(class, NAME_labelWidth, NAME_layout, "int", 0,
	    "Get minimal width required for label",
	    getLabelWidthSlider);
  getMethod(class, NAME_width, NAME_area, "int", 0,
	    "Get width of bar",
	    getWidthSlider);
  getMethod(class, NAME_selection, NAME_selection, "int|real", 0,
	    "Current value of the selection",
	    getSelectionSlider);
  getMethod(class, NAME_modified, NAME_apply, "bool", 0,
	    "If @on, slider has been modified",
	    getModifiedSlider);
  getMethod(class, NAME_default, NAME_apply, "int|real", 0,
	    "Current default value",
	    getDefaultSlider);
  getMethod(class, NAME_reference, DEFAULT, "point", 0,
	    "Baseline of label",
	    getReferenceSlider);

  succeed;
}

