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
#include <h/dialog.h>

static void	compute_slider(Slider, int *, int *, int *, int *,
			       int *, int *, int *, int *, int *);
static status	applySlider(Slider, Bool);
static status	restoreSlider(Slider s);
static Type	getTypeSlider(Slider s);
static status	displayedValueSlider(Slider s, Any val);
static status	widthSlider(Slider s, Int val);

#define SLIDER_HEIGHT 20
#define VALUE_GAP 20
#define BAR_WIDTH 5

#define OL_BAR_HEIGHT 5
#define OL_BOX_WIDTH 10

static status
initialiseSlider(Slider s, Name name, Any low, Any high, Any def, Message msg)
{ createDialogItem(s, name);

  assign(s, label_width,   DEFAULT);
  assign(s, show_label,    ON);
  assign(s, show_value,    ON);
  assign(s, low,	   low);
  assign(s, high,	   high);
  assign(s, message,	   msg);
  assign(s, width,	   toInt(200));
  assign(s, drag,	   OFF);
  assign(s, format,	   DEFAULT);

  assign(s, default_value, def);
  if ( !restoreSlider(s) )
  { assign(s, selection, s->low);
    displayedValueSlider(s, s->low);
  }

  return requestComputeGraphical(s, DEFAULT);
}


static double
convert_value(Any val)
{ return isInteger(val) ? (double)valInt(val) : valReal(val);
}


static void
format_value(Slider s, char *buf, Any val)
{ int deffmt = isDefault(s->format);

  if ( isInteger(val) )
    sprintf(buf, deffmt ? "%ld" : strName(s->format), valInt(val));
  else
    sprintf(buf, deffmt ? "%g"  : strName(s->format), valReal(val));
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
  int lflags = (s->active == ON ? 0 : LABEL_INACTIVE);

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

    RedrawLabelDialogItem(s,
			  accelerator_code(s->accelerator),
			  x, y+ny, vx-ex, 0,
			  s->label_format, NAME_top,
			  lflags);
  }
      
  if ( s->look == NAME_motif )
  { int by = y+sy+(SLIDER_HEIGHT-OL_BAR_HEIGHT)/2;
    int ex  = x + sx + valInt(s->width);
    Elevation z = getClassVariableValueObject(s, NAME_elevation);

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
    str_label(&str, 0, s->value_font,
	      x+vx, y+vy, 0, 0, NAME_left, NAME_top, lflags);
    format_value(s, buf, s->low);
    str_set_ascii(&str, buf);
    str_label(&str, 0, s->value_font,
	      x+lx, y+ly, 0, 0, NAME_left, NAME_top, lflags);
    format_value(s, buf, s->high);
    str_set_ascii(&str, buf);
    str_label(&str, 0, s->value_font,
	      x+hx, y+hy, 0, 0, NAME_left, NAME_top, lflags);
  }

  return RedrawAreaGraphical(s, a);
}


static void
compute_label_slider(Slider s, int *lw, int *lh)
{ if ( s->show_label == ON )
  { if ( isDefault(s->label_font) )
      obtainClassVariablesObject(s);

    dia_label_size(s, lw, lh, NULL);
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

    obtainClassVariablesObject(s);
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
  { int ny, vx, vy, lx, ly, sx, sy, hx, hy;
    int ascent;

    ComputeGraphical(s);
    compute_slider(s, &ny, &vx, &vy, &lx, &ly, &sx, &sy, &hx, &hy);
    ascent = valInt(getAscentFont(s->label_font));

    ref = answerObject(ClassPoint, ZERO, toInt(ascent + ny), EAV);
  }
  
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

static status
geometrySlider(Slider s, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) )
  { int extra;
    int width;

    ComputeGraphical(s);
    extra = valInt(s->area->w) - valInt(s->width);
    width = valInt(w) - extra;
    
    if ( width < 20 )
    { w = toInt(valInt(w) + 20 - width);
      width = 20;
    }

    widthSlider(s, toInt(width));
  }

  return geometryGraphical(s, x, y, w, DEFAULT);
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
    return send(s, NAME_focus, EAV);

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

/*  if ( ex > sx - 10 && ex < se + 10 ) */
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

      send(s, NAME_displayedValue, val, EAV);
      if ( isUpEvent(ev) && !send(s->device, NAME_modifiedItem, s, ON, EAV) )
	applySlider(s, ON);		/* TBD: or ->modified_item! */
      else if ( s->drag == ON && instanceOfObject(s->message, ClassCode) )
	forwardReceiverCode(s->message, s, s->displayed_value, EAV);
    }
  } else if ( isAEvent(ev, NAME_areaCancel) )
    sendv(s, NAME_displayedValue, 1, (Any *) &s->selection);

  fail;
}


		/********************************
		*          ATTRIBUTES		*
		********************************/


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
  { forwardReceiverCode(s->message, s, val, EAV);
    succeed;
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name", "low=int|real", "high=int|real", "value=int|real|function", "message=[code]*" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_slider[] =
{ SV(NAME_selection, "int|real", IV_GET|IV_STORE, selectionSlider,
     NAME_selection, "Current selection"),
  IV(NAME_default, "int|real|function", IV_NONE,
     NAME_apply, "The default selection or function to get it"),
  SV(NAME_displayedValue, "int|real", IV_GET|IV_STORE, displayedValueSlider,
     NAME_selection, "Currently displayed value"),
  SV(NAME_valueFont, "font", IV_GET|IV_STORE, valueFontSlider,
     NAME_appearance, "Font for current selection"),
  SV(NAME_showLabel, "bool", IV_GET|IV_STORE, showLabelSlider,
     NAME_appearance, "Whether label is shown"),
  SV(NAME_showValue, "bool", IV_GET|IV_STORE, showValueSlider,
     NAME_appearance, "Whether selection is shown"),
  SV(NAME_format, "[name]", IV_GET|IV_STORE, formatSlider,
     NAME_appearance, "Format for the printed values"),
  SV(NAME_low, "int|real", IV_GET|IV_STORE, lowSlider,
     NAME_selection, "Minimum of range"),
  SV(NAME_high, "int|real", IV_GET|IV_STORE, highSlider,
     NAME_selection, "Maximum of range"),
  SV(NAME_width, "int", IV_NONE|IV_STORE, widthSlider,
     NAME_area, "Length of the bar"),
  IV(NAME_drag, "bool", IV_BOTH,
     NAME_event, "Send messages while dragging")
};

/* Send Methods */

static senddecl send_slider[] =
{ SM(NAME_compute, 0, NULL, computeSlider,
     DEFAULT, "Compute desired size"),
  SM(NAME_event, 1, "event", eventSlider,
     DEFAULT, "Process an event"),
  SM(NAME_geometry, 4, T_geometry, geometrySlider,
     DEFAULT, "Adjust <-width"),
  SM(NAME_initialise, 5, T_initialise, initialiseSlider,
     DEFAULT, "Create from label, low, high, default and message"),
  SM(NAME_apply, 1, "always=[bool]", applySlider,
     NAME_apply, "->execute if <-modified or @on"),
  SM(NAME_default, 1, "value=int|real|function", defaultSlider,
     NAME_apply, "Set variable -default and ->selection"),
  SM(NAME_modified, 1, "bool", modifiedSlider,
     NAME_apply, "Reset modified flag"),
  SM(NAME_restore, 0, NULL, restoreSlider,
     NAME_apply, "Set ->selection to <-default"),
  SM(NAME_labelWidth, 1, "pixels=[int]", labelWidthSlider,
     NAME_layout, "Set width of label in pixels")
};

/* Get Methods */

static getdecl get_slider[] =
{ GM(NAME_reference, 0, "point", NULL, getReferenceSlider,
     DEFAULT, "Baseline of label"),
  GM(NAME_default, 0, "int|real", NULL, getDefaultSlider,
     NAME_apply, "Current default value"),
  GM(NAME_modified, 0, "bool", NULL, getModifiedSlider,
     NAME_apply, "If @on, slider has been modified"),
  GM(NAME_width, 0, "int", NULL, getWidthSlider,
     NAME_area, "Get width of bar"),
  GM(NAME_labelWidth, 0, "int", NULL, getLabelWidthSlider,
     NAME_layout, "Get minimal width required for label"),
  GM(NAME_selection, 0, "int|real", NULL, getSelectionSlider,
     NAME_selection, "Current value of the selection")
};

/* Resources */

static classvardecl rc_slider[] =
{ RC(NAME_look, RC_REFINE, "when(@colour_display, motif, open_look)", NULL)

};

/* Class Declaration */

static Name slider_termnames[] = { NAME_label, NAME_low, NAME_high, NAME_selection, NAME_message };

ClassDecl(slider_decls,
          var_slider, send_slider, get_slider, rc_slider,
          5, slider_termnames,
          "$Rev$");

status
makeClassSlider(Class class)
{ declareClass(class, &slider_decls);
  setRedrawFunctionClass(class, RedrawAreaSlider);

  succeed;
}

