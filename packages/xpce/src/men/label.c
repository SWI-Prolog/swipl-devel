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

static status	statusLabel(Label lb, Name stat);
static status	selectionLabel(Label lb, Any selection);

static status
initialiseLabel(Label lb, Name name, Any selection, FontObj font)
{ if ( isDefault(name) )
    name = NAME_reporter;
  if ( isDefault(selection) )	
    selection = NAME_;

  createDialogItem(lb, name);

  if ( notDefault(font) )
    assign(lb, font, font);
  selectionLabel(lb, selection);
  assign(lb, width, DEFAULT);
  
  return requestComputeGraphical(lb, DEFAULT);
}


static status
RedrawAreaLabel(Label lb, Area a)
{ int x, y, w, h;
  Elevation z = lb->elevation;
  int preview = (lb->status == NAME_preview && notNil(lb->message));

  initialiseDeviceGraphical(lb, &x, &y, &w, &h);

  if ( notNil(z) )
    r_3d_box(x, y, w, h, 0, z, !preview);

  x += valInt(lb->border);
  y += valInt(lb->border);

  if ( instanceOfObject(lb->selection, ClassCharArray) )
  { CharArray s = lb->selection;

    if ( notNil(z) )
      x += valInt(getExFont(lb->font))/2;

    str_label(&s->data, 0, lb->font, x, y, w, h, NAME_left, NAME_top,
	      lb->active == ON ? 0 : LABEL_INACTIVE);
  } else /*if ( instanceOfObject(lb->selection, ClassImage) )*/
  { Image image = (Image) lb->selection;

    r_image(image, 0, 0, x, y, w, h, ON);
  }

  if ( preview && isNil(z) )
    r_complement(x, y, w, h);    

  return RedrawAreaGraphical(lb, a);
}


static status
eventLabel(Label lb, EventObj ev)
{ if ( eventDialogItem(lb, ev) )
    succeed;

  if ( notNil(lb->message) && lb->active == ON )
  { makeButtonGesture();

    return eventGesture(GESTURE_button, ev);
  }

  fail;
}


static status
executeLabel(Label lb)
{ if ( notNil(lb->message) && notDefault(lb->message) )
  { statusLabel(lb, NAME_execute);
    flushGraphical(lb);
    forwardReceiverCode(lb->message, lb, EAV);
    if ( !isFreedObj(lb) )
    { statusLabel(lb, NAME_inactive);
      flushGraphical(lb);
    }
  }

  succeed;
}



static status
statusLabel(Label lb, Name stat)
{ if ( stat != lb->status )
  { Name oldstat = lb->status;

    assign(lb, status, stat);

    if ( oldstat == NAME_preview || stat == NAME_preview )
      changedDialogItem(lb);
  }

  succeed;
}


static status
computeLabel(Label lb)
{ if ( notNil(lb->request_compute) )
  { int w, h, b;

    TRY(obtainClassVariablesObject(lb));
    b = valInt(lb->border);
    if ( notNil(lb->elevation) )
      b += abs(valInt(lb->elevation->height));

    if ( instanceOfObject(lb->selection, ClassCharArray) )
    { CharArray s = (CharArray) lb->selection;
      int minw;
      int ex = valInt(getExFont(lb->font));

      str_size(&s->data, lb->font, &w, &h);
      w += ex;

      if ( notDefault(lb->width) )
	minw = valInt(lb->width) - 2*b;
      else
	minw = (valInt(lb->length)+1) * ex;

      w = max(w, minw);
    } else /*if ( instanceOfObject(lb->selection, ClassImage) )*/
    { Image image = (Image) lb->selection;

      w = valInt(image->size->w);
      h = valInt(image->size->h);
    }

    w += 2*b;
    h += 2*b;

    CHANGING_GRAPHICAL(lb,
	assign(lb->area, w, toInt(w));
	assign(lb->area, h, toInt(h));
	changedEntireImageGraphical(lb));

    assign(lb, request_compute, NIL);
  }

  succeed;
}


static Point
getReferenceLabel(Label lb)
{ Point ref;

  if ( !(ref = getReferenceDialogItem(lb)) )
  { if ( instanceOfObject(lb->selection, ClassCharArray) )
      ref = answerObject(ClassPoint,
			 ZERO, getAscentFont(lb->font), EAV);
    else
      ref = answerObject(ClassPoint,
			 ZERO, ((Image) lb->selection)->size->h, EAV);
  }

  answer(ref);
}



		/********************************
		*          ATTRIBUTES		*
		********************************/

static status
selectionLabel(Label lb, Any selection)
{ if ( lb->selection != selection )
  { assign(lb, selection, selection);
    requestComputeGraphical(lb, DEFAULT);
  }

  succeed;
}


static status
clearLabel(Label lb)
{ return selectionLabel(lb, CtoName(""));
}


static status
formatLabel(Label lb, CharArray fm, int argc, Any *argv)
{ ArgVector(av, argc+1);
  int ac;
  StringObj str;
  
  av[0] = fm;
  for(ac=1; ac <= argc; ac++)
    av[ac] = argv[ac-1];

  TRY(str = newObjectv(ClassString, ac, av));
  return selectionLabel(lb, str);
}


static status
fontLabel(Label lb, FontObj font)
{ if ( lb->font != font )
  { assign(lb, font, font);
    requestComputeGraphical(lb, DEFAULT);
  }

  succeed;
}


static status
lengthLabel(Label lb, Int length)
{ return assignGraphical(lb, NAME_length, length);
}


static status
widthLabel(Label lb, Int w)
{ return assignGraphical(lb, NAME_width, w);
}


static status
geometryLabel(Label lb, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) && valInt(w) > 0 )
    assign(lb, width, w);

  return geometryGraphical(lb, x, y, w, h);
}


static status
borderLabel(Label lb, Int border)
{ return assignGraphical(lb, NAME_border, border);
}


static status
elevationLabel(Label lb, Elevation e)
{ return assignGraphical(lb, NAME_elevation, e);
}


static status
catchAllLabelv(Label lb, Name selector, int argc, Any *argv)
{ if ( hasSendMethodObject(lb->selection, selector) )
  { status rval = sendv(lb->selection, selector, argc, argv);

    if ( rval )
      requestComputeGraphical(lb, DEFAULT);

    return rval;
  } else if ( instanceOfObject(lb->selection, ClassCharArray) &&
	      getSendMethodClass(ClassString, selector) )
  { status rval;

    assign(lb, selection, newObject(ClassString,
				    name_procent_s, lb->selection, EAV));
    if ( (rval = sendv(lb->selection, selector, argc, argv)) )
      requestComputeGraphical(lb, DEFAULT);

    return rval;
  }

  return errorPce(lb, NAME_noBehaviour, CtoName("->"), selector);
}


static status
reportLabel(Label lb, Name kind, CharArray fmt, int argc, Any *argv)
{ if ( isDefault(fmt) )
    fmt = (CharArray) (kind == NAME_done ? NAME_done : CtoName(""));

  if ( kind == NAME_done )
  { if ( instanceOfObject(lb->selection, ClassCharArray) )
    { CharArray t1 = getEnsureSuffixCharArray(lb->selection,
					      (CharArray) CtoName(" "));
      StringObj str;
      ArgVector(av, argc+1);
      int ac;
  
      av[0] = fmt;
      for(ac=1; ac <= argc; ac++)
	av[ac] = argv[ac-1];

      TRY(str = newObjectv(ClassString, ac, av));
      t1 = getAppendCharArray(t1, (CharArray) str);
      doneObject(str);

      selectionLabel(lb, t1);
      doneObject(t1);
    } else
      TRY(formatLabel(lb, fmt, argc, argv));

    flushGraphical(lb);
  } else
  { TRY(formatLabel(lb, fmt, argc, argv));

    if ( kind == NAME_error || kind == NAME_warning )
    { send(lb, NAME_flash, EAV); 
      alertReporteeVisual((VisualObj) lb);
    } else if ( kind == NAME_progress )
      flushGraphical(lb);
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=[name]", "selection=[string|image]", "font=[font]" };
static char *T_format[] =
        { "name", "any ..." };
static char *T_catchAll[] =
        { "name", "unchecked ..." };
static char *T_geometry[] =
	{ "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_label[] =
{ SV(NAME_font, "font", IV_GET|IV_STORE, fontLabel,
     NAME_appearance, "Font for selection"),
  SV(NAME_length, "0..", IV_GET|IV_STORE, lengthLabel,
     NAME_area, "Length in characters (with text)"),
  SV(NAME_width, "[0..]", IV_NONE|IV_STORE, widthLabel,
     NAME_area, "Width in pixels"),
  SV(NAME_selection, "char_array|image", IV_GET|IV_STORE, selectionLabel,
     NAME_selection, "Text or image displayed"),
  SV(NAME_border, "0..", IV_GET|IV_STORE, borderLabel,
     NAME_appearance, "Space around the image"),
  SV(NAME_elevation, "elevation*", IV_GET|IV_STORE, elevationLabel,
     NAME_appearance, "3D-Elevation of the area")
};

/* Send Methods */

static senddecl send_label[] =
{ SM(NAME_compute, 0, NULL, computeLabel,
     DEFAULT, "Recompute layout"),
  SM(NAME_status, 1, "{inactive,active,preview,execute}", statusLabel,
     DEFAULT, "Status for event-processing"),
  SM(NAME_event, 1, "event", eventLabel,
     DEFAULT, "Act as button if <-message not @nil"),
  SM(NAME_initialise, 3, T_initialise, initialiseLabel,
     DEFAULT, "Create from name, selection and font"),
  SM(NAME_geometry, 4, T_geometry, geometryLabel,
     DEFAULT, "Resize, sets ->width if not @default"),
  SM(NAME_catchAll, 2, T_catchAll, catchAllLabelv,
     NAME_delegate, "Delegate to <->selection"),
  SM(NAME_execute, 0, NULL, executeLabel,
     NAME_execute, "Execute associated message"),
  SM(NAME_format, 2, T_format, formatLabel,
     NAME_format, "Create string from format and make it the selection"),
  SM(NAME_report, 3, T_report, reportLabel,
     NAME_report, "Report message"),
  SM(NAME_clear, 0, NULL, clearLabel,
     NAME_selection, "Equivalent to ->selection: ''")
};

/* Get Methods */

static getdecl get_label[] =
{ GM(NAME_reference, 0, "point", NULL, getReferenceLabel,
     DEFAULT, "Baseline or bottom (image)")
};

/* Resources */

static classvardecl rc_label[] =
{ RC(NAME_border, "0..", "1",
     "Space around image/string"),
  RC(NAME_font, "font", "normal",
     "Default font for selection"),
  RC(NAME_length, "int", "25",
     "Default length in characters"),
  RC(NAME_elevation, RC_REFINE, "@nil", NULL)
};

/* Class Declaration */

static Name label_termnames[] = { NAME_name, NAME_selection, NAME_font };

ClassDecl(label_decls,
          var_label, send_label, get_label, rc_label,
          3, label_termnames,
          "$Rev$");

status
makeClassLabel(Class class)
{ declareClass(class, &label_decls);
  setRedrawFunctionClass(class, RedrawAreaLabel);

  succeed;
}

