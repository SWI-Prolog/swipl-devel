/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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
    selection = CtoName("");

  createDialogItem(lb, name);

  assign(lb, font,      font);
  assign(lb, length,    DEFAULT);
  assign(lb, border,    DEFAULT);
  selectionLabel(lb, selection);
  
  return requestComputeGraphical(lb, DEFAULT);
}


static status
RedrawAreaLabel(Label lb, Area a)
{ int x, y, w, h;
  Elevation z = getResourceValueObject(lb, NAME_elevation);
  int preview = (lb->status == NAME_preview && notNil(lb->message));

  initialiseDeviceGraphical(lb, &x, &y, &w, &h);

  if ( z && notNil(z) )
    r_3d_box(x, y, w, h, 0, z, !preview);

  x += valInt(lb->border);
  y += valInt(lb->border);

  if ( instanceOfObject(lb->selection, ClassCharArray) )
  { CharArray s = lb->selection;

    str_string(&s->data, lb->font, x, y, w, h, NAME_left, NAME_top);
  } else /*if ( instanceOfObject(lb->selection, ClassImage) )*/
  { Image image = (Image) lb->selection;

    r_image(image, 0, 0, x, y, w, h, ON);
  }

  if ( preview && !z )
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
    forwardReceiverCode(lb->message, lb, 0);
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
  { int w, h;

    TRY(obtainResourcesObject(lb));

    if ( instanceOfObject(lb->selection, ClassCharArray) )
    { CharArray s = (CharArray) lb->selection;
      int ex = valInt(getExFont(lb->font));

      str_size(&s->data, lb->font, &w, &h);
      w = max(w, valInt(lb->length) * ex);
    } else /*if ( instanceOfObject(lb->selection, ClassImage) )*/
    { Image image = (Image) lb->selection;

      w = valInt(image->size->w);
      h = valInt(image->size->h);
    }

    w += 2*valInt(lb->border);
    h += 2*valInt(lb->border);

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
			 ZERO, getAscentFont(lb->font), 0);
    else
      ref = answerObject(ClassPoint,
			 ZERO, ((Image) lb->selection)->size->h, 0);
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
{ if ( lb->length != length )
  { assign(lb, length, length);
    requestComputeGraphical(lb, DEFAULT);
  }

  succeed;
}


static status
borderLabel(Label lb, Int border)
{ return assignGraphical(lb, NAME_border, border);
}


static status
catchAllLabelv(Label lb, Name selector, int argc, Any *argv)
{ if ( hasSendMethodObject(lb->selection, selector) )
  { assign(PCE, last_error, NIL);

    if ( sendv(lb->selection, selector, argc, argv) )
      return requestComputeGraphical(lb, DEFAULT);
  } else if ( instanceOfObject(lb->selection, ClassCharArray) &&
	      getSendMethodClass(ClassString, selector) )
  { assign(lb, selection, newObject(ClassString,
				    name_procent_s, lb->selection, 0));
    assign(PCE, last_error, NIL);
    if ( sendv(lb->selection, selector, argc, argv) )
      return requestComputeGraphical(lb, DEFAULT);
  }

  fail;
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
    { send(lb, NAME_flash, 0); 
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

static const char *T_report[] =
        { "kind={status,inform,progress,done,warning,error}", "format=[char_array]", "argument=any ..." };
static const char *T_initialise[] =
        { "name=[name]", "selection=[string|image]", "font=[font]" };
static const char *T_format[] =
        { "name", "any ..." };
static const char *T_catchAll[] =
        { "name", "unchecked ..." };

/* Instance Variables */

static const vardecl var_label[] =
{ SV(NAME_font, "font", IV_GET|IV_STORE, fontLabel,
     NAME_appearance, "Font for selection"),
  SV(NAME_length, "int", IV_GET|IV_STORE, lengthLabel,
     NAME_area, "Length in characters (with text)"),
  SV(NAME_selection, "char_array|image", IV_GET|IV_STORE, selectionLabel,
     NAME_selection, "Text or image displayed"),
  SV(NAME_border, "0..", IV_GET|IV_STORE, borderLabel,
     NAME_appearance, "Space around the image")
};

/* Send Methods */

static const senddecl send_label[] =
{ SM(NAME_compute, 0, NULL, computeLabel,
     DEFAULT, "Recompute layout"),
  SM(NAME_status, 1, "{inactive,active,preview,execute}", statusLabel,
     DEFAULT, "Status for event-processing"),
  SM(NAME_event, 1, "event", eventLabel,
     DEFAULT, "Act as button if <-message not @nil"),
  SM(NAME_initialise, 3, T_initialise, initialiseLabel,
     DEFAULT, "Create from name, selection and font"),
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

static const getdecl get_label[] =
{ GM(NAME_reference, 0, "point", NULL, getReferenceLabel,
     DEFAULT, "Baseline or bottom (image)")
};

/* Resources */

static const resourcedecl rc_label[] =
{ RC(NAME_border, "0..", "0",
     "Space around image/string"),
  RC(NAME_font, "font", "normal",
     "Default font for selection"),
  RC(NAME_length, "int", "25",
     "Default length in characters"),
  RC(NAME_length, "int", "25",
     "Default length in characters")
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

