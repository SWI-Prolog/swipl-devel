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

static status	nameDialogItem(DialogItem di, Name name);

		/********************************
		*         CREATE/DESTROY	*
		********************************/

status
createDialogItem(Any obj, Name name)
{ DialogItem di = obj;

  initialiseGraphical(di, ZERO, ZERO, ZERO, ZERO);

  if ( isDefault(name) )
    name = getClassNameObject(di);
  nameDialogItem(di, name);

  assign(di, status,		 NAME_inactive);
/*assign(di, message,		 NIL);
  assign(di, popup,		 NIL);
  assign(di, above,		 NIL);
  assign(di, below,		 NIL);
  assign(di, right,		 NIL);
  assign(di, left,		 NIL); */
  assign(di, reference,		 DEFAULT);
  assign(di, label_width,	 DEFAULT);
  assign(di, auto_label_align,	 ON);
  assign(di, auto_value_align,	 ON);
  assign(di, auto_align,	 ON);

  succeed;
}


status
unlinkDialogItem(DialogItem di)
{ return unlinkGraphical((Graphical)di);
}


status
RedrawLabelDialogItem(Any obj, int acc,
		      int x, int y, int w, int h,
		      Name hadjust, Name vadjust, int flags)
{ DialogItem di = obj;

  if ( instanceOfObject(di->label, ClassImage) )
  { Image i = di->label;
    int iw = valInt(i->size->w);
    int ih = valInt(i->size->h);
    int ix, iy;

    if ( hadjust == NAME_left )
      ix = x;
    else if ( hadjust == NAME_center )
      ix = x + (w-iw)/2;
    else 
      ix = x + w-iw;
    
    if ( vadjust == NAME_top )
      iy = y;
    else if ( vadjust == NAME_center )
      iy = y + (h-ih)/2;
    else 
      iy = y + h-ih;
    
    r_image(i, 0, 0, ix, iy, iw, ih, ON);
  } else if ( instanceOfObject(di->label, ClassCharArray) )
  { CharArray label = di->label;

    str_label(&label->data, acc, di->label_font,
	      x, y, w, h,
	      hadjust, vadjust, flags);
  }

  succeed;
}


status
dia_label_size(Any obj, int *w, int *h, int *isimage)
{ DialogItem di = obj;

  if ( instanceOfObject(di->label, ClassImage) )
  { Image i = di->label;

    *w = valInt(i->size->w);
    *h = valInt(i->size->h);
    if ( isimage )
      *isimage = TRUE;
  } else
  { if ( isimage )
      *isimage = FALSE;

    if ( instanceOfObject(di->label, ClassCharArray) )
    { CharArray ca = di->label;

      str_size(&ca->data, di->label_font, w, h);
    } else
    { *w = *h = 0;
    }
  }

  succeed;
}



static status
deviceDialogItem(DialogItem di, Device dev)
{ if ( di->device != dev && notNil(di->device) )
  { Graphical gr = (Graphical) di;

    aboveGraphical(gr, NIL);
    belowGraphical(gr, NIL);
    rightGraphical(gr, NIL);
    leftGraphical(gr, NIL);
  }

  return deviceGraphical((Graphical)di, dev);
}


		/********************************
		*          ATTRIBUTES		*
		********************************/

status
labelDialogItem(DialogItem di, Any label)
{ return assignGraphical(di, NAME_label, label);
}


status
labelFontDialogItem(DialogItem di, FontObj font)
{ return assignGraphical(di, NAME_labelFont, font);
}


static CharArray
getLabelNameDialogItem(DialogItem di, Name name)
{ Any suffix, label = GetLabelNameName(name);

  if ( !label || !instanceOfObject(label, ClassCharArray) )
    label = name;			/* play safe */

  if ( (suffix = getClassVariableValueObject(di, NAME_labelSuffix)) )
    label = getEnsureSuffixCharArray(label, suffix);

  answer(label);
}


static status
nameDialogItem(DialogItem di, Name name)
{ Any label = get(di, NAME_labelName, name, EAV);

  assign(di, name, name);
  if ( !label )
    label = name;

  return sendv(di, NAME_label, 1, &label);
}


static status
lookDialogItem(DialogItem di, Name look)
{ return assignGraphical(di, NAME_look, look);
}


static status
acceleratorDialogItem(DialogItem di, Name acc)
{ return assignGraphical(di, NAME_accelerator, acc);
}


static status
backgroundDialogItem(DialogItem di, Any bg)
{ return assignGraphical(di, NAME_background, bg);
}


static status
labelFormatDialogItem(DialogItem di, Name format)
{ return assignGraphical(di, NAME_labelFormat, format);
}


		/********************************
		*        EVENT_HANDLING		*
		********************************/

static status
advanceEventDialogItem(Any obj, EventObj ev)
{ if ( (ev->id == toInt(9) ||
	ev->id == NAME_cursorRight ||
	ev->id == NAME_cursorLeft) &&
       getKeyboardFocusGraphical(obj) == ON )
  { Name dir = (ev->id == NAME_cursorLeft ? NAME_backwards : NAME_forwards);
    Device dev = ((Graphical)obj)->device;
  
    send(dev, NAME_advance, obj, DEFAULT, dir, EAV);

    succeed;
  }

  fail;
}


status
eventDialogItem(Any obj, EventObj ev)
{ DialogItem di = obj;
  
  if ( eventGraphical(di, ev) )
    succeed;
  if ( advanceEventDialogItem(obj, ev) )
    succeed;

  if ( di->active == ON && notNil(di->popup) && isDownEvent(ev) &&
       send(popupGesture(), NAME_event, ev, EAV) )
    succeed;

  fail;
}


static status
statusDialogItem(DialogItem di, Name stat)
{ assign(di, status, stat);
  changedDialogItem(di);

  succeed;
}


static status
cancelDialogItem(DialogItem di)
{ return send(di, NAME_status, NAME_inactive, EAV);
}


		/********************************
		*           REPAINT		*
		********************************/


status
changedDialogItem(Any obj)
{ DialogItem di = obj;

  CHANGING_GRAPHICAL(di,
	changedEntireImageGraphical(di));

  succeed;
}


static status
showDialogItem(DialogItem di, Bool val)
{ if ( val == OFF )
  { PceWindow sw = getWindowGraphical((Graphical) di);

    if ( sw != FAIL && sw->keyboard_focus == (Graphical) di )
      send(di->device, NAME_advance, di, EAV);
  }

  return DisplayedGraphical(di, val);
}


static Bool
getShowDialogItem(DialogItem di)
{ answer(di->displayed);
}


		/********************************
		*          POSITIONS		*
		********************************/

Point
getReferenceDialogItem(Any obj)
{ DialogItem i = obj;

  ComputeGraphical(i);
  if ( notDefault(i->reference) )
    answer(i->reference);

  fail;
}


static status
resetDialogItem(DialogItem i)
{ send(i, NAME_status, NAME_inactive, EAV);

  succeed;
}


static status
openDialogItem(DialogItem di)
{ if ( isNil(di->device) )
  { Dialog d;

    TRY( d = newObject(ClassDialog, EAV) );
    TRY( send(d, NAME_append, di, EAV) );
  }

  return send(di->device, NAME_open, EAV);
}


		/********************************
		*        COMMUNICATION		*
		********************************/

static Bool
getModifiedDialogItem(Dialog di)
{ answer(OFF);
}


status
modifiedDialogItem(Any di, Bool modified)
{ Dialog d = di;

  if ( modified == ON )
    return send(d->device, NAME_modifiedItem, d, ON, EAV);

  succeed;
}


		 /*******************************
		 *	   ACCELERATORS		*
		 *******************************/

#define ACC_WSEP  0
#define ACC_UPPER 1
#define ACC_LOWER 2
#define ACC_DIGIT 3

#define ACC_CHARSETSIZE 256

typedef struct
{ int	      acc;
  int	      index;
  int	      mode;			/* ACC_UPPER, ACC_ALNUM */
  const char *label;
  Any	      object;
} abin, *Abin;


static status
acc_index(Abin a, unsigned char *used)
{ int i;

  if ( a->mode == ACC_WSEP )
  { i = a->index+1;

    do
    { int acc = a->label[i];
      
      if ( isletter(acc) )
      { acc = tolower(acc);
  
	if ( !(used && used[acc]) )
	{ a->index = i;
	  a->acc = acc;
	  succeed;
	}
      }
      while( a->label[i] && !islayout(a->label[i]) )
	i++;
      while( a->label[i] && islayout(a->label[i]) )
	i++;
    } while( a->label[i] );

    a->mode = ACC_UPPER;
  }

  if ( a->mode == ACC_UPPER )
  { for( i = a->index+1; a->label[i]; i++ )
    { int acc = a->label[i];
  
      if ( isupper(acc) )
      { acc = tolower(acc);
  
	if ( used && used[acc] )
	  continue;
  
	a->index = i;
	a->acc = acc;
	succeed;
      }
    }

    a->mode = ACC_LOWER;
  }

  if ( a->mode == ACC_LOWER )
  { for( i = a->index+1; a->label[i]; i++ )
    { int acc = a->label[i];

      if ( islower(acc) )
      { if ( used && used[acc] )
	  continue;

	a->index = i;
	a->acc = acc;
	succeed;
      }
    }

    a->mode = ACC_DIGIT;
  }


  for( i = a->index+1; a->label[i]; i++ )
  { int acc = a->label[i];

    if ( isdigit(acc) )
    { if ( used && used[acc] )
	continue;

      a->index = i;
      a->acc = acc;
      succeed;
    }
  }

  fail;
}


status
assignAccelerators(Chain objects, Name prefix, Name label_method)
{ int  size = valInt(objects->size);
  Abin bins = alloca(sizeof(abin) * size);
  int  n;
  Cell cell;
  Abin a = bins;
  unsigned char used[ACC_CHARSETSIZE];
  int do_free = FALSE;

  if ( size && !bins )
  { bins = pceMalloc(sizeof(abin) * size);
    do_free = TRUE;
  }

  for(n=0; n<ACC_CHARSETSIZE; n++)
    used[n] = 0;

  for_cell(cell, objects)
  { Any lbl;
    const char *s;

    if ( !hasSendMethodObject(cell->value, NAME_accelerator) )
      continue;

    if ( hasGetMethodObject(cell->value, label_method) &&
	 (lbl = get(cell->value, label_method, EAV)) &&
	 ( !instanceOfObject(lbl, ClassCharArray) ||
	   !((CharArray)lbl)->data.b16 ) &&
	 (s = toCharp(lbl)) )
    { a->label = s;
      a->index = -1;
      a->mode  = ACC_WSEP;
      if ( acc_index(a, NULL) )
      { used[tolower(a->acc)]++;
	a->object = cell->value;
	DEBUG(NAME_accelerator,
	      Cprintf("Proposing %c for %s\n", a->acc, pp(cell->value)));
	a++;
      } else
	send(cell->value, NAME_accelerator, NIL, EAV);
    } else
      send(cell->value, NAME_accelerator, NIL, EAV);
  }

  size = a - bins;
  DEBUG(NAME_accelerator,
	Cprintf("Trying to find accelerators for %d objects\n", size));

  for(n=0; n<size; n++)
  { int acc = bins[n].acc;

    if ( used[acc] > 1 )
    { int m;

      for( m=n+1; m<size; m++ )
      { if ( acc == bins[m].acc )
	{ if ( acc_index(&bins[m], used) )
	    used[bins[m].acc] = 1;
	  else
	    bins[m].acc = 0;

	  used[acc]--;
	}
      }
    }
  }

  for(n=0; n<size; n++)
  { int acc = bins[n].acc;

    if ( acc > 0 )
    { char buf[100];

      sprintf(buf, "%s%c", strName(prefix), acc);
      send(bins[n].object, NAME_accelerator, CtoKeyword(buf), EAV);
    } else
      send(bins[n].object, NAME_accelerator, NIL, EAV);
  }

  if ( do_free )
    pceFree(bins);

  succeed;
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_dialogItem[] =
{ SV(NAME_label, "char_array|image*", IV_GET|IV_STORE, labelDialogItem,
     NAME_label, "Label of the item"),
  SV(NAME_labelFont, "font*", IV_GET|IV_STORE, labelFontDialogItem,
     NAME_label, "Font used for the label if not an image"),
  IV(NAME_labelWidth, "[int]", IV_NONE,
     NAME_layout, "Width of the label in pixels"),
  SV(NAME_labelFormat, "{left,center,right}", IV_GET|IV_STORE,
     labelFormatDialogItem,
     NAME_layout, "Align labels in their box"),
  SV(NAME_background, "image|colour*", IV_GET|IV_STORE, backgroundDialogItem,
     NAME_appearance, "Opaque background for item"),
  SV(NAME_status, "{inactive,active,preview,execute}", IV_GET|IV_STORE,
     statusDialogItem,
     NAME_event, "Status for event-processing"),
  IV(NAME_message, "[code]*", IV_BOTH,
     NAME_action, "Associated command"),
  IV(NAME_popup, "popup*", IV_BOTH,
     NAME_menu, "Associated popup menu"),
  SV(NAME_look, "{x,open_look,motif,win}|name",IV_GET|IV_STORE,lookDialogItem,
     NAME_appearance, "Look-and-feel switch"),
  IV(NAME_autoAlign, "bool", IV_BOTH,
     NAME_layout, "Item is automatically placed by its dialog"),
  IV(NAME_reference, "[point]", IV_SEND,
     NAME_layout, "Reference point for alignment"),
  IV(NAME_above, "graphical*", IV_GET,
     NAME_layout, "DialogItem above me"),
  IV(NAME_below, "graphical*", IV_GET,
     NAME_layout, "DialogItem below me"),
  IV(NAME_right, "graphical*", IV_GET,
     NAME_layout, "DialogItem right of me"),
  IV(NAME_left, "graphical*", IV_GET,
     NAME_layout, "DialogItem left of me"),
  IV(NAME_alignment, "{column,left,center,right}", IV_BOTH,
     NAME_layout, "Align in columns or right of item to the left"),
  IV(NAME_autoLabelAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align label"),
  IV(NAME_autoValueAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align value"),
  SV(NAME_accelerator, "name*", IV_GET|IV_STORE, acceleratorDialogItem,
     NAME_layout, "Automatically align value")
};

/* Send Methods */

static senddecl send_dialogItem[] =
{ SM(NAME_initialise, 1, "name=name", createDialogItem,
     DEFAULT, "Create from name"),
  SM(NAME_device, 1, "device*", deviceDialogItem,
     DEFAULT, "Device I'm displayed on"),
  SM(NAME_name, 1, "name", nameDialogItem,
     DEFAULT, "Change <-name, update <-label"),
  SM(NAME_reset, 0, NULL, resetDialogItem,
     DEFAULT, "Change status to `inactive'"),
/*SM(NAME_unlink, 0, NULL, unlinkDialogItem,		empty these days
     DEFAULT, "Remove left,right,above,below links"), */
  SM(NAME_default, 1, "any", virtualObject,
     NAME_apply, "Virtual method"),
  SM(NAME_modified, 1, "bool", modifiedDialogItem,
     NAME_apply, "Forward modification to associated <-device"),
  SM(NAME_cancel, 0, NULL, cancelDialogItem,
     NAME_event, "Cancel operation (enter inactive state)"),
  SM(NAME_event, 1, "event", eventDialogItem,
     NAME_event, "Process an event"),
  SM(NAME_labelWidth, 1, "[int]", virtualObject,
     NAME_layout, "Virtual method"),
  SM(NAME_valueWidth, 1, "[int]", virtualObject,
     NAME_layout, "Virtual method"),
  SM(NAME_open, 0, NULL, openDialogItem,
     NAME_organisation, "Create dialog with this item and ->open"),
  SM(NAME_show, 1, "bool", showDialogItem,
     NAME_organisation, "Equivalent to <->displayed")
};

/* Get Methods */

static getdecl get_dialogItem[] =
{ GM(NAME_default, 0, "any", NULL, getVirtualObject,
     NAME_apply, "Virtual method"),
  GM(NAME_modified, 0, "bool", NULL, getModifiedDialogItem,
     NAME_apply, "Virtual method (return @off)"),
  GM(NAME_labelName, 1, "name", "name", getLabelNameDialogItem,
     NAME_label, "Determine default-label from the name"),
  GM(NAME_labelWidth, 0, "int", NULL, getVirtualObject,
     NAME_layout, "Virtual method"),
  GM(NAME_reference, 0, "point", NULL, getReferenceDialogItem,
     NAME_layout, "Reference point for alignment"),
  GM(NAME_valueWidth, 0, "int", NULL, getVirtualObject,
     NAME_layout, "Virtual method"),
  GM(NAME_show, 0, "bool", NULL, getShowDialogItem,
     NAME_organisation, "Equivalent to <-displayed")
};

/* Resources */

static classvardecl rc_dialogItem[] =
{ RC(NAME_alignment, "{column,left,center,right}", "column",
     "Alignment in the row"),
  RC(NAME_background, "colour|pixmap*", "@_dialog_bg",
     "Background of the item"),
  RC(NAME_elevation, "elevation*", "when(@colour_display, 1, 0)",
     "3-D elevation"),
  RC(NAME_labelFont, "font", "bold",
     "Default font for labels"),
  RC(NAME_labelFormat, "{left,center,right}", "right",
     "Alignment of the label in its box"),
  RC(NAME_labelSuffix, "name", ":",
     "Ensured suffix of label"),
  RC(NAME_look, "{x,open_look,motif,win,gtk}",
     UXWIN("open_look", "win"),
     "Look-and-feel switch"),
  RC(NAME_selectionHandles, RC_REFINE, "@nil",
     NULL),
  RC(NAME_valueFont, "font", "normal",
     "Default font for values")
};

/* Class Declaration */

static Name dialogItem_termnames[] = { NAME_label };

ClassDecl(dialogItem_decls,
          var_dialogItem, send_dialogItem, get_dialogItem, rc_dialogItem,
          1, dialogItem_termnames,
          "$Rev$");

status
makeClassDialogItem(Class class)
{ return declareClass(class, &dialogItem_decls);
}

