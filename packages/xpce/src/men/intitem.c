/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

Class ClassIntItem;

typedef struct int_item *IntItem;

NewClass(int_item)
    ABSTRACT_TEXTITEM
End;

#define II_BOX_W   14
#define II_BOX_GAP 5

static status rangeIntItem(IntItem ii, Int low, Int high);
static status detachTimerIntItem(IntItem ii);

static status
initialiseIntItem(IntItem ii, Name name, Int selection, Code msg,
		  Int low, Int high)
{ if ( isDefault(name) )
    name = NAME_integer;
  if ( isDefault(selection) )	
    selection = ZERO;

  initialiseTextItem((TextItem)ii, name, selection, msg);
  return rangeIntItem(ii, low, high);
}


static status
unlinkIntItem(IntItem ii)
{ detachTimerIntItem(ii);

  return unlinkDialogItem((DialogItem) ii);
}


static status
computeIntItem(IntItem ii)
{ if ( notNil(ii->request_compute) )
  { computeTextItem((TextItem)ii);

    CHANGING_GRAPHICAL(ii,
		       assign(ii->area, w,
			      toInt(valInt(ii->area->w)+II_BOX_W+II_BOX_GAP)));
  }

  succeed;
}


static status
RedrawAreaIntItem(IntItem ii, Area a)
{ int x, y, w, h;
  int bx, bh;
  int ix, dy, iw, ih;
  static Elevation e;

  if ( !e )
    e = newObject(ClassElevation, TWO, 0);

  initialiseDeviceGraphical(ii, &x, &y, &w, &h);

  a->w = toInt(w-(II_BOX_W+II_BOX_GAP));
  RedrawAreaTextItem((TextItem)ii, a);
  a->w = toInt(w);

  bx = x+w-II_BOX_W;
  bh = (h+1)/2;
  r_3d_box(bx, y,    II_BOX_W, bh,   0, e,
	   ii->status != NAME_increment);
  r_3d_box(bx, y+bh, II_BOX_W, h-bh, 0, e,
	   ii->status != NAME_decrement);
  
  iw = valInt(INT_ITEM_IMAGE->size->w)/2;
  ih = valInt(INT_ITEM_IMAGE->size->h);
  ix = x + w - (II_BOX_W+iw+1)/2;
  dy = (bh-ih+1)/2;

  r_image(INT_ITEM_IMAGE, 0,  0, ix, y+dy,      iw, ih, ON);
  r_image(INT_ITEM_IMAGE, iw, 0, ix, y+h-dy-ih, iw, ih, ON);
  
  succeed;
}


static int
width_text(FontObj f, const char *s)
{ CharArray ctmp = CtoScratchCharArray(s);
  Int w = getWidthFont(f, ctmp);
  
  doneScratchCharArray(ctmp);

  return(valInt(w));
}


static status
rangeIntItem(IntItem ii, Int low, Int high)
{ char buf[40];
  Type t = NULL;
  char s1[20], s2[20];
  int b = valInt(getResourceValueObject(ii, NAME_border));

  obtainResourcesObject(ii);

  if ( isDefault(low) )
  { if ( isDefault(high) )
    { t = TypeInt;
      sprintf(s1, "%ld", PCE_MIN_INT);
      sprintf(s2, "%ld", PCE_MAX_INT);
    } else
    { sprintf(s1, "%ld", PCE_MIN_INT);
      sprintf(s2, "%ld", valInt(high));
      sprintf(buf, "..%ld", valInt(high));
    }
  } else
  { if ( isDefault(high) )
    { sprintf(s1, "%ld", valInt(low));
      sprintf(s2, "%ld", PCE_MAX_INT);
      sprintf(buf, "%ld..", valInt(low));
    } else
    { sprintf(s1, "%ld", valInt(low));
      sprintf(s2, "%ld", valInt(high));
      sprintf(buf, "%ld..%ld", valInt(low), valInt(high));
    }
  }

  if ( !t )
    t = checkType(CtoName(buf), TypeType, NIL);
      
  assign(ii, type, t);
  assign(ii, hor_stretch, ZERO);
  valueWidthTextItem((TextItem)ii,
		     toInt(max(width_text(ii->value_font, s1),
			       width_text(ii->value_font, s2))
			   + 2*b));

  succeed;
}


static Int
getLowIntItem(IntItem ii)
{ Type t = ii->type;

  if ( t->kind == NAME_intRange )
  { Tuple tup = t->context;
    if ( isInteger(tup->first) )
      answer(tup->first);
  }

  answer(toInt(PCE_MIN_INT));
}


static Int
getHighIntItem(IntItem ii)
{ Type t = ii->type;

  if ( t->kind == NAME_intRange )
  { Tuple tup = t->context;
    if ( isInteger(tup->second) )
      answer(tup->second);
  }

  answer(toInt(PCE_MAX_INT));
}



		 /*******************************
		 *	   EVENT HANDLING	*
		 *******************************/

static status
statusIntItem(IntItem ii, Name status)
{ if ( ii->status != status )
  { statusTextItem((TextItem)ii, status);
    changedDialogItem(ii);		/* overkill */
  }

  succeed;
}


static Name
getDirectionIntItem(IntItem ii, EventObj ev)
{ Int X, Y;
  int x, y;
  int r = valInt(ii->area->w);
  
  get_xy_event(ev, ii, OFF, &X, &Y);
  x = valInt(X);
  y = valInt(Y);
    
  if ( x >= r - II_BOX_W && x < r && y >= 0 && y <= valInt(ii->area->h) )
  { if ( y < valInt(ii->area->h)/2 )
      answer(NAME_increment);
    else
      answer(NAME_decrement);
  }
  
  fail;
}


static status
attachTimerIntItem(IntItem ii)
{ Real delay = getResourceValueObject(ii, NAME_repeatDelay);

  if ( delay )
  { Timer t = newObject(ClassTimer, delay,
			newObject(ClassMessage, ii, NAME_repeat, 0), 0);
    attributeObject(ii, NAME_Timer, t);
    startTimer(t, NAME_once);
  }

  succeed;
}


static status
detachTimerIntItem(IntItem ii)
{ Timer t;

  if ( (t = getAttributeObject(ii, NAME_Timer)) )
  { freeObject(t);
    deleteAttributeObject(ii, NAME_Timer);
  }
  
  succeed;
}


static status
repeatIntItem(IntItem ii)
{ Timer t;
  Real i = getResourceValueObject(ii, NAME_repeatInterval);

  send(ii, ii->status, 0);

  if ( (t = getAttributeObject(ii, NAME_Timer)) )
  { intervalTimer(t, i);
    statusTimer(t, NAME_once);
  }

  succeed;
}


static status
typedIntItem(IntItem ii, EventId id)
{ CharArray save = getCopyCharArray(ii->value_text->string);
  status rval = typedTextItem((TextItem)ii, id);
  
  if ( rval &&
       !checkType(ii->value_text->string, ii->type, NIL) &&
       getSizeCharArray(ii->value_text->string) != ZERO )
  { displayedValueTextItem((TextItem)ii, save);
    return errorPce(ii, NAME_cannotConvertText,
		    ii->value_text->string, ii->type);
  }

  doneObject(save);
  return rval;
}


static status
eventIntItem(IntItem ii, EventObj ev)
{ Name dir;

  if ( isAEvent(ev, NAME_msLeft) &&
       (dir=getDirectionIntItem(ii, ev)) )
  { if ( isUpEvent(ev) )
    { send(ii, dir, 0);
      detachTimerIntItem(ii);
      statusIntItem(ii, NAME_active);
    } else
    { statusIntItem(ii, dir);

      if ( isDownEvent(ev) )
	attachTimerIntItem(ii);
    }

    succeed;
  }
       
  return eventTextItem((TextItem)ii, ev);
}


static status
addIntItem(IntItem ii, Int change)
{ Int ival;
  long val;
  char buf[100];
  CharArray ctmp;
  Int low, high;

  if ( (ival = toInteger(ii->value_text->string)) )
    val = valInt(ival);
  else
    val = 0;
  val += valInt(change);
  if ( (low=getLowIntItem(ii)) )
    val = max(val, valInt(low));
  if ( (high = getHighIntItem(ii)) )
    val = min(val, valInt(high));
  sprintf(buf, "%ld", val);
  ctmp = CtoScratchCharArray(buf);
  displayedValueTextItem((TextItem)ii, ctmp);
  doneScratchCharArray(ctmp);

  if ( applyTextItem((TextItem)ii, OFF) && !isFreedObj(ii) )
  { if ( ii->advance == NAME_next )
      send(ii->device, NAME_advance, ii, 0);
  }

  succeed;
}


static status
incrementIntItem(IntItem ii)
{ return addIntItem(ii, ONE);
}


static status
decrementIntItem(IntItem ii)
{ return addIntItem(ii, toInt(-1));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=[name]", "default=[function|int]", "message=[code]*",
	  "low=[int]", "high=[int]" };
static char *T_range[] =
	{ "low=[int]", "high=[int]" };

/* Instance Variables */

static vardecl var_int_item[] =
{ SV(NAME_status, "{inactive,active,preview,execute,increment,decrement}",
     IV_GET|IV_STORE|IV_REDEFINE,
     statusIntItem,
     NAME_event, "Status for event-processing")
};

/* Send Methods */

static senddecl send_int_item[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseIntItem,
     DEFAULT, "Create from name, selection and font"),
  SM(NAME_unlink, 0, NULL, unlinkIntItem,
     DEFAULT, "Detach repeat timer"),
  SM(NAME_range, 2, T_range, rangeIntItem,
     NAME_type, "Allowed range"),
  SM(NAME_compute, 0, NULL, computeIntItem,
     DEFAULT, "Recompute layout"),
  SM(NAME_event, 1, "event", eventIntItem,
     DEFAULT, "Handle increment/decrement buttons"),
  SM(NAME_typed, 1, "event_id", typedIntItem,
     NAME_event, "Process keyboard event"),
  SM(NAME_repeat, 0, NULL, repeatIntItem,
     DEFAULT, "Repeat in/decrement"),
  SM(NAME_increment, 0, NULL, incrementIntItem,
     NAME_selection, "Increment the selection"),
  SM(NAME_decrement, 0, NULL, decrementIntItem,
     NAME_selection, "Decrement the selection")
};

/* Get Methods */

#define get_int_item NULL
/*
static getdecl get_int_item[] =
{ 
};
*/

/* Resources */

static resourcedecl rc_int_item[] =
{ RC(NAME_repeatDelay, "real", "0.35",
     "Time to wait until start of repeat"),
  RC(NAME_repeatInterval, "real", "0.06",
     "Interval between repeats")
};

/* Class Declaration */

static Name int_item_termnames[] = { NAME_name, NAME_selection, NAME_font };

ClassDecl(int_item_decls,
          var_int_item, send_int_item, get_int_item, rc_int_item,
          3, int_item_termnames,
          "$Rev$");

status
makeClassIntItem(Class class)
{ declareClass(class, &int_item_decls);
  setRedrawFunctionClass(class, RedrawAreaIntItem);

  succeed;
}

