/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static HashTable ModifierTable;

static status
initialiseModifier(Modifier m, Name shift, Name ctrl, Name meta)
{ assign(m, shift,   shift);
  assign(m, control, ctrl);
  assign(m, meta,    meta);

  succeed;
}


static Modifier
getConvertModifier(Class class, Name name)
{ Modifier m;

  if ( (m = getMemberHashTable(ModifierTable, name)) )
    answer(m);
  else
  { String s = &name->data;
    int i, size = s->size;
    Name shift   = NAME_up;
    Name control = NAME_up;
    Name meta    = NAME_up;

    for(i=0; i<size; i++)
    { wchar c = str_fetch(s, i);

      switch(tolower(c))
      { case 's':
	  shift = NAME_down;
	  break;
	case 'c':
	  control = NAME_down;
	  break;
	case 'm':
	  meta = NAME_down;
	  break;
	default:
	  fail;
      }
    }
  
    m = answerObject(ClassModifier, shift, control, meta, 0);
    protectObject(m);
    appendHashTable(ModifierTable, name, m);

    answer(m);
  }
}


status
makeClassModifier(Class class)
{ sourceClass(class, makeClassModifier, __FILE__, "$Revision$");

  localClass(class, NAME_shift, NAME_modifier, "[{up,down}]", NAME_both,
	     "Condition on shift");
  localClass(class, NAME_control, NAME_modifier, "[{up,down}]", NAME_both,
	     "Condition on control");
  localClass(class, NAME_meta, NAME_modifier, "[{up,down}]", NAME_both,
	     "Condition on meta");

  termClass(class, "modifier", 3, NAME_shift, NAME_control, NAME_meta);

  sendMethod(class, NAME_initialise, DEFAULT,
	     3, "shift=[{up,down}]", "control=[{up,down}]", "meta=[{up,down}]",
	     "Create from shift, control and meta",
	     initialiseModifier);

  getMethod(class, NAME_convert, NAME_conversion, "modifier", 1, "name",
	    "Convert name, consisting of {s|m|c}",
	    getConvertModifier);

  MODIFIER_shift = globalObject(NAME_ModifierShift, ClassModifier,
				NAME_down, NAME_up, NAME_up, 0);
  MODIFIER_allup = globalObject(NAME_ModifierAllUp, ClassModifier,
				NAME_up, NAME_up, NAME_up, 0);
  ModifierTable = globalObject(NAME_modifiers, ClassHashTable, 0);

  succeed;
}

