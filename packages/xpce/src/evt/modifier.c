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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "shift=[{up,down}]", "control=[{up,down}]", "meta=[{up,down}]" };

/* Instance Variables */

static vardecl var_modifier[] =
{ IV(NAME_shift, "[{up,down}]", IV_BOTH,
     NAME_modifier, "Condition on shift"),
  IV(NAME_control, "[{up,down}]", IV_BOTH,
     NAME_modifier, "Condition on control"),
  IV(NAME_meta, "[{up,down}]", IV_BOTH,
     NAME_modifier, "Condition on meta")
};

/* Send Methods */

static senddecl send_modifier[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseModifier,
     DEFAULT, "Create from shift, control and meta")
};

/* Get Methods */

static getdecl get_modifier[] =
{ GM(NAME_convert, 1, "modifier", "name", getConvertModifier,
     NAME_conversion, "Convert name, consisting of {s|m|c}")
};

/* Resources */

#define rc_modifier NULL
/*
static resourcedecl rc_modifier[] =
{ 
};
*/

/* Class Declaration */

static Name modifier_termnames[] = { NAME_shift, NAME_control, NAME_meta };

ClassDecl(modifier_decls,
          var_modifier, send_modifier, get_modifier, rc_modifier,
          3, modifier_termnames,
          "$Rev$");

status
makeClassModifier(Class class)
{ declareClass(class, &modifier_decls);

  MODIFIER_shift = globalObject(NAME_ModifierShift, ClassModifier,
				NAME_down, NAME_up, NAME_up, 0);
  MODIFIER_allup = globalObject(NAME_ModifierAllUp, ClassModifier,
				NAME_up, NAME_up, NAME_up, 0);
  ModifierTable = globalObject(NAME_modifiers, ClassHashTable, 0);

  succeed;
}

