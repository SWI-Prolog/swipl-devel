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
  
    m = answerObject(ClassModifier, shift, control, meta, EAV);
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
static classvardecl rc_modifier[] =
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

  MODIFIER_shift   = globalObject(NAME_ModifierShift, ClassModifier,
				  NAME_down, NAME_up, NAME_up, EAV);
  MODIFIER_control = globalObject(NAME_ModifierControl, ClassModifier,
				  NAME_up, NAME_down, NAME_up, EAV);
  MODIFIER_allup   = globalObject(NAME_ModifierAllUp, ClassModifier,
				  NAME_up, NAME_up, NAME_up, EAV);
  ModifierTable = globalObject(NAME_modifiers, ClassHashTable, EAV);

  succeed;
}

