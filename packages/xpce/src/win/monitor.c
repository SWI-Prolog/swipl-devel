/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2006, University of Amsterdam

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


status
initialiseMonitor(Monitor m, Name name, Area a)
{ if ( isDefault(name) )
    name = NIL;

  assign(m, name, name);
  assign(m, area, a);

  succeed;
}


static Monitor
getConvertMonitor(Class class, Any value)
{ DisplayObj d = CurrentDisplay(NIL);

  if ( d )
  { Chain ch = get(d, NAME_monitors, EAV);

    if ( ch && instanceOfObject(ch, ClassChain) )
    { if ( isInteger(value) )
	return getNth0Chain(ch, value);
    } else
    { Cell cell;

      for_cell(cell, ch)
      { Monitor m = cell->value;

	if ( m->name == value)
	  return m;
      }
    }
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=[name|int]*", "area=area" };

/* Instance Variables */

static vardecl var_monitor[] =
{ IV(NAME_name, "name|int*", IV_GET,
     NAME_name, "Name of the monitor"),
  IV(NAME_area, "area", IV_GET,
     NAME_area, "Area of the monitor"),
  IV(NAME_workArea, "area*", IV_GET,
     NAME_area, "User area of the monitor"),
  IV(NAME_primary, "bool*", IV_GET,
     NAME_monitor, "If @on, this is the primary monitor")
};

/* Send Methods */

static senddecl send_monitor[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseMonitor,
     DEFAULT, "Create monitor from name and area")
};

/* Get Methods */

static getdecl get_monitor[] =
{ GM(NAME_convert, 1, "monitor", "int|name", getConvertMonitor,
     DEFAULT, "Convert number or name of monitor"),
};

/* Resources */

#define rc_monitor NULL
/*
static classvardecl rc_monitor[] =
{ 
};
*/

/* Class Declaration */

static Name monitor_termnames[] = { NAME_name, NAME_area };

ClassDecl(monitor_decls,
          var_monitor, send_monitor, get_monitor, rc_monitor,
          2, monitor_termnames,
          "$Rev$");


status
makeClassMonitor(Class class)
{ return declareClass(class, &monitor_decls);
}

