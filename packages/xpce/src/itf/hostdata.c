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
#define QUICK_AND_DIRTY 1

		 /*******************************
		 *	 PUBLIC INTERFACE	*
		 *******************************/

HostData
CtoHostData(Class class, void *h, int flags)
{ HostData hd;
#ifdef QUICK_AND_DIRTY
  hd = allocObject(class, TRUE);

  hd->handle = h;
  setFlag(hd, F_ISHOSTDATA|F_NOTANY);
  incrInt(class->no_created);
  clearCreatingObj(hd);
#else
  hd = newObjectv(class, 1, &h);
#endif

  if ( flags & PCE_ANSWER )
    pushAnswerObject(hd);

  return hd;
}


void
setHostDataHandle(HostData hd, void *h)
{ hd->handle = h;
}


void *
getHostDataHandle(HostData hd)
{ if ( isHostData(hd) )
    return hd->handle;

  return NULL;
}


int
freeHostData(HostData hd)
{
  if ( refsObject(hd) == 0 )
  {
#ifdef QUICK_AND_DIRTY
    Class class = classOfObject(hd);

    if ( !onFlag(hd, F_FREED) )
    { incrInt(class->no_freed);
      unalloc(valInt(class->instance_size), hd);
    }
#else
    freeObject(hd);
#endif
    succeed;
  }

  fail;
}


void
makeAnyHostData(HostData hd)
{ clearFlag(hd, F_NOTANY);
}




		 /*******************************
		 *	      CLASS		*
		 *******************************/


static status
initialiseHostData(HostData hd, void *h)
{
#ifdef QUICK_AND_DIRTY
  return errorPce(classOfObject(hd), NAME_cannotCreateInstances);
#else
  hd->handle = h;
  setFlag(hd, F_ISHOSTDATA|F_NOTANY);

  succeed;
#endif
}


static StringObj
getPrintNameHostData(HostData hd)
{ char tmp[25];

  sprintf(tmp, "@%ld/%s",
	  valInt(PointerToInt(hd)),
	  strName(classOfObject(hd)->name));

  return CtoString(tmp);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

/* Instance Variables */

static vardecl var_host_data[] =
{ IV(NAME_handle, "alien:void *", IV_NONE,
     NAME_storage, "Foreign-data handle")
};

/* Send Methods */

static senddecl send_host_data[] =
{ SM(NAME_initialise, 1, "alien:void *", initialiseHostData,
     DEFAULT, "Create from handle")
};

/* Get Methods */

static getdecl get_host_data[] =
{ GM(NAME_printName, 0, "text=string", NULL, getPrintNameHostData,
     DEFAULT, "Returns string holding @<ref>/<class>")
};

/* Resources */

#define rc_host_data NULL
/*
static classvardecl rc_host_data[] =
{ 
};
*/

/* Class Declaration */

static Name host_data_termnames[] = { NAME_handle };

ClassDecl(host_data_decls,
          var_host_data, send_host_data, get_host_data, rc_host_data,
          1, host_data_termnames,
          "$Rev$");


status
makeClassHostData(Class class)
{ return declareClass(class, &host_data_decls);
}

