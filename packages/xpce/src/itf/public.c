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

#define XPCE_PUBLIC_IMPL 1
#include <h/XPCE.h>

int
XPCE_define_classes(const XPCE_class_definition_t *classes)
{ for(; classes->name; classes++)
  { Class class = defineClass(CtoName(classes->name),
			      CtoName(classes->super),
			      staticCtoString(classes->summary),
			      classes->makefunction);

    if ( classes->global )
      *classes->global = class;
  }

  numberTreeClass(ClassObject, 0);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE_declare_class() calls declareClass() after internalising all names,
so we can load the class without assigning all names at compiletime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
charpToName(Name *n)
{ if ( !n )
    return;

  *n = CtoName((char *)*n);
}

static void
groupToName(Name *n)
{ if ( !n )
    *n = DEFAULT;			/* meaning inherit from superclass */

  *n = CtoName((char *)*n);
}

#define ToName(a)  charpToName(&a)
#define GToName(a) groupToName(&a)

static void
intern_vardef(vardecl *vd)
{ ToName(vd->name);
  GToName(vd->group);
}


static void
intern_send(senddecl *sd)
{ ToName(sd->name);
  GToName(sd->group);
}


static void
intern_get(getdecl *gd)
{ ToName(gd->name);
  GToName(gd->group);
}


static void
intern_cvardef(classvardecl *cvd)
{ ToName(cvd->name);
}


static void
intern_term_name(Name *np)
{ charpToName(np);
}


int
XPCE_declare_class(Class class, classdecl *decl)
{ int i;

  for(i=0; i<decl->nvar; i++)
    intern_vardef(&decl->variables[i]);
  for(i=0; i<decl->nsend; i++)
    intern_send(&decl->send_methods[i]);
  for(i=0; i<decl->nget; i++)
    intern_get(&decl->get_methods[i]);
  for(i=0; i<decl->nclassvars; i++)
    intern_cvardef(&decl->class_variables[i]);
  for(i=0; i<decl->term_arity; i++)
    intern_term_name(&decl->term_names[i]);

  return declareClass(class, decl);
}


void
XPCE_assignField(Instance instance, Any *field, Any value)
{ assignField(instance, field, value);
}


Any
XPCE_constant(xpce_constant_id id)
{ static Any constants[] =
  { NIL,
    DEFAULT,
    CLASSDEFAULT,
    ON,
    OFF
  };

  return constants[id];
}


int
initPublicInterface()
{ succeed;
}
