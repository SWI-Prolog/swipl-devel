/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

forwards void initVars(void);
forwards VarBinding findVarEnvironment(VarEnvironment ev, Var v);

static HashTable VarTable;

static status
initialiseVar(Var v, Type type, Name name, Any value)
{ if ( isDefault(type) )
    type = TypeUnchecked;
  if ( isDefault(name) )		/* local var */
    name = NIL;

  assign(v, name, name);
  assign(v, type, type);
  assign(v, global_value, value);

  v->value = value;
  if ( isObject(value) )
    addCodeReference(value);

  if ( notNil(name) )
  { if ( getMemberHashTable(VarTable, name) )
      errorPce(v, NAME_redeclaredVar);
    appendHashTable(VarTable, name, v);
    protectObject(v);
  }

  return initialiseFunction((Function) v);
}


static status
unlinkVar(Var v)
{ VarEnvironment ev = varEnvironment;

  for(; ev; ev = ev->parent)
  { VarBinding b;

    if ( (b = findVarEnvironment(ev, v)) )
      b->variable = NULL;
  }

  if ( isObject(v->value) )
    delCodeReference(v->value);
  

  succeed;
}


static Var
getConvertVar(Class class, Any name)
{ answer(getMemberHashTable(VarTable, name));
}


/*				see assignVar()
static status
valueVar(Var v, Any value)
{ if ( v->value != value )
  { if ( isObject(v->value) )
      delCodeReference(v->value);
    v->value = value;
    if ( isObject(value) )
      addCodeReference(value);
  }
  
  succeed;
}
*/

static Any
getValueVar(Var v)
{ answer(v->value);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "type=[type]", "name=[name]", "value=[any]" };
static char *T_assign[] =
        { "value=any", "scope=[{local,outer,global}]" };

/* Instance Variables */

static vardecl var_var[] =
{ IV(NAME_Name, "name*", IV_GET,
     NAME_name, "Name of the var"),
  IV(NAME_Type, "type", IV_BOTH,
     NAME_type, "Type of the <-_value"),
  IV(NAME_Value, "alien:Any", IV_NONE,
     NAME_value, "Value of the var"),
  IV(NAME_GlobalValue, "any", IV_GET,
     NAME_abort, "Global value of the var")
};

/* Send Methods */

static senddecl send_var[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseVar,
     DEFAULT, "Create var from name and value"),
  SM(NAME_unlink, 0, NULL, unlinkVar,
     DEFAULT, "Release code reference of value"),
  SM(NAME_assign, 2, T_assign, assignVar,
     NAME_value, "Assign value to variable (with scope)")
};

/* Get Methods */

static getdecl get_var[] =
{ GM(NAME_convert, 1, "var", "name", getConvertVar,
     NAME_conversion, "Converts name to var from @variables"),
  GM(NAME_Execute, 0, "unchecked", NULL, getValueVar,
     NAME_execute, "Current value of the variable"),
  GM(NAME_Value, 0, "unchecked", NULL, getValueVar,
     NAME_value, "Current value of the variable")
};

/* Resources */

#define rc_var NULL
/*
static classvardecl rc_var[] =
{ 
};
*/

/* Class Declaration */

static Name var_termnames[] = { NAME_Value };

ClassDecl(var_decls,
          var_var, send_var, get_var, rc_var,
          1, var_termnames,
          "$Rev$");

status
makeClassVar(Class class)
{ declareClass(class, &var_decls);
  saveStyleClass(class, NAME_external);

  VarTable = globalObject(NAME_variables, ClassHashTable, 0);
  initVars();

  succeed;
}


static Var
initVar(Name name, char *type, Any value)
{ return globalObject(name, ClassVar, CtoType(type), name, value, 0);
}


static Var
initGrVar(Name ref, Name name)
{ return globalObject(ref, ClassVar, TypeInt, name, ZERO, 0);
}


static void
initVars(void)
{ int n;

  RECEIVER       = initVar(NAME_receiver,	"object*", NIL);
  RECEIVER_CLASS = initVar(NAME_receiverClass,	"class*",  NIL);
  EVENT		 = initVar(NAME_event,		"event*",  NIL);
  SELECTOR	 = initVar(NAME_selector,	"name*",   NIL);
  REPORTEE	 = initVar(NAME_reportee,	"chain*",  NIL);

  VarX		 = initGrVar(NAME_xVar, NAME_x);
  VarY		 = initGrVar(NAME_yVar, NAME_y);
  VarW		 = initGrVar(NAME_wVar, NAME_w);
  VarH		 = initGrVar(NAME_hVar, NAME_h);
  VarW2		 = initGrVar(NAME_w2Var, NAME_w2);
  VarH2		 = initGrVar(NAME_h2Var, NAME_h2);
  VarXref	 = initGrVar(NAME_xrefVar, NAME_xref);
  VarYref	 = initGrVar(NAME_yrefVar, NAME_yref);

  for(n = 1; n <= FWD_PCE_MAX_ARGS; n++)
  { char varname[100];

    sprintf(varname, "arg%d", n);
    Arg(n) = initVar(CtoName(varname), "unchecked", DEFAULT);
  }
}


void
resetVars(void)
{ varEnvironment = NULL;

  if ( VarTable )
    for_hash_table(VarTable, s,
		   { Var v = s->value;

		     v->value = v->global_value;
		   });
}


		/********************************
		*          ENVIRONMENTS		*
		********************************/

#define sizeofVarExtension(n) ((int)(long)(&((VarExtension)NULL)->bindings[n]))

#define EXTBLOCKSIZE 8

static VarBinding
findVarEnvironment(VarEnvironment ev, Var v)
{ int i;
  VarBinding b;

  b = ev->bindings; i = 0;
  while( i < ev->size )
  { if ( b->variable == v )
      return b;

    if ( ++i == BINDINGBLOCKSIZE && ev->extension )
      b = ev->extension->bindings;
    else
      b++;
  }

  return NULL;
}


static VarExtension
expandVarExtension(VarExtension ext, int size)
{ if ( ext == NULL )
  { ext = alloc(sizeofVarExtension(EXTBLOCKSIZE));
    ext->allocated = EXTBLOCKSIZE;
    return ext;
  } else if ( size > ext->allocated )
  { int a = ((size + EXTBLOCKSIZE - 1) / EXTBLOCKSIZE) * EXTBLOCKSIZE;
    int i;

    VarExtension new = alloc(sizeofVarExtension(a));
    new->allocated = a;
    for(i=0; i<ext->allocated; i++)
      new->bindings[i] = ext->bindings[i];
    unalloc(sizeofVarExtension(ext->allocated), ext);

    return new;
  } else

    return ext;
}


static VarBinding
appendVarEnvironment(VarEnvironment ev, Var v)
{ VarBinding b;

  DEBUG(NAME_var, Cprintf("Appending %s to env %p\n", pp(v), ev));

  if ( ev->size < BINDINGBLOCKSIZE )
    b = &ev->bindings[ev->size++];
  else
  { int ext =  ev->size - BINDINGBLOCKSIZE;

    ev->extension = expandVarExtension(ev->extension, ext+1);
    b = &ev->extension->bindings[ext];
  }

  b->variable = v;
  b->value = v->value;

  return b;
}


void
popVarEnvironment(void)
{ int i;
  VarBinding b;
  VarEnvironment ev = varEnvironment;

  b = ev->bindings; i = 0;
  while( i < ev->size )
  { if ( b->variable )			/* may be ->unlink'ed! */
    { if ( isObject(b->variable->value) )
	delCodeReference(b->variable->value);
      b->variable->value = b->value;
    }

    DEBUG(NAME_var, Cprintf("Restoring %s to %s\n",
			    pp(b->variable), pp(b->value)));

    if ( ++i == BINDINGBLOCKSIZE && ev->extension )
      b = ev->extension->bindings;
    else
      b++;
  }

  if ( ev->extension )
    unalloc(sizeofVarExtension(ev->extension->allocated), ev->extension);
  
  varEnvironment = ev->parent;
}


static void
valueVarBinding(VarBinding b, Any value)
{ if ( isObject(b->variable->value) )
    delCodeReference(b->variable->value);
  b->value = value;
}


status
assignVar(Var v, Any value, Name scope)
{ if ( isDefault(scope) || scope == NAME_local )
  { if ( varEnvironment && !findVarEnvironment(varEnvironment, v) )
      appendVarEnvironment(varEnvironment, v);
  } else if ( scope == NAME_outer )
  { VarBinding b;

    if ( varEnvironment )
    { if ( !(b = findVarEnvironment(varEnvironment, v)) )
	b = appendVarEnvironment(varEnvironment, v);

      valueVarBinding(b, value);
    }
  } else /* if ( scope == NAME_global ) */
  { VarEnvironment ev = varEnvironment;

    for(; ev; ev = ev->parent)
    { VarBinding b;

      if ( (b = findVarEnvironment(ev, v)) )
	valueVarBinding(b, value);
    }
    assign(v, global_value, value);
  }

  DEBUG(NAME_var, Cprintf("assignVar(%s) %s --> %s\n",
			  pp(v), pp(v->value), pp(value)));
  v->value = value;
  if ( isObject(value) )
    addCodeReference(value);

  succeed;
}
