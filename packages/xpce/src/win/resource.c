/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/lang.h>

forwards Name getResourceClassNameResource(Resource);

static Constant NotObtained;

static status
initialiseResource(Resource r,
		   Name name, Name class, Type type,
		   StringObj def, Any context, StringObj doc)
{ if ( isDefault(context) )
    context = NIL;
  if ( isDefault(class) )
    class = getCapitaliseName(name);
  if ( isDefault(doc) )
    doc = NIL;

  if ( (isDefault(type) || isDefault(doc)) &&
       instanceOfObject(context, ClassClass))
  { Class class = context;
    Variable var = getInstanceVariableClass(class, name);

    if ( var )
    { if ( isDefault(type) )
	type = var->type;
      if ( isDefault(doc) )
	doc = var->summary;
    }
  }

  assign(r, name,          name);
  assign(r, r_class,       class);
  assign(r, r_type,        type);
  assign(r, r_default,     def);
  assign(r, context,       context);
  assign(r, value,         NotObtained);
  assign(r, summary,	   doc);

  if ( isDefault(type) )
    return errorPce(r, NAME_inconsistentArguments);

  succeed;
}


static status
contextResource(Resource r, Any obj)
{ if ( r->context != obj )
  { assign(r, context, obj);
    assign(r, value, NotObtained);
  }
  
  succeed;
}


static status
convertFunctionRequiresName(Type t)
{ if ( t->kind == NAME_class )
  { Class cl = t->context;
    GetMethod m = getGetMethodClass(cl, NAME_convert);
    
    if ( m && m->types->size == ONE )
    { Type at = m->types->elements[0];
    
      if ( at == TypeName || at->kind == NAME_nameOf )
	succeed;
    }
  }

  fail;
}


static struct TAGop
{ char *name;
  int  priority;
  Name kind;
} operators[] = 
{ { "?", 150, NAME_yfx},
  { ":=", 990, NAME_xfx},
  { "@=", 990, NAME_xfx},
  { "*", 400, NAME_yfx},
  { "/", 400, NAME_yfx},
  { "<", 700, NAME_xfx},
  { "=", 700, NAME_xfx},
  { "=<", 700, NAME_xfx},
  { ">=", 700, NAME_xfx},
  { "==", 700, NAME_xfx},
  { ">", 700, NAME_xfx},
  { "-", 500, NAME_yfx},
  { "-", 500, NAME_fx},
  { "\\==", 700, NAME_xfx},
  { "+", 500, NAME_yfx},
  { "+", 500, NAME_fx},
  { NULL, 0, NULL }
};


static Parser
TheResourceParser()
{ static Parser p;

  if ( !p )
  { SyntaxTable  st = newObject(ClassSyntaxTable, 0);
    Tokeniser     t = newObject(ClassTokeniser, st, 0);
    struct TAGop *o = operators;
 
    p = globalObject(NAME_resourceParser, ClassParser, t, 0);
    
    send(p, NAME_active, CtoName("@"),
	 newObject(ClassObtain, PCE, NAME_objectFromReference,
		   newObject(ClassObtain, RECEIVER, NAME_token, 0),
		   0),
	 0);
    send(p, NAME_active, CtoName("["),
	 newObject(ClassObtain, RECEIVER, NAME_list,
		   CtoName("]"), CtoName(","), NAME_chain, 0),
	 0);
    send(p, NAME_sendMethod,
	 newObject(ClassSendMethod,
		   NAME_syntaxError,
		   newObject(ClassVector, NAME_charArray, 0),
		   newObject(ClassOr, 0),
		   CtoString("Just fail on syntax-error"),
		   0),
	 0);

    for( ; o->name; o++)
      send(p, NAME_operator,
	   newObject(ClassOperator,
		     CtoName(o->name), toInt(o->priority), o->kind, 0), 0);
  }

  return p;
}


static Any
getConvertStringResource(Resource r, CharArray value)
{ Any val;

  if ( r->r_type->fullname == NAME_geometry )
    return checkType(value, r->r_type, r->context);

  if ( (val = qadGetv(TheResourceParser(), NAME_parse, 1, (Any *)&value)) )
    answer(checkType(val, r->r_type, r->context));

  if ( syntax.uppercase && specialisedType(r->r_type, TypeName) )
  { val = CtoKeyword(strName(value));
    answer(checkType(val, r->r_type, r->context));
  }

  if ( specialisedType(r->r_type, TypeCharArray) ||
       value->data.size == 0 )		/* empty atom */
    answer(checkType(value, r->r_type, r->context));

  if ( syntax.uppercase &&
       (specialisedType(r->r_type, TypeName) ||
	convertFunctionRequiresName(r->r_type)) )
    value = (CharArray) CtoKeyword(strName(value));

  if ( (val = checkType(value, r->r_type, r->context)) )
  { if ( !includesType(r->r_type, nameToType(NAME_font)) )
      errorPce(r, NAME_oldResourceFormat, value);

    answer(val);
  }

  fail;
}


char *
resourceName(Name name)
{ if ( syntax.uppercase )
  { String s = &name->data;
    int size = s->size;
    LocalString(buf, s, s->size);
    int i;

    for(i=0; i<size; i++)
    { wchar c = str_fetch(s, i);

      if ( iswordsep(c) )
	str_store(buf, i, '_');
      else
	str_store(buf, i, tolower(c));
    }
    
    return strName(StringToName(buf));
  }

  return strName(name);
}


Resource
getSubResource(Resource r, Class class)
{ if ( r->context == class )
  { answer(r);
  } else
  { Any val;
    DisplayObj d = CurrentDisplay(NIL);
    Name name = class->name;

    openDisplay(d);
    if ( (val = ws_get_resource_value(d,
				      getCapitaliseName(name),
				      name,
				      r->r_class,
				      r->name) ) )
    { Resource clone = get(r, NAME_clone, 0);

      assert(clone);
      contextResource(clone, class);
      doneObject(val);			/* What to do with this? */

      answer(clone);
    } else
    { answer(r);
    }
  }
}


static CharArray
getStringValueResource(Resource r, Any obj)
{ Any val;
  DisplayObj d = CurrentDisplay(NIL);
  Name class_name, name_name;

  if ( notDefault(obj) )
    contextResource(r, obj);

  if ( nonObject(r->context) )
    fail;
    
  if ( instanceOfObject(r->context, ClassClass) )
    class_name = ((Class)r->context)->name;
  else
    class_name = getClassNameObject(r->context);

  if ( !(name_name = get(r->context, NAME_name, 0)) )
    name_name = class_name;

  openDisplay(d);
  val = ws_get_resource_value(d,
			      getCapitaliseName(class_name),
			      name_name,
			      r->r_class,
			      r->name);
  if ( val )
    answer(val);
  else
    answer((CharArray) r->r_default);
}


static status
obtainResource(Resource r, Any obj)
{ if ( notDefault(obj) )
    contextResource(r, obj);
  
  if ( r->value == NotObtained )
  { Any rval;
    CharArray str;

    TRY(str = getStringValueResource(r, DEFAULT));

    if ( !(rval = qadGetv(r, NAME_convertString, 1, (Any *)&str)) )
    { errorPce(r, NAME_cannotConvertResource, str);
      if ( equalCharArray(str, (CharArray) r->r_default) )
	fail;				/* default is same, no use */
      if ( !(rval = qadGetv(r, NAME_convertString, 1, (Any *)&r->r_default)) )
      { errorPce(r, NAME_cannotConvertResourceDefault, r->r_default);
	fail;
      }
    }

    assign(r, value, rval);
    doneObject(str);
  }
  
  succeed;
}


static status
valueResource(Resource r, Any val)
{ if ( (val = checkType(val, r->r_type, r->context)) != FAIL )
  { assign(r, value, val);

    succeed;
  }

  return errorTypeMismatch(r, getMethodFromFunction((Any)valueResource), 1,
			   r->r_type);
}



static Any
getValueResource(Resource r, Any obj)
{ if ( r->value == NotObtained )
  { TRY(obtainResource(r, obj));
  }

  answer(r->value);
}

		/********************************
		*         CLASS CATCHALL	*
		********************************/

status
resourceClass(Class class, Resource r)
{ Cell cell;

  realiseClass(class);

  for_cell(cell, class->resources)
  { Resource r2 = cell->value;

    if ( r2->name == r->name )
    { cellValueChain(class->resources, PointerToInt(cell), r);
      succeed;
    }
  }

  return appendChain(class->resources, r);
}


Resource
getResourceClass(Class class, Name name)
{ Resource r;
  Cell cell;

  realiseClass(class);

  if ( isNil(class->resource_table) )
    assign(class, resource_table, newObject(ClassHashTable, 0));
  else if ( (r=getMemberHashTable(class->resource_table, name)) )
    answer(r);
 
  for_cell(cell, class->resources)
  { Resource r = cell->value;
    
    if ( r->name == name )
    { appendHashTable(class->resource_table, name, r);
      answer(r);
    }
  }

  if ( notNil(class->super_class) )
  { Resource super = getResourceClass(class->super_class, name);

    if ( super )
    { r = getSubResource(super, class);
      appendHashTable(class->resource_table, name, r);
      answer(r);
    }
  }
  
  fail;
}


status
resourceValueClass(Class cl, Name name, Any val)
{ Resource r;

  if ( (r = getResourceClass(cl, name)) != FAIL )
    return valueResource(r, val);

  fail;
}


Any
getResourceValueClass(Class cl, Name name)
{ Resource r;

  if ( (r = getResourceClass(cl, name)) )
    answer(getValueResource(r, DEFAULT));

  fail;
}


status
attach_resource(Class cl, char *name, char *type, char *def, char *doc)
{ Resource r;
  StringObj s = (doc && strlen(doc) > 0 ? CtoString(doc) : DEFAULT);
  Name tp = (type ? CtoName(type) : DEFAULT);

  TRY( r = newObject(ClassResource, CtoName(name), DEFAULT, tp,
		     CtoString(def), cl, s, 0) );

  return resourceClass(cl, r);
}


status
refine_resource(Class cl, char *name_s, char *def)
{ Class super;
  Name name = CtoName(name_s);

  for( super = cl->super_class; notNil(super); super = super->super_class)
  { Cell cell;
    
    for_cell(cell, super->resources)
    { Resource r = cell->value;
      
      if ( r->name == name )		/* found it! */
      { Resource new = newObject(ClassResource, name, DEFAULT,
				 r->r_type, CtoString(def), cl, r->summary, 0);

	return resourceClass(cl, new);
      }
    }
  }
				       
  sysPce("Could not find super-resource to refine %s.%s\n",
	 pp(cl->name), name_s);
  fail;					/* notreached */
}


status
variable_resource(Class cl, Name name, char *def)
{ Resource r;

  TRY( r = newObject(ClassResource, name, DEFAULT, DEFAULT,
		     CtoString(def), cl, DEFAULT, 0) );

  return resourceClass(cl, r);
}


		/********************************
		*         MANUAL SUPPORT	*
		********************************/


static Name
getResourceClassNameResource(Resource r)
{ if ( isObject(r->context) )
  { Name class_name;

    if ( instanceOfObject(r->context, ClassClass) )
      class_name = ((Class)r->context)->name;
    else
      class_name = getClassNameObject(r->context);

    answer(getCapitaliseName(class_name));
  }

  answer(CtoName("???"));
}


static Name
getManIdResource(Resource r)
{ char8 buf[LINESIZE];

  sprintf(buf, "R.%s.%s",
	  strName(instanceOfObject(r->context, ClassClass)
		  ? ((Class)r->context)->name
		  : getClassNameObject(r->context)),
	  strName(r->name));

  answer(CtoName(buf));
}


static Name
getManIndicatorResource(Resource r)
{ answer(CtoName("R"));
}


static StringObj
getManSummaryResource(Resource r)
{ char8 buf[LINESIZE];

  buf[0] = EOS;
  strcat(buf, "R\t");

  if ( isObject(r->context) )
  { strcat(buf, strName(getResourceClassNameResource(r)));
    strcat(buf, ".");
  }

  strcat(buf, strName(r->name));
  strcat(buf, ": ");
  strcat(buf, strName(getCapitaliseName(r->r_type->fullname)));

  if ( notNil(r->summary) )
  { strcat(buf, "\t");
    strcat(buf, strName(r->summary));
  }
  if ( send(r, NAME_manDocumented, 0) != FAIL )
    strcat(buf, " (+)");

  answer(CtoString(buf));
}


static Name
getPrintNameResource(Resource r)
{ char8 buf[LINESIZE];

  sprintf(buf, "%s.%s",
	  strName(getResourceClassNameResource(r)),
	  strName(r->name));

  answer(CtoName(buf));
}


static Name
getGroupResource(Resource r)
{ Class class = r->context;
  Variable var;

  if ( instanceOfObject(class, ClassClass) &&
       (var = getInstanceVariableClass(class, r->name)) )
    answer(getGroupVariable(var));

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=name", "class=[name]", "type=[type]", "default=string", "context=[object]*", "summary=[string]*" };

/* Instance Variables */

static vardecl var_resource[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Name of the resource"),
  IV(NAME_resourceClass, "name", IV_GET,
     NAME_name, "Class of the resource"),
  IV(NAME_type, "type", IV_GET,
     NAME_type, "Type requested"),
  IV(NAME_default, "string", IV_GET,
     NAME_resource, "Default if no resource defined"),
  IV(NAME_context, "object*", IV_GET,
     NAME_context, "Object resource is associated with"),
  IV(NAME_value, "any", IV_NONE,
     NAME_cache, "Value of the resource"),
  IV(NAME_summary, "string*", IV_BOTH,
     NAME_manual, "Summary documentation")
};

/* Send Methods */

static senddecl send_resource[] =
{ SM(NAME_context, 1, "object", contextResource,
     DEFAULT, "Attach resource to an object"),
  SM(NAME_initialise, 6, T_initialise, initialiseResource,
     DEFAULT, "Create from name, class, type, default and context"),
  SM(NAME_obtain, 1, "[object]", obtainResource,
     NAME_value, "Compute its value"),
  SM(NAME_value, 1, "any", valueResource,
     NAME_value, "Set value of the resource")
};

/* Get Methods */

static getdecl get_resource[] =
{ GM(NAME_group, 0, "name", NULL, getGroupResource,
     NAME_manual, "Same as related variable"),
  GM(NAME_manId, 0, "name", NULL, getManIdResource,
     NAME_manual, "Card Id for resource"),
  GM(NAME_manIndicator, 0, "name", NULL, getManIndicatorResource,
     NAME_manual, "Manual type indicator (`R')"),
  GM(NAME_manSummary, 0, "string", NULL, getManSummaryResource,
     NAME_manual, "New string with documentation summary"),
  GM(NAME_resourceClassName, 0, "name", NULL, getResourceClassNameResource,
     NAME_name, "Resource class name"),
  GM(NAME_printName, 0, "name", NULL, getPrintNameResource,
     NAME_textual, "Class.name"),
  GM(NAME_sub, 1, "resource", "class", getSubResource,
     NAME_type, "Refined resource when redefined by user"),
  GM(NAME_convertString, 1, "any|function", "textual=char_array", getConvertStringResource,
     NAME_value, "Convert textual value into typed value"),
  GM(NAME_stringValue, 1, "char_array", "context=[object]", getStringValueResource,
     NAME_value, "Obtain and return value as a char_array"),
  GM(NAME_value, 1, "any", "context=[object]", getValueResource,
     NAME_value, "Compute and return the value")
};

/* Resources */

#define rc_resource NULL
/*
static resourcedecl rc_resource[] =
{ 
};
*/

/* Class Declaration */

static Name resource_termnames[] = { NAME_name, NAME_class, NAME_type, NAME_default, NAME_context, NAME_summary };

ClassDecl(resource_decls,
          var_resource, send_resource, get_resource, rc_resource,
          6, resource_termnames,
          "$Rev$");


status
makeClassResource(Class class)
{ declareClass(class, &resource_decls);

  cloneStyleVariableClass(class, NAME_summary, NAME_reference);
  cloneStyleVariableClass(class, NAME_value,   NAME_reference);

  NotObtained = globalObject(NAME_notObtained, ClassConstant,
			     NAME_notObtained,
			     CtoString("Value of not-obtained resource"), 0);
  TheResourceParser();

  succeed;
}
