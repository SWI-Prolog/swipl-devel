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
#include <h/lang.h>
#include <h/unix.h>

static status	 contextClassVariable(ClassVariable cv, Class context);
static StringObj getDefault(Class class, Name name, int accept_default);
static status	 appendClassVariableClass(Class class, ClassVariable cv);

static Constant NotObtained;

static status
initialiseClassVariable(ClassVariable cv,
			Class context, Name name, Any def,
			Type type, StringObj summary)
{ initialiseProgramObject(cv);

  assign(cv, name,       name);
  assign(cv, type,       type);
  assign(cv, cv_default, def);
  assign(cv, value,      NotObtained);
  assign(cv, summary,    summary);

  contextClassVariable(cv, context);

  return appendClassVariableClass(context, cv);
}


static status
contextClassVariable(ClassVariable cv, Class context)
{ if ( cv->context != context )
  { Variable var = getInstanceVariableClass(context, cv->name);

    assign(cv, context, context);
    assign(cv, value, NotObtained);

    if ( isDefault(cv->type) )
      assign(cv, type, var ? var->type : TypeAny);
  }
  
  succeed;
}

static StringObj
getSummaryClassVariable(ClassVariable cv)
{ Class class = cv->class;
  Variable var;

  if ( instanceOfObject(cv->summary, ClassString) )
    answer(cv->summary);

  if ( isDefault(cv->summary) &&
       (var = getInstanceVariableClass(class, cv->name)) )
  { if ( notNil(var->summary) )
      answer(var->summary);		/* TBD: getSummaryVariable() */
  }
					/* TBD: look for inheritence */
  fail;
}

		 /*******************************
		 *	  STRING --> VALUE	*
		 *******************************/

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
{ { "?",    150, NAME_yfx},
  { ":=",   990, NAME_xfx},
  { "@=",   990, NAME_xfx},
  { "*",    400, NAME_yfx},
  { "/",    400, NAME_yfx},
  { "<",    700, NAME_xfx},
  { "=",    700, NAME_xfx},
  { "=<",   700, NAME_xfx},
  { ">=",   700, NAME_xfx},
  { "==",   700, NAME_xfx},
  { ">",    700, NAME_xfx},
  { "-",    500, NAME_yfx},
  { "-",    500, NAME_fx},
  { "\\==", 700, NAME_xfx},
  { "+",    500, NAME_yfx},
  { "+",    500, NAME_fx},
  { NULL,     0, NULL }
};


static Parser
TheObjectParser()
{ static Parser p;

  if ( !p )
  { SyntaxTable  st = newObject(ClassSyntaxTable, EAV);
    Tokeniser     t = newObject(ClassTokeniser, st, EAV);
    struct TAGop *o = operators;
 
    p = globalObject(NAME_objectParser, ClassParser, t, EAV);
    
    send(p, NAME_active, CtoName("@"),
	 newObject(ClassObtain, PCE, NAME_objectFromReference,
		   newObject(ClassObtain, RECEIVER, NAME_token, EAV),
		   EAV),
	 EAV);
    send(p, NAME_active, CtoName("["),
	 newObject(ClassObtain, RECEIVER, NAME_list,
		   CtoName("]"), CtoName(","), NAME_chain, EAV),
	 EAV);
    send(p, NAME_sendMethod,
	 newObject(ClassSendMethod,
		   NAME_syntaxError,
		   newObject(ClassVector, NAME_charArray, EAV),
		   newObject(ClassOr, EAV),
		   CtoString("Just fail on syntax-error"),
		   EAV),
	 EAV);

    for( ; o->name; o++)
      send(p, NAME_operator,
	   newObject(ClassOperator,
		     CtoName(o->name), toInt(o->priority), o->kind, EAV), EAV);
  }

  return p;
}


static Any
getConvertStringClassVariable(ClassVariable cv, CharArray value)
{ Any val;

  if ( cv->type->fullname == NAME_geometry )
    return checkType(value, cv->type, cv->context);

  if ( (val = qadGetv(TheObjectParser(), NAME_parse, 1, (Any *)&value)) )
    answer(checkType(val, cv->type, cv->context));

  if ( syntax.uppercase && specialisedType(cv->type, TypeName) )
  { val = CtoKeyword(strName(value));
    answer(checkType(val, cv->type, cv->context));
  }

  if ( specialisedType(cv->type, TypeCharArray) ||
       value->data.size == 0 )		/* empty atom */
    answer(checkType(value, cv->type, cv->context));

  if ( syntax.uppercase &&
       (specialisedType(cv->type, TypeName) ||
	convertFunctionRequiresName(cv->type)) )
    value = (CharArray) CtoKeyword(strName(value));

  if ( (val = checkType(value, cv->type, cv->context)) )
  { if ( !includesType(cv->type, nameToType(NAME_font)) )
      errorPce(cv, NAME_oldDefaultFormat, value);

    answer(val);
  }

  fail;
}


static ClassVariable
getSubClassVariable(ClassVariable cv, Class class)
{ if ( cv->context == class )
  { answer(cv);
  } else
  { Any val;
    Name name = class->name;

    if ( (val = getDefault(class, name, FALSE)) )
    { ClassVariable clone = get(cv, NAME_clone, EAV);

      assert(clone);
      contextClassVariable(clone, class);
      doneObject(val);			/* What to do with this? */

      answer(clone);
    } else
    { answer(cv);
    }
  }
}


static CharArray
getStringValueClassVariable(ClassVariable cv)
{ Any val;
  Class class = cv->context;

  if ( (val = getDefault(class, cv->name, TRUE)) )
    answer(val);

  fail;
}


Any
getValueClassVariable(ClassVariable cv)
{ if ( cv->value == NotObtained )
  { CharArray str;
    Any rval = FAIL;

    if ( (str = getStringValueClassVariable(cv)) )
    { if ( !(rval=qadGetv(cv, NAME_convertString, 1, (Any *)&str)) )
	errorPce(cv, NAME_cannotConvertDefault, str);
    }

    if ( !rval )
    { if ( onDFlag(cv, DCV_TEXTUAL) )
	rval = qadGetv(cv, NAME_convertString, 1, (Any *)&cv->cv_default);
      else
	rval = checkType(cv->cv_default, cv->type, cv->context);

      if ( !rval )
      { errorPce(cv, NAME_cannotConvertProgramDefault, cv->cv_default);
	fail;
      }
    }

    assign(cv, value, rval);
    if ( str )
      doneObject(str);
  }
  
  answer(cv->value);
}


static status
valueClassVariable(ClassVariable cv, Any value)
{ Any val;

  if ( (val = checkType(value, cv->type, cv->context)) )
  { assign(cv, value, val);

    succeed;
  }

  return errorTypeMismatch(cv,
			   getMethodFromFunction((Any)valueClassVariable), 1,
			   cv->type,
			   value);
}


		/********************************
		*         CLASS CATCHALL	*
		********************************/

static void
fixInstanceProtoClass(Class class)
{ if ( class->realised == ON )
  { unallocInstanceProtoClass(class);

    if ( notNil(class->sub_classes) )
    { Cell cell;

      for_cell(cell, class->sub_classes)
	fixInstanceProtoClass(cell->value);
    }
  }
}


static status
appendClassVariableClass(Class class, ClassVariable cv)
{ Cell cell;

  fixInstanceProtoClass(class);
  realiseClass(class);

  for_cell(cell, class->class_variables)
  { ClassVariable r2 = cell->value;

    if ( r2->name == cv->name )
    { cellValueChain(class->class_variables, PointerToInt(cell), cv);
      succeed;
    }
  }

  return appendChain(class->class_variables, cv);
}


ClassVariable
getClassVariableClass(Class class, Name name)
{ ClassVariable cv;
  Cell cell;

  realiseClass(class);

  if ( isNil(class->class_variable_table) )
    assign(class, class_variable_table, newObject(ClassHashTable, EAV));
  else if ( (cv=getMemberHashTable(class->class_variable_table, name)) )
    answer(cv);
 
  for_cell(cell, class->class_variables)
  { cv = cell->value;
    
    if ( cv->name == name )
    { appendHashTable(class->class_variable_table, name, cv);
      answer(cv);
    }
  }

  if ( notNil(class->super_class) )
  { ClassVariable super = getClassVariableClass(class->super_class, name);

    if ( super )
    { cv = getSubClassVariable(super, class);
      appendHashTable(class->class_variable_table, name, cv);
      answer(cv);
    }
  }
  
  fail;
}


status
classVariableValueClass(Class cl, Name name, Any val)
{ ClassVariable cv;

  if ( (cv = getClassVariableClass(cl, name)) )
    return valueClassVariable(cv, val);

  fail;
}


Any
getClassVariableValueClass(Class cl, Name name)
{ ClassVariable cv;

  if ( (cv = getClassVariableClass(cl, name)) )
    answer(getValueClassVariable(cv));

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: default value is *NOT* copied, so   the  caller must pass a string
that will not be deleted: either a   character constant or a value saved
using save_string.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
attach_class_variable(Class cl,
		      Name name, const char *type,
		      const char *def,
		      const char *doc)
{ ClassVariable cv;
  StringObj  s = (doc && strlen(doc) > 0 ? CtoString(doc) : DEFAULT);
  Name	    tp = (type ? CtoName(type) : DEFAULT);

					/* TBD: Default value! */
  if ( (cv = newObject(ClassClassVariable,
		       cl, name, DEFAULT, tp, s, EAV)) )
  { assign(cv, cv_default, staticCtoString(def));
    setDFlag(cv, DCV_TEXTUAL);		/* value is textual */

    succeed;
  }

  fail;
}


status
refine_class_variable(Class cl, const char *name_s, const char *def)
{ Class super;
  Name name = CtoName(name_s);

  for( super = cl->super_class; notNil(super); super = super->super_class)
  { Cell cell;
    
    for_cell(cell, super->class_variables)
    { ClassVariable cv = cell->value;
      
      if ( cv->name == name )		/* found it! */
      { ClassVariable cv2;

	if ( (cv2 = newObject(ClassClassVariable,
			      cl, name, DEFAULT, cv->type, cv->summary, EAV)) )
	{ assign(cv2, cv_default, staticCtoString(def));
	  setDFlag(cv2, DCV_TEXTUAL);		/* value is textual */

	  succeed;
	}
	assert(0);
      }
    }
  }
				       
  sysPce("Could not find super-class-variable to refine %s.%s\n",
	 pp(cl->name), name_s);
  fail;					/* NOTREACHED */
}


		/********************************
		*         MANUAL SUPPORT	*
		********************************/


static Name
getManIdClassVariable(ClassVariable cv)
{ char buf[LINESIZE];

  sprintf(buf, "R.%s.%s",
	  strName(((Class)cv->context)->name),
	  strName(cv->name));

  answer(CtoName(buf));
}


static Name
getManIndicatorClassVariable(ClassVariable cv)
{ answer(CtoName("R"));			/* TBD: may be a class-variable :-) */
}


static StringObj
getManSummaryClassVariable(ClassVariable cv)
{ char buf[LINESIZE];
  StringObj tmp;

  buf[0] = EOS;
  strcat(buf, "R\t");

  strcat(buf, strName(((Class)cv->context)->name));
  strcat(buf, ".");
  strcat(buf, strName(cv->name));
  strcat(buf, ": ");
  strcat(buf, strName(getCapitaliseName(cv->type->fullname)));

  if ( (tmp = getSummaryClassVariable(cv)) )
  { strcat(buf, "\t");
    strcat(buf, strName(tmp));
  }
  if ( send(cv, NAME_hasHelp, EAV) )
    strcat(buf, " (+)");

  answer(CtoString(buf));
}


static Name
getPrintNameClassVariable(ClassVariable cv)
{ char buf[LINESIZE];

  sprintf(buf, "%s.%s",
	  strName(((Class)cv->context)->name),
	  strName(cv->name));

  answer(CtoName(buf));
}


static Name
getGroupClassVariable(ClassVariable cv)
{ Variable var;

  if ( (var = getInstanceVariableClass(cv->context, cv->name)) )
    answer(getGroupVariable(var));

  fail;
}

		 /*******************************
		 *	     BEHAVIOUR		*
		 *******************************/

static Any
getGetClassVariable(ClassVariable var, Any rec)
{ answer(getValueClassVariable(var));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "class=class", "name=name", "default=any",
	  "type=[type]", "summary=[string]*" };

/* Instance Variables */

static vardecl var_class_variable[] =
{ IV(NAME_type, "type", IV_GET,
     NAME_type, "Type of the class-variable"),
  IV(NAME_value, "any", IV_NONE,
     NAME_cache, "Current value"),
  IV(NAME_default, "any", IV_GET,
     NAME_default, "Program default value"),
  IV(NAME_summary, "[string]*", IV_GET,
     NAME_manual, "Summary documentation")
};

/* Send Methods */

static senddecl send_class_variable[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseClassVariable,
     DEFAULT, "Create and associate to class"),
  SM(NAME_value, 1, "any", valueClassVariable,
     NAME_value, "Set value of the class variable")
};

/* Get Methods */

static getdecl get_class_variable[] =
{ GM(NAME_group, 0, "name", NULL, getGroupClassVariable,
     NAME_manual, "Same as related variable"),
  GM(NAME_manId, 0, "name", NULL, getManIdClassVariable,
     NAME_manual, "Card Id for class variable"),
  GM(NAME_manIndicator, 0, "name", NULL, getManIndicatorClassVariable,
     NAME_manual, "Manual type indicator (`R')"),
  GM(NAME_manSummary, 0, "string", NULL, getManSummaryClassVariable,
     NAME_manual, "New string with documentation summary"),
  GM(NAME_printName, 0, "name", NULL, getPrintNameClassVariable,
     NAME_textual, "class.name"),
  GM(NAME_convertString, 1, "any|function", "textual=char_array",
     getConvertStringClassVariable,
     NAME_value, "Convert textual value into typed value"),
  GM(NAME_stringValue, 0, "char_array", NULL, getStringValueClassVariable,
     NAME_value, "Obtain and return value as a char_array"),
  GM(NAME_value, 0, "any", NULL, getValueClassVariable,
     NAME_value, "Compute and return the value"),
  GM(NAME_get, 1, "unchecked", "object", getGetClassVariable,
     NAME_execute, "Invoke (read) class-variable")
};

/* ClassVariables */

#define rc_class_variable NULL
/*
static classvardecl rc_class_variable[] =
{ 
};
*/

/* Class Declaration */

static Name cv_termnames[] = { NAME_context, NAME_name, NAME_default,
			       NAME_type, NAME_summary };

ClassDecl(class_variable_decls,
          var_class_variable, send_class_variable, get_class_variable, rc_class_variable,
          5, cv_termnames,
          "$Rev$");


status
makeClassClassVariable(Class class)
{ declareClass(class, &class_variable_decls);

  cloneStyleVariableClass(class, NAME_summary, NAME_reference);
  cloneStyleVariableClass(class, NAME_value,   NAME_reference);

  NotObtained=globalObject(NAME_notObtained, ClassConstant,
			   NAME_notObtained,
			   CtoString("Value of not-obtained class-variable"),
			   EAV);

  succeed;
}

		 /*******************************
		 *    FETCH FROM Defaults file	*
		 *******************************/

static ChainTable ClassVariableTable;
static Name name_star;			/* '*' */

#define LBUFSIZE 256			/* initial value buffer */
#define MAXFIELDS 10			/* Max # x.y.z... fields */
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif

static void
add_class_variable(int nfields, Name *fields, StringObj value)
{ if ( nfields > 0 )
  { Name resname = fields[nfields-1];

    if ( resname != name_star )
    { Any argv[10];
      int i, argc;

      for(argc = 0, i=0; i < (nfields-1); i++)
	argv[argc++] = fields[i];
      argv[argc++] = value;

      appendChainTable(ClassVariableTable, resname,
		       newObjectv(ClassVector, argc, argv));
    }
  }
}


static char *
matchword(const char *s, const char *m)
{ while(*m && *s == *m)
    m++, s++;

  if ( !*m && islayout(*s) )
    return (char *)s;

  return NULL;
}


static StringObj
getword(const char *s, char **end)
{ string str;
  const char *e;

  for(e=s; !islayout(*e); e++)
    ;
  str_set_n_ascii(&str, e-s, (char *)s);
  if ( end )
    *end = (char *)e;

  return StringToString(&str);
}


static status
loadDefaultClassVariables(SourceSink f)
{ int lineno = 0;
  IOSTREAM *fd;

  if ( (fd = Sopen_object(f, "rbr")) )
  { char line[LINESIZE];

    while( Sfgets(line, sizeof(line), fd) )
    { char *s = line;
      char *e;
      Name fields[MAXFIELDS];
      int nfields = 0;
      StringObj value;

      lineno++;

      while(isblank(*s))
	s++;
      if ( s[0] == '!' || s[0] == '\n' )
	continue;

      if ( s[0] == '#' )		/* #include file */
      { s++;
	while(isblank(*s))
	  s++;
	if ( (s = matchword(s, "include")) )
	{ while(isblank(*s))
	    s++;
	  if ( s )
	  { StringObj fn = getword(s, NULL);
	    Any fincluded = newObject(ClassFile, fn, EAV);
	    
	    if ( send(fincluded, NAME_exists, EAV) )
	      loadDefaultClassVariables(fincluded);

	    doneObject(fincluded);
	  }
	}
	continue;
      }

      for(;;)
      { if ( isalnum(*s) )
	{ string str;

	  for(e=s; isalnum(*e); e++)
	    ;
	  str_set_n_ascii(&str, e-s, s);
	  fields[nfields++] = StringToName(&str);
	  s = e;
	  DEBUG(NAME_default, Cprintf("found %s\n", pp(fields[nfields-1])));
	  continue;
	}

	if ( *s == '*' )
	{ fields[nfields++] = name_star;
	  DEBUG(NAME_default, Cprintf("found %s\n", pp(fields[nfields-1])));
	  s++;
	  continue;
	}
	  
	if ( *s == '.' )		/* field separator */
	{ s++;
	  continue;
	}

	if ( *s == ':' )		/* value separator */
	{ char localbuf[LBUFSIZE];
	  char *buf = localbuf;
	  int bufsize = LBUFSIZE;
	  int size = 0;
	  int l;
	  string str;

	  s++;				/* skip the ':' */

	  for(;;)
	  { for(s++; isblank(*s); s++)
	      ;
	    l = strlen(s);
					/* delete [\r\n]*$ */
	    while( l > 0 && (s[l-1] == '\n' || s[l-1] == '\r') )
	      s[--l] = EOS;
					/* make buffer big enough */
	    while ( size + l > bufsize )
	    { bufsize *= 2;
	      if ( buf == localbuf )
	      { buf = pceMalloc(bufsize);
		strncpy(buf, localbuf, size);
	      } else
		buf = pceRealloc(buf, bufsize);
	    }

					/* copy the new line to the buf */
	    strncpy(&buf[size], s, l);
	    size += l;

					/* continue if ended in a `\' */
	    if ( s[l-1] == '\\' )
	    { buf[size-1] = ' ';
	      if ( !Sfgets(line, sizeof(line), fd) )
	      { errorPce(PCE, NAME_defaultSyntaxError, f, toInt(lineno));
		goto out;
	      }
	      s = line;
	      
	      continue;
	    }

	    break;
	  }
	  
	  str_set_n_ascii(&str, size, buf);
	  value = StringToString(&str);
	  DEBUG(NAME_default, Cprintf("Value = %s\n", pp(value)));
	  add_class_variable(nfields, fields, value);
	  goto next;
	} else
	{ errorPce(PCE, NAME_defaultSyntaxError, f, toInt(lineno));
	  goto next;
	}
      }
    next:
      ;
    }
    out:
      ;

    Sclose(fd);
    succeed;
  }

  fail;
}


status
loadDefaultsPce(Pce pce, SourceSink from)
{ if ( !ClassVariableTable )
    ClassVariableTable = globalObject(NAME_defaultTable, ClassChainTable, EAV);

  if ( isDefault(from) )
    from = pce->defaults;

  if ( send(from, NAME_access, NAME_read, EAV) )
  { loadDefaultClassVariables(from);

    succeed;
  }

  fail;
}


static int
class_match(Class class, Name name)
{ int ok = 100;

  for( ; notNil(class); class = class->super_class, ok-- )
  { if ( class->name == name )
      return ok;
  }

  return 0;				/* no match */
}



static StringObj
getDefault(Class class, Name name, int accept_default)
{ Chain ch;
  static int initialized = FALSE;

  if ( !initialized )
  { Code code;

    initialized = TRUE;

    name_star = CtoName("*");

    if ( !ClassVariableTable )
      loadDefaultsPce(PCE, DEFAULT);
    
    if ( (code = getClassVariableValueObject(PCE, NAME_initialise)) &&
	 instanceOfObject(code, ClassCode) )
      forwardReceiverCodev(code, PCE, 0, NULL);
  }

  ch = getMemberHashTable((HashTable)ClassVariableTable, name);

  if ( ch )
  { Vector best = NIL;
    int bestok = -1;
    Cell cell;

    for_cell(cell, ch)
    { Vector v = cell->value;
      int size = valInt(v->size);
      Any *elements = v->elements;
      int ok = 0;

      if ( size == 2 )			/* class.attribute */
      { Name cname = elements[0];

	if ( accept_default && cname == name_star )
	  ok = 10;
	else
	  ok = class_match(class, cname);

	DEBUG(NAME_default, Cprintf("%s using %s: ok = %d (e0=%s)\n",
				     pp(name), pp(v), ok, pp(cname)));

      }

      if ( ok && ok >= bestok )
      { best = v;
	bestok = ok;
      }
    }

    if ( notNil(best) )
      return getTailVector(best);
  }

  fail;					/* uses the default */
}

