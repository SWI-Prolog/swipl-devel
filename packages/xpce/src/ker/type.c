/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>
#include <h/trace.h>
#include <h/graphics.h>
#include <math.h>

#define MAX_TYPE_TRANSLATE_NESTING 10

static status	kindType(Type t, Name kind);

#define TV_CLASS	0
#define TV_OBJECT	1
#define TV_INT		2
#define TV_ARG		3
#define TV_VALUE	4
#define TV_VALUESET	5
#define TV_UNCHECKED	6
#define TV_ANY		7
#define TV_ALIEN	8
#define TV_NAMEOF	9
#define TV_INTRANGE	10
#define TV_REALRANGE	11
#define TV_MEMBER	12
#define TV_COMPOUND	13
#define TV_ALIAS	14
#define TV_CHAR		15
#define TV_EVENTID	16
#define TV_ATOMIC	17

status
initialiseType(Type t, Name name, Name kind, Any context, Chain supers)
{ assign(t, fullname, name);
  assign(t, argument_name, NIL);	/* default is typename */

  if ( getMemberHashTable(TypeTable, name) )
    return errorPce(t, NAME_nameAlreadyExists, name);

  initialiseProgramObject(t);

  if ( isDefault(supers) )
    supers = NIL;
  if ( isDefault(context) )
    context = NIL;

  assign(t, context, context);
  assign(t, supers, supers);
  assign(t, vector, OFF);
  TRY(kindType(t, kind));

  appendHashTable(TypeTable, name, t);
  protectObject(t);

  succeed;
}


Type
getLookupType(Class class, Name name)
{ answer(getMemberHashTable(TypeTable, name));
}


static status
storeType(Type t, FileObj file)
{ return storeSlotsObject(t, file);
}


static status
loadType(Type t, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(t, fd, def));

  return kindType(t, t->kind);
}


static Type
getCopyType(Type t, Name name)
{ Type t2 = newObject(ClassType, name,
		      t->kind, t->context,
		      getCopyChain(t->supers),
		      0);
  if ( t2 )
    assign(t2, vector, t->vector);

  answer(t2);
}


Type
createType(Name name, Name kind, Any context)
{ Type t = alloc(sizeof(struct type));

  initHeaderObj(t, ClassType);
  lockObj(t);
  initialiseProgramObject(t);

  t->fullname = name;
  t->argument_name = name;
  t->context = NIL;
  t->supers = NIL;
  t->vector = OFF;
  t->kind = (Name) NIL;

  assign(t, context, context);
  kindType(t, kind);

  appendHashTable(TypeTable, name, t);

  createdObject(t, NAME_new);

  return t;
}


static Type
getConvertType(Class class, Name name)
{ answer(nameToType(name));
}

		/********************************
		*             NAMES		*
		********************************/

Name
getNameType(Type t)
{ char *s = strName(t->fullname);

  if ( isalnum(*s) )
  { while(isalnum(*s))
      s++;
    if ( *s == '=' )
      return CtoName(s+1);
  }

  return t->fullname;
}


		/********************************
		*         CHANGING TYPES	*
		********************************/

void
superType(Type t, Type t2)
{ if ( isNil(t->supers) )
    assign(t, supers, newObject(ClassChain, t2, 0));
  else
    appendChain(t->supers, t2);
}


status
vectorType(Type t, Bool val)
{ assign(t, vector, val);

  succeed;
}


		/********************************
		*            ???		*
		********************************/

status
isClassType(Type t)
{ return t->kind == NAME_class || t->kind == NAME_object;
}

#define SpecialClass(c) ( isAClass(c, ClassFunction) || \
			  isAClass(c, ClassHostData) )

static inline status
realiseClassType(Type t)
{ if ( t->validate_function == TV_CLASS &&
       onFlag(t->context, F_ISNAME) )
  { Class class;

    if ( (class=getConvertClass(ClassClass, t->context)) )
      assign(t, context, class);
    else
      fail;
  }

  succeed;
}


/* t1 is specialised regarding to t2 */

status
specialisedType(Type t1, Type t2)
{ while(t1->validate_function == TV_ALIAS )
    t1 = t1->context;
  realiseClassType(t1);

l2:
  switch(t2->validate_function)
  { case TV_ALIAS:
      t2 = t2->context;
      goto l2;
    case TV_CLASS:
      realiseClassType(t2);
      if ( t1->validate_function == TV_CLASS &&
	   isAClass(t1->context, t2->context) )
	succeed;
      break;
    case TV_OBJECT:
      if ( t1->validate_function == TV_CLASS && !SpecialClass(t1->context) )
	succeed;
      break;
    case TV_ANY:
      if ( !(t1->validate_function == TV_CLASS && SpecialClass(t1->context)) )
	succeed;
      break;
    case TV_UNCHECKED:
      succeed;
  }

  if ( t1->validate_function == t2->validate_function &&
       t1->context           == t2->context )
    succeed;

  switch(t1->validate_function)
  { case TV_NAMEOF:
      t1 = TypeName;
      goto l2;
    case TV_INTRANGE:
      t1 = TypeInt;
      goto l2;
    case TV_REALRANGE:
      t1 = TypeReal;
      goto l2;
    case TV_CHAR:
      t1 = TypeInt;
      goto l2;
  }

  if ( notNil(t2->supers) )
  { Cell cell;

    for_cell(cell, t2->supers)
    { if ( specialisedType(t1, cell->value) )
      	succeed;
    }
  }

  fail;
}


#if 0
status
specialisedType(Type t1, Type t2)	/* t1 is specialised regarding to t2 */
{ while(t1->kind == NAME_alias)
    t1 = t1->context;
  while(t2->kind == NAME_alias)
    t2 = t2->context;

  if ( t1 == t2 ||			/* Equivalence */
       (t1->context == t2->context && t1->kind == t2->kind) )
    succeed;
					/* normal subclassing */
  if ( t1->kind == NAME_class && t2->kind == NAME_class &&
       isAClass(t1->context, t2->context) )
    succeed;
  
					/* class object */
  if ( t2->kind == NAME_object && t1->kind == NAME_class &&
       !SpecialClass(t1->context) )
    succeed;

					/* relative to any */
  if ( t2->kind == NAME_any &&
       !(t1->kind == NAME_class && SpecialClass(t1->context)) )
    succeed;

  if ( t2->kind == NAME_unchecked )
    succeed;

					/* special types. */
  if ( t1->kind == NAME_nameOf )
    return specialisedType(TypeName, t2);
  if ( t1->kind == NAME_intRange )
    return specialisedType(TypeInt, t2);
  if ( t1->kind == NAME_realRange )
    return specialisedType(TypeReal, t2);
  if ( t1->kind == NAME_char )
    return specialisedType(TypeInt, t2);

  if ( notNil(t2->supers) )
  { Cell cell;

    for_cell(cell, t2->supers)
      if ( specialisedType(t1, cell->value) )
      	succeed;
  }

  fail;
}
#endif


status
equalType(Type t1, Type t2)
{ while(t1->kind == NAME_alias)
    t1 = t1->context;
  while(t2->kind == NAME_alias)
    t2 = t2->context;

  if ( t1 == t2 )
    succeed;

  fail;
}



status
includesType(Type t1, Type t2)		/* t1 includes t2 */
{ while(t1->kind == NAME_alias)
    t1 = t1->context;
  while(t2->kind == NAME_alias)
    t2 = t2->context;

  if ( t1 == t2 ||
       (t1->context == t2->context && t1->kind == t2->kind) )
    succeed;

  if ( notNil(t1->supers) )
  { Cell cell;

    for_cell(cell, t1->supers)
    { if ( includesType(cell->value, t2) )
      	succeed;
    }
  }

  fail;
}


static void
value_set_type(Type t, Any ctx, Chain *set)
{ Chain ch = *set;

  if ( t->kind == NAME_nameOf )
  { if ( ch )
      mergeChain(ch, t->context);
    else
      ch = getCopyChain(t->context);
  } else if ( t->kind == NAME_class )
  { if ( t->context == ClassBool )
    { if ( ch )
      { appendChain(ch, ON);
	appendChain(ch, OFF);
      } else
	ch = answerObject(ClassChain, ON, OFF, 0);
    }
  } else if ( t->kind == NAME_value )
  { if ( ch )
      appendChain(ch, t->context);
    else
      ch = answerObject(ClassChain, t->context, 0);
  } else if ( t->kind == NAME_intRange )
  { Tuple tpl = t->context;
    int n;

    if ( valInt(tpl->second) - valInt(tpl->first) < 10 )
    { if ( !ch )
	ch = answerObject(ClassChain, 0);
      for(n=valInt(tpl->first); n<=valInt(tpl->second); n++)
	appendChain(ch, toInt(n));
    }
  } else if ( t->kind == NAME_valueSet )
  { Chain ch2;
    
    if ( isFunction(t->context) )
    { if ( !(ch2 = getForwardReceiverFunctionv(t->context, ctx, 1, &ctx)) ||
	   !instanceOfObject(ch2, ClassChain) )
	ch2 = FAIL;
    } else if ( instanceOfObject(t->context, ClassQuoteFunction) )
    { Any f = ((QuoteFunction)t->context)->function;

      if ( !(ch2 = getForwardReceiverFunctionv(f, ctx, 1, &ctx)) ||
	   !instanceOfObject(ch2, ClassChain) )
	ch2 = FAIL;
    } else
      ch2 = t->context;

    if ( ch2 )
    { if ( ch )
	mergeChain(ch, ch2);
      else
	ch = getCopyChain(ch2);
    }
  } else if ( t->kind == NAME_alias )
  { value_set_type(t->context, ctx, &ch);
  }

  if ( notNil(t->supers) )
  { Cell cell;

    for_cell(cell, t->supers)
      value_set_type(cell->value, ctx, &ch);
  }

  *set = ch;
}


Chain
getValueSetType(Type t, Any ctx)
{ Chain rval = FAIL;

  value_set_type(t, ctx, &rval);

  answer(rval);
}

		/********************************
		*   TYPE CHECKING/CONVERSION	*
		********************************/

static int translate_type_nesting = 0;

Any
getTranslateType(Type t, Any val, Any ctx)
{ Any rval;

  CheckTypeError = CTE_OK;

  if ( isObject(val) && onFlag(val, F_ACTIVE|F_ISHOSTDATA) )
  { if ( onFlag(val, F_ISHOSTDATA) )
    { if ( !(val = (*TheCallbackFunctions.translate)(val, t)) )
	fail;
      if ( validateType(t, val, ctx) )
	return val;
      if ( !isFunction(val) )
	goto skipf;
    }
					/* i.e. isFunction(val) */
    if ( !(val = expandFunction(val)) )
    { CheckTypeError = CTE_OBTAINER_FAILED;
      fail;
    }
    if ( validateType(t, val, ctx) )
      return val;
  }

skipf:
  if ( translate_type_nesting++ > MAX_TYPE_TRANSLATE_NESTING )
  { errorPce(t, NAME_typeLoop, val);
    fail;
  }

  rval = (*t->translate_function)(t, val, ctx);
  if ( rval )
    goto out;

  if ( notNil(t->supers) )
  { Cell cell;

    for_cell(cell, t->supers)
    { rval = getTranslateType(cell->value, val, ctx);
      if ( rval )
	goto out;
    }
  }
  
out:
  translate_type_nesting--;
  return rval;
}


static Any
getCheckType(Type t, Any val, Any ctx)
{ if ( isDefault(ctx) )
    ctx = NIL;

  answer(checkType(val, t, ctx));
}


		/********************************
		*        VALIDATE-FUNCTIONS	*
		********************************/

static inline status
failType(const Type t, const Any val, const Any ctx)
{ fail;
}


static inline status
succeedType(const Type t, const Any val, const Any ctx)
{ succeed;
}


static inline status
valueType(const Type t, const Any val, const Any ctx)
{ if ( val == t->context )
    succeed;

  fail;
}


static inline status
intType(const Type t, const Any val, const Any ctx)
{ return isInteger(val);
}


static inline status
classType(const Type t, const Any val, const Any ctx)
{ if ( onFlag(t->context, F_ISNAME) )	/* isName(), but it *is* an object */
  { Class class;

    if ( (class=getConvertClass(ClassClass, t->context)) )
      assign(t, context, class);
    else
      fail;
  }
      
  return instanceOfObject(val, t->context);
}


static inline status
objectType(const Type t, const Any val, const Any ctx)
{ if ( isObject(val) && !onFlag(val, F_ACTIVE|F_ISHOSTDATA) )
    succeed;

  fail;
}


static inline status
anyType(const Type t, const Any val, const Any ctx)
{ if ( isObject(val) )
    return offFlag(val, F_ACTIVE|F_ISHOSTDATA);

  succeed;
}


static inline status
argType(const Type t, const Any val, const Any ctx)
{ return isFunction(val);
}


static inline status
charType(const Type t, const Any val, const Any ctx)
{ return isInteger(val) && valInt(val) >= 0 && valInt(val) <= 2*META_OFFSET;
}


static inline status
eventIdType(const Type t, const Any val, const Any ctx)
{ return charType(t, val, ctx) || (isName(val) && eventName(val));
}


static inline status
atomicType(const Type t, const Any val, const Any ctx)
{ if ( isInteger(val) )
    succeed;
  if ( isObject(val) && onFlag(val, F_ISREAL|F_ISNAME) )
    succeed;

  fail;
}


static inline status
nameOfType(const Type t, const Any val, const Any ctx)
{ if ( isName(val) )
    return memberChain(t->context, val);

  fail;
}


static inline status
valueSetType(const Type t, const Any val, const Any ctx)
{ if ( isFunction(t->context) )
  { Any rval;

    if ( (rval = getForwardReceiverFunctionv(t->context, ctx, 1, &ctx)) &&
	 instanceOfObject(rval, ClassChain) &&
	 memberChain(rval, val) )
      succeed;
  } else if ( instanceOfObject(t->context, ClassQuoteFunction) )
  { Any rval;
    Any f = ((QuoteFunction)t->context)->function;

    if ( (rval = getForwardReceiverFunctionv(f, ctx, 1, &ctx)) &&
	 instanceOfObject(rval, ClassChain) &&
	 memberChain(rval, val) )
      succeed;
  } else
    return memberChain(t->context, val);

  fail;
}


static inline status
intRangeType(const Type t, const Any val, const Any ctx)
{ if ( isInteger(val) )
  { Tuple tp = t->context;
    int i = valInt(val);

    if ( i >= valInt(tp->first) && i <= valInt(tp->second) )
      succeed;
  }
  
  fail;
}


static inline status
realRangeType(const Type t, const Any val, const Any ctx)
{ if ( instanceOfObject(val, ClassReal) )
  { Tuple tp = t->context;
    Real low = tp->first, high = tp->second, r = val;

    if ( valReal(r) >= valReal(low) && valReal(r) <= valReal(high) )
      succeed;
  }
  
  fail;
}


static inline status
memberType(const Type t, const Any val, const Any ctx)
{ return validateType(t->context, val, ctx);
}


static inline status
aliasType(const Type t, const Any val, const Any ctx)
{ return validateType(t->context, val, ctx);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: only alien and unchecked types   pass HostData. Anything else will
force conversion.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
validateType(Type t, const Any val, const Any ctx)
{ int rval;
  again:

  switch( t->validate_function )
  { case TV_CLASS:	rval = classType(t, val, ctx);		break;
    case TV_OBJECT:	rval = objectType(t, val, ctx);		break;
    case TV_INT:	rval = intType(t, val, ctx);		break;
    case TV_ARG:	rval = argType(t, val, ctx);		break;
    case TV_VALUE:	rval = valueType(t, val, ctx);		break;
    case TV_VALUESET:	rval = valueSetType(t, val, ctx);	break;
    case TV_UNCHECKED:	succeed;
    case TV_ANY:	rval = anyType(t, val, ctx);		break;
    case TV_ALIEN:	succeed;
    case TV_NAMEOF:	rval = nameOfType(t, val, ctx);		break;
    case TV_INTRANGE:	rval = intRangeType(t, val, ctx);	break;
    case TV_REALRANGE:	rval = realRangeType(t, val, ctx);	break;
    case TV_MEMBER:	rval = memberType(t, val, ctx);		break;
    case TV_COMPOUND:	rval = FAIL;				break;
    case TV_ALIAS:	if ( isNil(t->supers) )
			{ t = t->context;
			  goto again;
			} else
			{ rval = aliasType(t, val, ctx);
			  break;
			}
    case TV_CHAR:	rval = charType(t, val, ctx);		break;
    case TV_EVENTID:	rval = eventIdType(t, val, ctx);	break;
    case TV_ATOMIC:	rval = atomicType(t, val, ctx);		break;
    default:
      return sysPce("%s: Invalid type.  Kind is %s, validate = 0x%x",
		    pp(t), pp(t->kind), (int)t->validate_function);
  }

/*if ( isHostData(val) )
    fail;
*/

  if ( rval )
    return rval;

  if ( notNil(t->supers) )
  { Cell cell;

    for_cell(cell, t->supers)
    { if ( validateType(cell->value, val, ctx) )
      	succeed;
    }
  }

  fail;
}

		/********************************
		*        CONVERT-FUNCTIONS	*
		********************************/

static Any
getFailType(const Type t, const Any val, const Any ctx)
{ fail;
}


static Any
getIntType(const Type t, const Any val, const Any ctx)
{ return (Any) toInteger(val);
}


static int
charpToChar(char *s)
{ if ( s[0] != EOS && s[1] == EOS )
    return s[0];

  if ( s[0] == '\\' && s[2] == EOS )
  { switch(s[1])
    { case 'n':	 return '\n';
      case 't':	 return '\t';
      case 'f':	 return '\f';
      case 'b':	 return '\b';
      case 'r':	 return '\r';
      case '\\': return '\\';
      default:	 return -1;
    }
  } else if ( s[0] == '^' && s[1] != EOS && s[2] == EOS )
  { return toupper(s[1]) - '@';
  } else if ( prefixstr(s, "\\C-") && s[4] == EOS )
  { return toupper(s[3]) - '@';
  } else if ( prefixstr(s, "M-") || prefixstr(s, "\\e") )
  { int c;

    if ( (c = charpToChar(s+2)) >= 0 )
      return c + META_OFFSET;
  }

  return -1;
}


static Any
getCharType(const Type t, const Any val, const Any ctx)
{ if ( instanceOfObject(val, ClassCharArray) )
  { CharArray ca = val;
    String s = &ca->data;
    int c;

    if ( s->size == 1 )
      return toInt(str_fetch(s, 0));
    if ( isstr8(s) && (c = charpToChar((char *)s->s_text)) >= 0 )
      return toInt(c);
  } else
  { Int i = toInteger(val);

    if ( valInt(i) >= 0 && valInt(i) <= 2*META_OFFSET )
      return i;
  }

  fail;
}


static Any
getEventIdType(const Type t, const Any val, const Any ctx)
{ Any rval;

  if ( instanceOfObject(val, ClassEvent) )
    return getIdEvent(val);

  if ( (rval = getCharType(t, val, ctx)) )
    return rval;

  TRY(rval = toName(val));
  if ( eventName(rval) )
    return rval;

  fail;
}


static Any
getAtomicType(const Type t, const Any val, const Any ctx)
{ Any rval;

  if ( (rval = toInteger(val)) )
    return rval;
  if ( (rval = toReal(val)) )
    return rval;
  if ( (rval = toName(val)) )
    return rval;

  fail;
}


static Any
getClassType(const Type t, const Any val, const Any ctx)
{ Class class = t->context;

  if ( isName(class) )
  { if ( (class = getConvertClass(ClassClass, t->context)) )
      assign(t, context, class);
    else
    { errorPce(t, NAME_unresolvedType);
      fail;
    }
  }

  realiseClass(class);
  if ( isDefault(class->convert_method) )
  { GetMethod m;

    if ( (m=getGetMethodClass(class, NAME_convert)) )
    { assign(class, convert_method, m);
      setDFlag(m, D_TYPENOWARN);
    }
  }

  if ( notNil(class->convert_method) )
    return getGetGetMethod(class->convert_method, ctx, 1, &val);

  fail;
}


static Any
getValueType(const Type t, const Any val, const Any ctx)
{ Any obj;

  if ( (obj = getConvertObject(ctx, val)) && valueType(t, obj, ctx) )
    return obj;

  fail;
}


static Any
convertValueSetType(const Type t, const Any val, const Any ctx)
{ Any obj;

  if ( (obj = getConvertObject(ctx, val)) && valueSetType(t, obj, ctx) )
    return obj;

  fail;
}


static Any
getNameOfType(const Type t, const Any val, const Any ctx)
{ Name name = getClassType(TypeName, val, ctx);
  
  if ( name != FAIL && nameOfType(t, (Any)name, ctx) )
    return (Any) name;

  fail;
}


static Any
getIntRangeType(const Type t, const Any val, const Any ctx)
{ Int i = (Int) getIntType(t, val, ctx);
  
  if ( i != FAIL && intRangeType(t, (Any)i, ctx) )
    return (Any) i;

  fail;
}


static Any
getRealRangeType(const Type t, const Any val, const Any ctx)
{ Real r = getConvertReal(ClassReal, val);
  
  if ( r != FAIL && realRangeType(t, r, ctx) )
    return r;

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This method should just call `get(ctx, NAME_member, 0)'; but this is not
possible as this might give typing errors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Any
getMemberType(const Type t, const Any val, const Any ctx)
{ GetMethod m;
  Type at;
  Any rval, a;

  if ( isObject(ctx) &&
       (m = getGetMethodClass(classOfObject(ctx), NAME_member)) &&
       instanceOfObject(m, ClassGetMethod) &&
       (at = getArgumentTypeMethod((Method) m, ONE)) &&
       (a = checkType(val, at, NIL)) &&
       (rval = getGetGetMethod(m, ctx, 1, &a)) )
    return rval;

  fail;
}


static Any
getAliasType(const Type t, const Any val, const Any ctx)
{ return getTranslateType(t->context, val, ctx);
}


static status
kindType(Type t, Name kind)
{ if ( equalName(kind, NAME_class) )
  { t->validate_function  = TV_CLASS;
    t->translate_function = getClassType;
  } else if ( equalName(kind, NAME_object) )
  { t->validate_function  = TV_OBJECT;
    t->translate_function = getClassType;
  } else if ( equalName(kind, NAME_int) )
  { t->validate_function  = TV_INT;
    t->translate_function = getIntType;
  } else if ( equalName(kind, NAME_arg) )
  { t->validate_function  = TV_ARG;
    t->translate_function = getFailType;
  } else if ( equalName(kind, NAME_value) )
  { t->validate_function  = TV_VALUE;
    t->translate_function = getValueType;
  } else if ( equalName(kind, NAME_valueSet) )
  { t->validate_function  = TV_VALUESET;
    t->translate_function = convertValueSetType;
  } else if ( equalName(kind, NAME_unchecked) )
  { t->validate_function  = TV_UNCHECKED;
    t->translate_function = getFailType;
  } else if ( equalName(kind, NAME_any) )
  { t->validate_function  = TV_ANY;
    t->translate_function = getFailType;
  } else if ( equalName(kind, NAME_alien) )
  { t->validate_function  = TV_ALIEN;
    t->translate_function = getFailType;
  } else if ( equalName(kind, NAME_nameOf) )
  { t->validate_function  = TV_NAMEOF;
    t->translate_function = getNameOfType;
  } else if ( equalName(kind, NAME_intRange) )
  { t->validate_function  = TV_INTRANGE;
    t->translate_function = getIntRangeType;
  } else if ( equalName(kind, NAME_realRange) )
  { t->validate_function  = TV_REALRANGE;
    t->translate_function = getRealRangeType;
  } else if ( equalName(kind, NAME_member) )
  { t->validate_function  = TV_MEMBER;
    t->translate_function = getMemberType;
  } else if ( equalName(kind, NAME_compound) )
  { t->validate_function  = TV_COMPOUND;
    t->translate_function = getFailType;
  } else if ( equalName(kind, NAME_alias) )
  { t->validate_function  = TV_ALIAS;
    t->translate_function = getAliasType;
  } else if ( equalName(kind, NAME_char) )
  { t->validate_function  = TV_CHAR;
    t->translate_function = getCharType;
  } else if ( equalName(kind, NAME_eventId) )
  { t->validate_function  = TV_EVENTID;
    t->translate_function = getEventIdType;
  } else if ( equalName(kind, NAME_atomic) )
  { t->validate_function  = TV_ATOMIC;
    t->translate_function = getAtomicType;
  } else
    return errorPce(t, NAME_noTypeKind, kind);

  assign(t, kind, kind);
  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=name", "kind=[name]", "context=[any]", "supers=[chain*]" };
static char *T_validate[] =
        { "unchecked", "[object]*" };
static char *T_valueAunchecked_contextADobjectDN[] =
        { "value=unchecked", "context=[object]*" };

/* Instance Variables */

static vardecl var_type[] =
{ SV(NAME_kind, "name", IV_GET|IV_STORE, kindType,
     NAME_check, "Type of type"),
  IV(NAME_fullname, "name", IV_GET,
     NAME_name, "Symbolic name for this type"),
  IV(NAME_argumentName, "name*", IV_GET,
     NAME_argument, "Name of the argument"),
  IV(NAME_supers, "chain*", IV_GET,
     NAME_components, "Super-types"),
  IV(NAME_context, "any", IV_GET,
     NAME_check, "Context for check- and convert functions"),
  IV(NAME_vector, "bool", IV_GET,
     NAME_argument, "Methods: variable number of arguments"),
  IV(NAME_validateFunction, "alien:SendFunc", IV_NONE,
     NAME_internal, "C-function to check this type"),
  IV(NAME_translateFunction, "alien:Func", IV_NONE,
     NAME_internal, "C-function to convert to this")
};

/* Send Methods */

static senddecl send_type[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseType,
     DEFAULT, "Create type from name, kind, context and supers"),
  SM(NAME_validate, 2, T_validate, validateType,
     NAME_check, "Validate argument is of this type"),
  SM(NAME_equal, 1, "type", equalType,
     NAME_compare, "Test if both types are the same"),
  SM(NAME_includes, 1, "type", includesType,
     NAME_meta, "Type includes its argument"),
  SM(NAME_specialised, 1, "type", specialisedType,
     NAME_meta, "Test if argument is a specialised type")
};

/* Get Methods */

static getdecl get_type[] =
{ GM(NAME_check, 2, "unchecked", T_valueAunchecked_contextADobjectDN, getCheckType,
     NAME_check, "Validate and translate if necessary"),
  GM(NAME_convert, 1, "type", "name", getConvertType,
     NAME_check, "Convert symbolic type-name"),
  GM(NAME_translate, 2, "unchecked", T_valueAunchecked_contextADobjectDN, getTranslateType,
     NAME_check, "Translate argument given context"),
  GM(NAME_copy, 1, "type", "name", getCopyType,
     NAME_copy, "Get a copy of a type with a different name"),
  GM(NAME_valueSet, 1, "chain", "[object]*", getValueSetType,
     NAME_meta, "Chain with values that satisfy this type"),
  GM(NAME_name, 0, "name", NULL, getNameType,
     NAME_name, "Name without argument specification"),
  GM(NAME_lookup, 1, "type", "name", getLookupType,
     NAME_oms, "Lookup type in type-database")
};

/* Resources */

#define rc_type NULL
/*
static classvardecl rc_type[] =
{ 
};
*/

/* Class Declaration */

static Name type_termnames[] = { NAME_name, NAME_kind,
				 NAME_context, NAME_supers };

ClassDecl(type_decls,
          var_type, send_type, get_type, rc_type,
          4, type_termnames,
          "$Rev$");


status
makeClassType(Class class)
{ declareClass(class, &type_decls);

  setLoadStoreFunctionClass(class, loadType, storeType);
  cloneStyleClass(class, NAME_none);

  succeed;
}


		/********************************
		*         CREATING TYPES	*
		********************************/

static Type
createClassType(Name name)
{ Type type;

  if ( (type = getMemberHashTable(TypeTable, name)) )
    return type;
  else
    return createType(name, NAME_class, inBoot ? (Any) typeClass(name)
		      			       : (Any) name);
}

		/********************************
		*           CONVERSION		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type syntax:

	Type		::= SingleType
			  | SingleType '...'
			  | 'alien:' Ctype
	SingleType	::= PrimType
			  | SingleType '*'
			  | '[' SingleType ']'
			  | SingleType '|' SingleType
	SingleType	::= 'int'
			  | 'any'
			  | 'unchecked'
			  | ClassName
			  | Int '...' Int
			  | Int '..'
			  | '..' Int
			  | Float '...' Float
			  | Float '..'
			  | '..' Float
			  | {Atom, ...}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ char *start;
  char *end;
  char text[LINESIZE];
} tmp_string, *TmpString;


forwards void	strip_string(TmpString);
forwards void	init_string(TmpString, String);
forwards int	suffix_string(TmpString, char *);

static void
strip_string(TmpString s)
{ while(*s->start == ' ')
    s->start++;
  while(*s->end == ' ' && s->end >= s->start)
    *s->end-- = EOS;
}


static void
init_string(TmpString s, String t)
{ assert(isstr8(t));
  strcpy(s->text, (char *)t->s_text);
  s->start = s->text;
  s->end = &s->text[t->size - 1];
  strip_string(s);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check whether `s' has suffix `suff' and something non-blank before the
suffix.  If so, delete the suffix and trailing blanks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
suffix_string(TmpString s, char *suff)
{ char *ts = suff + strlen(suff) - 1;
  char *es = s->end;

  for(; *ts == *es; ts--, es--)
  { if ( ts == suff )
    { es--;
      while(*es == ' ' && es >= s->start)
    	es--;

      if ( es >= s->start )
      { s->end = es;
	s->end[1] = EOS;

	return TRUE;
      } else
	return FALSE;
    }
  }

  return FALSE;
}


static int
prefix_string(TmpString s, char *pref)
{ char *q = s->start;

  while(*pref && *pref == *q)
    pref++, q++;

  if ( !*pref )
  { s->start = q;
    strip_string(s);
    return TRUE;
  }

  return FALSE;
}



static Type
name_of_type(TmpString str)
{ if ( *str->start == '{' && *str->end == '}' )
  { Type type = newObject(ClassType, CtoName(str->start),
			  NAME_nameOf, newObject(ClassChain, 0), 0);
    char *s, *e;

    str->start++;
    strip_string(str);
    while(str->start < str->end)
    { for(s=str->start; s<str->end && *s != ','; s++)
	;
      for(e=s-1; e > str->start && *e == ' '; e--)
	;
      e[1] = EOS;
      appendChain(type->context, CtoName(str->start));
      str->start = s+1;
      strip_string(str);
    }

    return type;
  }
  
  fail;
}


static Type
int_range_type(TmpString str)
{ char *e, *e2;
  long low, high;
  Type type;

  if ( *(e=str->start) == '.' )
    low = PCE_MIN_INT;
  else
  { low = strtol(str->start, &e, 10);
    if ( e == str->start )
      fail;
  }

  while( *e == ' ' )
    e++;
  if ( e[0] != '.' || e[1] != '.' )
    fail;
  e += 2;
  if ( e == str->end + 1 )
    high = PCE_MAX_INT;
  else
  { high = strtol(e, &e2, 10);
    if ( e2 != str->end+1 )
      fail;
  }
  type = newObject(ClassType, CtoName(str->start), NAME_intRange, 0);

  assign(type, context, newObject(ClassTuple,
				  toInt(low), toInt(high), 0));

  return type;
}


static Type
real_range_type(TmpString str)
{ char *e, *e2;
  double low, high;
  Type type;

  low = StrTod(str->start, &e);
  if ( e > str->start )
  { while( *e == ' ' )
      e++;
    if ( e[0] != '.' || e[1] != '.' )
      fail;
    e += 2;
    high = StrTod(e, &e2);
    if ( e2 != str->end+1 )
      fail;
    type = newObject(ClassType, CtoName(str->start), NAME_realRange, 0);

    assign(type, context, newObject(ClassTuple,
				    CtoReal(low), CtoReal(high), 0));

    return type;
  }

  fail;
}


static Type
disjunctive_type(TmpString str)
{ char *s;

  if ( (s = strchr(str->start, '|')) != NULL )
  { Type type;
    char *e;
    Name name = CtoName(str->start);

    *s = EOS;
    TRY(type = CtoType(str->start));
    TRY(type = getCopyType(type, name));
    s++;
    while( s < str->end && (e = strchr(s, '|')) != NULL )
    { *e = EOS;
      superType(type, CtoType(s));
      s = e+1;
    }
    if ( s < str->end )
      superType(type, CtoType(s));

    return type;
  }

  fail;
}


static Type
kind_type(TmpString str)
{ char *s;
  char *e;
  Name name, kind;
  Type type;

  if ( !isalnum(*str->start) )
    fail;
  for(s = str->start; isalnum(*s); s++)
    ;
  for(e=s; isblank(*e); e++)
    ;
  if ( *e != ':' )
    fail;

  name = CtoName(str->start);
  *s = EOS;
  kind = CtoName(str->start);
  str->start = e + 1;
  strip_string(str);

  TRY(type = newObject(ClassType, name, kind, 0));

  if ( equalName(kind, NAME_alien) )
    assign(type, context, CtoName(str->start));
  else if ( equalName(kind, NAME_member) )
    assign(type, context, CtoType(str->start));
  else
  { errorPce(type, NAME_noTypeKind, kind);
    fail;
  }

  return type;
}


static Type
named_type(TmpString str)
{ char *s;
  char *e;
  Name name, argname;
  Type type, rval;

  if ( !isalnum(*str->start) )
    fail;

  for(s = str->start; isalnum(*s); s++)
    ;
  for(e=s; isblank(*e); e++)
    ;
  if ( *e != '=' )
    fail;

  name = CtoName(str->start);
  *s = EOS;
  argname = CtoName(str->start);
  str->start = e + 1;
  strip_string(str);

  TRY(type = CtoType(str->start));
  TRY(rval = newObject(ClassType, name, NAME_alias, type, 0));
  assign(rval, vector, type->vector);
  assign(rval, argument_name, argname);

  return rval;
}


Type
nameToType(Name name)
{ Type type;
  tmp_string str;

  if ( (type = getMemberHashTable(TypeTable, name)) )
    return type;

  init_string(&str, &name->data);

  if ( (type = named_type(&str)) )
    return type;

  if ( prefix_string(&str, "alien:") )
  { TRY(type = newObject(ClassType, name, NAME_alien, 0));
    assign(type, context, CtoName(str.start));

    return type;
  }

  if ( suffix_string(&str, "...") )	/* SimpleType ... */
  { Name sn = CtoName(str.start);
    Type st;

    if ( (st = nameToType(sn)) )
    { Type t2 = getCopyType(st, name);
      
      vectorType(t2, ON);
      return t2;
    }
  } else
  { int arg=0, def=0, var=0;
    int och = -1, changed = 0;

    while(och != changed)
    { och = changed;

      if ( suffix_string(&str, "*") )
      { var++;
	changed++;
      } else if ( suffix_string(&str, "?") )
      { arg++;
	changed++;
      } else if ( *str.start == '[' && *str.end == ']' )
      { str.start++; *str.end-- = EOS;
	strip_string(&str);
	def++;
	changed++;
      }
    }

    if ( changed )
    { Name sn = CtoName(str.start);
      Type st;

      if ( (st = nameToType(sn)) )
      { Type t2 = getCopyType(st, name);

	if ( var ) superType(t2, TypeNil);
	if ( def ) superType(t2, TypeDefault);
	if ( arg ) superType(t2, TypeArg);

	return t2;
      }
    } else
    { if ( (type = name_of_type(&str)) )
    	return type;

      if ( (isdigit(*str.start) || *str.start == '.' || *str.start == '-') &&
	   (isdigit(*str.end) || *str.end == '.') )
      { if ( (type = int_range_type(&str)) )
	  return type;
	if ( (type = real_range_type(&str)) )
	  return type;
      }

      if ( (type = disjunctive_type(&str)) )
      	return type;

      if ( (type = kind_type(&str)) )
	return type;

      return createClassType(CtoName(str.start));
    }
  }

  errorPce(name, NAME_badTypeSyntax);
  fail;
}


		/********************************
		*             RESET		*
		********************************/

void
resetTypes(void)
{ translate_type_nesting = 0;
}


		/********************************
		*        INITIALISATION		*
		********************************/

struct built_in_type
{ Type *	global;
  Name		name;
  Name		kind;
  Any 		context;
} built_in_types[] =
{ { &TypeUnchecked, NAME_unchecked, NAME_unchecked, NIL },
  { &TypeAlien,	    NAME_alien,	    NAME_alien,     NIL },
  { &TypeAny,	    NAME_any,	    NAME_any,	    NIL },
  { &TypeNil,	    NAME_nil,	    NAME_value,     NIL },
  { &TypeDefault,   NAME_default,   NAME_value,     DEFAULT },
  { &TypeArg,       NAME_arg,       NAME_arg,	    NIL },
  { &TypeInt,       NAME_int,       NAME_int,	    NIL },
  { &TypeChar,      NAME_char,      NAME_char,	    NIL },
  { &TypeEventId,   NAME_eventId,   NAME_eventId,   NIL },
  { &TypeAtomic,    NAME_atomic,    NAME_atomic,    NIL },
  { NULL,	    NAME_none,	    NAME_none,      NIL }
};
    

static void
bootType(Name name, Class *classp, Type *typep)
{ *classp = typeClass(name);

  if ( typep )
    *typep  = createType(name, NAME_class, *classp);
  else
    createType(name, NAME_class, *classp);
}


void
initTypes(void)
{ struct built_in_type *i = built_in_types;

  TypeTable = createHashTable(toInt(256), NAME_none);

  ClassClass        = typeClass(NAME_class);
  ClassClass->class = ClassClass;
  ClassType         = typeClass(NAME_type);
  ClassObject	    = typeClass(NAME_object);
  ClassConstant	    = typeClass(NAME_constant);
  ClassBool	    = typeClass(NAME_bool);
  
  ((Constant)NIL)->class = ((Constant)DEFAULT)->class = ClassConstant;
  ON->class              = OFF->class                 = ClassBool;

  TypeClass    = createType(NAME_class,    NAME_class,  ClassClass);
  TypeType     = createType(NAME_type,     NAME_class,  ClassType);
  TypeObject   = createType(NAME_object,   NAME_object, ClassObject);
  TypeBool     = createType(NAME_bool,     NAME_class,  ClassBool);
  TypeConstant = createType(NAME_constant, NAME_class,  ClassConstant);

  bootType(NAME_charArray,  &ClassCharArray,     &TypeCharArray);
  bootType(NAME_name,       &ClassName,          &TypeName);
  bootType(NAME_var,        &ClassVar,           &TypeVar);
  bootType(NAME_variable,   &ClassObjOfVariable, NULL);
  bootType(NAME_vector,     &ClassVector,        &TypeVector);
  bootType(NAME_method,     &ClassMethod,        NULL);
  bootType(NAME_getMethod,  &ClassGetMethod,     NULL);
  bootType(NAME_sendMethod, &ClassSendMethod,    NULL);
  bootType(NAME_hashTable,  &ClassHashTable,     NULL);
  bootType(NAME_chain,      &ClassChain,         &TypeChain);
  bootType(NAME_function,   &ClassFunction,      &TypeFunction);
  bootType(NAME_graphical,  &ClassGraphical,     &TypeGraphical);
  bootType(NAME_real,       &ClassReal,          &TypeReal);

  for( ; i->global; i++ )
    *i->global = createType(i->name, i->kind, i->context);
}


Type
defineType(char *name, char *def)
{ Type t = CtoType(def);

  return getCopyType(t, CtoName(name));
}

		 /*******************************
		 *    DIRECT CALLING SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This series of functions is exported   from  the XPCE shared object/DLL.
They are added to the interface to facilitate direct calling. I.e. calls
from the host directly back into the   host because the method called is
defined in the host-language.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* t is included in super.  I.e. any object satisfying type t will also
   satisfy type super
*/

int
pceIncludesType(PceType t, PceType super)
{ return specialisedType(super, t);
}


int
pceIncludesHostDataType(PceType t, PceClass cl)
{ l1:

  switch(t->validate_function)
  { case TV_ALIAS:
      t = t->context;
      goto l1;
    case TV_UNCHECKED:
      succeed;
    case TV_CLASS:
      realiseClassType(t);
      if ( isAClass(cl, t->context) )
	succeed;
  }
  
  if ( notNil(t->supers) )
  { Cell cell;

    for_cell(cell, t->supers)
    { if ( pceIncludesHostDataType(cell->value, cl) )
	succeed;
    }
  }

  fail;
}


PceObject
pceCheckType(PceGoal g, PceType t, PceObject val)
{ PceObject rval;

  if ( validateType(t, val, g->receiver) )
    return val;
  
  if ( (rval = getTranslateType(t, val, g->receiver)) )
    return rval;

  if ( CheckTypeError == CTE_OBTAINER_FAILED )
    pceSetErrorGoal(g, PCE_ERR_FUNCTION_FAILED, val);

  fail;
}


int
pceCheckIntType(PceType t, long val)
{ return validateType(t, toInt(val), NIL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pceCheckNameType()
    This is the hard one, as there is no way to create a temporary name in
    XPCE, and we want to avoid creating a permantent one.  In many cases this
    is not required too, and we just want to know the type accepts an
    name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
pceCheckNameType(PceType t, const char *s)
{ int rval = FALSE;
  again:

  switch( t->validate_function )
  { case TV_CLASS:
    { if ( onFlag(t->context, F_ISNAME) )	/* isName() */
      { Class class;

	if ( (class=getConvertClass(ClassClass, t->context)) )
	  assign(t, context, class);
	else
	  fail;
      }
      
      rval = isAClass(ClassName, t->context);
      break;
    }
    case TV_OBJECT:
    case TV_ANY:
    case TV_ALIEN:
    case TV_UNCHECKED:
    case TV_ATOMIC:
      succeed;
    case TV_MEMBER:
    case TV_COMPOUND:
    case TV_INT:
    case TV_INTRANGE:
    case TV_ARG:
    case TV_REALRANGE:
    case TV_CHAR:
      break;
    case TV_VALUE:
      if ( isName(t->context) && streq(s, strName(t->context)) )
	succeed;
      break;
    case TV_VALUESET:
    case TV_NAMEOF:
    { Chain ch = getValueSetType(t, NIL);
      
      if ( ch )
      { Cell cell;
	
	for_cell(cell, ch)
	{ if ( isName(cell->value) && streq(s, strName(cell->value)) )
	    succeed;
	}
      }
      break;
    }
    case TV_ALIAS:
      if ( isNil(t->supers) )
      { t = t->context;
	goto again;
      } else
      { rval = pceCheckNameType(t->context, s);
	break;
      }
    case TV_EVENTID:
      if ( eventName(CtoName(s)) )
	succeed;
    break;
    default:
      return sysPce("%s: Invalid type.  Kind is %s, validate = %d",
		    pp(t), pp(t->kind), t->validate_function);
  }
  
  if ( rval )
    return rval;

  if ( notNil(t->supers) )
  { Cell cell;

    for_cell(cell, t->supers)
    { if ( pceCheckNameType(cell->value, s) )
      	succeed;
    }
  }

  fail;
}


int
pceCheckFloatType(PceType t, double f)
{ static Real tmp = NULL;

  if ( !tmp )
  { tmp = newObject(ClassReal, ZERO, 0);
    assert(tmp);
    lockObj(tmp);
  }

  setReal(tmp, f);

  return validateType(t, tmp, NIL);
}

