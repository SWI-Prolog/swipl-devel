/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/arith.h>

static int	get_var_in_binary_expression(Any e, Var var, int n);

#ifdef O_NOFLOAT			/* old fixed-point arithmetic */

PseudoFloat
getPseudoFloatExpression(Any e)
{ Int ival;

  if ( isFunction(e) )
  { Any e2;

    if ( instanceOfObject(e, ClassBinaryExpression) )
    { Class class = classOfObject(e);
      PseudoFloat fl, fr;

      fl = getPseudoFloatExpression(LEFTHAND(e));
      fr = getPseudoFloatExpression(RIGHTHAND(e));
      if ( arithError )
	return Int_PSF(1);

      if ( class == ClassPlus )		/* + */
	return PSF_add(fl, fr);
      if ( class == ClassMinus )	/* - */
	return PSF_sub(fl, fr);
      if ( class == ClassTimes )	/* * */
	return PSF_mul(fl, fr);
      if ( class == ClassDivide )	/* / */
	return PSF_div(fl, fr);
    }

    if ( !(e2 = expandFunction(e)) )
    { errorPce(e, NAME_evalFailed);
      arithError = TRUE;
      return Int_PSF(0);
    } else
      e = e2;
  }

  if ( isInteger(e) )			/* int */
    return Int_PSF(valInt(e));

  if ( instanceOfObject(e, ClassNumber) ) /* number */
    return Int_PSF(((Number)e)->value);
  if ( instanceOfObject(e, ClassReal) )	/* real */
    return Float_PSF(valReal(e));
  
  if ( (ival = (Int) checkType(e, TypeInt, NIL)) )
    return Int_PSF(valInt(ival));

  errorPce(e, NAME_unexpectedType, TypeExpression);
  arithError = TRUE;
  return Int_PSF(1);
}

#else /* O_NOFLOAT*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The arithmetic code below is from SWI-Prolog  2.6.0. As the copyright to
this software is in the same hands, this should be ok.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define intNumericValue(n) ((n)->type == V_INTEGER)

void
promoteToRealNumericValue(NumericValue n)
{ if ( intNumericValue(n) )
  { n->value.f = (double)n->value.i;
    n->type = V_DOUBLE;
  }
}


status
ar_add(NumericValue n1, NumericValue n2, NumericValue r)
{ if ( intNumericValue(n1) && intNumericValue(n2) ) 
  { r->value.i = n1->value.i + n2->value.i; 
    
    if ( n1->value.i > 0 && n2->value.i > 0 && r->value.i <= 0 )
      goto overflow;
    if ( n1->value.i < 0 && n2->value.i < 0 && r->value.i >= 0 )
      goto overflow;

    r->type = V_INTEGER;
    succeed;
  } 

overflow:
  promoteToRealNumericValue(n1);
  promoteToRealNumericValue(n2);
  r->value.f = n1->value.f + n2->value.f; 
  r->type = V_DOUBLE;

  succeed;
}


int
ar_minus(NumericValue n1, NumericValue n2, NumericValue r)
{ if ( intNumericValue(n1) && intNumericValue(n2) ) 
  { r->value.i = n1->value.i - n2->value.i; 
    
    if ( n1->value.i > 0 && n2->value.i < 0 && r->value.i <= 0 )
      goto overflow;
    if ( n1->value.i < 0 && n2->value.i > 0 && r->value.i >= 0 )
      goto overflow;

    r->type = V_INTEGER;
    succeed;
  } 

overflow:
  promoteToRealNumericValue(n1);
  promoteToRealNumericValue(n2);
  r->value.f = n1->value.f - n2->value.f; 
  r->type = V_DOUBLE;

  succeed;
}

int
ar_divide(NumericValue n1, NumericValue n2, NumericValue r)
{ if ( intNumericValue(n1) && intNumericValue(n2) )
  { if ( n1->value.i % n2->value.i == 0)
    { r->value.i = n1->value.i / n2->value.i;
      r->type = V_INTEGER;
      succeed;
    }
  }

  promoteToRealNumericValue(n1);
  promoteToRealNumericValue(n2);

  r->value.f = n1->value.f / n2->value.f;
  r->type = V_DOUBLE;
  succeed;
}


int
ar_times(NumericValue n1, NumericValue n2, NumericValue r)
{ if ( intNumericValue(n1) && intNumericValue(n2) )
  { if ( abs(n1->value.i) >= (1 << 15) || abs(n2->value.i) >= (1 << 15) )
    { r->value.f = (double)n1->value.i * (double)n2->value.i;
      r->type = V_DOUBLE;
      succeed;
    }
    r->value.i = n1->value.i * n2->value.i;
    r->type = V_INTEGER;
    succeed;
  }
  
  promoteToRealNumericValue(n1);
  promoteToRealNumericValue(n2);

  r->value.f = n1->value.f * n2->value.f;
  r->type = V_DOUBLE;
  succeed;
}


status
evaluateExpression(Any e, NumericValue v)
{ Real fval;

  if ( isFunction(e) )
  { Any e2;

    if ( instanceOfObject(e, ClassBinaryExpression) )
    { Class class = classOfObject(e);
      numeric_value vl, vr;

      if ( !evaluateExpression(LEFTHAND(e), &vl) ||
	   !evaluateExpression(RIGHTHAND(e), &vr) )
	fail;

      if ( class == ClassPlus )		/* + */
	return ar_add(&vl, &vr, v);
      if ( class == ClassMinus )	/* - */
	return ar_minus(&vl, &vr, v);
      if ( class == ClassTimes )	/* * */
	return ar_times(&vl, &vr, v);
      if ( class == ClassDivide )	/* / */
	return ar_divide(&vl, &vr, v);
      
      errorPce(e, NAME_unknownFunction);
      v->type = V_ERROR;
      fail;
    }

    if ( !(e2 = expandFunction(e)) )
    { errorPce(e, NAME_evalFailed);
      v->type = V_ERROR;
      fail;
    } else
      e = e2;
  }

  if ( isInteger(e) )			/* int */
  { v->value.i = valInt(e);
    v->type    = V_INTEGER;
    succeed;
  }
  if ( instanceOfObject(e, ClassNumber) ) /* number */
  { Number n = e;

    v->value.i = n->value;
    v->type    = V_INTEGER;
    succeed;
  }
  if ( instanceOfObject(e, ClassReal) )	/* real */
  { Real r = e;

    v->value.f = valReal(r);
    v->type    = V_DOUBLE;
    succeed;
  }
  if ( (fval = checkType(e, TypeReal, NIL)) )
  { v->value.f = valReal(fval);
    v->type    = V_DOUBLE;
    succeed;
  }

  errorPce(e, NAME_unexpectedType, TypeExpression);

  v->type = V_ERROR;
  fail;
}

Any
ar_result(NumericValue n)
{ switch(n->type)
  { case V_INTEGER:
    case_int:
      if ( n->value.i > PCE_MIN_INT && n->value.i < PCE_MAX_INT )
	return toInt(n->value.i);
      else
	return CtoNumber(n->value.i);
    case V_DOUBLE:
    { long l = (long)n->value.f;

      if ( (double)l == n->value.f )
      { n->value.i = l;
        goto case_int;
      }

      return CtoReal(n->value.f);
    }
    default:
      fail;
  }
}


Int
ar_int_result(Any e, NumericValue n)
{ switch(n->type)
  { case V_INTEGER:
      if ( n->value.i > PCE_MIN_INT && n->value.i < PCE_MAX_INT )
	return toInt(n->value.i);
      else
      { errorPce(e, NAME_outOfIntRange);
	fail;
      }
    case V_DOUBLE:
    { if ( n->value.f > (double)PCE_MIN_INT &&
	   n->value.f < (double)PCE_MAX_INT )
	return toInt(rfloat(n->value.f));
      else
      { errorPce(e, NAME_outOfIntRange);
	fail;
      }
    }
    default:
      fail;
  }
}


#endif /*O_NOFLOAT*/

		/********************************
		*       BINARY EXPRESSIONS	*
		********************************/

static status
initialiseBinaryExpression(BinaryExpression e,
			   Expression left, Expression right)
{ assign(e, left,  left);
  assign(e, right, right);

  return initialiseFunction((Function) e);
}


static Any
getExecuteExpression(BinaryExpression e)
{
#ifdef O_NOFLOAT
  PseudoFloat f;

  arithError = FALSE;
  f = getPseudoFloatExpression(e);
  if ( arithError )
    fail;

  answer(toInt(PSF_Int(f)));
#else
  numeric_value v;

  if ( evaluateExpression(e, &v) )
    return ar_result(&v);

  fail;
#endif
}


static Any
getValueExpressionv(Any e, int argc, Equation *argv)
{ Int rval;
  int n;

  withLocalVars({ for(n=0; n<argc; n++)
		  { Var v;
		    Any value;

		    TRY(v = checkType(argv[n]->left, TypeVar, NIL));
		    value = argv[n]->right;
		    assignVar(v, value, NAME_local);
		  }

		  rval = getExecuteExpression(e);
		});

  answer(rval);
}


Int
getVarInBinaryExpression(BinaryExpression e, Var var)
{ answer(toInt(get_var_in_binary_expression(e, var, 0)));
}


static int
get_var_in_binary_expression(Any e, Var var, int n)
{ if ( (Var)e == var )
    return n+1;

  if ( instanceOfObject(e, ClassBinaryExpression) )
    return get_var_in_binary_expression(LEFTHAND(e), var, n) +
           get_var_in_binary_expression(RIGHTHAND(e), var, 0);

  return n;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "left=expression", "right=expression" };

/* Instance Variables */

static vardecl var_binaryExpression[] =
{ IV(NAME_left, "expression", IV_BOTH,
     NAME_operant, "Left-hand side of expression"),
  IV(NAME_right, "expression", IV_BOTH,
     NAME_operant, "Right-hand side of expression")
};

/* Send Methods */

static senddecl send_binaryExpression[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBinaryExpression,
     DEFAULT, "Create binary expression")
};

/* Get Methods */

static getdecl get_binaryExpression[] =
{ GM(NAME_Execute, 0, "value=int|number|real", NULL, getExecuteExpression,
     DEFAULT, "Evaluate, given variable bindings"),
  GM(NAME_value, 1, "value=int|number|real", "binding== ...", getValueExpressionv,
     NAME_calculate, "Evaluate, given variable bindings"),
  GM(NAME_varIn, 1, "number=int", "variable=var", getVarInBinaryExpression,
     NAME_meta, "Count occurrences of (named) variable")
};

/* Resources */

#define rc_binaryExpression NULL
/*
static resourcedecl rc_binaryExpression[] =
{ 
};
*/

/* Class Declaration */

static Name binaryExpression_termnames[] = { NAME_left, NAME_right };

ClassDecl(binaryExpression_decls,
          var_binaryExpression, send_binaryExpression,
	  get_binaryExpression, rc_binaryExpression,
          2, binaryExpression_termnames,
          "$Rev$");

status
makeClassBinaryExpression(Class class)
{ return declareClass(class, &binaryExpression_decls);
}


status
makeClassDivide(Class class)
{ sourceClass(class, makeClassDivide, __FILE__, "$Revision$");
  succeed;
}


status
makeClassTimes(Class class)
{ sourceClass(class, makeClassTimes, __FILE__, "$Revision$");
  succeed;
}


status
makeClassPlus(Class class)
{ sourceClass(class, makeClassPlus, __FILE__, "$Revision$");
  succeed;
}


static status				/* deal with unary - */
initialiseMinus(Minus m, Expression left, Expression right)
{ if ( isDefault(right) )
  { right = left;
    left = ZERO;
  }

  return initialiseBinaryExpression((BinaryExpression)m, left, right);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Types */

static char *T_minitialise[] =
        { "left=expression", "right=[expression]" };

/* Instance Variables */

#define var_minus NULL
/*
vardecl var_minus[] =
{ 
};
*/

/* Send Methods */

static senddecl send_minus[] =
{ SM(NAME_initialise, 2, T_minitialise, initialiseMinus,
     DEFAULT, "Create unary or binary `-'")
};

/* Get Methods */

#define get_minus NULL
/*
static getdecl get_minus[] =
{ 
};
*/

/* Resources */

#define rc_minus NULL
/*
static resourcedecl rc_minus[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(minus_decls,
          var_minus, send_minus, get_minus, rc_minus,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassMinus(Class class)
{ return declareClass(class, &minus_decls);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The function below is used by   regions, handles, graphical constraints,
etc. it is supposed to return an integer value.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Int
getValueExpression(Expression e, ...)
{ if ( isInteger(e) )			/* happens often! */
    answer(e);
  else
  { va_list args;
    int argc, i;
    Var vars[FWD_PCE_MAX_ARGS];
    Any vals[FWD_PCE_MAX_ARGS];
    Any savd[FWD_PCE_MAX_ARGS];
    numeric_value v;

    va_start(args, e);
    for(argc = 0; (vars[argc] = va_arg(args, Var)) != NULL; argc++)
    { assert(argc <= FWD_PCE_MAX_ARGS);
      assert(instanceOfObject(vars[argc], ClassVar));

      vals[argc] = va_arg(args, Expression);
      assert(vals[argc] != NULL);
    }
    va_end(args);

    for(i=0; i<argc; i++)
    { savd[i] = vars[i]->value;
      setVar(vars[i], vals[i]);
    }

    evaluateExpression(e, &v);

    for(i=0; i<argc; i++)
      setVar(vars[i], savd[i]);

    return ar_int_result(e, &v);
  }
}
