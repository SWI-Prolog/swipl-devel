/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/arith.h>

static int	get_var_in_binary_expression(Any e, Var var, int n);

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
    return Int_PSF(valInt(((Number)e)->value));
  if ( instanceOfObject(e, ClassReal) )	/* real */
    return Float_PSF(((Real)e)->value);
  
  if ( (ival = (Int) checkType(e, TypeInt, NIL)) )
    return Int_PSF(valInt(ival));

  errorPce(e, NAME_unexpectedType, TypeExpression);
  arithError = TRUE;
  return Int_PSF(1);
}


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


static Int
getExecuteExpression(BinaryExpression e)
{ PseudoFloat f;

  arithError = FALSE;
  f = getPseudoFloatExpression(e);
  if ( arithError )
    fail;

  answer(toInt(PSF_Int(f)));
}


static Int
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


status
makeClassBinaryExpression(Class class)
{ sourceClass(class, makeClassBinaryExpression, __FILE__, "$Revision$");

  localClass(class, NAME_left, NAME_operant, "expression", NAME_both,
	     "Left-hand side of expression");
  localClass(class, NAME_right, NAME_operant, "expression", NAME_both,
	     "Right-hand side of expression");

  termClass(class, "binary_expression", 2, NAME_left, NAME_right);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "left=expression", "right=expression",
	     "Create binary expression",
	     initialiseBinaryExpression);

  getMethod(class, NAME_value, NAME_calculate, "value=int", 1, "binding== ...",
	    "Evaluate, given variable bindings",
	    getValueExpressionv);
  getMethod(class, NAME_Execute, DEFAULT, "int", 0,
	    "Evaluate, given variable bindings",
	    getExecuteExpression);
  getMethod(class, NAME_varIn, NAME_meta, "number=int", 1, "variable=var",
	    "Count occurrences of (named) variable",
	    getVarInBinaryExpression);

  succeed;
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


status
makeClassMinus(Class class)
{ sourceClass(class, makeClassMinus, __FILE__, "$Revision$");

  sendMethod(ClassMinus, NAME_initialise, DEFAULT,
	     2, "left=expression", "right=[expression]",
	     "Create unary or binary `-'",
	     initialiseMinus);

  succeed;
}


/* (JW) Internal function which returns the value of an expression given a 
	list of variable values. The argumentlist consists of tuples which 
	form the left and right hand side of the argument equations.  It
	should be closed with a '0'.
 */

static inline Int
_getValueExpression(Expression e, va_list args)
{ if ( isInteger(e) )			/* happens often! */
    answer(e);
  else
  { int argc, i;
    Var vars[FWD_PCE_MAX_ARGS];
    Any vals[FWD_PCE_MAX_ARGS];
    Any savd[FWD_PCE_MAX_ARGS];
    Int rval;

    for(argc = 0; (vars[argc] = va_arg(args, Var)) != NULL; argc++)
    { assert(argc <= FWD_PCE_MAX_ARGS);
      assert(instanceOfObject(vars[argc], ClassVar));

      vals[argc] = va_arg(args, Expression);
      assert(vals[argc] != NULL);
    }

    for(i=0; i<argc; i++)
    { savd[i] = vars[i]->value;
      setVar(vars[i], vals[i]);
    }

    rval = getExecuteExpression((BinaryExpression) e);

    for(i=0; i<argc; i++)
      setVar(vars[i], savd[i]);

    return rval;
  }
}


Int
getValueExpression(Expression e, ...)
{ va_list args;
  Int rval;

  va_start(args, e);
  rval = _getValueExpression(e, args);
  va_end(args);

  return rval;
}

