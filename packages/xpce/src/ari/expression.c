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
{ GM(NAME_Execute, 0, "int", NULL, getExecuteExpression,
     DEFAULT, "Evaluate, given variable bindings"),
  GM(NAME_value, 1, "value=int", "binding== ...", getValueExpressionv,
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

