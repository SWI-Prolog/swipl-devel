/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/arith.h>

static status
initialiseBinaryCondition(BinaryCondition c, Any l, Any r)
{ initialiseCode((Code) c);
  assign(c, left, l);
  assign(c, right, r);
  
  succeed;
}


status
makeClassBinaryCondition(Class class)
{ localClass(class, NAME_left, NAME_operant,  "expression", NAME_both,
	     "Left-hand side of conditional expression");
  localClass(class, NAME_right, NAME_operant, "expression", NAME_both,
	     "Right-hand side of conditional expression");

  termClass(class, "binary_condition", 2, NAME_left, NAME_right);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "left=expression", "right=expression",
	     "Initialise from 2 expressions",
	     initialiseBinaryCondition);

  succeed;
}


/*	Could be made more efficient by computing the sub-tree in which
	the target variable occurs during creation time.  Or by returning
	a left/right bit-pattern while searching for the target variable
	with getVarInBinaryExpression().
 */

Int
getVarEquation(Equation e, Var var)
{ register int inleft, inright;
  Expression left, right;
  PseudoFloat rval, v; 

  inleft = valInt(getVarInBinaryExpression((BinaryExpression) LEFTHAND(e),
					   var));
  inright = valInt(getVarInBinaryExpression((BinaryExpression) RIGHTHAND(e),
					    var));
  if (inleft + inright == 0)
  { errorPce(e, NAME_noVar, var);
    fail;
  }
  if (inleft + inright > 1)
  { errorPce(e, NAME_multipleVar, var);
    fail;
  }
  if (inleft == 0)
  { left = RIGHTHAND(e);
    right = LEFTHAND(e);
  } else
  { left = LEFTHAND(e);
    right = RIGHTHAND(e);
  }

  arithError = FALSE;
  rval	= getPseudoFloatExpression(right);

  while((Var) left != var)
  { Class left_class;

    inleft = valInt(getVarInBinaryExpression((BinaryExpression) LEFTHAND(left),
					     var));
    if ( isObject(left) )
      left_class = classOfObject(left);
    else
      left_class = NIL;

    if	(inleft == 1)
    { v = getPseudoFloatExpression(RIGHTHAND(left));

      if ( left_class == ClassPlus )
	rval = PSF_sub(rval, v);
      else if ( left_class == ClassMinus )
	rval = PSF_add(rval, v);
      else if ( left_class == ClassTimes )
      	rval = PSF_div(rval, v);
      else if ( left_class == ClassDivide )
	rval = PSF_mul(rval, v);
      else
      { errorPce(left, NAME_unexpectedType, TypeEquation);
	fail;
      }
    } else
    { v = getPseudoFloatExpression(LEFTHAND(left));

      if ( left_class == ClassPlus )
	rval = PSF_sub(rval, v);
      else if ( left_class == ClassMinus )
	rval = PSF_add(rval, v);
      else if ( left_class == ClassTimes )
	rval = PSF_div(rval, v);
      else if ( left_class == ClassDivide )
	rval = PSF_div(v, rval);
      else
      { errorPce(left, NAME_unexpectedType, TypeEquation);
	fail;
      }
    }

    left = (inleft == 1 ? LEFTHAND(left) : RIGHTHAND(left));
  }

  if (arithError == TRUE)
    fail;

  answer(toInt(PSF_Int(rval)));
}


static Int
getVarEquationv(Equation e, Var var, int argc, Equation *argv)
{ Int rval;
  int n;

  withLocalVars({ int error = 0;

		  for(n=0; n<argc; n++)
		  { Var v;
		    Any value;

		    if ( (v = checkType(argv[n]->left, TypeVar, NIL)) )
		    { value = argv[n]->right;
		      assignVar(v, value, NAME_local);
		    } else
		    { error++;
		      break;
		    }
		  }

		  if ( error )
		    rval = FAIL;
		  else
		    rval = getVarEquation(e, var);
		});

  answer(rval);
}


static status
ExecuteEquation(Equation e)
{ PseudoFloat lv, rv;

  arithError = FALSE;

  lv =	getPseudoFloatExpression(LEFTHAND(e));
  rv =	getPseudoFloatExpression(RIGHTHAND(e));

  if ( arithError == TRUE )
    fail;

  if ( lv == rv )
    succeed;

  fail;
}


status
makeClassEquation(Class class)
{ sourceClass(class, makeClassEquation, __FILE__, "$Revision$");

  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Test if equation is true",
	     ExecuteEquation);

  getMethod(class, NAME_var, NAME_calculate, "value=int", 2,
	    "variable=var", "bindings== ...",
	    "Get value of a variable",
	    getVarEquationv);

  succeed;
}
/* - -	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				COMPARISON
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
ExecuteLess(Equation e)
{ PseudoFloat lv, rv;

  arithError = FALSE;

  lv = getPseudoFloatExpression(LEFTHAND(e));
  rv = getPseudoFloatExpression(RIGHTHAND(e));

  if ( arithError == TRUE )
    fail;

  if ( lv < rv )
    succeed;

  fail;
}

static status
ExecuteLessEqual(Equation e)
{ PseudoFloat lv, rv;

  arithError = FALSE;

  lv = getPseudoFloatExpression(LEFTHAND(e));
  rv = getPseudoFloatExpression(RIGHTHAND(e));

  if ( arithError == TRUE )
    fail;

  if ( lv <= rv )
    succeed;

  fail;
}

static status
ExecuteGreater(Equation e)
{ PseudoFloat lv, rv;

  arithError = FALSE;

  lv = getPseudoFloatExpression(LEFTHAND(e));
  rv = getPseudoFloatExpression(RIGHTHAND(e));

  if ( arithError == TRUE )
    fail;

  if ( lv > rv )
    succeed;

  fail;
}

static status
ExecuteGreaterEqual(Equation e)
{ PseudoFloat lv, rv;

  arithError = FALSE;

  lv = getPseudoFloatExpression(LEFTHAND(e));
  rv = getPseudoFloatExpression(RIGHTHAND(e));

  if ( arithError == TRUE )
    fail;

  if ( lv >= rv )
    succeed;

  fail;
}


status
makeClassLess(Class class)
{ sourceClass(class, makeClassLess, __FILE__, "$Revision$");

  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate arguments and compare",
	     ExecuteLess);

  succeed;
}

status
makeClassLessEqual(Class class)
{ sourceClass(class, makeClassLessEqual, __FILE__, "$Revision$");

  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate arguments and compare",
	     ExecuteLessEqual);

  succeed;
}


status
makeClassGreater(Class class)
{ sourceClass(class, makeClassGreater, __FILE__, "$Revision$");

  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate arguments and compare",
	     ExecuteGreater);

  succeed;
}


status
makeClassGreaterEqual(Class class)
{ sourceClass(class, makeClassGreaterEqual, __FILE__, "$Revision$");

  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate arguments and compare",
	     ExecuteGreaterEqual);

  succeed;
}

