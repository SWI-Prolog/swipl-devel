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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_initialise[] =
        { "left=expression", "right=expression" };

/* Instance Variables */

static const vardecl var_binaryCondition[] =
{ IV(NAME_left, "expression", IV_BOTH,
     NAME_operant, "Left-hand side of conditional expression"),
  IV(NAME_right, "expression", IV_BOTH,
     NAME_operant, "Right-hand side of conditional expression")
};

/* Send Methods */

static const senddecl send_binaryCondition[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBinaryCondition,
     DEFAULT, "Initialise from 2 expressions")
};

/* Get Methods */

static const getdecl get_binaryCondition[] =
{ 
};

/* Resources */

static const resourcedecl rc_binaryCondition[] =
{ 
};

/* Class Declaration */

static Name binaryCondition_termnames[] = { NAME_left, NAME_right };

ClassDecl(binaryCondition_decls,
          var_binaryCondition, send_binaryCondition,
	  get_binaryCondition, rc_binaryCondition,
          2, binaryCondition_termnames,
          "$Rev$");

status
makeClassBinaryCondition(Class class)
{ return declareClass(class, &binaryCondition_decls);
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_var[] =
        { "variable=var", "bindings== ..." };

/* Instance Variables */

static const vardecl var_equation[] =
{ 
};

/* Send Methods */

static const senddecl send_equation[] =
{ SM(NAME_Execute, 0, NULL, ExecuteEquation,
     DEFAULT, "Test if equation is true")
};

/* Get Methods */

static const getdecl get_equation[] =
{ GM(NAME_var, 2, "value=int", T_var, getVarEquationv,
     NAME_calculate, "Get value of a variable")
};

/* Resources */

static const resourcedecl rc_equation[] =
{ 
};

/* Class Declaration */

ClassDecl(equation_decls,
          var_equation, send_equation, get_equation, rc_equation,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassEquation(Class class)
{ return declareClass(class, &equation_decls);
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


		 /*******************************
		 *	     CLASS <		*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static const vardecl var_less[] =
{ 
};

/* Send Methods */

static const senddecl send_less[] =
{ SM(NAME_Execute, 0, NULL, ExecuteLess,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

static const getdecl get_less[] =
{ 
};

/* Resources */

static const resourcedecl rc_less[] =
{ 
};

/* Class Declaration */

ClassDecl(less_decls,
          var_less, send_less, get_less, rc_less,
          ARGC_INHERIT, NULL,
          "$Rev$");

status
makeClassLess(Class class)
{ return declareClass(class, &less_decls);
}


		 /*******************************
		 *	     CLASS =<		*
		 *******************************/

/* Instance Variables */

static const vardecl var_lessEqual[] =
{ 
};

/* Send Methods */

static const senddecl send_lessEqual[] =
{ SM(NAME_Execute, 0, NULL, ExecuteLessEqual,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

static const getdecl get_lessEqual[] =
{ 
};

/* Resources */

static const resourcedecl rc_lessEqual[] =
{ 
};

/* Class Declaration */

ClassDecl(lessEqual_decls,
          var_lessEqual, send_lessEqual, get_lessEqual, rc_lessEqual,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassLessEqual(Class class)
{ return declareClass(class, &lessEqual_decls);
}


		 /*******************************
		 *	      CLASS >		*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static const vardecl var_greater[] =
{ 
};

/* Send Methods */

static const senddecl send_greater[] =
{ SM(NAME_Execute, 0, NULL, ExecuteGreater,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

static const getdecl get_greater[] =
{ 
};

/* Resources */

static const resourcedecl rc_greater[] =
{ 
};

/* Class Declaration */

ClassDecl(greater_decls,
          var_greater, send_greater, get_greater, rc_greater,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassGreater(Class class)
{ return declareClass(class, &greater_decls);
}


		 /*******************************
		 *	     CLASS >=		*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static const vardecl var_greaterEqual[] =
{ 
};

/* Send Methods */

static const senddecl send_greaterEqual[] =
{ SM(NAME_Execute, 0, NULL, ExecuteGreaterEqual,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

static const getdecl get_greaterEqual[] =
{ 
};

/* Resources */

static const resourcedecl rc_greaterEqual[] =
{ 
};

/* Class Declaration */

ClassDecl(greaterEqual_decls,
          var_greaterEqual, send_greaterEqual,
	  get_greaterEqual, rc_greaterEqual,
          ARGC_INHERIT, NULL,
          "$Rev$");

status
makeClassGreaterEqual(Class class)
{ return declareClass(class, &greaterEqual_decls);
}

