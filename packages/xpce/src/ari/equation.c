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

static char *T_initialise[] =
        { "left=expression", "right=expression" };

/* Instance Variables */

static vardecl var_binaryCondition[] =
{ IV(NAME_left, "expression", IV_BOTH,
     NAME_operant, "Left-hand side of conditional expression"),
  IV(NAME_right, "expression", IV_BOTH,
     NAME_operant, "Right-hand side of conditional expression")
};

/* Send Methods */

static senddecl send_binaryCondition[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBinaryCondition,
     DEFAULT, "Initialise from 2 expressions")
};

/* Get Methods */

#define get_binaryCondition NULL
/*
static getdecl get_binaryCondition[] =
{ 
};
*/

/* Resources */

#define rc_binaryCondition NULL
/*
static classvardecl rc_binaryCondition[] =
{ 
};
*/

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

status
evaluateEquation(Equation e, Var var, NumericValue result)
{ int inleft, inright;
  Expression left, right;
  numeric_value v;

  inleft = valInt(getVarInBinaryExpression((BinaryExpression) LEFTHAND(e),
					   var));
  inright = valInt(getVarInBinaryExpression((BinaryExpression) RIGHTHAND(e),
					    var));
  if (inleft + inright == 0)
  { errorPce(e, NAME_noVar, var);
    result->type = V_ERROR;
    fail;
  }
  if (inleft + inright > 1)
  { errorPce(e, NAME_multipleVar, var);
    result->type = V_ERROR;
    fail;
  }
  if (inleft == 0)
  { left = RIGHTHAND(e);
    right = LEFTHAND(e);
  } else
  { left = LEFTHAND(e);
    right = RIGHTHAND(e);
  }

  if ( !evaluateExpression(right, &v) )
  { result->type = V_ERROR;
    fail;
  }

  while((Var) left != var)
  { Class left_class;
    numeric_value vt, v2;

    inleft = valInt(getVarInBinaryExpression((BinaryExpression) LEFTHAND(left),
					     var));
    if ( isObject(left) )
      left_class = classOfObject(left);
    else
    { errorPce(left, NAME_unexpectedType, TypeEquation);
      result->type = V_ERROR;
      fail;
    }

    if	( inleft )
    { if ( !evaluateExpression(RIGHTHAND(left), &v2) )
      { result->type = V_ERROR;
	fail;
      }

      if ( left_class == ClassPlus )
	ar_minus(&v, &v2, &vt);
      else if ( left_class == ClassMinus )
	ar_add(&v, &v2, &vt);
      else if ( left_class == ClassTimes )
	ar_divide(&v, &v2, &vt);
      else if ( left_class == ClassDivide )
	ar_times(&v, &v2, &vt);
      else
      { errorPce(left, NAME_unexpectedType, TypeEquation);
	result->type = V_ERROR;
	fail;
      }
    } else
    { if ( !evaluateExpression(LEFTHAND(left), &v2) )
      { result->type = V_ERROR;
	fail;
      }

      if ( left_class == ClassPlus )
	ar_minus(&v, &v2, &vt);
      else if ( left_class == ClassMinus )
	ar_add(&v, &v2, &vt);
      else if ( left_class == ClassTimes )
	ar_divide(&v, &v2, &vt);
      else if ( left_class == ClassDivide )
	ar_times(&v, &v2, &vt);
      else
      { errorPce(left, NAME_unexpectedType, TypeEquation);
	result->type = V_ERROR;
	fail;
      }
    }
    v = vt;

    left = (inleft ? LEFTHAND(left) : RIGHTHAND(left));
  }

  *result = v;
  succeed;
}


static Any
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
		  { numeric_value v;

		    evaluateEquation(e, var, &v);
		    rval = ar_result(&v);
		  }
		});

  answer(rval);
}


static status
ExecuteEquation(Equation e)
{ numeric_value vl, vr;
  
  if ( evaluateExpression(LEFTHAND(e), &vl) &&
       evaluateExpression(LEFTHAND(e), &vr) )
  { if ( vl.type == V_INTEGER && vr.type == V_INTEGER )
    { if ( vl.value.i == vr.value.i )
	succeed;
    } else
    { promoteToRealNumericValue(&vl);
      promoteToRealNumericValue(&vr);
      if ( vl.value.i == vr.value.i )
	succeed;
    }
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_var[] =
        { "variable=var", "bindings== ..." };

/* Instance Variables */

#define var_equation NULL
/*
vardecl var_equation[] =
{ 
};
*/

/* Send Methods */

static senddecl send_equation[] =
{ SM(NAME_Execute, 0, NULL, ExecuteEquation,
     DEFAULT, "Test if equation is true")
};

/* Get Methods */

static getdecl get_equation[] =
{ GM(NAME_var, 2, "value=int|number|real", T_var, getVarEquationv,
     NAME_calculate, "Get value of a variable")
};

/* Resources */

#define rc_equation NULL
/*
static classvardecl rc_equation[] =
{ 
};
*/

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
{ numeric_value vl, vr;
  
  if ( evaluateExpression(LEFTHAND(e), &vl) &&
       evaluateExpression(RIGHTHAND(e), &vr) )
  { if ( vl.type == V_INTEGER && vr.type == V_INTEGER )
    { if ( vl.value.i < vr.value.i )
	succeed;
    } else
    { promoteToRealNumericValue(&vl);
      promoteToRealNumericValue(&vr);
      if ( vl.value.f < vr.value.f )
	succeed;
    }
  }

  fail;
}

static status
ExecuteLessEqual(Equation e)
{ numeric_value vl, vr;
  
  if ( evaluateExpression(LEFTHAND(e), &vl) &&
       evaluateExpression(RIGHTHAND(e), &vr) )
  { if ( vl.type == V_INTEGER && vr.type == V_INTEGER )
    { if ( vl.value.i <= vr.value.i )
	succeed;
    } else
    { promoteToRealNumericValue(&vl);
      promoteToRealNumericValue(&vr);
      if ( vl.value.f <= vr.value.f )
	succeed;
    }
  }

  fail;
}

static status
ExecuteGreater(Equation e)
{ numeric_value vl, vr;
  
  if ( evaluateExpression(LEFTHAND(e), &vl) &&
       evaluateExpression(RIGHTHAND(e), &vr) )
  { if ( vl.type == V_INTEGER && vr.type == V_INTEGER )
    { if ( vl.value.i > vr.value.i )
	succeed;
    } else
    { promoteToRealNumericValue(&vl);
      promoteToRealNumericValue(&vr);
      if ( vl.value.f > vr.value.f )
	succeed;
    }
  }

  fail;
}

static status
ExecuteGreaterEqual(Equation e)
{ numeric_value vl, vr;
  
  if ( evaluateExpression(LEFTHAND(e), &vl) &&
       evaluateExpression(RIGHTHAND(e), &vr) )
  { if ( vl.type == V_INTEGER && vr.type == V_INTEGER )
    { if ( vl.value.i >= vr.value.i )
	succeed;
    } else
    { promoteToRealNumericValue(&vl);
      promoteToRealNumericValue(&vr);
      if ( vl.value.f >= vr.value.f )
	succeed;
    }
  }

  fail;
}


		 /*******************************
		 *	     CLASS <		*
		 *******************************/

/* Type declarations */


/* Instance Variables */

#define var_less NULL
/*
vardecl var_less[] =
{ 
};
*/

/* Send Methods */

static senddecl send_less[] =
{ SM(NAME_Execute, 0, NULL, ExecuteLess,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

#define get_less NULL
/*
static getdecl get_less[] =
{ 
};
*/

/* Resources */

#define rc_less NULL
/*
static classvardecl rc_less[] =
{ 
};
*/

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

#define var_lessEqual NULL
/*
vardecl var_lessEqual[] =
{ 
};
*/

/* Send Methods */

static senddecl send_lessEqual[] =
{ SM(NAME_Execute, 0, NULL, ExecuteLessEqual,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

#define get_lessEqual NULL
/*
static getdecl get_lessEqual[] =
{ 
};
*/

/* Resources */

#define rc_lessEqual NULL
/*
static classvardecl rc_lessEqual[] =
{ 
};
*/

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

#define var_greater NULL
/*
vardecl var_greater[] =
{ 
};
*/

/* Send Methods */

static senddecl send_greater[] =
{ SM(NAME_Execute, 0, NULL, ExecuteGreater,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

#define get_greater NULL
/*
static getdecl get_greater[] =
{ 
};
*/

/* Resources */

#define rc_greater NULL
/*
static classvardecl rc_greater[] =
{ 
};
*/

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

#define var_greaterEqual NULL
/*
vardecl var_greaterEqual[] =
{ 
};
*/

/* Send Methods */

static senddecl send_greaterEqual[] =
{ SM(NAME_Execute, 0, NULL, ExecuteGreaterEqual,
     DEFAULT, "Evaluate arguments and compare")
};

/* Get Methods */

#define get_greaterEqual NULL
/*
static getdecl get_greaterEqual[] =
{ 
};
*/

/* Resources */

#define rc_greaterEqual NULL
/*
static classvardecl rc_greaterEqual[] =
{ 
};
*/

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

