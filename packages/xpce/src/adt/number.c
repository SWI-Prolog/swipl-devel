/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseNumber(Number n, Int i)
{ assign(n, value, i);

  succeed;
}


static Number
getConvertNumber(Class class, Any obj)
{ Int i;

  TRY( i = toInteger(obj) );
  answer( answerObject(ClassNumber, i, 0) );
}


static StringObj
getPrintNameNumber(Number n)
{ char buf[100];

  sprintf(buf, "%ld", valInt(n->value));

  answer(CtoString(buf));
}


static status
equalNumber(Number n, Int i)
{ if (n->value == i)
    succeed;
  fail;
}


static status
notEqualNumber(Number n, Int i)
{ if (n->value != i)
    succeed;
  fail;
}


static status
smallerNumber(Number n, Int i)
{ if (valInt(n->value) < valInt(i))
    succeed;
  fail;
}


static status
largerNumber(Number n, Int i)
{ if (valInt(n->value) > valInt(i))
    succeed;
  fail;
}


static status
lessEqualNumber(Number n, Int i)
{ if (valInt(n->value) <= valInt(i))
    succeed;
  fail;
}


static status
largerEqualNumber(Number n, Int i)
{ if (valInt(n->value) >= valInt(i))
    succeed;
  fail;
}


static status
plusNumber(Number n, Int i)
{ assign(n, value, add(n->value,i));
  succeed;
}


static status
minusNumber(Number n, Int i)
{ assign(n, value, sub(n->value,i));
  succeed;
}


static status
timesNumber(Number n, Int i)
{ assign(n, value, mul(n->value,i));
  succeed;
}


static status
divideNumber(Number n, Int i)
{ assign(n, value, div(n->value,i));
  succeed;
}


static status
maximumNumber(Number n, Int i)
{ if ( valInt(n->value) < valInt(i) )
    assign(n, value, i);

  succeed;
}


static status
minimumNumber(Number n, Int i)
{ if ( valInt(n->value) > valInt(i) )
    assign(n, value, i);

  succeed;
}


static Name
getCompareNumber(Number n, Int i)
{ if ( valInt(n->value) > valInt(i) )
    answer(NAME_larger);
  if ( valInt(n->value) < valInt(i) )
    answer(NAME_smaller);
  answer(NAME_equal);
}


static Number
getCatchAllNumber(Number n, Name selector, int argc, Any *argv)
{ Number result = answerObject(classOfObject(n), n->value, 0);

  TRY(sendv(result, selector, argc, argv));

  answer(result);
}


static Int
getValueNumber(Number n)
{ answer(n->value);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_catchAll[] =
        { "selector=name", "argument=unchecked ..." };

/* Instance Variables */

static vardecl var_number[] =
{ IV(NAME_value, "int", IV_SEND,
     NAME_storage, "Value of number object")
};

/* Send Methods */

static senddecl send_number[] =
{ SM(NAME_initialise, 1, "value=int", initialiseNumber,
     DEFAULT, "Create from integer"),
  SM(NAME_divide, 1, "int", divideNumber,
     NAME_calculate, "Divide value by argument"),
  SM(NAME_maximum, 1, "int", maximumNumber,
     NAME_calculate, "Set value to largest of current and argument"),
  SM(NAME_minimum, 1, "int", minimumNumber,
     NAME_calculate, "Set value to largest of current and argument"),
  SM(NAME_minus, 1, "int", minusNumber,
     NAME_calculate, "Subtract argument from value"),
  SM(NAME_plus, 1, "int", plusNumber,
     NAME_calculate, "Add argument to value"),
  SM(NAME_times, 1, "int", timesNumber,
     NAME_calculate, "Multiply value by argument"),
  SM(NAME_equal, 1, "int", equalNumber,
     NAME_compare, "Test if equal to argument"),
  SM(NAME_larger, 1, "int", largerNumber,
     NAME_compare, "Test if larger than argument"),
  SM(NAME_largerEqual, 1, "int", largerEqualNumber,
     NAME_compare, "Test if larger-or-equal than argument"),
  SM(NAME_lessEqual, 1, "int", lessEqualNumber,
     NAME_compare, "Test if less-or-equal than argument"),
  SM(NAME_notEqual, 1, "int", notEqualNumber,
     NAME_compare, "Test if not-equal to argument"),
  SM(NAME_smaller, 1, "int", smallerNumber,
     NAME_compare, "Test if less than argument")
};

/* Get Methods */

static getdecl get_number[] =
{ GM(NAME_convert, 1, "number", "any", getConvertNumber,
     DEFAULT, "Converts int, real and char_array"),
  GM(NAME_compare, 1, "{smaller,equal,larger}", "int", getCompareNumber,
     NAME_compare, "Compare with argument"),
  GM(NAME_catchAll, 2, "copy=number", T_catchAll, getCatchAllNumber,
     NAME_copy, "Create copy and run method on it"),
  GM(NAME_printName, 0, "string", NULL, getPrintNameNumber,
     NAME_textual, "Formatted version (%d) of value"),
  GM(NAME_value, 0, "int", NULL, getValueNumber,
     NAME_value, "Integer representing value")
};

/* Resources */

#define rc_number NULL
/*
static resourcedecl rc_number[] =
{ 
};
*/

/* Class Declaration */

static Name number_termnames[] = { NAME_value };

ClassDecl(number_decls,
          var_number, send_number, get_number, rc_number,
          1, number_termnames,
          "$Rev$");

status
makeClassNumber(Class class)
{ return declareClass(class, &number_decls);
}
