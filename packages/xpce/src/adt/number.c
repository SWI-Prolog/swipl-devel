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


status
makeClassNumber(Class class)
{ sourceClass(class, makeClassNumber, __FILE__, "$Revision$");

  localClass(class, NAME_value, NAME_storage, "int", NAME_send,
	     "Value of number object");

  termClass(class, "number", 1, NAME_value);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "value=int",
	     "Create from integer",
	     initialiseNumber);
  sendMethod(class, NAME_divide, NAME_calculate, 1, "int",
	     "Divide value by argument",
	     divideNumber);
  sendMethod(class, NAME_equal, NAME_compare, 1, "int",
	     "Test if equal to argument",
	     equalNumber);
  sendMethod(class, NAME_larger, NAME_compare, 1, "int",
	     "Test if larger than argument",
	     largerNumber);
  sendMethod(class, NAME_largerEqual, NAME_compare, 1, "int",
	     "Test if larger-or-equal than argument",
	     largerEqualNumber);
  sendMethod(class, NAME_lessEqual, NAME_compare, 1, "int",
	     "Test if less-or-equal than argument",
	     lessEqualNumber);
  sendMethod(class, NAME_minus, NAME_calculate, 1, "int",
	     "Subtract argument from value",
	     minusNumber);
  sendMethod(class, NAME_notEqual, NAME_compare, 1, "int",
	     "Test if not-equal to argument",
	     notEqualNumber);
  sendMethod(class, NAME_plus, NAME_calculate, 1, "int",
	     "Add argument to value",
	     plusNumber);
  sendMethod(class, NAME_smaller, NAME_compare, 1, "int",
	     "Test if less than argument",
	     smallerNumber);
  sendMethod(class, NAME_times, NAME_calculate, 1, "int",
	     "Multiply value by argument",
	     timesNumber);
  sendMethod(class, NAME_maximum, NAME_calculate, 1, "int",
	     "Set value to largest of current and argument",
	     maximumNumber);
  sendMethod(class, NAME_minimum, NAME_calculate, 1, "int",
	     "Set value to largest of current and argument",
	     minimumNumber);

  getMethod(class, NAME_value, NAME_value, "int", 0,
	    "Integer representing value",
	    getValueNumber);
  getMethod(class, NAME_convert, DEFAULT, "number", 1, "any",
	    "Converts int, real and char_array",
	    getConvertNumber);
  getMethod(class, NAME_printName, NAME_textual, "string", 0,
	    "Formatted version (%d) of value",
	    getPrintNameNumber);
  getMethod(class, NAME_compare, NAME_compare, "{smaller,equal,larger}", 1,
	    "int",
	    "Compare with argument",
	    getCompareNumber);
  getMethod(class, NAME_catchAll, NAME_copy, "copy=number", 2,
	    "selector=name", "argument=unchecked ...",
	    "Create copy and run method on it",
	    getCatchAllNumber);

  succeed;
}
