/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <math.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
New reals  can be created  using newObject(ClassReal, value,  0).  The
argument should be a proper PCE data type, which is converted into a C
float by initialiseReal.  When a Real should be created from a C float
use the function CtoReal().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Real
CtoReal(float f)
{ Real r = answerObject(ClassReal, ZERO, 0);
  r->value = f;
  DEBUG(NAME_real, Cprintf("CtoReal(%f)\n", f));

  return r;
}


static status
initialiseReal(Real r, Any arg)
{ if ( isInteger(arg) )
  { r->value = (float)valInt(arg);
  } else if ( instanceOfObject(arg, ClassNumber) )
  { r->value = (float)valInt(((Number)arg)->value);
  } else if ( instanceOfObject(arg, ClassReal) )
  { r->value = ((Real)arg)->value;
  } else
    return errorPce(ClassReal, NAME_cannotConvert, arg);

  succeed;
}


Real
getConvertReal(Class class, Any obj)
{ char *s;

  if ( isInteger(obj) || instanceOfObject(obj, ClassNumber) )
    answer(answerObjectv(ClassReal, 1, &obj));
  else if ( (s = toCharp(obj)) != FAIL && s[0] != EOS )
  { char *end;
    double f;

    f = StrTod(s, &end);
    if (end == s + strlen(s))
      return CtoReal((float)f);
  }

  fail;
}


static StringObj
getPrintNameReal(Real r)
{ char buf[100];

  sprintf(buf, "%f", r->value);

  answer(CtoString(buf));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Store/load reals to/from file. Format:

<real>		::= <word>		(word holding float)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeReal(Real r, FileObj file)
{ union { long l; float f; } u;

  TRY(storeSlotsObject(r, file));
  u.f = r->value;
  return storeWordFile(file, (Any) u.l);
}

static status
loadReal(Real r, FILE *fd, ClassDef def)
{ union { long l; float f; } u;

  TRY(loadSlotsObject(r, fd, def));
  u.l = loadWord(fd);
  r->value = u.f;

  succeed;
}


status
equalReal(Real r, Real r2)
{ if (r->value == r2->value)
    succeed;
  fail;
}


static status
notEqualReal(Real r, Real r2)
{ if (r->value != r2->value)
    succeed;
  fail;
}


static status
smallerReal(Real r, Real r2)
{ if (r->value < r2->value)
    succeed;
  fail;
}


static status
largerReal(Real r, Real r2)
{ if (r->value > r2->value)
    succeed;
  fail;
}


static status
lessEqualReal(Real r, Real r2)
{ if (r->value <= r2->value)
    succeed;
  fail;
}


static status
largerEqualReal(Real r, Real r2)
{ if (r->value >= r2->value)
    succeed;
  fail;
}


static status
plusReal(Real r, Real r2)
{ r->value = r->value + r2->value;
  succeed;
}


static status
minusReal(Real r, Real r2)
{ r->value = r->value - r2->value;
  succeed;
}


static status
timesReal(Real r, Real r2)
{ r->value = r->value * r2->value;
  succeed;
}


static status
divideReal(Real r, Real r2)
{ r->value = r->value / r2->value;
  succeed;
}


static Real
getCatchAllReal(Real r, Name selector, int argc, Any *argv)
{ Real result = answerObject(classOfObject(r), r, 0);

  TRY(sendv(result, selector, argc, argv));

  answer(result);
}


status
valueReal(Real r, Real v)
{ r->value = v->value;

  succeed;
}


static Real
getValueReal(Real r)
{ answer(r);
}


status
makeClassReal(Class class)
{ sourceClass(class, makeClassReal, __FILE__, "$Revision$");

  localClass(class, NAME_value, NAME_storage, "alien:float", NAME_none,
	     "C float value");

  termClass(class, "real", 1, NAME_value);
  setLoadStoreFunctionClass(class, loadReal, storeReal);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "value=any",
	     "Create real by converting argument",
	     initialiseReal);
  sendMethod(class, NAME_divide, NAME_calculate, 1, "real",
	     "Divide value by argument",
	     divideReal);
  sendMethod(class, NAME_equal, NAME_compare, 1, "real",
	     "Test if equal to argument",
	     equalReal);
  sendMethod(class, NAME_larger, NAME_compare, 1, "real",
	     "Test if larger than argument",
	     largerReal);
  sendMethod(class, NAME_largerEqual, NAME_compare, 1, "real",
	     "Test if larger-or-equal than argument",
	     largerEqualReal);
  sendMethod(class, NAME_lessEqual, NAME_compare, 1, "real",
	     "Test if less-or-equal than argument",
	     lessEqualReal);
  sendMethod(class, NAME_minus, NAME_calculate, 1, "real",
	     "Substract argument from value",
	     minusReal);
  sendMethod(class, NAME_notEqual, NAME_compare, 1, "real",
	     "Test if not-equal to argument",
	     notEqualReal);
  sendMethod(class, NAME_plus, NAME_calculate, 1, "real",
	     "Add argument to value",
	     plusReal);
  sendMethod(class, NAME_smaller, NAME_compare, 1, "real",
	     "Test if less than argument",
	     smallerReal);
  sendMethod(class, NAME_times, NAME_calculate, 1, "real",
	     "Multiply value by argument",
	     timesReal);
  sendMethod(class, NAME_value, NAME_value, 1, "real",
	     "Set the value to argument",
	     valueReal);

  getMethod(class, NAME_value, NAME_value, "real", 0,
	    "Returns itself",
	    getValueReal);
  getMethod(class, NAME_printName, NAME_textual, "string", 0,
	    "Formatted version (%f) of value",
	    getPrintNameReal);
  getMethod(class, NAME_convert, DEFAULT, "real", 1, "any",
	    "Converts int, number and char_array",
	    getConvertReal);
  getMethod(class, NAME_catchAll, NAME_copy, "copy=real", 2,
	    "selector=name", "argument=unchecked ...",
	    "Create copy and run method on it",
	    getCatchAllReal);

  succeed;
}

