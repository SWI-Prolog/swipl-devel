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

  setFlag(r, F_ISREAL);

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


static status
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


static Name
getCompareReal(Real r1, Real r2)
{ if ( r1->value > r2->value )
    answer(NAME_larger);
  if ( r1->value < r2->value )
    answer(NAME_smaller);
  answer(NAME_equal);
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_catchAll[] =
        { "selector=name", "argument=unchecked ..." };

/* Instance Variables */

static vardecl var_real[] =
{ IV(NAME_value, "alien:float", IV_NONE,
     NAME_storage, "C float value")
};

/* Send Methods */

static senddecl send_real[] =
{ SM(NAME_initialise, 1, "value=any", initialiseReal,
     DEFAULT, "Create real by converting argument"),
  SM(NAME_divide, 1, "real", divideReal,
     NAME_calculate, "Divide value by argument"),
  SM(NAME_minus, 1, "real", minusReal,
     NAME_calculate, "Subtract argument from value"),
  SM(NAME_plus, 1, "real", plusReal,
     NAME_calculate, "Add argument to value"),
  SM(NAME_times, 1, "real", timesReal,
     NAME_calculate, "Multiply value by argument"),
  SM(NAME_equal, 1, "real", equalReal,
     NAME_compare, "Test if equal to argument"),
  SM(NAME_larger, 1, "real", largerReal,
     NAME_compare, "Test if larger than argument"),
  SM(NAME_largerEqual, 1, "real", largerEqualReal,
     NAME_compare, "Test if larger-or-equal than argument"),
  SM(NAME_lessEqual, 1, "real", lessEqualReal,
     NAME_compare, "Test if less-or-equal than argument"),
  SM(NAME_notEqual, 1, "real", notEqualReal,
     NAME_compare, "Test if not-equal to argument"),
  SM(NAME_smaller, 1, "real", smallerReal,
     NAME_compare, "Test if less than argument"),
  SM(NAME_value, 1, "real", valueReal,
     NAME_value, "Set the value to argument")
};

/* Get Methods */

static getdecl get_real[] =
{ GM(NAME_convert, 1, "real", "any", getConvertReal,
     DEFAULT, "Converts int, number and char_array"),
  GM(NAME_compare, 1, "{smaller,equal,larger}", "real", getCompareReal,
     NAME_compare, "Compare with argument"),
  GM(NAME_catchAll, 2, "copy=real", T_catchAll, getCatchAllReal,
     NAME_copy, "Create copy and run method on it"),
  GM(NAME_printName, 0, "string", NULL, getPrintNameReal,
     NAME_textual, "Formatted version (%f) of value"),
  GM(NAME_value, 0, "real", NULL, getValueReal,
     NAME_value, "Returns itself")
};

/* Resources */

#define rc_real NULL
/*
static resourcedecl rc_real[] =
{ 
};
*/

/* Class Declaration */

static Name real_termnames[] = { NAME_value };

ClassDecl(real_decls,
          var_real, send_real, get_real, rc_real,
          1, real_termnames,
          "$Rev$");


status
makeClassReal(Class class)
{ declareClass(class, &real_decls);

  setLoadStoreFunctionClass(class, loadReal, storeReal);

  succeed;
}

