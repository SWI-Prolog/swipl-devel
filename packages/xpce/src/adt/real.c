/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <math.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
New reals  can be created  using newObject(ClassReal, value,  EAV).  The
argument should be a proper PCE data type, which is converted into a C
double by initialiseReal.  When a Real should be created from a C double
use the function CtoReal().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
setReal(Real r, double f)
{ union
  { double f;
    unsigned long l[2];
  } v;

  v.f = f;
  r->value1 = v.l[0];
  r->value2 = v.l[1];
}


double
valReal(Real r)
{ union
  { double f;
    unsigned long l[2];
  } v;

  v.l[0] = r->value1;
  v.l[1] = r->value2;

  return v.f;
}


Real
CtoReal(double f)
{ Real r = answerObject(ClassReal, ZERO, EAV);
  setReal(r, f);

  return r;
}


static status
initialiseReal(Real r, Any arg)
{ double v;

  setFlag(r, F_ISREAL);

  if ( isInteger(arg) )
  { v = (double)valInt(arg);
  } else if ( instanceOfObject(arg, ClassNumber) )
  { v = (double)((Number)arg)->value;
  } else if ( instanceOfObject(arg, ClassReal) )
  { Real a = arg;
    
    return valueReal(r, a);
  } else
    return errorPce(ClassReal, NAME_cannotConvert, arg);

  setReal(r, v);

  succeed;
}


Real
getConvertReal(Class class, Any obj)
{ char *s;

  if ( isInteger(obj) || instanceOfObject(obj, ClassNumber) )
    answer(answerObjectv(ClassReal, 1, &obj));
  else if ( (s = toCharp(obj)) && s[0] != EOS )
  { char *end;
    double f;

    f = StrTod(s, &end);
    if (end == s + strlen(s))
      return CtoReal(f);
  }

  fail;
}


static StringObj
getPrintNameReal(Real r)
{ char buf[100];

  sprintf(buf, "%g", valReal(r));

  answer(CtoString(buf));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Store/load reals to/from file. Format:

<real>		::= <word>		(first part of double)
		    <word>		(second part of double)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeReal(Real r, FileObj file)
{ TRY(storeSlotsObject(r, file));

#ifndef WORDS_BIGENDIAN
  storeWordFile(file, (Any) r->value2);
  storeWordFile(file, (Any) r->value1);
#else
  storeWordFile(file, (Any) r->value1);
  storeWordFile(file, (Any) r->value2);
#endif

  succeed;
}


static status
loadReal(Real r, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(r, fd, def));

  setFlag(r, F_ISREAL);

  if ( restoreVersion < 16 )		/* saved as single */
  { union { long l; float f; } u;

    u.l = loadWord(fd);
    setReal(r, (double)u.f);
  } else
  { 
#ifndef WORDS_BIGENDIAN
    r->value2 = loadWord(fd);
    r->value1 = loadWord(fd);
#else
    r->value1 = loadWord(fd);
    r->value2 = loadWord(fd);
#endif
  }
  succeed;
}


static status
cloneReal(Real r1, Real r2)
{ clonePceSlots(r1, r2);
  setFlag(r2, F_ISREAL);

  r2->value1 = r1->value1;
  r2->value2 = r1->value2;

  succeed;
}


static status
equalReal(Real r, Real r2)
{ if ( r->value1 == r2->value1 &&
       r->value2 == r2->value2 )
    succeed;
  fail;
}


static status
notEqualReal(Real r, Real r2)
{ if ( r->value1 == r2->value1 &&
       r->value2 == r2->value2 )
    fail;
  succeed;
}


static status
smallerReal(Real r, Real r2)
{ if (valReal(r) < valReal(r2))
    succeed;
  fail;
}


static status
largerReal(Real r, Real r2)
{ if (valReal(r) > valReal(r2))
    succeed;
  fail;
}


static status
lessEqualReal(Real r, Real r2)
{ if (valReal(r) <= valReal(r2))
    succeed;
  fail;
}


static status
largerEqualReal(Real r, Real r2)
{ if (valReal(r) >= valReal(r2))
    succeed;
  fail;
}


static status
plusReal(Real r, Real r2)
{ setReal(r, valReal(r) + valReal(r2));
  succeed;
}


static status
minusReal(Real r, Real r2)
{ setReal(r, valReal(r) - valReal(r2));
  succeed;
}


static status
timesReal(Real r, Real r2)
{ setReal(r, valReal(r) * valReal(r2));
  succeed;
}


static status
divideReal(Real r, Real r2)
{ setReal(r, valReal(r) / valReal(r2));
  succeed;
}


static Name
getCompareReal(Real r1, Real r2)
{ if ( valReal(r1) > valReal(r2) )
    answer(NAME_larger);
  if ( valReal(r1) < valReal(r2) )
    answer(NAME_smaller);
  answer(NAME_equal);
}


static Real
getCatchAllReal(Real r, Name selector, int argc, Any *argv)
{ Real result = answerObject(classOfObject(r), r, EAV);

  TRY(sendv(result, selector, argc, argv));

  answer(result);
}


status
valueReal(Real r, Real v)
{ r->value1 = v->value1;
  r->value2 = v->value2;

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
{ IV(NAME_value1, "alien:double1", IV_NONE,
     NAME_storage, "1-st part of double-value"),
  IV(NAME_value2, "alien:double2", IV_NONE,
     NAME_storage, "2-nd part of double-value")
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
     NAME_textual, "Formatted version (%g) of value"),
  GM(NAME_value, 0, "real", NULL, getValueReal,
     NAME_value, "Returns itself")
};

/* Resources */

#define rc_real NULL
/*
static classvardecl rc_real[] =
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

  setCloneFunctionClass(class, cloneReal);
  setLoadStoreFunctionClass(class, loadReal, storeReal);

  succeed;
}

