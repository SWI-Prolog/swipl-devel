/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef ARITH_H_INCLUDED
#define ARITH_H_INCLUDED

#ifdef O_NOFLOAT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Perform  operations involving  floating point numbers  (on a Sun 4/110
the ratio  int:float is about  1:3000). As  floats  are used to  avoid
rounding errors in   expressions  and equations  we have written  some
macro's that   avoid  the use   of   floating  point numbers   and use
bit-shifts instead.  A  consequence is  that integer   overflow is not
unlikely on large numbers, there are no checks on overflow.


The current  implementation uses a   simple  bit-shift  to    get  the
necessary precision.   The shifted numbers  are  called PseudoFloat's.
Functions:

	  p = Int_PSF(x)	Convert (int) to (PseudoFloat)
	  x = PSF_Int(p)	Convert (PseudoFloat) to (int)
	  p = Float_PSF(f)	Convert (float) to (PseudoFloat)
	  f = PSF_Float(p)	Convert (PseudoFloat) to (float)

	  p = PSF_div(p1, p2)	p = p1/p2
	  p = PSF_mul(p1, p2)	p = p1*p2
	  p = PSF_add(p1, p2)	p = p1+p2
	  p = PSF_sub(p1, p2)	p = p1-p2
	
Note that the implementation may change and one  should also use these
macro's for addition and subtraction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define PSF_SHIFT	10		/* precision shift */
#define PSF_FACTOR	(1<<PSF_SHIFT)	/* precision factor */

#define Int_PSF(a)	((a) << PSF_SHIFT)
#define PSF_Int(a)	(((a)+(1<<(PSF_SHIFT-1))-1) >> PSF_SHIFT)
#define Float_PSF(f)	((int)((f)*(float)PSF_FACTOR))
#define PSF_Float(p)	(((float)(p))/(float) PSF_FACTOR)

#define PSF_div(a, b)	((a)/PSF_Int(b))
#define PSF_mul(a, b)	((a)*PSF_Int(b))
#define PSF_add(a, b)	((a)+(b))
#define PSF_sub(a, b)	((a)-(b))

GLOBAL int	arithError;		/* error occurred */

#else /*O_NOFLOAT*/

#define V_ERROR	      (-1)
#define V_INTEGER      (0)
#define V_DOUBLE       (1)

typedef struct
{ int type;				/* V_INTEGER or V_FLOAT */
  union
  { long	i;			/* integer value */
    double	f;			/* float value */
  } value;
} numeric_value, *NumericValue;

#endif /*O_NOFLOAT*/

#include <ari/proto.h>
#endif /*ARITH_H_INCLUDED*/
