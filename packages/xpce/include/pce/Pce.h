/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_H
#define _PCE_H

#ifndef XPCE_USE_ABSTRACT_INTERFACE
#define XPCE_USE_ABSTRACT_INTERFACE 0
#endif

#if __GNUC__
#define __constf const
typedef void (*func_ptr) ();		/* for the ctor handling */
typedef void*	Any;			/* Anonymous handle */
#else
#define __constf			/* constant function only for gcc */
#define Any void *			/* Well, at least MSVC 4.2 is broken */
#endif

#ifdef WIN32
#define __external_var _declspec(dllimport)
#else
#define __external_var extern
#endif

#define PceExternalClass(ptr) extern "C" { __external_var Any ptr; }

enum PceStatus				/* Pce procedure return */
{ FAIL = 0,
  SUCCEED = 1
};

extern "C" {
					/* C ---> XPCE */

Any		XPCE_to_real(const float value);
__constf Any	XPCE_to_name(const char *text);
__constf Any	XPCE_to_integer(const long value);
__constf Any	XPCE_to_object(const Any name);
__constf Any 	XPCE_to_class(const Any name);

					/* XPCE ---> C */

char *		XPCE_charp_of(const Any string);
float		XPCE_float_of(const Any real);
__constf long	XPCE_int_of(const Any integer);
void *		XPCE_pointer_of(const Any integer);


					/* VMI */
PceStatus	XPCE_sendv(Any receiver, Any selector,
			   int argc, const Any argv[]);
Any 		XPCE_getv(Any receiver, Any selector,
			  int argc, const Any argv[]);
Any 		XPCE_newv(Any cl, const Any name,
			  int argc, const Any argv[]);
PceStatus	XPCE_free(Any);


					/* TOPLEVEL */
PceStatus	pceInitApplication(int argc, char *argv[]);
void		initCPlusPlusGlobals(void); /* make @pce, @arg1, ... */
void		XPCE_initialise(void);
#ifdef __GNUC__
extern 		func_ptr __CTOR_LIST__[];
void		do_ctors(func_ptr *p);
#endif

					/* DEBUGGING */
char *		pcePP(const Any datum);
}

#if !XPCE_USE_ABSTRACT_INTERFACE

#define XPCE_INT_MASK	0x00000002	/* 10 mask for Int (integers) */
#define XPCE_TAG_BITS	2		/* number of mask bits for INT */

inline int
PceIsInt(Any o)
{ return ((unsigned long)(o) & XPCE_INT_MASK);
}

inline Any
PceToInt(long i)
{ return ((Any)(((long)(i)<<XPCE_TAG_BITS)|XPCE_INT_MASK)); /* toInt */
}

inline long
PceValInt(Any o)
{ if ( ((unsigned long)(o) & XPCE_INT_MASK) )	/* isInteger */
    return (((long)(o))>>XPCE_TAG_BITS); /* valInt */

  return XPCE_int_of(o);
}

#else

#define PceToInt(i)	XPCE_to_integer(i)
#define PceValInt(o)	XPCE_int_of(o)

#endif /*XPCE_USE_ABSTRACT_INTERFACE*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Proper start ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef NULL				/* TBD */
#define NULL ((Any)0)
#endif

#ifndef TRUE
#define TRUE (SUCCEED)
#endif
#ifndef FALSE
#define FALSE (FAIL)
#endif

class PceArg
{ 
public:
  Any self;

  PceArg()
  {
  }

  PceArg(Any a)
  { self = a;
  }
#ifndef __GNUC__
  PceArg(const Any a)
  { self = (Any)a;
  }
#endif

					/* C --> PceArg */
  PceArg(char *text)
  { self = (Any)XPCE_to_name(text);
  }
  PceArg(long i)
  { self = PceToInt(i);
  }
  PceArg(int i)
  { self = PceToInt(i);
  }
  PceArg(float f)
  { self = XPCE_to_real(f);
  }
  PceArg(double f)
  { self = XPCE_to_real((float) f);
  }
  PceArg(const PceArg &obj)
  { self = obj.self;
  }
  PceArg(PceStatus /*s*/)
  { self = NULL;			/* error iff SUCCEED */
  }
					/* PceArg --> C */
  operator PceStatus(void)		/* why is this necessary??? */
  { return self ? SUCCEED : FAIL;
  }
  operator char *(void)
  { return XPCE_charp_of(self);
  }
  operator long(void)
  { return PceIsInt(self) ? PceValInt(self) : (long)self;
  }
  operator int(void)
  { return PceIsInt(self) ? PceValInt(self) : (int)self;
  }
  operator double(void)
  { return (double)XPCE_float_of(self);
  }
  operator float(void)
  { return XPCE_float_of(self);
  }
  operator Any (void)
  { return self;
  }
#if !defined(__GNUC__) || __GNUC_MINOR__ < 8
  operator const Any (void)		/* 2.8.0 says ambiguous */
  { return (const Any)self;
  }
#endif
#if defined(__GNUC__) && __GNUC_MINOR__ > 6
  operator bool (void)
  { return self ? TRUE : FALSE;
  }
#endif

  PceStatus send(const PceArg& sel)
  { return XPCE_sendv(self, sel.self, 0, (const Any *)NULL);
  }
  PceStatus send(const PceArg& sel, const PceArg& a1)
  { Any av[1];
    av[0] = a1.self;
    return XPCE_sendv(self, sel.self, 1, (const Any *)av); 
  }
  PceStatus send(const PceArg& sel, const PceArg& a1, const PceArg& a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    return XPCE_sendv(self, sel.self, 2, (const Any *)av);
  }
  PceStatus send(const PceArg& sel,
		 const PceArg& a1, const PceArg& a2, const PceArg& a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    return XPCE_sendv(self, sel.self, 3, (const Any *)av);
  }
  PceStatus send(const PceArg& sel,
		 const PceArg& a1, const PceArg& a2,
		 const PceArg& a3, const PceArg& a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    return XPCE_sendv(self, sel.self, 4, (const Any *)av);
  }
  PceStatus send(const PceArg& sel,
		 const PceArg& a1, const PceArg& a2,
		 const PceArg& a3, const PceArg& a4,
		 const PceArg& a5)
  { Any av[5];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    return XPCE_sendv(self, sel.self, 5, (const Any *)av);
  }
					/* GET */
  PceArg get(const PceArg& sel)
  { return PceArg(XPCE_getv(self, sel.self, 0, (const Any *)NULL));
  }
  PceArg get(const PceArg& sel, const PceArg& a1)
  { Any av[1];
    av[0] = a1.self;
    return PceArg(XPCE_getv(self, sel.self, 1, (const Any *)av));
  }
  PceArg get(const PceArg& sel, const PceArg& a1, const PceArg& a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    return PceArg(XPCE_getv(self, sel.self, 2, (const Any *)av));
  }
  PceArg get(const PceArg& sel,
	     const PceArg& a1, const PceArg& a2, const PceArg& a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    return PceArg(XPCE_getv(self, sel.self, 3, (const Any *)av));
  }

  PceStatus free(void)			/* FREE */
  { return XPCE_free(self);
  }
  
					/* (Integer) Arithmetic */
  PceArg operator +(long i)		/* x + y */
  { return PceArg(PceValInt(self) + i);
  }
  PceArg operator /(long i)		/* x / y */
  { return PceArg(PceValInt(self) / i);
  }
  PceArg operator *(long i)		/* x * y */
  { return PceArg(PceValInt(self) * i);
  }
  PceArg operator -(long i)		/* x - y */
  { return PceArg(PceValInt(self) - i);
  }
  PceArg operator -(void)		/* -x */
  { return PceArg(-PceValInt(self));
  }
  PceArg& operator --(void)		/* --x */
  { self = PceToInt(PceValInt(self)-1);
    return *this;
  }
  PceArg operator --(int)		/* x-- */
  { void *old = self;
    self = PceToInt(PceValInt(self)-1);
    return PceArg(old);
  }
  PceArg& operator ++(void)		/* ++x */
  { self = PceToInt(PceValInt(self)+1);
    return *this;
  }
  PceArg operator ++(int)		/* x++ */
  { void *old = self;
    self = PceToInt(PceValInt(self)+1);
    return PceArg(old);
  }
  PceArg& operator +=(long i)		/* += */
  { self = PceToInt(PceValInt(self)+i);
    return *this;
  }
  PceArg& operator -=(long i)		/* -= */
  { self = PceToInt(PceValInt(self)-i);
    return *this;
  }
  PceArg& operator *=(long i)		/* *= */
  { self = PceToInt(PceValInt(self)*i);
    return *this;
  }
  PceArg& operator /=(long i)		/* /= */
  { self = PceToInt(PceValInt(self)/i);
    return *this;
  }

  int operator >(long i)		/* x > y */
  { return PceValInt(self) > i;
  }
  int operator >=(long i)		/* x >= y */
  { return PceValInt(self) >= i;
  }
  int operator <(long i)		/* x < y */
  { return PceValInt(self) < i;
  }
  int operator <=(long i)		/* x <= y */
  { return PceValInt(self) <= i;
  }
  PceArg& operator=(const PceArg&q)	/* x = y */
  { self = q.self;
    return *this;
  }

  int operator ==(const PceArg&q)	/* x == y */
  { return self == q.self;
  }
  int operator !=(const PceArg&q)	/* x != y */
  { return self != q.self;
  }
  int operator ==(const PceStatus q)	/* x == FAIL || SUCCEED */
  { return (q == FAIL ? self == NULL : self != NULL);
  }
  char *pp()				/* pretty-print */
  { return pcePP(self);
  }
  void *pointer()			/* PcePointer support */
  { return XPCE_pointer_of(self);
  }
};


class PceObject :public PceArg
{
public:					/* NEW */
  PceObject()
  {
  }
  PceObject(const PceObject&q)
  { self = q.self;
  }
  PceObject(PceArg cl) : PceArg(XPCE_newv(cl.self, NULL,
					  0, (const Any *)NULL))
  { 
  }
  PceObject(const char *classname) :
    PceArg(XPCE_newv((Any)XPCE_to_name(classname), NULL,
		     0, (const Any *)NULL))
  {
  }
  PceObject(Any cl) : PceArg(XPCE_newv(cl, NULL,
				       0, (const Any *)NULL))
  {
  }
  PceObject(PceArg cl, PceArg a1)
  { Any av[1];
    av[0] = a1.self;
    self = XPCE_newv(cl.self, NULL, 1, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    self = XPCE_newv(cl.self, NULL, 2, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2, PceArg a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    self = XPCE_newv(cl.self, NULL, 3, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2, PceArg a3, PceArg a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    self = XPCE_newv(cl.self, NULL, 4, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
	    PceArg a5)
  { Any av[5];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    self = XPCE_newv(cl.self, NULL, 5, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
	    PceArg a5, PceArg a6)
  { Any av[6];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    av[5] = a6.self;
    self = XPCE_newv(cl.self, NULL, 6, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
	    PceArg a5, PceArg a6, PceArg a7)
  { Any av[7];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    av[5] = a6.self;
    av[6] = a7.self;
    self = XPCE_newv(cl.self, NULL, 7, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
	    PceArg a5, PceArg a6, PceArg a7, PceArg a8)
  { Any av[8];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    av[5] = a6.self;
    av[6] = a7.self;
    av[7] = a8.self;
    self = XPCE_newv(cl.self, NULL, 8, (const Any *)av);
  }
  PceObject(PceArg cl, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
	    PceArg a5, PceArg a6, PceArg a7, PceArg a8, PceArg a9)
  { Any av[9];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    av[5] = a6.self;
    av[6] = a7.self;
    av[7] = a8.self;
    av[8] = a9.self;
    self = XPCE_newv(cl.self, NULL, 9, (const Any *)av);
  }
};


		 /*******************************
		 *	  GLOBAL OBJECTS	*
		 *******************************/

#ifndef __GLOBAL
#define __GLOBAL extern
#endif

class PceGlobal :public PceArg
{
public:
  PceGlobal()
  {
  }
  PceGlobal(const char *name) :
    PceArg(XPCE_to_object(XPCE_to_name(name))) {}
  PceGlobal(PceArg name) :
    PceArg(XPCE_to_object(name)) {}
  PceGlobal(PceArg name, PceArg cl)
  { self = XPCE_newv(cl.self, name.self, 0, (const Any *)NULL);
  }
  PceGlobal(PceArg name, PceArg cl, PceArg a1)
  { Any av[1];
    av[0] = a1.self;
    self = XPCE_newv(cl.self, name.self, 1, (const Any *)av);
  }
  PceGlobal(PceArg name, PceArg cl, PceArg a1, PceArg a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    self = XPCE_newv(cl.self, name.self, 2, (const Any *)av);
  }
  PceGlobal(PceArg name, PceArg cl, PceArg a1, PceArg a2, PceArg a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    self = XPCE_newv(cl.self, name.self, 3, (const Any *)av);
  }
  PceGlobal(PceArg name, PceArg cl, PceArg a1, PceArg a2, PceArg a3, PceArg a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    self = XPCE_newv(cl.self, name.self, 4, (const Any *)av);
  }
};

		 /*******************************
		 *	       GLOBALS		*
		 *******************************/

extern "C" {
__external_var Any XPCE_on;
__external_var Any XPCE_off;
__external_var Any XPCE_nil;
__external_var Any XPCE_default;
__external_var Any XPCE_arg1;
__external_var Any XPCE_arg2;
__external_var Any XPCE_arg3;
__external_var Any XPCE_arg4;
__external_var Any XPCE_arg5;
__external_var Any XPCE_arg6;
__external_var Any XPCE_arg7;
__external_var Any XPCE_arg8;
__external_var Any XPCE_arg9;
__external_var Any XPCE_arg10;
__external_var Any XPCE_event;
__external_var Any XPCE_receiver;
__external_var Any XPCE_pce;
__external_var Any XPCE_display;
}

#define TheOn		PceArg(XPCE_on)
#define TheOff		PceArg(XPCE_off)
#define TheNil		PceArg(XPCE_nil)
#define TheDefault	PceArg(XPCE_default)
#define TheArg1		PceArg(XPCE_arg1)
#define TheArg2		PceArg(XPCE_arg2)
#define TheArg3		PceArg(XPCE_arg3)
#define TheArg4		PceArg(XPCE_arg4)
#define TheArg5		PceArg(XPCE_arg5)
#define TheArg6		PceArg(XPCE_arg6)
#define TheArg7		PceArg(XPCE_arg7)
#define TheArg8		PceArg(XPCE_arg8)
#define TheArg9		PceArg(XPCE_arg9)
#define TheArg10	PceArg(XPCE_arg10)
#define TheEvent	PceArg(XPCE_event)
#define TheReceiver	PceArg(XPCE_receiver)
#define ThePce		PceArg(XPCE_pce)
#define TheDisplay	PceArg(XPCE_display)

#endif /*!_PCE_H*/
