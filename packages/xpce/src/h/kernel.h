/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <md.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#if O_XOS
#include <xos/xos.h>
#endif

#if !defined(HAVE_SYS_FILE_H) || HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

#ifndef TIME_H
#define TIME_H <sys/time.h>
#endif

#define PCE_INCLUDED

#ifndef GLOBAL
#define GLOBAL extern			/* global variables */
#endif

		/********************************
		*             LIMITS		*
		********************************/

#define PCE_MAX_RECURSION	1000	/* maximum send() recursion */
#define VA_PCE_MAX_ARGS		10	/* send(), etc. */
#define FWD_PCE_MAX_ARGS	10	/* @arg1 ... @arg10 */
#define SCAN_MAX_ARGS 		32	/* scanstr maximum arguments */
#define PCE_MAX_INT		((1 << (32 - TAG_BITS - 1)) - 1)
#define PCE_MIN_INT		(-PCE_MAX_INT - 1)

#define LINESIZE		2048	/* maximum length of a line */
#define FORMATSIZE		10000	/* maximum length of a ->format */
#define BROWSER_LINE_WIDTH	256	/* maximum #chars of line in browser */

		/********************************
		*           OS STUFF		*
		********************************/

#ifndef SIGNAL_HANDLER_TYPE		/* type returned by signal-handler */
#define SIGNAL_HANDLER_TYPE void
#endif

		/********************************
		*       SAVING OBJECTS		*
		********************************/

#define SAVEMAGIC   		"PCE version 4"
#define SAVEVERSION 		14

		/********************************
		*             ASSERTS		*
		********************************/

#undef assert
#ifdef NDEBUG
#define assert(expr) ((void)0)
#else
#define assert(expr) ((expr) ? (void)0 : \
			       (void)pceAssert(0,#expr,__FILE__,__LINE__))
#endif

		/********************************
		*        COMPILER STUFF		*
		********************************/

#ifdef __STDC__				/* Prototype handling */
#define P(s) s
#include <stdarg.h>
#else
#define P(s) ()
#include <varargs.h>
#endif

#if __STRICT_ANSI__
#define O_NO_TAGGED_LVALUE 1
#endif

#ifdef __GNUC__
#if !__STRICT_ANSI__			/* gcc -ansi */
#define O_INLINE 1
#define O_CONST_FUNCTION 1
#endif
#define Promote(type) int
#else
#define Promote(type) type
#endif

#if !O_INLINE
#define inline
#endif

#if O_CONST_FUNCTION
#define constf const
#else
#define constf
#endif

#define forwards	static		/* Local functions */

#if __GNUC__ && !__STRICT_ANSI__
#define LocalArray(t, n, s)	t n[s]
#else
#define LocalArray(t, n, s)	t *n = (t *) alloca((s)*sizeof(t))
#endif

#define ArgVector(name, s)	LocalArray(Any, name, s)
#define CharBuf(name, s)	LocalArray(unsigned char, name, (s)+1)


#define NOTREACHED	assert(0)	/* should not get here */

#ifdef O_EXTRA_SYSTEM_TYPES
#include O_EXTRA_SYSTEM_TYPES
#endif

		 /*******************************
		 * OS-IDENTIFIERS (STRICT_ANSI) *
		 *******************************/

#if defined(__unix__) && !defined(unix)
#define unix 1
#endif

#if defined(__sun__) && !defined(sun)
#define sun 1
#endif

		 /*******************************
		 *	 LIBRARY PROBLEMS	*
		 *******************************/

#ifndef StrTod
#define StrTod(s, e)	strtod(s, e)
#endif

		/********************************
		*         NAME CONFLICTS	*
		********************************/

#define	CtoInt(i)	toInt(i)	/* int --> Int */
#define	pp(x)		pcePP((Any)(x))	/* interface name */
#define get             getPCE          /* avoid common name-conflict */
#define send            sendPCE         /* same */
#define toString	toStringPCE	/* SWI-Prolog name-conflict */

		 /*******************************
		 *	    BASIC TYPES		*
		 *******************************/

#if !O_ULONG_PREDEFINED
typedef unsigned long		ulong;
#endif

#ifdef NEED_USHORT
typedef unsigned short		ushort;
#endif

typedef int			status;		/* FAIL, SUCCEED */
typedef void *			Any;		/* Arbitrary object */

typedef Any			Int;		/* ZERO, ONE, ... */
typedef Any			(*Func)();	/* Absolete GetFunc (TBD) */
typedef Any			(*GetFunc)();	/* GetMethod implementation */
typedef status			(*SendFunc)();	/* SendMethod implementation */
typedef void			(*VoidFunc)();

typedef void *			WsRef;		/* Window-system reference */
typedef struct xref *		Xref;		/* Entry in ws-table */

typedef struct classdef        *ClassDef; 	/* See pce-save.c */
typedef struct dCell 	      **DelegateList;   /* See msg-passing.c */

#include "types.h"

		/********************************
		*	    POINTERS	 	*
		********************************/

#ifndef POINTER_OFFSET
#define POINTER_OFFSET (0L)
#endif

#ifndef TEXT_OFFSET
#define TEXT_OFFSET (0L)
#endif

#ifndef TEXT_PTR_ALIGNMENT
#define TEXT_PTR_ALIGNMENT sizeof(int)
#endif

#define PointerToInt(p)	toInt(((ulong)(p) - POINTER_OFFSET)/sizeof(int))
#define longToPointer(i) ((Any) (i * sizeof(int) + POINTER_OFFSET))
#define IntToPointer(i) longToPointer(valInt(i))

#define TextPointerToInt(p) toInt(((ulong)(p) - TEXT_OFFSET) / \
				  TEXT_PTR_ALIGNMENT)
#define IntToTextPointer(i) ((Func) (valInt(i) * TEXT_PTR_ALIGNMENT \
				  + TEXT_OFFSET))


		/********************************
		*           TAG MASKS		*
		********************************/

#define INT_MASK	0x00000002	/* 10 mask for Int (integers) */
#define MASK_MASK	0x00000003	/* 11 Mask Mask */
#define TAG_BITS	2		/* number of mask bits for INT */

#define MaskOf(obj)		((ulong)(obj) & MASK_MASK)
#define UnMask(obj)		((ulong)(obj) & ~MASK_MASK)


		/********************************
		*           EQUALITY		*
		********************************/

#define EQI(o1, o2)	((Any)(o1) == (Any)(o2))
#define EQ(o1, o2)	EQI(o1, o2)


		/********************************
		*             TYPES		*
		********************************/

#define ARGC_UNKNOWN		(-1)


		/********************************
		*           FUNCTIONS		*
		********************************/

#define isFunction(obj)		(isObject(obj) && onFlag(obj, F_ACTIVE))


		/********************************
		*         PCE INTEGERS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PCE uses    tagged   integers  rather   than    C integers.  The   top
TAG_BITS bits hold the MASK whereas the remaining  bits hold the
integer itself.  A PCE integer is declared as of type Int (for casting
purposes).  The following test, conversion and computation macro's are
provided.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef max
#undef min
#define max(a, b)	((a) > (b) ? (a) : (b))
#define min(a, b)	((a) < (b) ? (a) : (b))

#define isInteger(i)	((ulong)(i) & INT_MASK)
#define toInt(i)	((Int)(((long)(i)<<TAG_BITS)|INT_MASK))
#define valInt(i)	(((long)(i))>>TAG_BITS)
#define incrInt(i)	((i) = toInt(valInt(i)+1))
#define decrInt(i)	((i) = toInt(valInt(i)-1))
#define addInt(i, j)	((i) = toInt(valInt(i) + valInt(j)))
#define subInt(i, j)	((i) = toInt(valInt(i) - valInt(j)))
#define maxInt(i, j)	toInt(max(valInt(i), valInt(j)))

#define neg(i)		(toInt(-valInt(i)))
#define add(i, j)	(toInt(valInt(i) + valInt(j)))
#define sub(i, j)	(toInt(valInt(i) - valInt(j)))
#define div(i, j)	(toInt(valInt(i) / valInt(j)))
#define mul(i, j)	(toInt(valInt(i) * valInt(j)))
#define avg(i, j)	(toInt((valInt(i) + valInt(j))/2))
#define mid(i, j)	(toInt((valInt(i) + valInt(j)/2)))
#define dif(i, j)	(toInt((valInt(i) - valInt(j)/2)))
#define inc(i)		(toInt(valInt(i) + 1))
#define dec(i)		(toInt(valInt(i) - 1))
#define minInt(i)	(toInt(-valInt(i)))

#define ZERO		toInt(0)	/* PCE Int 0 */
#define ONE		toInt(1)	/* PCE Int 1 */
#define TWO		toInt(2)	/* PCE Int 2 */


		/********************************
		*          DFLAG VALUES		*
		********************************/


#define makeDFlag(n)		(1L << ((n) - 1 + TAG_BITS))
#define DFlags(obj)		(((ProgramObject)(obj))->dflags)
#if O_NO_TAGGED_LVALUE
void	setDFlagProgramObject P((Any, ulong));
void	clearDFlagProgramObject P((Any, ulong));
#define setDFlag(obj, mask)     setDFlagProgramObject((obj), (mask))
#define clearDFlag(obj, mask)	clearDFlagProgramObject((obj), (mask))
#else
#define setDFlag(obj, mask)	(DFlags(obj) |= (mask))
#define clearDFlag(obj, mask)	(DFlags(obj) &= ~(mask))
#endif
#define onDFlag(obj, mask)	(DFlags(obj) & (mask))
#define offDFlag(obj, mask)	(!onDFlag(obj, mask))

					/* Debugging flags */
#define D_TRACE_ENTER	   makeDFlag(1)	/* Trace enter port of method */
#define D_TRACE_EXIT	   makeDFlag(2)	/* Trace exit port of method */
#define D_TRACE_FAIL	   makeDFlag(3)	/* Trace fail port of method */
#define D_TRACE		   (D_TRACE_ENTER|D_TRACE_EXIT|D_TRACE_FAIL)
#define D_TRACE_INHERIT	   makeDFlag(4)	/* Inherit trace attribute */
#define D_TRACE_CONDITION  makeDFlag(5)	/* Object has trace-condition */

#define D_BREAK_ENTER	   makeDFlag(6)	/* Break enter port of method */
#define D_BREAK_EXIT	   makeDFlag(7)	/* Break exit port of method */
#define D_BREAK_FAIL	   makeDFlag(8)	/* Break fail port of method */
#define D_BREAK		   (D_BREAK_ENTER|D_BREAK_EXIT|D_BREAK_FAIL)
#define D_BREAK_INHERIT	   makeDFlag(9)	/* Inherit trace attribute */
#define D_BREAK_CONDITION  makeDFlag(10) /* Inherit trace attribute */

#define D_SYSTEM	   makeDFlag(11) /* Generate system trace frame */

					/* Variable attributes */
#define D_SAVE_NORMAL	   makeDFlag(12) /* Save normally */
#define D_SAVE_NIL	   makeDFlag(13) /* Save as NIL */
#define D_SAVE 		   (D_SAVE_NORMAL|D_SAVE_NIL)
#define D_DELEGATE_SIDE_EFFECTS makeDFlag(14) /* Delegation has side-effects */

#define D_CLONE_RECURSIVE  makeDFlag(15) /* Clone object recursively */
#define D_CLONE_REFERENCE  makeDFlag(16) /* Clone object reference */
#define D_CLONE_NIL	   makeDFlag(17) /* Cloned value is @nil */
#define D_CLONE_VALUE	   makeDFlag(18) /* Clone the plain PCE value */
#define D_CLONE_ALIEN	   makeDFlag(19) /* Clone alien values */
#define D_CLONE		   (D_CLONE_RECURSIVE|D_CLONE_REFERENCE|\
			    D_CLONE_NIL|D_CLONE_VALUE|D_CLONE_ALIEN)

#define D_ALIEN		   makeDFlag(20) /* Variable is alien */

#define D_TYPENOWARN	   makeDFlag(21) /* Methods: donot warn */


		/********************************
		*    CHAR_ARRAY, STRING, NAME	*
		********************************/

#include "str.h"			/* string type and friends */
#include "../txt/proto.h"		/* prototypes */

#define LocalString(name, proto, size) \
  string _s_ ## name ## _hdr; \
  void  *_s_ ## name ## _buf = alloca((proto)->b16 ? (size) * sizeof(char16) \
						   : (size) * sizeof(char8)); \
  String name = str_init(&_s_ ## name ## _hdr, (proto), _s_ ## name ## _buf)

#ifndef NO_BUILT_IN_DECL
extern struct name builtin_names[];	/* object-array of built-in's */
#endif
#include "names.ih"			/* #defines for code used names */

#define isName(name)	instanceOfObject(name, ClassName)
#define notName(name)	(!isName(name))
#define equalName(a, b) ((a) == (b))
#define strName(s)	((char *)((Name)(s))->data.s_text8)

#define getAppendName(n, s) \
	((Name) getAppendCharArray((CharArray)(n), (CharArray)(s)))

		/********************************
		*         OBJECT HEADER		*
		********************************/

#define assign(o, s, v)	assignField((Instance) (o), \
				    (Any *) &((o)->s), \
				    (Any) (v))

#define makeFlag(n)		(1L << ((n) - 1))
#define setFlag(obj, mask)	(((Instance)(obj))->flags |= (mask))
#define clearFlag(obj, mask)	(((Instance)(obj))->flags &= ~(mask))
#define onFlag(obj, mask)	(((Instance)(obj))->flags & (mask))
#define offFlag(obj, mask)	(!onFlag(obj, mask))

#define	F_LOCKED		makeFlag(1)
#define	F_CREATING		makeFlag(2)
#define	F_FREED			makeFlag(3)
#define	F_FREEING		makeFlag(4)
#define	F_PROTECTED		makeFlag(5)
#define	F_ANSWER		makeFlag(6)
#define F_INSPECT		makeFlag(7)
#define F_ACTIVE		makeFlag(8)  /* Active object */
#define F_CONSTRAINT		makeFlag(9)  /* has constraints */
#define F_ATTRIBUTE		makeFlag(10) /* has attributes */
#define F_SENDMETHOD		makeFlag(11) /* has send-methods */
#define F_GETMETHOD		makeFlag(12) /* has get-methods */
#define F_HYPER			makeFlag(13) /* has hypers */
#define F_RECOGNISER		makeFlag(14) /* has recognisers */
#define F_ASSOC			makeFlag(15) /* has name-assoc */
#define F_ITFNAME		makeFlag(16) /* Name known to itf table */
#define F_SOLID			makeFlag(17) /* Solid graphical object */
#define F_RESOURCES_OBTAINED	makeFlag(18) /* obtainResourcesObject() */

#define initHeaderObj(obj, cl) \
  { obj->class	      = cl; \
    obj->flags        = F_CREATING; \
    obj->references   = 0L; \
  }

#define classOfObject(obj)	(((Instance)(obj))->class)

#define	setProtectedObj(obj)	setFlag(obj, F_PROTECTED)
#define	clearProtectedObj(obj)	clearFlag(obj, F_PROTECTED)
#define	isProtectedObj(obj)	onFlag(obj, F_PROTECTED)
#define	setCreatingObj(obj)	setFlag(obj, F_CREATING)
#define	clearCreatingObj(obj)	clearFlag(obj, F_CREATING)
#define	isCreatingObj(obj)	onFlag(obj, F_CREATING)
#define	setAnswerObj(obj)	setFlag(obj, F_ANSWER)
#define	clearAnswerObj(obj)	clearFlag(obj, F_ANSWER)
#define	isAnswerObj(obj)	onFlag(obj, F_ANSWER)

#define ONE_CODE_REF		(1L<<20)

#define refsObject(obj)		(((Instance)obj)->references % ONE_CODE_REF)
#define codeRefsObject(obj)	(((Instance)obj)->references / ONE_CODE_REF)
#define noRefsObj(obj)		(((Instance)obj)->references == 0L)
#define addRefObj(obj)		(((Instance)obj)->references++)
#define delRefObj(obj)		(((Instance)obj)->references--)
#define addCodeReference(obj)	(((Instance)obj)->references += ONE_CODE_REF)
#define delCodeReference(obj)	if ( !isFreedObj(obj) ) \
				  (((Instance)obj)->references -= ONE_CODE_REF)
#define lockObj(obj)		setFlag(obj, F_LOCKED)
#define unlockObj(obj)		clearFlag(obj, F_LOCKED)
#define lockedObj(obj)		onFlag(obj, F_LOCKED)
#define setFreedObj(obj)	setFlag(obj, F_FREED)
#define isFreedObj(obj)		onFlag(obj, F_FREED)
#define setFreeingObj(obj)	setFlag(obj, F_FREEING)
#define isFreeingObj(obj)	onFlag(obj, F_FREEING)
#define isVirginObj(o)		(noRefsObj(o) && \
				 !onFlag(o, F_LOCKED|F_PROTECTED|F_ANSWER))
#define freeableObj(o)		if ( isVirginObj(o) ) \
  				  freeObject(o)

#define GcProtect(o, g)		do { \
				addCodeReference(o); \
				g; \
				delCodeReference(o); } while(0)

				

		/********************************
		*            CONSTANTS		*
		********************************/

#define NIL		((Any)(&ConstantNil))
#define DEFAULT		((Any)(&ConstantDefault))
#define ON		(&BoolOn)
#define OFF		(&BoolOff)

#define isOn(val)	((Bool)(val) == ON)
#define isOff(val)	((Bool)(val) == OFF)
#define isBoolean(val)	((Bool)(val) == ON || (Bool)(val) == OFF)

#define isNil(o)	((Constant)(o) == NIL)
#define notNil(o)	((Constant)(o) != NIL)
#define isDefault(o)	((Constant)(o) == DEFAULT)
#define notDefault(o)	((Constant)(o) != DEFAULT)

#define TrueOrFalse(b)	(isOn(b) ? TRUE : FALSE)

#define nonObject(obj)	(((int) (obj) & MASK_MASK) || (Any)(obj) == NULL)
#define isObject(obj)	(!nonObject(obj))

		/********************************
		*         CAREFUL CHECKERS	*
		********************************/

#define isProperObject(obj) \
			(isObject(obj) && isAddress(obj) && \
			 (classOfObject(classOfObject(obj)) == ClassClass || \
			  (isObject(classOfObject(obj)) && \
			   isAddress(classOfObject(obj)) && \
			   instanceOfObject(classOfObject(obj), ClassClass))))
#define isAddress(a)	((ulong)(a) >= allocBase && \
			 (ulong)(a) < allocTop && \
			 !((ulong)(a) & (sizeof(Any)-1)))
#define validPceDatum(x) (isInteger(x) || isProperObject(x))


#ifndef TRUE
#define TRUE		1	/* boolean truth value */
#define FALSE		0	/* boolean false value */
#endif

#define FAIL		0	/* message failed */
#define SUCCEED		1	/* message completed successful */

#define fail		return FAIL
#define succeed		return SUCCEED
#define answer(v)	return (v)

#define DONE(goal)	{ if ( (status) (goal) == SUCCEED ) \
			    succeed; \
			}
#define TRY(goal)	{ if ( (status) (goal) == FAIL ) \
			    fail; \
			}
#define EXISTS(object)	{ if ( isNil(object) ) \
			    fail; \
			}


		/********************************
		*       CLASS STRUCTURES	*
		********************************/

#define OBJECT_HEADER \
  ulong		flags;			/* general flag field */ \
  ulong		references;		/* reference count */ \
  Class		class;			/* Associated class */

#define ABSTRACT_OBJECT

#define ABSTRACT_PROGRAM_OBJECT \
  ulong		dflags;			/* Debugging flags */

#define ABSTRACT_RECOGNISER \
  Bool		active;			/* Does accept events? */

#define ABSTRACT_CODE \
  ABSTRACT_PROGRAM_OBJECT

#define ABSTRACT_FUNCTION  \
  ABSTRACT_CODE
  
#define ABSTRACT_BINARY_EXPRESSION \
  ABSTRACT_FUNCTION \
  Expression	left;			/* Left-hand side */ \
  Expression	right;			/* Right-hand side */

#define ABSTRACT_BINARY_CONDITION \
  ABSTRACT_CODE \
  Expression	left;			/* Left-hand side */ \
  Expression	right;			/* Right-hand side */

#define ABSTRACT_VISUAL

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NewClass(class) privides the structure header for any class  that is a
subclass of class `object'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define NewClass(x) \
  struct x \
  { OBJECT_HEADER \
    ABSTRACT_OBJECT
#define End \
  }

struct instance
{ OBJECT_HEADER				/* non-pce header */
  Any		slots[1];		/* array of slots. */
};

NewClass(object)
End;

NewClass(program_object)
  ABSTRACT_PROGRAM_OBJECT
End;

NewClass(vmi)
  ABSTRACT_PROGRAM_OBJECT
  Name		name;			/* Name of vmi */
End;

NewClass(area)
  Int		x;			/* position and dimension */
  Int		y;
  Int		w;
  Int		h;
End;

NewClass(atable)
  Vector	keys;			/* bool vector stating key fields */
  Vector	names;			/* parameter names of the entries */
  Vector	tables;			/* hash tables */
End;

NewClass(tuple)
  Any		first;			/* first of tuple */
  Any		second;			/* second element of tuple */
End;

#define ABSTRACT_BEHAVIOUR \
  ABSTRACT_PROGRAM_OBJECT \
  Name		name;			/* Name of the behaviour */ \
  Any		context;		/* Object or class I belong too */

#define ABSTRACT_VARIABLE \
  ABSTRACT_BEHAVIOUR \
  Name		group;			/* Conceptual group */ \
  Name		access;			/* whether send/get may be used */ \
  Type		type;			/* type of contents */ \
  Int		offset;			/* offset from base (from 0) */ \
  StringObj	summary;		/* Summary for variable */ \
  Any		init_function;		/* Function to initialise */ \
  Any		alloc_value;		/* Allocate value of variable */

#define ABSTRACT_METHOD \
  ABSTRACT_BEHAVIOUR \
  Name		group;			/* Conceptual group */ \
  Vector 	types;			/* type checking codes */ \
  StringObj	summary;		/* Summary of this method */ \
  SourceLocation source;		/* Location of def in sources */ \
  Code		message;		/* message implementing method */ \
  Func		function;		/* C-function implementing method */ \

NewClass(behaviour)
  ABSTRACT_BEHAVIOUR
End;

NewClass(method)
  ABSTRACT_METHOD
End;

NewClass(send_method)
  ABSTRACT_METHOD
End;

NewClass(get_method)
  ABSTRACT_METHOD
  Type		return_type;		/* Type of returned value */
End;

NewClass(variable)
  ABSTRACT_VARIABLE
End;

NewClass(delegate_variable)
  ABSTRACT_VARIABLE
  Name		wrapper;		/* Wrapper for side-effects */
End;

NewClass(attribute)
  ABSTRACT_PROGRAM_OBJECT
  Any		name;			/* name of the attribute */
  Any		value;			/* value for the attribute */
End;

NewClass(binding)
  Name		name;			/* name of the binding */
  Any		value;			/* Value of the binding */
End;

NewClass(error)
  Name		id;			/* Id of the error */
  Name		format;			/* Format of the error message */
  Name		kind;			/* {message,warning,error} */
  Name		feedback;		/* {inform,print} */
End;

NewClass(chain)
  Int		size;			/* # elements in the chain */
  Cell		head;			/* first element */
  Cell		tail;			/* last element */
  Cell		current;		/* current element */
End;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE

If you  add/delete slots, do  not forget to  change PCE_CLASS_SLOTS in
pce-class.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

NewClass(class)
  ABSTRACT_PROGRAM_OBJECT
  Name		name;			/* name for this class */
  Class		super_class;		/* (abstract) super-class */
  Chain		sub_classes;		/* list of sub-classes */
  Vector	instance_variables;	/* local variables */
  Chain		send_methods;		/* send methods for this class */
  Chain		get_methods;		/* get methods for this class */
  Name		term_functor;		/* functor name of term */
  Vector	term_names;		/* get method to obtain arguments */
  Chain		delegate;		/* variables I delegate to */
  Chain		resources;		/* resources of this class */
  Name		cloneStyle;		/* style of clone method */
  Name		saveStyle;		/* special save method */
  Int		no_created;		/* how many were created */
  Int		no_freed;		/* how many were freed */
  Bool		solid;			/* graphicals: OFF by default */
  Name		selection_style;	/* graphicals: feedback selected */
  Chain		handles;		/* graphicals only: connection pts */
  Int		instance_size;		/* Instance size in bytes */
  Int		slots;			/* # instance variables */
  StringObj	summary;		/* Summary of the class */
  SourceLocation source;		/* Source location */
  Name		rcs_revision;		/* Current rcs-revision of source */
  Name		creator;		/* Created from where? */
  Chain		changed_messages;	/* Trap instance changes */
  Chain		created_messages;	/* Trap instance creation */
  Chain		freed_messages;		/* Trap instance destruction */
  Bool		un_answer;		/* Decide on slot assignment */

  Code		make_class_message;	/* Message to build the class */

  SendMethod	initialise_method;	/* Initialise instance */
  SendMethod	send_catch_all;		/* Catch failed sends */
  GetMethod	get_catch_all;		/* Catch failed gets */
  GetMethod	convert_method;		/* Convert to this type */
  GetMethod	lookup_method;		/* Reusable object-lookup */

  HashTable	send_table;		/* hash-table of send methods */
  HashTable	get_table;		/* hash-table of get methods */
  HashTable	local_table;		/* hash-table of instance variables */
  HashTable	resource_table;		/* hash-table of resources */
  HashTable	instances;		/* hash-table holding the instances */

  Bool		realised;		/* Class has been realised? */
  Bool		has_init_functions;	/* instance variables use functions */

  int		tree_index;		/* Index in depth-first tree */
  int		neighbour_index;	/* Index of my neighbour */

  GetFunc	get_function;		/* `Get' on Code objects */
  SendFunc	send_function;		/* `Send' on Code objects */
  SendFunc	unlink_function;	/* Unlink from environment */
  SendFunc	saveFunction;		/* function handling saveFile */
  SendFunc	loadFunction;		/* function handling loadFile */
  SendFunc	cloneFunction;		/* function to clone object */
  SendFunc	redrawFunction;		/* redraw a graphical */
  SendFunc	changedFunction;	/* Trap instance changes */
  SendFunc	in_event_area_function;	/* Test if event is in area */
  SendFunc	make_class_function;	/* makeClass function pointer */
  VoidFunc	trace_function;		/* Trace execution */
  int		boot;			/* When booting: #pce-slots; else 0 */
End;

NewClass(type)
  ABSTRACT_PROGRAM_OBJECT
  Name		kind;			/* Kind of type */
  Name		fullname;		/* Logical name of the type */
  Name		argument_name;		/* Name of the argument */
  Chain		supers;			/* Super-types */
  Any		context;		/* Context argument for functions */
  Bool		vector;			/* Method: vector of these */
  SendFunc	validate_function;	/* Function to check the type */
  Func		translate_function;	/* Function to convert the type */
End;

NewClass(constraint)
  Any	        from;			/* 'From' object of constraint */
  Any	        to;			/* 'To' object of constraint */
  Relation	relation;		/* relation they have */
  Name		locked;			/* locked fro further messages? */
End;
 
NewClass(date)
  unsigned long	date;			/* Unix view of time */
End;

NewClass(dict)
  Any		browser;		/* browser showing contents */
  Chain		members;		/* list of dict_items */
  HashTable	table;			/* hash table for associative lookup */
End;

NewClass(dictitem)
  ABSTRACT_VISUAL
  Name		key;			/* key (often same as string) */
  Name		label;			/* label displayed in browser */
  Any		object;			/* associated object (often a sheet) */
  Name		style;			/* Display style */
  Int		index;			/* index number (0 upwards) */
  Dict		dict;			/* dict object in which item resides */
End;

NewClass(divide)
  ABSTRACT_BINARY_EXPRESSION
End;

NewClass(equation)
  ABSTRACT_BINARY_CONDITION
End;

NewClass(binary_expression)
  ABSTRACT_BINARY_EXPRESSION
End;

NewClass(binary_condition)
  ABSTRACT_BINARY_CONDITION
End;

NewClass(handle)
  Expression	xPosition;		/* X position of handle */
  Expression	yPosition;		/* Y position of handle */
  Name		kind;			/* Kind of handle */
  Name		name;			/* Logical nam of connection */
End;

NewClass(modifier)
  Name		shift;			/* {up,down,@default} */
  Name		control;		/* {up,down,@default} */
  Name		meta;			/* {up,down,@default} */
End;


#define ABSTRACT_GESTURE \
  ABSTRACT_RECOGNISER \
  Name		button;			/* {left,middle,right} */ \
  Modifier	modifier;		/* shift-control-meta */ \
  Code		condition;		/* Additional conditions */ \
  Name		status;			/* {inactive, ...} */ \
  Any		cursor;			/* Cursor while acitive */
  

NewClass(gesture)
  ABSTRACT_GESTURE
End;

NewClass(handler)
  ABSTRACT_RECOGNISER
  Name		event;			/* type of event handled by handler */
  Code		message;		/* message associated with handler */
  RegionObj	region;			/* region of the receiver */
End;

NewClass(handlergroup)
  ABSTRACT_RECOGNISER
  Chain		members;		/* Handlers of the group */
End;

#define ABSTRACT_HASH_TABLE \
  Bool		refer;			/* Maintain references */ \
  Int		size;			/* # symbols in table */ \
  int		buckets;		/* # buckets in symbol-array */ \
  Symbol	symbols;		/* Symbol-array */

NewClass(hash_table)
  ABSTRACT_HASH_TABLE
End;

NewClass(chain_table)
  ABSTRACT_HASH_TABLE
End;

NewClass(hyper)
  ABSTRACT_PROGRAM_OBJECT
  Any		from;			/* first linked object */
  Any		to;			/* second linked object */
  Name		forward_name;		/* name of the link from <-from */
  Name		backward_name;		/* name of the link from <-to */
End;

NewClass(identity)
  Name		from;			/* selector of 'from' object */
  Name		to;			/* selector of 'to' object */
End;

NewClass(minus)
  ABSTRACT_BINARY_EXPRESSION
End;

#define ABSTRACT_CHAR_ARRAY \
  string	data;			/* the represented data */

NewClass(char_array)
  ABSTRACT_CHAR_ARRAY
End;

NewClass(name)
  ABSTRACT_CHAR_ARRAY
End;

NewClass(string)
  ABSTRACT_CHAR_ARRAY
End;

NewClass(number)
  Int		value;			/* value of the number */
End;

NewClass(pce)
  Bool		debugging;		/* debugging? (watching spy points) */
  Name		trace;			/* Current trace mode */
  Name		last_error;		/* Last error occured */
  Chain		catched_errors;		/* Stack of catched error-id's */
  Bool		catch_error_signals;	/* Catch Unix signals */
  Bool		print_c_stack;		/* Try to dump the C-stack? */

  Chain		exit_messages;		/* Called on exit */
  Sheet		exception_handlers;	/* exception-name --> code */
  Name		home;			/* Home directory */

  Name		version;		/* Version number of PCE */
  Name		machine;		/* Architecture */
  Name		operating_system;	/* Name of operating system*/
  Int		xt_version;		/* Version of Xt library used */
  Int		xt_revision;		/* Revision of Xt library used */
End;

NewClass(plus)
  ABSTRACT_BINARY_EXPRESSION
End;

NewClass(point)
  Int		x;			/* the x- and y-coordinates */
  Int		y;
End;

#define ABSTRACT_HOST \
  Name		language;		/* Prolog, Lisp, ... */ \
  Name		system;			/* host system we are connected to */ \
  Bool		callBack;		/* if @on can be called directly */ \
  Chain		messages;		/* messages waiting in queue */


NewClass(host)
  ABSTRACT_HOST
End;

NewClass(real)
  float	 	value;			/* value of real */
End;

NewClass(recogniser)
  ABSTRACT_RECOGNISER
End;

NewClass(region)
  Expression	x;			/* describe x of region */
  Expression	y;			/* describe y of region */
  Expression	w;			/* describe w of region */
  Expression	h;			/* describe h of region */
End;

NewClass(relation)			/* empty abstract super class */
End;

NewClass(size)
  Int		w;			/* width and height */
  Int		h;
End;

#define ABSTRACT_SHEET \
  Chain		attributes;		/* list of attributes */  

NewClass(sheet)
  ABSTRACT_SHEET
End;

NewClass(source_location)
  Name		file_name;		/* Name of the file */
  Int		line_no;		/* Line of the source location */
End;

NewClass(spatial)
  Equation	xFrom;			/* X reference point of from */
  Equation	yFrom;			/* Y reference point of from */
  Equation	xTo;			/* X reference point of to */
  Equation	yTo;			/* Y reference point of to */
  Equation	wTo;			/* W of to */
  Equation	hTo;			/* H of to */
End;

NewClass(times)
  ABSTRACT_BINARY_EXPRESSION
End;

/* NOTE: context and name share with class method!!! */

NewClass(vector)
  Int		offset;			/* index of element 0 of array */
  Int		size;			/* number of valid entries */
  Any		*elements;		/* array of elements */
End;

NewClass(visual)
  ABSTRACT_VISUAL
End;


struct cell
{ Cell		next;			/* pointer to next cell */
  Any		value;			/* value pointer */
};


struct symbol
{ Any		name;			/* name entry of symbol */
  Any		value;			/* associated value with name */
};

#define ABSTRACT_CONSTANT \
  Name		name;			/* Name of the constant */ \
  StringObj	summary;		/* Summary description */

NewClass(constant)			/* @nil, @default */
  ABSTRACT_CONSTANT
End;

NewClass(bool)				/* @on, @off */
  ABSTRACT_CONSTANT
End;

NewClass(code)
  ABSTRACT_CODE
End;

NewClass(function)
  ABSTRACT_FUNCTION
End;

NewClass(quote_function)
  Function	function;		/* the function quoted */
End;

#define ABSTRACT_AND \
  ABSTRACT_CODE \
  Chain		members;		/* members of the and */

NewClass(and)
  ABSTRACT_AND
End;

NewClass(assignment)
  ABSTRACT_CODE
  Var		var;			/* Variable to bind */
  Any		value;			/* Value (or function) */
  Name		scope;			/* Local or global binding */
End;

NewClass(var)
  ABSTRACT_FUNCTION
  Name		name;			/* Name of the variable */
  Type		type;			/* Type of the variable */
  Any		value;			/* Current value of the variable */
  Any		global_value;		/* Initial or global value */
End;

NewClass(obtain)
  ABSTRACT_FUNCTION
  Any		receiver;		/* receiver of the message */
  Name		selector;		/* selector of the message */
  Vector	arguments;		/* argument vector of the message */
End;

NewClass(create)
  ABSTRACT_FUNCTION
  Class		c_class;		/* Class to create instance from */
  Vector	arguments;		/* Initialisation arguments */
End;

NewClass(message)
  ABSTRACT_CODE
  Any		receiver;		/* receiver of the message */
  Name		selector;		/* selector of the message */
  Vector	arguments;		/* argument vector of the message */
End;

NewClass(block)
  ABSTRACT_AND
  Vector	parameters;		/* formal-parameter-list */
End;

NewClass(if_obj)
  ABSTRACT_CODE
  Code		condition;		/* codition of the `if' */
  Code		then_branch;		/* if condition succeeds */
  Code		else_branch;		/* if condition fails */
End;

NewClass(while_obj)
  ABSTRACT_CODE
  Code		condition;		/* condition of the `while' */
  Code		body;			/* body of the `while' */
End;

NewClass(equal)				/* == */
  ABSTRACT_CODE
  Any		left;
  Any		right;
End;
  
NewClass(non_equal)			/* \== */
  ABSTRACT_CODE
  Any		left;
  Any		right;
End;

NewClass(or)
  ABSTRACT_CODE
  Chain		members;		/* members of the or */
End;

NewClass(not)
  ABSTRACT_CODE
       Code		argument;	/* Its argument */
End;

NewClass(progn)
  ABSTRACT_FUNCTION
  Chain			members;	/* statements */
End;

NewClass(when)
  ABSTRACT_FUNCTION
  Code		condition;		/* codition of the `when' */
  Function	then_branch;		/* value if condition succeeds */
  Function	else_branch;		/* value if condition fails */
End;

		 /*******************************
		 *	     CLASSES		*
		 *******************************/

struct class_definition
{ Name		name;			/* name of the class */
  Name		super;			/* Name of the super-class */
  SendFunc	makefunction;		/* Built the class */
  Class *	global;			/* Pointer to global class var */
  char *	summary;		/* Summary description */
};


		/********************************
		*         FORWARDING		*
		********************************/

#define Arg(i)			(ARG[((i)-1)])
#define	setVar(v, val)		((v)->value = val)

typedef struct
{ Var	variable;
  Any	value;
} var_binding, *VarBinding;

#define BINDINGBLOCKSIZE 8

typedef struct var_environment * VarEnvironment;
typedef struct var_extension * VarExtension;

GLOBAL VarEnvironment varEnvironment;

struct var_environment
{ VarEnvironment parent;
  int		 size;
  var_binding	 bindings[BINDINGBLOCKSIZE];
  VarExtension   extension;
};


struct var_extension
{ int		 allocated;
  var_binding	 bindings[BINDINGBLOCKSIZE];
};


#define withLocalVars(code) \
  { struct var_environment _var_env; \
 \
    _var_env.size = 0; \
    _var_env.parent = varEnvironment; \
    _var_env.extension = NULL; \
    varEnvironment = &_var_env; \
 \
    code; \
 \
    popVarEnvironment(); \
  }


#define withArgs(ac, av, code) \
  withLocalVars({ int _i; \
 \
		  for(_i=0; _i<ac; _i++) \
		    assignVar(Arg(_i+1), av[_i], NAME_local); \
 \
		  code; \
		})


		/********************************
		*        INCREMENTAL GC		*
		********************************/

typedef struct to_cell *ToCell;		/* TemporaryObjectCell */

struct to_cell
{ ToCell	next;			/* Next of the stack */
  Any		value;			/* Object there */
  long		index;			/* Index of the mark */
};

GLOBAL ToCell	AnswerStack;		/* Stack of `answer objects' */
GLOBAL int	deferredUnalloced;	/* # deferred unallocs in ->free */

typedef long	AnswerMark;

#define markAnswerStack(mark)	{(mark) = AnswerStack->index;}
#define rewindAnswerStack(mark, obj) \
	{ if ( (mark) != AnswerStack->index ) \
	    _rewindAnswerStack(&(mark), obj); }


		 /*******************************
		 *	 GLOBAL FUNCTIONS	*
		 *******************************/

#include "../ker/proto.h"
#include "../msg/proto.h"
#include "../adt/proto.h"

status		makeClassC(Class class);
status		initialiseHost(Host h, Name which);
status		makeClassHost(Class class);
Host		HostObject P((void));
int		hostGetc P((void));
void		pceWriteErrorGoal(void);
status		attach_resource(Class cl, char *name, char *type,
				char *def, char *doc);

#if O_CPLUSPLUS
void	initCPlusPlusGlobals(void);
status 	callCPlusPlusProc(void *f, int ac, const Any av[]);
Any	callCPlusPlusFunc(void *f, int ac, const Any av[]);
status 	callCPlusPlusPceMethodProc(Any o, void *f, int ac, const Any av[]);
Any 	callCPlusPlusPceMethodFunc(Any o, void *f, int ac, const Any av[]);
status 	callCPlusPlusMethodProc(Any o, void *f, int ac, const Any av[]);
Any 	callCPlusPlusMethodFunc(Any o, void *f, int ac, const Any av[]);
#endif

		/********************************
		*       GLOBAL VARIABLES	*
		********************************/

GLOBAL int	XPCE_initialised;	/* Is system initialised? */
GLOBAL Pce	PCE;			/* the one and only Pce object */
GLOBAL Host	HOST;			/* the one and only Host object */
GLOBAL SendFunc	DispatchEvents;		/* Dispatch function */
GLOBAL int	changedLevel;		/* Change forwarding levels */
GLOBAL HashTable ErrorTable;		/* @error_database */
GLOBAL ulong	ExecuteCalls;		/* Executed methods */

GLOBAL struct constant ConstantNil;	/* MUST be first! */
GLOBAL struct constant ConstantDefault;
GLOBAL struct bool     BoolOn;
GLOBAL struct bool     BoolOff;

GLOBAL Var	RECEIVER;		/* @receiver */
GLOBAL Var	RECEIVER_CLASS;		/* @receiver_class */
GLOBAL Var	EVENT;			/* @event */
GLOBAL Var	SELECTOR;		/* @selector */
GLOBAL Var	REPORTEE;		/* @reportee */
GLOBAL Var	ARG[FWD_PCE_MAX_ARGS];  /* @arg1 ... */
GLOBAL Var	VarX;			/* x */
GLOBAL Var	VarY;			/* y */
GLOBAL Var	VarW;			/* w */
GLOBAL Var	VarH;			/* h */
GLOBAL Var	VarW2;			/* w2 */
GLOBAL Var	VarH2;			/* h2 */
GLOBAL Var	VarXref;		/* xref */
GLOBAL Var	VarYref;		/* yref */


GLOBAL HashTable classTable;		/* @classes (name --> class) */
GLOBAL HashTable TypeTable;		/* @types (name --> type) */

#define CTE_OK			0	/* CheckType success */
#define CTE_OBTAINER_FAILED	1	/* Obtainer failed */

GLOBAL int	CheckTypeError;		/* Why did checkType fail? */
GLOBAL int	restoreVersion;		/* Version of save file */
GLOBAL FileObj	LoadFile;		/* Current file for <-object */
GLOBAL char    *SaveMagic;		/* Magic string for saved objects */
GLOBAL int	inBoot;			/* is the system in the boot cycle? */
GLOBAL ulong	allocBase;		/* lowest allocated memory */
GLOBAL ulong 	allocTop;		/* highest allocated memory */
GLOBAL int	PCEdebugging;		/* PCE->debugging == ON */
GLOBAL int	PCEdebugBoot;		/* Debug booting phase? */
GLOBAL Chain	PCEdebugSubjects;	/* Names of things we are debugging */
GLOBAL char    *symbolFile;		/* current symbol file */
GLOBAL int	PCEargc;		/* main() argument count */
GLOBAL char   **PCEargv;		/* main() argument vector */
GLOBAL char    *(*getFunctionNameFromAddress)();
					/* stack trace (pce-debug.c) */

GLOBAL HashTable ObjectConstraintTable;	/* object-level constraints */
GLOBAL HashTable ObjectAttributeTable;	/* object-level attributes */
GLOBAL HashTable ObjectSendMethodTable;	/* object-level send_methods */
GLOBAL HashTable ObjectGetMethodTable;	/* object-level get_methods */
GLOBAL HashTable ObjectRecogniserTable;	/* object-level recognisers */
GLOBAL HashTable ObjectHyperTable;	/* object-level hypers */

GLOBAL Name	name_procent_s;		/* "%s" */
GLOBAL Code	qsortCompareCode;	/* used by qsortCompareObjects() */
GLOBAL int	qsortReverse;		/* used by qsortCompareObjects() */

		/********************************
		*        SET ITERATION		*
		********************************/

#define copyArgs(n, f, t) \
  { int _i; for(_i=0; _i < (n); _i++) (t)[_i] = (f)[_i]; }

#define for_chain(ch, val, code) \
  { int _i=0, _size  = valInt(ch->size); \
    Any *_array = alloca(_size * sizeof(Any)); \
    Cell _cell = ch->head; \
	\
    for( ; notNil(_cell); _cell = _cell->next, _i++ ) \
      _array[_i] = _cell->value; \
	\
    for(_i = 0; _i < _size; _i++) \
    { (val) = _array[_i]; \
      if ( nonObject(val) || !isFreedObj(val) ) \
      { code; \
      } \
    } \
  }
	
#define for_vector(v, val, code) \
  { int _iv, _sizev = valInt((v)->size); \
    for(_iv = 0; _iv < _sizev; _iv++) \
    { val = (v)->elements[_iv]; \
      code; \
    } \
  }


#define for_hash_table(ht, var, code) \
  { int _iht, _sizeht = (ht)->buckets; \
    for(_iht = 0; _iht < _sizeht; _iht++) \
    { Symbol var = &(ht)->symbols[_iht]; \
      if ( var->name != NULL ) \
      { code; \
      } \
    } \
  }


#define for_cell(c, ch)	for(c=(ch)->head; notNil(c); c=c->next)
#define for_cell_save(p, q, ch)	if (notNil(p=(ch)->head))\
		for(q=p->next; notNil(p); p=q, q=(isNil(q) ? q : q->next))

		/********************************
		*          EXPRESSIONS		*
		********************************/

#define LEFTHAND(e)	(((BinaryExpression)e)->left)
#define RIGHTHAND(e)	(((BinaryExpression)e)->right)


		/********************************
		*             AREAS		*
		********************************/

/* An area has an orientation defined as the point where the origin
 * of the area is:
 *
 *   northWest	    northEast
 *	-----------------
 *      |		|
 *      |		|
 *	-----------------
 *   southWest	    southEast
 */

#define OrientationArea(w, h)	(w>=0 ? (h>=0 ? NAME_northWest		\
					       : NAME_southWest)	\
				       : (h>=0 ? NAME_northEast		\
					       : NAME_southEast))


#define OrientateArea(x, y, w, h, d) \
  { if ( equalName(d, NAME_northWest) ) \
    { if (w < 0) x += w+1, w = -w; \
      if (h < 0) y += h+1, h = -h; \
    } else if ( equalName(d, NAME_southWest) ) \
    { if (w < 0) x += w+1, w = -w; \
      if (h > 0) y += h-1, h = -h; \
    } else if ( equalName(d, NAME_northEast) ) \
    { if (w > 0) x += w-1, w = -w; \
      if (h < 0) y += h+1, h = -h; \
    } else if ( equalName(d, NAME_southEast) ) \
    { if (w > 0) x += w-1, w = -w; \
      if (h > 0) y += h-1, h = -h; \
    } \
  }


/* Normalise the area given by the C integers x, y, w, h
 * such that w and h are always positive.
 */
#define NormaliseArea(x,y,w,h)	OrientateArea(x,y,w,h,NAME_northWest)

#define DEBUG(subject, goal)	{ if ( PCEdebugging && \
				       memberChain(PCEdebugSubjects, subject) \
								== SUCCEED ) \
				  { goal; \
				  } \
				}

#define DEBUG_BOOT(goal)	{ if ( PCEdebugBoot ) \
				  { goal; \
				  } \
				}

#define O_COUNT 1

#if O_COUNT
#define COUNT(g) {g;}

GLOBAL int hash_cmp_failed;		/* failed comparisons for lookup */
GLOBAL int hash_lookups;		/* Total lookups */
GLOBAL int hash_resizes;		/* # resizes done */
#else
#define COUNT(g)
#endif


		/********************************
		*             SYNTAX		*
		********************************/

#include <h/syntax.h>


		/********************************
		*        INLINE SUPPORT		*
		********************************/


#if USE_PRIMES
#define hashKey(name, buckets) ((((ulong)(name)) >> 2) % (buckets))
#else
#define hashKey(name, buckets) ((((ulong)(name)) >> 2) & ((buckets)-1))
#endif


#include "../ker/inline.c"

