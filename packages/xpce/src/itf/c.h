/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

		 /*******************************
		 *	    BASIC TYPES		*
		 *******************************/

typedef void * 		XPCE_Object;		/* anonymous objects */
typedef XPCE_Object	XPCE_Variable;		/* just an alias */
typedef int    		XPCE_status;		/* send() return */
typedef XPCE_status	(*XPCE_Procedure)();	/* procedure */
typedef XPCE_Object	(*XPCE_Function)();	/* function */
typedef int		XPCE_Access;		/* enum ... */

#define XPCE_ACCESS_none	0
#define XPCE_ACCESS_get		1
#define XPCE_ACCESS_send	2
#define XPCE_ACCESS_both	3

#define XPCE_END		((XPCE_Object)0)


		 /*******************************
		 *	  DATA CONVERSION	*
		 *******************************/

					/* C ---> XPCE */

XPCE_Object	XPCE_to_string(char *text);
XPCE_Object	XPCE_to_name(char *text);
XPCE_Object	XPCE_to_integer(long value);
XPCE_Object	XPCE_to_real(float value);
XPCE_Object	XPCE_to_object(XPCE_Object name);
XPCE_Object	XPCE_to_class(XPCE_Object name);
XPCE_Object	XPCE_to_type(char *description);


					/* XPCE ---> C */

char *		XPCE_charp_of(XPCE_Object string);
long		XPCE_int_of(XPCE_Object integer);
float		XPCE_float_of(XPCE_Object real);


		 /*******************************
		 *	   ACCESS MACROS	*
		 *******************************/

#define Object		XPCE_Object
#define status		XPCE_status
#define Proc		XPCE_Procedure
#define Func		XPCE_Function

#define to_string(s)	XPCE_to_string(s)
#define to_name(s)	XPCE_to_name(s)
#define to_int(i)	XPCE_to_integer(i)
#define to_real(f)	XPCE_to_real(f)
#define to_object(s)	XPCE_to_object(XPCE_to_name(s))
#define to_class(s)	XPCE_to_class(XPCE_to_name(s))

#define charp_of(o)	XPCE_charp_of(o)
#define int_of(o)	XPCE_int_of(o)
#define float_of(o)	XPCE_float_of(o)


		 /*******************************
		 *	        VMI		*
		 *******************************/

status		XPCE_sendv(XPCE_Object receiver, XPCE_Object selector,
			   int argc, const XPCE_Object argv[]);
XPCE_Object	XPCE_getv(XPCE_Object receiver, XPCE_Object selector,
			  int argc, const XPCE_Object argv[]);
XPCE_Object	XPCE_newv(XPCE_Object class, const XPCE_Object name,
			  int argc, const XPCE_Object argv[]);

status		XPCE_send(XPCE_Object receiver, XPCE_Object selector, ...);
XPCE_Object	XPCE_get(XPCE_Object receiver, XPCE_Object selector, ...);
XPCE_Object	XPCE_new(XPCE_Object class, const XPCE_Object, ...);

status		XPCE_free(XPCE_Object);


		 /*******************************
		 *	      FUNCALL		*
		 *******************************/

XPCE_Object	XPCE_callv(XPCE_Procedure function,
			   int argc, const XPCE_Object arg[]);
XPCE_Object	XPCE_funcallv(XPCE_Function function,
			      int argc, const XPCE_Object arg[]);

XPCE_Object	XPCE_call(XPCE_Procedure function, ...);
XPCE_Object	XPCE_funcall(XPCE_Function function, ...);


		 /*******************************
		 *	     CLASSES		*
		 *******************************/

XPCE_Object	XPCE_defclass(XPCE_Object name, XPCE_Object super,
			      XPCE_Object summary, XPCE_Procedure function);

XPCE_Object	XPCE_makeclass(XPCE_Object name, XPCE_Object super,
			       XPCE_Object summary);


XPCE_Variable	XPCE_defvar(XPCE_Object class,
			    XPCE_Object name, XPCE_Object group,
			    XPCE_Object summary,
			    XPCE_Object access,
			    XPCE_Object initial,
			    XPCE_Object type);


XPCE_status	XPCE_defsendmethodv(XPCE_Object class,
				    XPCE_Object name, XPCE_Object group,
				    XPCE_Object summary,
				    XPCE_Procedure implementation,
				    int argc, const XPCE_Object types[]);


XPCE_status	XPCE_defgetmethodv(XPCE_Object class,
				   XPCE_Object name, XPCE_Object group,
				   XPCE_Object summary, XPCE_Object ret_type,
				   XPCE_Function implementation,
				   int argc, const XPCE_Object types[]);
 

XPCE_status	XPCE_store(XPCE_Object in,
			   XPCE_Variable var,
			   XPCE_Object value);


XPCE_Object	XPCE_fetch(XPCE_Object in,
			   XPCE_Variable var);

#if O_CPLUSPLUS
#define CPLUSPLUS_MASK 01L		/* mask for function-pointers */
#define isCppFunctionPointer(p)		(((ulong)(p)) & CPLUSPLUS_MASK)
#define valCppFunctionPointer(p)	((void*)((ulong)(p) & ~CPLUSPLUS_MASK))
#endif
