/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <h/interface.h>

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

__pce_export XPCE_Object	XPCE_to_string(char *text);
__pce_export XPCE_Object	XPCE_to_name(char *text);
__pce_export XPCE_Object	XPCE_to_integer(long value);
__pce_export XPCE_Object	XPCE_to_real(float value);
__pce_export XPCE_Object	XPCE_to_pointer(void *value);
__pce_export XPCE_Object	XPCE_to_object(XPCE_Object name);
__pce_export XPCE_Object	XPCE_to_class(XPCE_Object name);
__pce_export XPCE_Object	XPCE_to_type(char *description);


					/* XPCE ---> C */

__pce_export char *		XPCE_charp_of(XPCE_Object string);
__pce_export long		XPCE_int_of(XPCE_Object integer);
__pce_export float		XPCE_float_of(XPCE_Object real);
__pce_export void *		XPCE_pointer_of(XPCE_Object c_pointer);


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

__pce_export status	 XPCE_sendv(XPCE_Object receiver, XPCE_Object selector,
				    int argc, const XPCE_Object argv[]);
__pce_export XPCE_Object XPCE_getv(XPCE_Object receiver, XPCE_Object selector,
				   int argc, const XPCE_Object argv[]);
__pce_export XPCE_Object XPCE_newv(XPCE_Object class, const XPCE_Object name,
				   int argc, const XPCE_Object argv[]);

__pce_export status	 XPCE_send(XPCE_Object receiver, XPCE_Object selector,
				   ...);
__pce_export XPCE_Object XPCE_get(XPCE_Object receiver, XPCE_Object selector,
				  ...);
__pce_export XPCE_Object XPCE_new(XPCE_Object class, const XPCE_Object, ...);

__pce_export status	 XPCE_free(XPCE_Object);


		 /*******************************
		 *	      FUNCALL		*
		 *******************************/

__pce_export XPCE_Object XPCE_callv(XPCE_Procedure function,
				    int argc, const XPCE_Object arg[]);
__pce_export XPCE_Object XPCE_funcallv(XPCE_Function function,
				       int argc, const XPCE_Object arg[]);

__pce_export XPCE_Object XPCE_call(XPCE_Procedure function, ...);
__pce_export XPCE_Object XPCE_funcall(XPCE_Function function, ...);

#if O_CPLUSPLUS
__pce_export XPCE_Object
	XPCE_funcallCPlusPlusMethodv(XPCE_Function function,
				     void *obj,
				     int argc,
				     const XPCE_Object argv[]);
__pce_export XPCE_Object
	XPCE_callCPlusPlusMethodv(XPCE_Procedure function, void *obj,
				  int argc, const XPCE_Object argv[]);
__pce_export XPCE_Object
	XPCE_callCPlusPlusv(XPCE_Procedure f,
			    int argc, const XPCE_Object argv[]);
__pce_export XPCE_Object
	XPCE_funcallCPlusPlusv(XPCE_Function f,
			       int argc, const XPCE_Object argv[]);
#endif /*O_CPLUSPLUS*/

		 /*******************************
		 *	     CLASSES		*
		 *******************************/

__pce_export XPCE_Object XPCE_defclass(XPCE_Object name, XPCE_Object super,
				       XPCE_Object summary,
				       XPCE_Procedure function);

__pce_export XPCE_Object XPCE_defcxxclass(XPCE_Object name, XPCE_Object super,
				       XPCE_Object summary,
				       XPCE_Procedure function);

__pce_export XPCE_Object XPCE_makeclass(XPCE_Object name, XPCE_Object super,
					XPCE_Object summary);


__pce_export XPCE_Variable XPCE_defvar(XPCE_Object class,
				       XPCE_Object name, XPCE_Object group,
				       XPCE_Object summary,
				       XPCE_Object access,
				       XPCE_Object initial,
				       XPCE_Object type);


__pce_export XPCE_status XPCE_defsendmethodv(XPCE_Object class,
					     XPCE_Object name,
					     XPCE_Object group,
					     XPCE_Object summary,
					     XPCE_Procedure implementation,
					     int argc,
					     const XPCE_Object types[]);


__pce_export XPCE_status XPCE_defgetmethodv(XPCE_Object class,
					    XPCE_Object name,
					    XPCE_Object group,
					    XPCE_Object summary,
					    XPCE_Object ret_type,
					    XPCE_Function implementation,
					    int argc,
					    const XPCE_Object types[]);
 
__pce_export XPCE_status XPCE_store(XPCE_Object in,
				    XPCE_Variable var,
				    XPCE_Object value);


__pce_export XPCE_Object XPCE_fetch(XPCE_Object in,
				    XPCE_Variable var);

#if O_CPLUSPLUS
#define CPLUSPLUS_MASK 01L	 /* mask for function-pointers */
#define isCppFunctionPointer(p)	 (((unsigned long)(p)) & CPLUSPLUS_MASK)
#define valCppFunctionPointer(p) ((void*)((unsigned long)(p) & ~CPLUSPLUS_MASK))
#endif

		 /*******************************
		 *	       CHAIN		*
		 *******************************/

__pce_export XPCE_Object XPCE_chain_head(XPCE_Object chain);
__pce_export XPCE_Object XPCE_next_cell(XPCE_Object cell);
__pce_export XPCE_Object XPCE_cell_value(XPCE_Object cell);


		 /*******************************
		 *	       GLOBALS		*
		 *******************************/

__pce_export Any XPCE_on;
__pce_export Any XPCE_off;
__pce_export Any XPCE_nil;
__pce_export Any XPCE_default;
__pce_export Any XPCE_arg1;
__pce_export Any XPCE_arg2;
__pce_export Any XPCE_arg3;
__pce_export Any XPCE_arg4;
__pce_export Any XPCE_arg5;
__pce_export Any XPCE_arg6;
__pce_export Any XPCE_arg7;
__pce_export Any XPCE_arg8;
__pce_export Any XPCE_arg9;
__pce_export Any XPCE_arg10;
__pce_export Any XPCE_event;
__pce_export Any XPCE_receiver;
__pce_export Any XPCE_pce;
__pce_export Any XPCE_display;
