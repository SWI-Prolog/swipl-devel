/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef _XPCE_INTERFACE_H_INCLUDED
#define _XPCE_INTERFACE_H_INCLUDED


		/********************************
		*            VERSIONS		*
		********************************/

#ifndef PCE_VERSION
#define PCE_VERSION "5.0.1, February 1999"
#endif

#ifndef OS_VERSION
#define OS_VERSION "unknown"
#endif

#ifndef MACHINE
#define MACHINE "unknown"
#endif


		 /*******************************
		 *	     WIN32 DLL		*
		 *******************************/

#ifndef __pce_export
#if defined(PCE_INCLUDED) && defined(__WINDOWS__)
#define __pce_export _declspec(dllexport)
#else
#define __pce_export extern
#endif /*PCE_INCLUDED*/
#endif /*__pce_export*/


		/********************************
		*           PRIMITIVES		*
		********************************/

#ifndef PCE_INCLUDED
typedef void *		PceObject;	/* PCE's view of an object */
typedef void *		PceName;	/* PCE's view of a name */
typedef void *		PceMethod;	/* PCE's view of a method */
typedef void *		PceType;	/* PCE's view of a type */
typedef void *		PceClass;	/* PCE's view of a class */
typedef void *		PceHostData;	/* PCE's view of a host data handle */

#define INT_MASK_SHIFT	1
#define PCE_MAX_INT	((long)((1L<<(sizeof(Any)*8 - INT_MASK_SHIFT-1))-1))
#define PCE_MIN_INT	(-(PCE_MAX_INT-1))

typedef long		AnswerMark;	/* Mark on AnswerStack */

__pce_export void _markAnswerStack(AnswerMark *);
__pce_export void _rewindAnswerStack(AnswerMark *, PceObject);

#define markAnswerStack(mark)	_markAnswerStack(&(mark))
#define rewindAnswerStack(mark, obj) _rewindAnswerStack(&(mark), obj)

#ifdef __GNUC__
#define ArgVector(name, size)	PceObject name[size]
#else
#define ArgVector(name, size)	PceObject *name = \
				  (PceObject *) alloca(size*sizeof(PceObject))
#endif
typedef struct pceITFSymbol    *PceITFSymbol;

#else /*PCE_INCLUDED*/

#define PceObject	Any
#define PceName		Name
#define PceMethod	Method
#define PceType		Type
#define PceClass	Class
#define PceHostData	HostData

#define PCE_MAX_HOSTHANDLES 10

GLOBAL HashTable	ObjectToITFTable;
GLOBAL HashTable	NameToITFTable;
GLOBAL HashTable        HandleToITFTables[PCE_MAX_HOSTHANDLES];
#endif /*PCE_INCLUDED*/




		/********************************
		*        INTERFACE TABLE	*
		********************************/

typedef void *	hostHandle;		/* Handle of host-language */

struct pceITFSymbol
{ PceObject	object;			/* global object associated */
  PceName	name;			/* Pce name associated */
#if __GNUC__ && !__STRICT_ANSI__
  hostHandle	handle[0];		/* Alien handles to operate on */
#else
  hostHandle	handle[1];		/* Alien handles to operate on */
#endif
};


__pce_export PceITFSymbol pceLookupHandle(int, hostHandle);
__pce_export void	  pceRegisterName(int, hostHandle, PceName);
__pce_export void	  pceRegisterAssoc(int, hostHandle, PceObject);
__pce_export int	  pceHostHandles(int);

__pce_export PceITFSymbol getITFSymbolName(PceName);

		/********************************
		*           CONSTANTS		*
		********************************/


#define PCE_FAIL	0
#define PCE_SUCCEED	1


		/********************************
		*          PCE --> C		*
		********************************/

typedef union
{ char		character;		/* Value is a C char */
  long		integer;		/* Value of PCE int or reference */
  double 	real;			/* Value of PCE real */
  char *	string;			/* Value is a C char * */
  void *	pointer;		/* Anonymous pointer */
  PceITFSymbol	itf_symbol;		/* Interface symbol */
} PceCValue;


#define PCE_INTEGER	1
#define PCE_NAME	2
#define PCE_REFERENCE	3
#define PCE_ASSOC	4 
#define PCE_REAL	5
#define PCE_HOSTDATA	6

#define PCE_NO_POINTER  ((void *) ~0L)

#define PCE_ANSWER	0x1		/* CtoHostData() flags */

__pce_export int    pceToC(PceObject datum, PceCValue *rval);
__pce_export int    pceToCReference(PceObject datum, PceCValue *rval);
__pce_export char * pceCharArrayToC(PceObject datum);
__pce_export char * pceStringToC(PceObject datum);
__pce_export void * pcePointerToC(PceObject datum);
__pce_export PceHostData  CtoHostData(PceClass class, void *handle, int flags);
__pce_export void *    getHostDataHandle(PceHostData hd);
__pce_export void  setHostDataHandle(PceHostData hd, void *handle);
__pce_export int   freeHostData(PceHostData hd);


		/********************************
		*             VMI		*
		********************************/

__pce_export PceObject	pceNew (PceName classname,
				PceObject assoc,
				int argc, PceObject *argv);
__pce_export int	pceSend(PceObject receiver,
				PceName classname,
				PceName selector,
				int argc, PceObject * argv);
__pce_export PceObject	pceGet (PceObject receiver,
				PceName classname,
				PceName selector,
				int argc, PceObject * argv);

__pce_export PceClass	nameToExistingClass(PceName name);

		 /*******************************
		 *     DIRECT GOAL-INTERFACE	*
		 *******************************/

#define PCE_GOAL_DIRECT_ARGS 4		/* # in-line optimised arguments */

					/* Goal errno values */
#define PCE_ERR_OK			0
#define PCE_ERR_NO_BEHAVIOUR		1
#define PCE_ERR_ARGTYPE			2
#define PCE_ERR_TOO_MANY_ARGS		3
#define PCE_ERR_ANONARG_AFTER_NAMED	4
#define PCE_ERR_NO_NAMED_ARGUMENT	5
#define PCE_ERR_MISSING_ARGUMENT	6
#define PCE_ERR_CODE_AS_GETMETHOD	7
#define PCE_ERR_PERMISSION		8
#define PCE_ERR_FUNCTION_FAILED		9
#define PCE_ERR_ERROR			10   /* id, vector(...args...) */
#define PCE_ERR_RETTYPE			11
#define PCE_ERR_USER_BASE		1000 /* base for user (host) errors */

#define	PCE_GF_CATCHALL			0x001 /* <->catch_all implemeted */
#define PCE_GF_SEND			0x002 /* a send operation */
#define PCE_GF_GET			0x004 /* a get operation */
#define PCE_GF_EXCEPTION		0x008 /* an error occurred */
#define PCE_GF_HOST			0x010 /* Implemented by the host */
#define PCE_GF_ALLOCATED		0x020 /* g->argv is allocated */
#define PCE_GF_VA_ALLOCATED		0x040 /* g->va_argv is allocated */
#define PCE_GF_CATCH			0x080 /* Catch exceptions */
#define PCE_GF_THROW			0x100 /* Ok, here is one :-) */
#define PCE_GF_HOSTARGS			0x200 /* Arguments are not in argv */

typedef struct _pce_goal
{ PceObject	implementation;		/* implementation of the method */
  PceObject	receiver;		/* Receiver */
  PceClass	class;			/* Class used for resolve action */

  struct _pce_goal *parent;		/* Parent goal (if any) */

  int		argc;			/* # arguments */
  PceObject    *argv;			/* argument vector */
  int		va_argc;		/* Vararg # arguments (-1: none) */
  PceObject    *va_argv;		/* Vararg argument vector */

  int		argn;			/* Current argument */
  PceName	selector;
  PceType      *types;
  int		flags;
  int		errcode;		/* Error code */
  void *	host_closure;		/* Arbitrary host-handle */
  PceObject	errc1;			/* Error context #1 */
  PceObject	errc2;			/* Error context #2 */
  PceObject	rval;			/* get-goal return-value */
  PceType	va_type;		/* type for varargs */
  PceType	return_type;		/* return-type (get-methods) */
  int		va_allocated;
  PceObject	_av[PCE_GOAL_DIRECT_ARGS];
} pce_goal, *PceGoal;

__pce_export int pcePushArgument(PceGoal g, PceObject argument);
__pce_export int pceResolveImplementation(PceGoal goal);
__pce_export void pceInitArgumentsGoal(PceGoal goal);
__pce_export void pceVaAddArgGoal(PceGoal goal, PceObject value);
__pce_export int  pcePushNamedArgument(PceGoal goal,
				       PceName name, PceObject arg);
__pce_export int  pceGetArgumentTypeGoal(PceGoal goal, PceName name,
					 PceType *type, int *i);
__pce_export int  pceSetErrorGoal(PceGoal goal, int err, ...);
__pce_export int  pceExecuteGoal(PceGoal goal);
__pce_export void pceFreeGoal(PceGoal goal);
__pce_export void pcePushGoal(PceGoal goal);
__pce_export void pceTraceBack(PceGoal from, int depth);
__pce_export void pceReportErrorGoal(PceGoal goal);
__pce_export void pcePrintEnterGoal(PceGoal goal);
__pce_export void pcePrintReturnGoal(PceGoal goal, int rval);
					/* Type logic */
__pce_export int	pceIncludesType(PceType t, PceType super);
__pce_export int	pceIncludesHostDataType(PceType t, PceClass hdclass);
__pce_export PceObject	pceCheckType(PceGoal g, PceType t, PceObject in);
__pce_export int	pceCheckIntType(PceType t, long val);
__pce_export int	pceCheckNameType(PceType t, const char *s);
__pce_export int	pceCheckFloatType(PceType t, double f);

__pce_export int	pceEnumElements(PceObject collection,
					int (*enumfunc)(PceObject element,
							void *closure),
					void *closure);

		/********************************
		*          PCE CALLING C	*
		********************************/

#define HOST_QUERY	0		/* execute interactive query */
#define HOST_TRACE	1		/* start debugger on host */
#define HOST_BACKTRACE	2		/* give stack trace on host */
#define HOST_HALT	3		/* exit host */
#define HOST_BREAK	4		/* start interactive session */
#define HOST_SYMBOLFILE	5		/* query symbol table */
#define HOST_ABORT	6		/* abort, return to toplevel */
#define HOST_SIGNAL	7		/* signal() replacement */
#define HOST_RECOVER_FROM_FATAL_ERROR 9 /* Error: don't return */
#define HOST_ATEXIT	10		/* Callback on exit */
#define HOST_CONSOLE	11		/* Win32: query HWND of console */
#define HOST_CHECK_INTERRUPT 12		/* Win32: periodic check for ^C */

#define PCE_METHOD_INFO_HANDLE_ONLY	0x01
#define PCE_METHOD_INFO_TRACE_ENTER	0x02
#define PCE_METHOD_INFO_TRACE_EXIT	0x04
#define PCE_METHOD_INFO_TRACE_FAIL	0x08
#define PCE_METHOD_INFO_TRACE		0x0E
#define PCE_METHOD_INFO_BREAK_ENTER	0x10
#define PCE_METHOD_INFO_BREAK_EXIT	0x20
#define PCE_METHOD_INFO_BREAK_FAIL	0x40
#define PCE_METHOD_INFO_BREAK		0x70

typedef struct
{ void *	handle;			/* anonymous data-handle */
  PceName	name;			/* name of the method */
  PceName	context;		/* Name of the context-class */
  int		flags;			/* Additional info */
  int		argc;			/* Argument-count */
  PceType      *types;			/* Type-vector */
} pce_method_info;

__pce_export int pceGetMethodInfo(PceMethod m, pce_method_info *info);

typedef struct
{ int       (*hostSend)     (PceObject, PceName, int, PceObject *);
  PceObject (*hostGet)      (PceObject, PceName, int, PceObject *);
  int	    (*hostCall)	    (PceGoal goal);
  int       (*hostQuery)    (int, PceCValue *);
  int	    (*hostActionv)  (int, va_list args);
  void	    (*vCprintf)	    (const char *fmt, va_list args);
  int	    (*Cputchar)	    (int);
  void	    (*Cflush)	    (void);
  char *    (*Cgetline)	    (char *line, int size);
  void *    (*malloc)	    (size_t size);
  void *    (*realloc)	    (void *ptr, size_t size);
  void      (*free)	    (void *ptr);
#ifdef SIO_MAGIC			/* defined from <SWI-Stream.h> */
  IOSTREAM* (*rc_open)	    (const char *name, const char *class,
			     const char *mode);
#else
  void *    rc_open;
#endif
  PceObject (*getHostContext)(PceObject host);
  PceObject (*setHostContext)(PceObject context);
  PceObject (*translate)     (PceObject handle, PceObject type);
  int       (*writeGoalArgs) (PceGoal g);
  void *    pad17;
  void *    pad18;
  void *    pad19;
  void *    pad20;
  void *    pad21;
  void *    pad22;
  void *    pad23;
} pce_callback_functions;

__pce_export void pceRegisterCallbacks(pce_callback_functions *funcs);


		/********************************
		*         INITIALISATION	*
		********************************/

__pce_export int pceInitialise(int handles, const char *home,
			       int argc, char **argv);


		/********************************
		*           C --> PCE		*
		********************************/

__pce_export PceObject	cToPceName(const char *);
__pce_export PceObject	cToPceInteger(long);
__pce_export PceObject	cToPceReal(double);
__pce_export PceObject	cToPceString(PceName assoc, char *, int translate);
__pce_export PceObject	cToPceAssoc(const char *);
__pce_export PceObject	cToPceReference(unsigned long);
__pce_export PceObject	cToPcePointer(void *ptr);
__pce_export int	pceLock(PceObject);

__pce_export PceObject	cToPceTmpCharArray(const char *text);
__pce_export void	donePceTmpCharArray(PceObject);

__pce_export int	pceExistsReference(unsigned long);
__pce_export char *	pcePPReference(PceObject ref);
__pce_export int	pceExistsAssoc(PceName assoc);
__pce_export PceObject  pceObjectFromName(PceName assoc);

__pce_export int	pceInstanceOf(PceObject obj, PceObject class);

		/********************************
		*            EVENTS		*
		********************************/

#define PCE_DISPATCH_INPUT	(0)
#define PCE_DISPATCH_TIMEOUT	(1)

__pce_export int	pceDispatch(int fd, int msecs);
__pce_export void	pceRedraw(int);
/* XtAppContext pceXtAppContext(XtAppContext); */


		/********************************
		*       DEBUGGER INTERFACE	*
		********************************/

#define PCE_EXEC_SERVICE	0	/* `service' call-back  */
#define PCE_EXEC_USER		1	/* application call-back */

__pce_export int	pceExecuteMode(void);
__pce_export void	pceReset(void);
__pce_export void	pceTrace(int); /* 1: trace; 0: notrace */
__pce_export void	pceWriteCurrentGoal(void); /* dump top stack */


					/* XPCE console interaction */
__pce_export void	Cprintf(const char *fmt, ...);
__pce_export void	Cvprintf(const char *fmt, va_list args);
__pce_export int	Cputchar(int chr);
__pce_export void	Cflush(void);
__pce_export char *	Cgetline(char *line, int size);


		 /*******************************
		 *	MEMORY ALLOCATION	*
		 *******************************/

#ifndef PCE_INCLUDED
__pce_export void *	pceMalloc(int size);
__pce_export void *	pceRealloc(void *ptr, int size);
__pce_export void	pceFree(void *ptr);
#endif

__pce_export void *	pceAlloc(int bytes);
__pce_export void	pceUnAlloc(int bytes, void *p);


		 /*******************************
		 *	     OBJECTS		*
		 *******************************/


__pce_export PceClass	pceClassOfObject(PceObject obj);
__pce_export int	pceReferencesOfObject(PceObject obj);
__pce_export int	pceFreeObject(PceObject obj);

		 /*******************************
		 *	       METHODS		*
		 *******************************/

__pce_export void	pceSendMethod(PceClass class,
				      const char *name,
				      const char *group,
				      int argc,
				      ...);
__pce_export void	pceGetMethod(PceClass class,
				     const char *name,
				     const char *group,
				     const char *rtype,
				     int argc,
				     ...);

		 /*******************************
		 *	   STREAM INTERFACE	*
		 *******************************/

#define PCE_OPEN_MAX	64		/* statically allocated max open */

__pce_export int	pceOpen(PceObject obj, int flags);
__pce_export int	pceClose(int handle);
__pce_export int	pceWrite(int handle, const char *buf, int size);
__pce_export int	pceRead(int handle, char *buf, int size);
__pce_export long	pceSeek(int handle, long offset, int whence);
__pce_export const char *pceOsError();

					/* flags for pceOpen() (or'ed) */
#define PCE_RDONLY	0x1
#define	PCE_WRONLY	0x2
#define	PCE_RDWR	0x3		/* == PCE_RDONLY|PCE_WRONLY */
#define	PCE_APPEND	0x4
#define	PCE_TRUNC	0x8

#define PCE_SEEK_SET	0
#define PCE_SEEK_CUR	1
#define PCE_SEEK_END	2

		 /*******************************
		 *	    CALL-BACK		*
		 *******************************/

#ifdef PCE_INCLUDED
extern pce_callback_functions TheCallbackFunctions;

#define hostCall(info) \
	(*TheCallbackFunctions.hostCall)(info)
#define pceMalloc(n) \
	(*TheCallbackFunctions.malloc)((n))
#define pceRealloc(ptr, n) \
	(*TheCallbackFunctions.realloc)((ptr), (n))
#define pceFree(ptr) \
	(*TheCallbackFunctions.free)((ptr))

int		hostSend(PceObject host, PceName selector,
			 int argc, PceObject argv []);
PceObject	hostGet(PceObject host, PceName selector,
			int argc, PceObject argv []);
int		hostQuery(int what, PceCValue *value);
int		hostAction(int what, ...);
#endif

#endif /*_XPCE_INTERFACE_H_INCLUDED*/
