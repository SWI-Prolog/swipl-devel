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
#define PCE_VERSION "4.9.7, December 1997"
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

#define INT_MASK_SHIFT	1
#define PCE_MAX_INT	((1L << (sizeof(Any)*8 - INT_MASK_SHIFT - 1)) - 1)
#define PCE_MIN_INT	(-(long)PCE_MAX_INT - 1)

typedef void *		AnswerMark;	/* Mark on AnswerStack */

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

char *	getHostSymbolTable(void);

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

#define PCE_NO_POINTER  ((void *) ~0L)

__pce_export int    pceToC(PceObject datum, PceCValue *rval);
__pce_export int    pceToCReference(PceObject datum, PceCValue *rval);
__pce_export char * pceCharArrayToC(PceObject datum);
__pce_export char * pceStringToC(PceObject datum);
__pce_export void * pcePointerToC(PceObject datum);

		/********************************
		*             VMI		*
		********************************/

__pce_export PceObject	pceNew(PceName, PceObject, int, PceObject *);
__pce_export int	pceSend(PceObject, PceName, int, PceObject *);
__pce_export PceObject	pceGet(PceObject, PceName, int, PceObject *);


__pce_export void *	pceResolveSend(PceObject receiver, PceName selector,
				       int *argc, PceObject **types);

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

typedef struct
{ int       (*hostSend)   (PceObject, PceName, int, PceObject *);
  PceObject (*hostGet)    (PceObject, PceName, int, PceObject *);
  int	    (*hostCallProc)(PceObject, PceObject, PceObject, int, PceObject *);
  PceObject (*hostCallFunc)(PceObject, PceObject, PceObject, int, PceObject *);
  int       (*hostQuery)  (int, PceCValue *);
  int	    (*hostActionv)(int, va_list args);
  void	    (*vCprintf)	  (const char *fmt, va_list args);
  int	    (*Cputchar)	  (int);
  void	    (*Cflush)	  (void);
  char *    (*Cgetline)	  (char *line, int size);
  void *    (*malloc)	  (size_t size);
  void *    (*realloc)	  (void *ptr, size_t size);
  void      (*free)	  (void *ptr);
  void *    pad13;			/* future enhancements */
  void *    pad14;
  void *    pad15;
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
__pce_export void	pceRedraw(void);
/* XtAppContext pceXtAppContext(XtAppContext); */


		/********************************
		*       DEBUGGER INTERFACE	*
		********************************/

#define PCE_EXEC_SERVICE	0	/* `service' call-back  */
#define PCE_EXEC_USER		1	/* application call-back */

__pce_export int	pceExecuteMode(void);
__pce_export void	pceReset(void);
__pce_export void	pceTrace(int); /* 1: trace; 0: notrace */
__pce_export void	pceTraceBack(int depth); /* dump message stack */
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
__pce_export void	pceFree(void);
#endif

__pce_export void *	pceAlloc(int bytes);
__pce_export void	pceUnAlloc(int bytes, void *p);


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

#define hostCallProc(h, r, s, ac, av) \
	(*TheCallbackFunctions.hostCallProc)((h), (r), (s), (ac), (av))
#define hostCallFunc(h, r, s, ac, av) \
	(*TheCallbackFunctions.hostCallFunc)((h), (r), (s), (ac), (av))
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
