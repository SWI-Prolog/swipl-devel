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
#define PCE_VERSION "4.8.15, Dec 1995"
#endif

#ifndef OS_VERSION
#define OS_VERSION "unknown"
#endif

#ifndef MACHINE
#define MACHINE "unknown"
#endif

		/********************************
		*          PROTOTYPES		*
		********************************/

#ifndef __P
#if defined(__STDC__) || defined(__WIN32__) /* Prototype handling */
# define __P(s) s
#else
# define __P(s) ()
#endif
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

#define INT_MASK_SHIFT	2
#define PCE_MAX_INT	((1 << (32 - INT_MASK_SHIFT - 1)) - 1)
#define PCE_MIN_INT	(-PCE_MAX_INT - 1)

typedef void *		AnswerMark;	/* Mark on AnswerStack */

__pce_export void _markAnswerStack __P((AnswerMark *));
__pce_export void _rewindAnswerStack __P((AnswerMark *, PceObject));

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

char *	getHostSymbolTable __P((void));

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


__pce_export PceITFSymbol pceLookupHandle __P((int, hostHandle));
__pce_export void	  pceRegisterName __P((int, hostHandle, PceName));
__pce_export void	  pceRegisterAssoc __P((int, hostHandle, PceObject));
__pce_export int	  pceHostHandles __P((int));

__pce_export PceITFSymbol getITFSymbolName __P((PceName));

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
  float 	real;			/* Value of PCE real */
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

__pce_export int    pceToC __P((PceObject datum, PceCValue *rval));
__pce_export int    pceToCReference __P((PceObject datum, PceCValue *rval));
__pce_export char * pceCharArrayToC __P((PceObject datum));
__pce_export char * pceStringToC __P((PceObject datum));
__pce_export void * pcePointerToC __P((PceObject datum));

		/********************************
		*             VMI		*
		********************************/

__pce_export PceObject	pceNew __P((PceName, PceObject, int, PceObject *));
__pce_export int	pceSend __P((PceObject, PceName, int, PceObject *));
__pce_export PceObject	pceGet __P((PceObject, PceName, int, PceObject *));


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
{ int       (*hostSend)    __P((PceObject, PceName, int, PceObject *));
  PceObject (*hostGet)     __P((PceObject, PceName, int, PceObject *));
  int	    (*hostCallProc)__P((PceObject, PceObject, PceObject, int, PceObject *));
  PceObject (*hostCallFunc)__P((PceObject, PceObject, PceObject, int, PceObject *));
  int       (*hostQuery)   __P((int, PceCValue *));
  int	    (*hostActionv) __P((int, va_list args));
  void	    (*vCprintf)	   __P((const char *fmt, va_list args));
  int	    (*Cputchar)	   __P((int));
  void	    (*Cflush)	   __P((void));
  char *    (*Cgetline)	   __P((char *line, int size));
  void *    (*malloc)	   __P((unsigned int size));
  void *    (*realloc)	   __P((void *ptr, unsigned int size));
  void      (*free)	   __P((void *ptr));
  void *    pad13;			/* future enhancements */
  void *    pad14;
  void *    pad15;
} pce_callback_functions;

__pce_export void pceRegisterCallbacks __P((pce_callback_functions *funcs));


		/********************************
		*         INITIALISATION	*
		********************************/

__pce_export int pceInitialise __P((int handles, const char *home,
				    int argc, char **argv));


		/********************************
		*           C --> PCE		*
		********************************/

__pce_export PceObject	cToPceName __P((const char *));
__pce_export PceObject	cToPceInteger __P((long));
__pce_export PceObject	cToPceReal __P((double));
__pce_export PceObject	cToPceString __P((PceName assoc, char *));
__pce_export PceObject	cToPceAssoc __P((const char *));
__pce_export PceObject	cToPceReference __P((unsigned long));
__pce_export PceObject	cToPcePointer __P((void *ptr));
__pce_export int	pceLock __P((PceObject));

__pce_export PceObject	cToPceTmpCharArray __P((const char *text));
__pce_export void	donePceTmpCharArray __P((PceObject));

__pce_export int	pceExistsReference __P((unsigned long));
__pce_export int	pceExistsAssoc __P((PceName assoc));

__pce_export int	pceInstanceOf __P((PceObject obj, PceObject class));

		/********************************
		*            EVENTS		*
		********************************/

#define PCE_DISPATCH_INPUT	(0)
#define PCE_DISPATCH_TIMEOUT	(1)

__pce_export int	pceDispatch __P((int fd, int msecs));
__pce_export void	pceRedraw __P((void));
/* XtAppContext pceXtAppContext __P((XtAppContext)); */


		/********************************
		*       DEBUGGER INTERFACE	*
		********************************/

__pce_export void	pceReset __P((void));
__pce_export void	pceTrace __P((int)); /* 1: trace; 0: notrace */
__pce_export void	pceTraceBack __P((int depth)); /* dump message stack */
__pce_export void	pceWriteCurrentGoal __P((void)); /* dump top stack */

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
