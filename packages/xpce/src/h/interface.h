/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

		/********************************
		*            VERSIONS		*
		********************************/

#ifndef PCE_VERSION
#define PCE_VERSION "4.8.0, April 1994"
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

#ifndef P
#ifdef __STDC__				/* Prototype handling */
# define P(s) s
#else
# define P(s) ()
#endif
#endif

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

void	  _markAnswerStack P((AnswerMark *));
void	  _rewindAnswerStack P((AnswerMark *, PceObject));

#define markAnswerStack(mark)	_markAnswerStack(&(mark))
#define rewindAnswerStack(mark, obj) _rewindAnswerStack(&(mark), obj)

#ifdef __GNUC__
#define ArgVector(name, size)	PceObject name[size]
#else
#define ArgVector(name, size)	PceObject *name = \
				  (PceObject *) alloca(size*sizeof(PceObject))
#endif
typedef struct pceITFSymbol    *PceITFSymbol;
#else					/* when included in PCE */
#define PceObject	Any
#define PceName		Name

char *	getHostSymbolTable P((void));

#define PCE_MAX_HOSTHANDLES 10

GLOBAL HashTable	ObjectToITFTable;
GLOBAL HashTable	NameToITFTable;
GLOBAL HashTable        HandleToITFTables[PCE_MAX_HOSTHANDLES];
#endif

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


PceITFSymbol	pceLookupHandle P((int, hostHandle));
void 		pceRegisterName P((int, hostHandle, PceName));
void		pceRegisterAssoc P((int, hostHandle, PceObject));
int		pceHostHandles P((int));

PceITFSymbol	getITFSymbolName P((PceName));

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
  PceITFSymbol	itf_symbol;		/* Interface symbol */
} PceCValue;


#define PCE_INTEGER	1
#define PCE_NAME	2
#define PCE_REFERENCE	3
#define PCE_ASSOC	4 
#define PCE_REAL	5

int		pceToC P((PceObject datum, PceCValue *rval));
int		pceToCReference P((PceObject datum, PceCValue *rval));
char *		pceCharArrayToC P((PceObject datum));
char *		pceStringToC P((PceObject datum));


		/********************************
		*             VMI		*
		********************************/

PceObject	pceNew P((char *, PceObject, int, PceObject *));
int		pceSend P((PceObject, PceName, int, PceObject *));
PceObject	pceGet P((PceObject, PceName, int, PceObject *));


		/********************************
		*          PCE CALLING C	*
		********************************/

int		hostSend P((PceObject, PceName, int, PceObject *));
PceObject	hostGet P((PceObject, PceName, int, PceObject *));

#define HOST_QUERY	0	/* execute interactive query on host */
#define HOST_TRACE	1	/* start debugger on host */
#define HOST_BACKTRACE	2	/* give stack trace on host */
#define HOST_HALT	3	/* exit host */
#define HOST_BREAK	4	/* start interactive session with host */
#define HOST_SYMBOLFILE	5	/* query symbol table */
#define HOST_ABORT	6	/* abort current query, return to toplevel */
#define HOST_SIGNAL	7	/* signal() replacement */
#define HOST_GETC	8	/* get a character from the host */
#define HOST_RECOVER_FROM_FATAL_ERROR 9 /* Error: don't return */
#define HOST_WRITE	10	/* Write a char * on the terminal */
#define HOST_FLUSH	11	/* Flush terminal */
#define HOST_ONEXIT	12	/* Callback on exit */

int		hostQuery P((int, PceCValue *));
int		hostAction P((int, ...));

		/********************************
		*         INITIALISATION	*
		********************************/

int		pceInitialise P((int handles, int argc, char **argv));
int		pceReInitialise P((int argc, char **argv));


		/********************************
		*           C --> PCE		*
		********************************/

PceObject	cToPceName P((char *));
PceObject	cToPceInteger P((long));
PceObject	cToPceReal P((double));
PceObject	cToPceString P((char *, char *));
PceObject	cToPceAssoc P((char *));
PceObject	cToPceReference P((unsigned long));
int		pceLock P((PceObject));

PceObject	cToPceTmpCharArray P((char *text));
void		donePceTmpCharArray P((PceObject));

int		pceExistsReference P((long));
int		pceExistsAssoc P((char *));

int		pceInstanceOf P((PceObject obj, PceObject class));

		/********************************
		*            EVENTS		*
		********************************/

#define PCE_DISPATCH_INPUT	(0)
#define PCE_DISPATCH_TIMEOUT	(1)

int		pceDispatch P((int fd, int msecs));
void		pceRedraw P((void));
/* XtAppContext pceXtAppContext P((XtAppContext)); */


		/********************************
		*       DEBUGGER INTERFACE	*
		********************************/

void		pceReset P((void));
void		pceTrace P((int));		/* 1: trace; 0: notrace */
void		pcePrintStack P((int depth));	/* dump C-stack */
void		pceTraceBack P((int depth));    /* dump PCE message stack */
void		pceWriteCurrentGoal P((void));	/* dump top of PCE stack */


