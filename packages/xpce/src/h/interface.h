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

#ifndef __P
#ifdef __STDC__				/* Prototype handling */
# define __P(s) s
#else
# define __P(s) ()
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

void	  _markAnswerStack __P((AnswerMark *));
void	  _rewindAnswerStack __P((AnswerMark *, PceObject));

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

char *	getHostSymbolTable __P((void));

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


PceITFSymbol	pceLookupHandle __P((int, hostHandle));
void 		pceRegisterName __P((int, hostHandle, PceName));
void		pceRegisterAssoc __P((int, hostHandle, PceObject));
int		pceHostHandles __P((int));

PceITFSymbol	getITFSymbolName __P((PceName));

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

int		pceToC __P((PceObject datum, PceCValue *rval));
int		pceToCReference __P((PceObject datum, PceCValue *rval));
char *		pceCharArrayToC __P((PceObject datum));
char *		pceStringToC __P((PceObject datum));


		/********************************
		*             VMI		*
		********************************/

PceObject	pceNew __P((char *, PceObject, int, PceObject *));
int		pceSend __P((PceObject, PceName, int, PceObject *));
PceObject	pceGet __P((PceObject, PceName, int, PceObject *));


		/********************************
		*          PCE CALLING C	*
		********************************/

int		hostSend __P((PceObject, PceName, int, PceObject *));
PceObject	hostGet __P((PceObject, PceName, int, PceObject *));

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

int		hostQuery __P((int, PceCValue *));
int		hostAction __P((int, ...));

		/********************************
		*         INITIALISATION	*
		********************************/

int		pceInitialise __P((int handles, int argc, char **argv));
int		pceReInitialise __P((int argc, char **argv));


		/********************************
		*           C --> PCE		*
		********************************/

PceObject	cToPceName __P((char *));
PceObject	cToPceInteger __P((long));
PceObject	cToPceReal __P((double));
PceObject	cToPceString __P((char *, char *));
PceObject	cToPceAssoc __P((char *));
PceObject	cToPceReference __P((unsigned long));
int		pceLock __P((PceObject));

PceObject	cToPceTmpCharArray __P((char *text));
void		donePceTmpCharArray __P((PceObject));

int		pceExistsReference __P((long));
int		pceExistsAssoc __P((char *));

int		pceInstanceOf __P((PceObject obj, PceObject class));

		/********************************
		*            EVENTS		*
		********************************/

#define PCE_DISPATCH_INPUT	(0)
#define PCE_DISPATCH_TIMEOUT	(1)

int		pceDispatch __P((int fd, int msecs));
void		pceRedraw __P((void));
/* XtAppContext pceXtAppContext __P((XtAppContext)); */


		/********************************
		*       DEBUGGER INTERFACE	*
		********************************/

void		pceReset __P((void));
void		pceTrace __P((int));		/* 1: trace; 0: notrace */
void		pcePrintStack __P((int depth));	/* dump C-stack */
void		pceTraceBack __P((int depth)); /* dump PCE message stack */
void		pceWriteCurrentGoal __P((void)); /* dump top of PCE stack */


		 /*******************************
		 *	   STREAM INTERFACE	*
		 *******************************/

#define PCE_OPEN_MAX	64		/* statically allocated max open */

int		pceOpen(PceObject obj, int flags);
int		pceClose(int handle);
int		pceWrite(int handle, const char *buf, int size);
int		pceRead(int handle, char *buf, int size);
long		pceSeek(int handle, long offset, int whence);
const char *	pceOsError();

					/* flags for pceOpen() (or'ed) */
#define PCE_RDONLY	0x1
#define	PCE_WRONLY	0x2
#define	PCE_RDWR	0x3		/* == PCE_RDONLY|PCE_WRONLY */
#define	PCE_APPEND	0x4
#define	PCE_TRUNC	0x8

#define PCE_SEEK_SET	0
#define PCE_SEEK_CUR	1
#define PCE_SEEK_END	2
