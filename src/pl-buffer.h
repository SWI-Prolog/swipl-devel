/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef BUFFER_H_INCLUDED
#define BUFFER_H_INCLUDED

#define STATIC_BUFFER_SIZE (512)
#define BUFFER_USES_MALLOC 1

typedef struct
{ char *	base;			/* allocated base */
  char *	top;			/* pointer to top */
  char *	max;			/* current location */
  char		static_buffer[STATIC_BUFFER_SIZE];
} tmp_buffer, *TmpBuffer;

typedef struct
{ char *	base;			/* allocated base */
  char *	top;			/* pointer to top */
  char *	max;			/* current location */
  char		static_buffer[sizeof(char *)];
} buffer, *Buffer;

void	growBuffer(Buffer, long);

#define addBuffer(b, obj, type) \
	do \
	{ if ( (b)->top + sizeof(type) > (b)->max ) \
	    growBuffer((Buffer)b, sizeof(type)); \
 	  *((type *)(b)->top) = obj; \
          (b)->top += sizeof(type); \
	} while(0)
  
#define addUnalignedBuffer(b, obj, type) \
	do \
	{ if ( (b)->top + sizeof(type) > (b)->max ) \
	    growBuffer((Buffer)b, sizeof(type)); \
	  memcpy((b)->top, (char *)&obj, sizeof(type)); \
          (b)->top += sizeof(type); \
	} while(0)
  
#define addMultipleBuffer(b, ptr, times, type) \
	do \
	{ int _len = sizeof(type) * (times); \
	  if ( (b)->top + _len > (b)->max ) \
	    growBuffer((Buffer)b, _len); \
	  memcpy((b)->top, ptr, _len); \
          (b)->top += _len; \
	} while(0)
  
#define baseBuffer(b, type)	 ((type *) (b)->base)
#define topBuffer(b, type)       ((type *) (b)->top)
#define inBuffer(b, addr)        ((char *) (addr) >= (b)->base && \
				  (char *) (addr)  < (b)->top)
#define fetchBuffer(b, i, type)	 (baseBuffer(b, type)[i])

#define seekBuffer(b, cnt, type) ((b)->top = sizeof(type) * (cnt) + (b)->base)
#define sizeOfBuffer(b)          ((b)->top - (b)->base)
#define freeSpaceBuffer(b)	 ((b)->max - (b)->top)
#define entriesBuffer(b, type)   (sizeOfBuffer(b) / sizeof(type))
#define initBuffer(b)            ((b)->base = (b)->top = (b)->static_buffer, \
				  (b)->max = (b)->base + \
				  sizeof((b)->static_buffer))
#define emptyBuffer(b)           ((b)->top  = (b)->base)

#ifdef BUFFER_USES_MALLOC
#define discardBuffer(b) \
	do \
	{ if ( (b)->base != (b)->static_buffer ) \
	    free((b)->base); \
	} while(0)
#else
#define discardBuffer(b) \
	do \
	{ if ( (b)->base != (b)->static_buffer ) \
	    freeHeap((b)->base, (b)->max - (b)->base); \
	} while(0)
#endif

#endif /*BUFFER_H_INCLUDED*/
