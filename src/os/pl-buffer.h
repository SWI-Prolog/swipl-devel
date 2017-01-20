/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2012, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef BUFFER_H_INCLUDED
#define BUFFER_H_INCLUDED

#define STATIC_BUFFER_SIZE (512)

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
} MAY_ALIAS buffer, *Buffer;

int	growBuffer(Buffer b, size_t minfree);

#define addBuffer(b, obj, type) \
	do \
	{ if ( (b)->top + sizeof(type) > (b)->max ) \
	  { if ( !growBuffer((Buffer)b, sizeof(type)) ) \
	      outOfCore(); \
	  } \
	  *((type *)(b)->top) = obj; \
          (b)->top += sizeof(type); \
	} while(0)

#define addMultipleBuffer(b, ptr, times, type) \
	do \
	{ size_t _tms = (times); \
          size_t _len = _tms * sizeof(type); \
          type *_d, *_s = (type *)ptr; \
	  if ( (b)->top + _len > (b)->max ) \
	  { if ( !growBuffer((Buffer)b, _len) ) \
	      outOfCore(); \
	  } \
          _d = (type *)(b)->top; \
          while ( _tms-- ) \
	    *_d++ = *_s++; \
	  (b)->top = (char *)_d; \
	} while(0)

#define allocFromBuffer(b, bytes) \
	f__allocFromBuffer((Buffer)(b), (bytes))

static inline void*
f__allocFromBuffer(Buffer b, size_t bytes)
{ if ( b->top + bytes <= b->max ||
       growBuffer(b, bytes) )
  { void *top = b->top;

    b->top += bytes;

    return top;
  }

  return NULL;
}


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
#define isEmptyBuffer(b)         ((b)->top == (b)->base)
#define popBuffer(b,type) \
	((b)->top -= sizeof(type), *(type*)(b)->top)

#define discardBuffer(b) \
	do \
	{ if ( (b)->base && (b)->base != (b)->static_buffer ) \
	  { free((b)->base); \
	    (b)->base = (b)->static_buffer; \
	  } \
	} while(0)


		 /*******************************
		 *	    FUNCTIONS		*
		 *******************************/

COMMON(Buffer)		findBuffer(int flags);
COMMON(int)		unfindBuffer(int flags);
COMMON(char *)		buffer_string(const char *s, int flags);

#endif /*BUFFER_H_INCLUDED*/
