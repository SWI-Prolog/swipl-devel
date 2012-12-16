/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
