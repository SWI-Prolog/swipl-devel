/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2020, University of Amsterdam
			      CWI, Amsterdam
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

#define STATIC_BUFFER_SIZE		(100)
#define SMALL_STATIC_BUFFER_SIZE	(512)
#define BUFFER_DISCARD_ABOVE	       (4096)

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
  char		static_buffer[SMALL_STATIC_BUFFER_SIZE];
} tmp_small_buffer;

typedef struct
{ char *	base;			/* allocated base */
  char *	top;			/* pointer to top */
  char *	max;			/* current location */
  char		static_buffer[sizeof(char *)];
} MAY_ALIAS buffer, *Buffer;

typedef struct string_buffer
{ word		frame;			/* frame reference */
  tmp_small_buffer buf;			/* actual buffer */
} string_buffer, *StringBuffer;

#define MAX_LG_STACKED_STRINGS 20

typedef struct string_stack
{ unsigned int   top;			/* Current # stacked strings */
  unsigned int   allocated;		/* Max depth reached */
  unsigned int   tripwire;		/* Warn on this number of buffers */
  string_buffer *buffers[MAX_LG_STACKED_STRINGS];
} string_stack;

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
#define emptyBuffer(b, sz)	 emptyBuffer_((Buffer)(b), sz, \
					      sizeof((b)->static_buffer))
#define isEmptyBuffer(b)         ((b)->top == (b)->base)
#define popBuffer(b,type) \
	((b)->top -= sizeof(type), *(type*)(b)->top)
#define popBufferP(b,type) \
	((b)->top -= sizeof(type), (type*)(b)->top)
#define discardBuffer(b)	 discardBuffer_((Buffer)(b))

static inline void
discardBuffer_(Buffer b)
{ if ( b->base && b->base != b->static_buffer )
    tmp_free(b->base);
}

static inline void
emptyBuffer_(Buffer b, size_t discardsize, size_t emptysize)
{ if ( b->max - b->base < discardsize )
  { b->top = b->base;
  } else
  { discardBuffer(b);
    b->base = b->top = b->static_buffer,
    b->max  = b->base + emptysize;
  }
}

		 /*******************************
		 *	    FUNCTIONS		*
		 *******************************/

#if USE_LD_MACROS
#define	PL_mark_string_buffers(mark)			LDFUNC(PL_mark_string_buffers, mark)
#define	PL_release_string_buffers_from_mark(mark)	LDFUNC(PL_release_string_buffers_from_mark, mark)
#define	release_string_buffers_from_frame(fr)		LDFUNC(release_string_buffers_from_frame, fr)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

Buffer		findBuffer(int flags);
int		unfindBuffer(Buffer b, int flags);
char *		buffer_string(const char *s, int flags);
void		PL_mark_string_buffers(buf_mark_t *mark);
void		PL_release_string_buffers_from_mark(buf_mark_t mark);
void		release_string_buffers_from_frame(LocalFrame fr);
void		discardStringStack(string_stack *stack);

#undef LDFUNC_DECLARATIONS

#endif /*BUFFER_H_INCLUDED*/
