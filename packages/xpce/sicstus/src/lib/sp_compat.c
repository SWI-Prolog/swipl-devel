/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

#include "../swi.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Predicates required by XPCE that are built-ins or part of the SWI-Prolog
library. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef EOS
#define EOS '\0'
#endif
#define streq(s, q)	(strcmp((s), (q)) == 0)

		 /*******************************
		 *	    BUFFER STUFF	*
		 *******************************/

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
} buffer, *Buffer;

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
	{ int len = sizeof(type) * (times); \
	  if ( (b)->top + len > (b)->max ) \
	    growBuffer((Buffer)b, len); \
	  memcpy((b)->top, ptr, len); \
          (b)->top += len; \
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

#define discardBuffer(b) \
	do \
	{ if ( (b)->base != (b)->static_buffer ) \
	    SP_free((b)->base); \
	} while(0)

static void
growBuffer(Buffer b, long int minfree)
{ long osz = b->max - b->base, sz = osz;
  long top = b->top - b->base;

  while( top + minfree > sz )
    sz *= 2;

  if ( b->base != b->static_buffer )
  {
    b->base = SP_realloc(b->base, sz);
    if ( !b->base )
    { outofcore:
      PL_warning("Out of memory");
      PL_action(PL_ACTION_HALT);
    }
  } else
  { char *old = b->base;
    b->base = SP_malloc(sz);
    if ( !b->base )
      goto outofcore;
    memcpy(b->base, old, osz);
  }

  b->top = b->base + top;
  b->max = b->base + sz;
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/


foreign_t
pl_concat(term_t a1, term_t a2, term_t a3)
{ char *s1 = NULL, *s2 = NULL, *s3 = NULL;
  long l1, l2, l3;
  char *tmp;

  PL_get_chars(a1, &s1, CVT_ATOMIC|BUF_RING);
  PL_get_chars(a2, &s2, CVT_ATOMIC|BUF_RING);
  PL_get_chars(a3, &s3, CVT_ATOMIC|BUF_RING);

  if (s1 && s2)
  { l1 = strlen(s1);
    l2 = strlen(s2);
    tmp = alloca(l1 + l2 + 1);
    strcpy(tmp, s1);
    strcpy(tmp+l1, s2);
    return PL_unify_atom_chars(a3, tmp);
  }

  if (!s3)
    return PL_warning("concat/3: instantiation fault");

  if (s1)
  { int l1 = strlen(s1);

    if ( strncmp(s1, s3, l1) == 0 )
      return PL_unify_atom_chars(a2, s3+l1);
    return FALSE;
  }

  if (s2)
  { int ld;
    char *q;

    l2 = strlen(s2);
    l3 = strlen(s3);
    ld = l3 - l2;
    if (l2 > l3 || !streq(s3+ld, s2) )
      return FALSE;
    q = alloca(ld+1);
    strncpy(q, s3, ld);
    q[ld] = EOS;
    return PL_unify_atom_chars(a1, q);
  }

  return PL_warning("concat/3: instantiation fault");
}


foreign_t
pl_concat_atom3(term_t list, term_t sep, term_t atom)
{ char *s;
  term_t l = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  int first = TRUE;
  char *sp;
  int splen;
  tmp_buffer b;
  
  if ( sep )
  { if ( !PL_get_chars(sep, &sp, CVT_ATOMIC|BUF_RING) )
      return PL_warning("concat_atom/3: illegal separator");
    splen = strlen(sp);
  } else
  { sp = NULL;
    splen = 0;
  }

  initBuffer(&b);
  while( PL_get_list(l, head, l) &&
	 PL_get_chars(head, &s, CVT_ATOMIC) )
  { if ( first )
      first = FALSE;
    else if ( splen )
      addMultipleBuffer(&b, sp, splen, char);

    addMultipleBuffer(&b, s, strlen(s), char);
  }

  if ( PL_get_nil(l) )
  { atom_t a;

    addBuffer(&b, EOS, char);
    a = PL_new_atom(baseBuffer(&b, char));
    discardBuffer(&b);

    return PL_unify_atom(atom, a);
  }

  discardBuffer(&b);
  return PL_warning("concat_atom/2: instantiation fault");
}


foreign_t
pl_concat_atom(term_t list, term_t atom)
{ return pl_concat_atom3(list, 0, atom);
}


int
pl_atom_length(atom_t a)
{ return SP_atom_length(a);
}


		 /*******************************
		 *     WITH-INPUT-FROM-CHARS	*
		 *******************************/

typedef struct _open_chars
{ char *chars;				/* character buffer */
  int index;				/* current insertion point */
  int size;
  atom_t atom;				/* atom we are reading from */
  int at_eof;				/* at end-of-file */
} open_chars, *OpenChars;


static int
ch_getc(void *arg)
{ OpenChars buf = arg;

  if ( buf->index < buf->size )
    return buf->chars[buf->index++] & 0xff;
  if ( buf->index == buf->size )
  { buf->index++;
    if ( buf->chars[buf->size-1] == '.' )
    { buf->at_eof = 1;
      return ' ';
    } else
      return '.';
  }
  if ( buf->index == buf->size+1 )
  { buf->at_eof = 1;
    return ' ';
  }
  return -1;
}


static int
ch_eof(void *arg)
{ OpenChars buf = arg;

  return buf->at_eof;
}


static int
ch_close(void *arg)
{ OpenChars buf = arg;

  SP_unregister_atom(buf->atom);
  SP_free(buf);
  return 0;
}


void
pl_open_atom(atom_t a, SP_stream **stream)
{ OpenChars buf = SP_malloc(sizeof(open_chars));

  SP_register_atom(a);
  SP_make_stream(buf, ch_getc, NULL, NULL, ch_eof, NULL, ch_close, stream);
  buf->atom = a;
  buf->index = 0;
  buf->chars = SP_string_from_atom(a);
  buf->size = SP_atom_length(a);
  buf->at_eof = 0;
}





