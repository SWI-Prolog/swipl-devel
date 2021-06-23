/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2016, University of Amsterdam
                              VU University Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide primitives for walking a term,  while protecting against cycles.
There are two scenarios: avoid walking a   sub-term twice in general and
avoid cycles. I.e. given the term A=a(1), T = t(A,A), we have

    - If walk-whole-term: walks A twice
    - If avoid double: walks A once

Next, sometimes we want to get control after processing the arguments of
a compound and sometimes we do not  care.   In  the  latter case, we can
simply jump to the last argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !AC_TERM_WALK

typedef struct aNode
{ Word		location;
  size_t	size;
} aNode;

typedef struct term_agenda
{ aNode		work;			/* current work */
  segstack	stack;
  char		first_chunk[256];
} term_agenda;


static inline void
initTermAgenda(term_agenda *a, size_t size, Word p)
{ initSegStack(&a->stack, sizeof(aNode),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.location = p;
  a->work.size = size;
}


static inline void
clearTermAgenda(term_agenda *a)
{ clearSegStack(&a->stack);
}


#define nextTermAgenda(a) LDFUNC(nextTermAgenda, a)
static inline Word
nextTermAgenda(DECL_LD term_agenda *a)
{ Word p;

  if ( a->work.size > 0 )
  { ok:
    a->work.size--;
    p = a->work.location++;
    deRef(p);

    return p;
  }

  if ( popSegStack(&a->stack, &a->work, aNode) )
    goto ok;

  return NULL;
}


static inline Word
nextTermAgendaNoDeRef(term_agenda *a)
{ Word p;

  if ( a->work.size > 0 )
  { ok:
    a->work.size--;
    p = a->work.location++;

    return p;
  }

  if ( popSegStack(&a->stack, &a->work, aNode) )
    goto ok;

  return NULL;
}




		 /*******************************
		 *	  PUSH VARIATIONS	*
		 *******************************/

static inline int
pushWorkAgenda(term_agenda *a, size_t amount, Word start)
{ if ( a->work.size > 0 )
  { if ( !pushSegStack(&a->stack, a->work, aNode) )
      return FALSE;
  }
  a->work.location = start;
  a->work.size = amount;

  return TRUE;
}

#endif /*!AC_TERM_WALK*/


#if AC_TERM_WALK

		 /*******************************
		 *	 WALK ACYCLIC TERM	*
		 *******************************/

typedef struct acNode
{ Functor	term;
  Word		location;
  size_t	size;
} acNode;

typedef struct ac_term_agenda
{ acNode	work;			/* current work */
  segstack	stack;
  char		first_chunk[64*sizeof(acNode)];
} ac_term_agenda;


static void
ac_initTermAgenda(ac_term_agenda *a, Word p)
{ initSegStack(&a->stack, sizeof(acNode),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.term     = NULL;
  a->work.location = p;
  a->work.size     = 1;
}


static void
ac_clearTermAgenda(ac_term_agenda *a)
{ do
  { if ( a->work.term )
      clear_marked((Word)&a->work.term->definition);
  } while(popSegStack(&a->stack, &a->work, acNode));
}



#define ac_nextTermAgenda(a) LDFUNC(ac_nextTermAgenda, a)
static Word
ac_nextTermAgenda(DECL_LD ac_term_agenda *a)
{ Word p;

  while ( a->work.size == 0 )
  { if ( a->work.term )
      clear_marked((Word)&a->work.term->definition);
    if ( !popSegStack(&a->stack, &a->work, acNode) )
      return NULL;
  }
  a->work.size--;

  p = a->work.location++;
  deRef(p);

  return p;
}


#define ac_pushTermAgenda(a, w, fp) LDFUNC(ac_pushTermAgenda, a, w, fp)
static int
ac_pushTermAgenda(DECL_LD ac_term_agenda *a, word w, functor_t *fp)
{ Functor term = valueTerm(w);

  if ( is_marked((Word)&term->definition) )
    return FALSE;			/* hit cycle */
  if ( !pushSegStack(&a->stack, a->work, acNode) )
    return -1;				/* no memory */
  a->work.term     = term;
  a->work.location = term->arguments;
  a->work.size     = arityFunctor(term->definition);
  *fp              = term->definition;
  set_marked((Word)&term->definition);

  return TRUE;
}

#endif /*AC_TERM_WALK*/


		 /*******************************
		 *	 POP ON ONE TERM	*
		 *******************************/

#if AC_TERM_WALK_POP

#define AC_TERM_POP(n)		((Word)(((n)<<1)|0x1))
#define IS_AC_TERM_POP(p)	(((uintptr_t)(p)&0x1) ? (uintptr_t)(p)>>1 : 0)

typedef struct aNode_P
{ Word		location;
  size_t	size;
  size_t	depth;
} aNode_P;

typedef struct term_agenda_P
{ aNode_P	work;			/* current work */
  segstack	stack;
  char		first_chunk[sizeof(segchunk)+sizeof(aNode_P)*64];
} term_agenda_P;


static void
initTermAgenda_P(term_agenda_P *a, size_t size, Word p)
{ initSegStack(&a->stack, sizeof(aNode_P),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.location = p;
  a->work.size     = size;
  a->work.depth    = 0;
}


static void
clearTermAgenda_P(term_agenda_P *a)
{ clearSegStack(&a->stack);
}


#define nextTermAgenda_P(a) LDFUNC(nextTermAgenda_P, a)
static inline Word
nextTermAgenda_P(DECL_LD term_agenda_P *a)
{ Word p;

  while ( a->work.size == 0 )
  { size_t popn;
    if ( (popn=a->work.depth) > 0 )
    { a->work.depth = 0;
      return AC_TERM_POP(popn);
    }
    if ( !popSegStack(&a->stack, &a->work, aNode_P) )
      return NULL;
  }

  a->work.size--;
  p = a->work.location++;
  deRef(p);

  return p;
}


		 /*******************************
		 *	  PUSH VARIATIONS	*
		 *******************************/

static inline int
pushWorkAgenda_P(term_agenda_P *a, size_t amount, Word start)
{ if ( a->work.size > 0 )
  { if ( !pushSegStack(&a->stack, a->work, aNode_P) )
      return FALSE;
    a->work.depth = 1;
  } else
    a->work.depth++;

  a->work.location = start;
  a->work.size     = amount;

  return TRUE;
}

#endif /*!AC_TERM_WALK_POP*/

#if AC_TERM_WALK_LR

		 /*******************************
		 *    OPERATIONS ON TWO TERMS	*
		 *******************************/

typedef struct aNodeLR
{ Word		left;			/* left term */
  Word		right;			/* right term */
  size_t	size;
} aNodeLR;

typedef struct term_agendaLR
{ aNodeLR	work;			/* current work */
  segstack	stack;
  char		first_chunk[256];
} term_agendaLR;


static void
initTermAgendaLR(term_agendaLR *a, size_t count, Word left, Word right)
{ initSegStack(&a->stack, sizeof(aNodeLR),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.left  = left;
  a->work.right = right;
  a->work.size  = count;
}


static inline void
initTermAgendaLR0(term_agendaLR *a)
{ initSegStack(&a->stack, sizeof(aNodeLR),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.size  = 0;
}


static void
clearTermAgendaLR(term_agendaLR *a)
{ clearSegStack(&a->stack);
}


static int
nextTermAgendaLR(term_agendaLR *a, Word *lp, Word *rp)
{ if ( a->work.size > 0 )
  { ok:
    a->work.size--;
    *lp = a->work.left++;
    *rp = a->work.right++;

    return TRUE;
  }

  if ( popSegStack(&a->stack, &a->work, aNodeLR) )
    goto ok;

  return FALSE;
}


static inline int
pushWorkAgendaLR(term_agendaLR *a, size_t amount, Word left, Word right)
{ if ( a->work.size > 0 )
  { if ( !pushSegStack(&a->stack, a->work, aNodeLR) )
      return FALSE;
  }
  a->work.left  = left;
  a->work.right = right;
  a->work.size  = amount;

  return TRUE;
}

#endif /*AC_TERM_WALK_LR*/

#if AC_TERM_WALK_LRD

		 /*************************************
		 * OPERATIONS ON TWO TERMS WITH DEPTH *
		 *************************************/

typedef struct aNodeLRD
{ Word		left;			/* left term */
  Word		right;			/* right term */
  size_t	size;
  size_t	depth;
} aNodeLRD;

typedef struct term_agendaLRD
{ aNodeLRD	work;			/* current work */
  segstack	stack;
  char		first_chunk[256];
} term_agendaLRD;


static void
initTermAgendaLRD(term_agendaLRD *a, size_t count, Word left, Word right)
{ initSegStack(&a->stack, sizeof(aNodeLRD),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.left  = left;
  a->work.right = right;
  a->work.size  = count;
  a->work.depth = 0;
}


static inline void
initTermAgendaLRD0(term_agendaLRD *a)
{ initSegStack(&a->stack, sizeof(aNodeLRD),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.size  = 0;
}


static void
clearTermAgendaLRD(term_agendaLRD *a)
{ clearSegStack(&a->stack);
}


static int
nextTermAgendaLRD(term_agendaLRD *a, Word *lp, Word *rp)
{ if ( a->work.size > 0 )
  { ok:
    a->work.size--;
    *lp = a->work.left++;
    *rp = a->work.right++;

    return TRUE;
  }

  if ( popSegStack(&a->stack, &a->work, aNodeLRD) )
    goto ok;

  return FALSE;
}


static inline int
pushWorkAgendaLRD(term_agendaLRD *a, size_t amount, Word left, Word right)
{ if ( a->work.size > 0 )
  { if ( !pushSegStack(&a->stack, a->work, aNodeLRD) )
      return FALSE;
  }
  a->work.left  = left;
  a->work.right = right;
  a->work.size  = amount;
  a->work.depth++;

  return TRUE;
}

#endif /*AC_TERM_WALK_LRD*/

#if AC_TERM_WALK_LRS

		 /*******************************
		 *       TWO TERMS WITH POP	*
		 *******************************/

typedef struct aNodeLRS
{ Functor	left;			/* left term */
  Functor	right;			/* right term */
  int		arg;
  int		arity;
  void	       *data;
} aNodeLRS;

typedef void (*popLRS)(Functor left, Functor right, void *data);

typedef struct term_agendaLRS
{ aNodeLRS	work;			/* current work */
  popLRS	pop;
  segstack	stack;
  char		first_chunk[sizeof(aNodeLRS)*25];
} term_agendaLRS;


static void
initTermAgendaLRS(term_agendaLRS *a,
		  Functor left, Functor right,
		  popLRS pop, void *data)
{ initSegStack(&a->stack, sizeof(aNodeLRS),
	       sizeof(a->first_chunk), a->first_chunk);
  a->pop	= pop;
  a->work.data  = data;
  a->work.left  = left;
  a->work.right = right;
  a->work.arg   = 0;
  a->work.arity = arityFunctor(left->definition);
}


static void
clearTermAgendaLRS(term_agendaLRS *a)
{ do
  { if ( a->work.arg != -1 )
      (*a->pop)(a->work.left, a->work.right, a->work.data);
  } while(popSegStack(&a->stack, &a->work, aNodeLRS));
}



#define nextTermAgendaLRS(a, lp, rp) LDFUNC(nextTermAgendaLRS, a, lp, rp)
static int
nextTermAgendaLRS(DECL_LD term_agendaLRS *a, Word *lp, Word *rp)
{ Word p;

  while ( a->work.arg == a->work.arity )
  { (*a->pop)(a->work.left, a->work.right, a->work.data);
    a->work.arg = -1;
    if ( !popSegStack(&a->stack, &a->work, aNodeLRS) )
      return FALSE;
  }

  deRef2(&a->work.left->arguments[a->work.arg], p); *lp = p;
  deRef2(&a->work.right->arguments[a->work.arg],p); *rp = p;
  a->work.arg++;

  return TRUE;
}


static int
pushWorkAgendaLRS(term_agendaLRS *a, Functor left, Functor right, void *data)
{ if ( !pushSegStack(&a->stack, a->work, aNodeLRS) )
    return FALSE;

  a->work.data  = data;
  a->work.left  = left;
  a->work.right = right;
  a->work.arg   = 0;
  a->work.arity = arityFunctor(left->definition);

  return TRUE;
}

#endif /*AC_TERM_WALK_LRS*/

