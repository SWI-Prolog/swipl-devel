/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
  size_t 	size;
} aNode;

typedef struct term_agenda
{ aNode		work;			/* current work */
  segstack	stack;
  char		first_chunk[256];
} term_agenda;


static void
initTermAgenda(term_agenda *a, Word p)
{ initSegStack(&a->stack, sizeof(aNode),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.location = p;
  a->work.size = 1;
}


static void
clearTermAgenda(term_agenda *a)
{ clearSegStack(&a->stack);
}

#define nextTermAgenda(a) \
	nextTermAgenda__LD(a PASS_LD)

static Word
nextTermAgenda__LD(term_agenda *a ARG_LD)
{ Word p;

  while ( a->work.size == 0 )
  { if ( !popSegStack(&a->stack, &a->work) )
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

static int
pushWorkAgenda(term_agenda *a, size_t amount, Word start)
{ if ( a->work.size > 0 )
  { if ( !pushSegStack(&a->stack, &a->work) )
      return FALSE;
  }
  a->work.location = start;
  a->work.size = amount;

  return TRUE;
}


#ifdef HAVE_VISITED
/* pushTermAgendaIfNotVisited(agenda, term)

Pushes a term if it has  not  yet   been  visited.  This version of this
function cooperates with the visited functions   and  ensure that common
subterms are visited only once.
*/

#define pushTermAgendaIfNotVisited(a, w) \
	pushTermAgendaIfNotVisited__LD(a, w PASS_LD)

static int
pushTermAgendaIfNotVisited__LD(term_agenda *a, word w ARG_LD)
{ Functor f;

  SECURE(assert(isTerm(w)));
  f = valueTerm(w);
  if ( visited(f PASS_LD) )
    return FALSE;

  return pushWorkAgenda(a, arityFunctor(f->definition), f->arguments);
}
#endif /*HAVE_VISITED*/

#endif /*!AC_TERM_WALK*/


#if AC_TERM_WALK

		 /*******************************
		 *	 WALK ACYCLIC TERM	*
		 *******************************/

typedef struct acNode
{ Functor	term;
  Word		location;
  size_t 	size;
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
  } while(popSegStack(&a->stack, &a->work));
}


#define ac_nextTermAgenda(a) \
	ac_nextTermAgenda__LD(a PASS_LD)

static Word
ac_nextTermAgenda__LD(ac_term_agenda *a ARG_LD)
{ Word p;

  while ( a->work.size == 0 )
  { if ( a->work.term )
      clear_marked((Word)&a->work.term->definition);
    if ( !popSegStack(&a->stack, &a->work) )
      return NULL;
  }
  a->work.size--;

  p = a->work.location++;
  deRef(p);

  return p;
}

#define ac_pushTermAgenda(a, w, fp) \
	ac_pushTermAgenda__LD(a, w, fp PASS_LD)

static int
ac_pushTermAgenda__LD(ac_term_agenda *a, word w, functor_t *fp ARG_LD)
{ Functor term = valueTerm(w);

  if ( is_marked((Word)&term->definition) )
    return FALSE;			/* hit cycle */
  if ( !pushSegStack(&a->stack, &a->work) )
    return -1;				/* no memory */
  a->work.term     = term;
  a->work.location = term->arguments;
  a->work.size     = arityFunctor(term->definition);
  *fp              = term->definition;
  set_marked((Word)&term->definition);

  return TRUE;
}

#endif /*AC_TERM_WALK*/


#if 0

		 /*******************************
		 *    OPERATIONS ON TWO TERMS	*
		 *******************************/

typedef struct aNodeLR
{ Word		left;			/* left term */
  Word		right;			/* right term */
  size_t 	size;
} aNode;

typedef struct term_agendaLR
{ aNodeLR	work;			/* current work */
  segstack	stack;
  char		first_chunk[256];
} term_agendaLR;


static void
initTermAgendaLR(term_agendaLR *a, Word left, Word right)
{ initSegStack(&a->stack, sizeof(aNode),
	       sizeof(a->first_chunk), a->first_chunk);
  a->work.left  = left;
  a->work.right = right;
  a->work.size  = 1;
}


static void
clearTermAgendaLR(term_agendaLR *a)
{ clearSegStack(&a->stack);
}


static int
nextTermAgendaLR(term_agendaLR *a, Word *lp, Word *rp)
{ Word p;

  while ( a->work.size == 0 )
  { if ( !popSegStack(&a->stack, &a->work) )
      return FALSE;
  }
  a->work.size--;

  *lp = a->work.left++;
  *rp = a->work.right++;

  return TRUE;
}


static int
pushWorkAgendaLR(term_agendaLR *a, size_t amount, Word left, Word right)
{ if ( a->work.size > 0 )
  { if ( !pushSegStack(&a->stack, &a->work) )
      return FALSE;
  }
  a->work.left  = left;
  a->work.right = right;
  a->work.size  = amount;

  return TRUE;
}

#endif
