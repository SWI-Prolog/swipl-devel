/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "dtd.h"
#include "model.h"

#define MAX_VISITED 256
#define MAX_ALLOWED 64

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module implements a finite state  engine for validating the content
model of elements. A state machine  is   the  only feasible approach for
realising an event-driven SGML parser.

The public functions are:

dtd_state *new_dtd_state(void)
    Create an anonymous new state.  Normally an element creates two of
    these for it ->initial_state and ->final_state attributes.

dtd_state *make_state_engine(dtd_element *e)
    Associate a state engine to this element and return the initial
    state of the engine.  If the element has an engine, simply return
    the initial state.

dtd_state *make_dtd_transition(dtd_state *here, dtd_element *e)
    Given the current state, see whether we can accept e and return
    the resulting state.  If no transition is possible return NULL.

int same_state(dtd_state *final, dtd_state *here)
    See whether two states are the same, or the final state can be
    reached only traversing equivalence links.

The A&B&... model

Models of the type a&b&c are hard   to translate, as the resulting state
machine is of size order N! In practice   only  a little of this will be
used however and we `fix' this problem using a `lazy state-engine', that
expands to the next level  only  after   reaching  some  level.  See the
function state_transitions(). The design takes more lazy generation into
consideration.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct _state_transition
{ dtd_element	     *element;		/* element on transition */
  dtd_state	     *state;		/* state to go to */
  struct _state_transition *next;	/* next possible transition */
} transition;

typedef struct _dtd_model_list		/* list (set) of models */
{ dtd_model *model;
  struct _dtd_model_list *next;
} dtd_model_list;

typedef enum
{ EX_AND				/* expand (a&b&...) */
} expand_type;

typedef struct _state_expander
{ dtd_state	       *target;		/* Target state to expand to */
  expand_type		type;		/* EX_* */
  union
  { struct
    { dtd_model_list *set;		/* Models we should still see */
    } and;				/* Expand (a&b&...) */
  } kind;
} expander;

typedef struct _visited
{ int	size;				/* set-size */
  dtd_state *states[MAX_VISITED];	/* The set */
} visited;


static void	translate_model(dtd_model *m, dtd_state *from, dtd_state *to);
static transition *state_transitions(dtd_state *state);

static int
visit(dtd_state *state, visited *visited)
{ int i;

  for(i=0; i<visited->size; i++)
  { if ( visited->states[i] == state )
      return FALSE;
  }
      
  if ( visited->size >= MAX_VISITED )
  { fprintf(stderr, "Reached MAX_VISITED!\n");
    return FALSE;
  }

  visited->states[visited->size++] = state;

  return TRUE;
}


static dtd_state *
do_make_dtd_transition(dtd_state *here, dtd_element *e, visited *visited)
{ transition *tset = state_transitions(here);
  transition *t;

  for(t=tset; t; t=t->next)
  { if ( t->element == e )
      return t->state;
  }

  for(t=tset; t; t=t->next)
  { if ( t->element == NULL && visit(t->state, visited) )
    { dtd_state *new;

      if ( (new=do_make_dtd_transition(t->state, e, visited)) )
	return new;
    }
  }

  return NULL;
}


dtd_state *
make_dtd_transition(dtd_state *here, dtd_element *e)
{ visited visited;
  visited.size = 0;

  if ( !here )				/* from nowhere to nowhere */
    return NULL;

  return do_make_dtd_transition(here, e, &visited);
}


static int
find_same_state(dtd_state *final, dtd_state *here, visited *visited)
{ transition *t;

  if ( final == here )
    return TRUE;

  for(t=state_transitions(here); t; t=t->next)
  { if ( t->element == NULL && visit(t->state, visited) )
    { if ( find_same_state(final, t->state, visited) )
	return TRUE;
    }
  }

  return FALSE;
}


int
same_state(dtd_state *final, dtd_state *here)
{ visited visited;
  visited.size = 0;

  return find_same_state(final, here, &visited);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
state_allows_for(dtd_state *state, dtd_element **allow, int *n)
    See what elements are allowed if we are in this state.  This is
    currently not used, but might prove handly for error messages or
    syntax-directed editors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
do_state_allows_for(dtd_state *here, dtd_element **allow, int *n,
		    visited *visited)
{ transition *t;

  for(t=state_transitions(here); t; t=t->next)
  { int i;

    if ( t->element == NULL )
    { if ( visit(t->state, visited) )
	do_state_allows_for(t->state, allow, n, visited);
    } else
    { for(i=0; i<*n; i++)
      { if ( allow[i] == t->element )
	  goto next;
      }
      allow[(*n)++] = t->element;
    }
  next:
    ;
  }
}


void
state_allows_for(dtd_state *state, dtd_element **allow, int *n)
{ visited visited;
  visited.size = 0;

  *n = 0;
  if ( state )
    do_state_allows_for(state, allow, n, &visited);
}


static int
do_find_omitted_path(dtd_state *state, dtd_element *e,
		     dtd_element **path, int *pl,
		     visited *visited)
{ transition *tset = state_transitions(state);
  transition *t;
  int pathlen = *pl;

  for(t=tset; t; t=t->next)
  { if ( t->element == e )
      return TRUE;

    if ( t->element &&
	 t->element != CDATA_ELEMENT &&
	 t->element->structure &&
	 t->element->structure->omit_open &&
	 visit(t->state, visited) )
    { dtd_state *initial = make_state_engine(t->element);

      path[pathlen] = t->element;
      *pl = pathlen+1;
      if ( do_find_omitted_path(initial, e, path, pl, visited) )
	return TRUE;
      *pl = pathlen;
    }
  }

  for(t=tset; t; t=t->next)
  { if ( !t->element &&
	 visit(t->state, visited) )
    { if ( do_find_omitted_path(t->state, e, path, pl, visited) )
	return TRUE;
    }
  }

  return FALSE;
}


int 
find_omitted_path(dtd_state *state, dtd_element *e, dtd_element **path)
{ int pl = 0;
  visited visited;
  visited.size = 0;

  if ( state && do_find_omitted_path(state, e, path, &pl, &visited) )
    return pl;

  return -1;
}


dtd_state *
new_dtd_state()
{ dtd_state *s = sgml_calloc(1, sizeof(*s));

  return s;
}


static void
link(dtd_state *from, dtd_state *to, dtd_element *e)
{ transition *t = sgml_calloc(1, sizeof(*t));

  t->state = to;
  t->element = e;
  t->next = from->transitions;
  from->transitions = t;
}


		 /*******************************
		 *	      EXPANSION		*
		 *******************************/

static void
add_model_list(dtd_model_list **list, dtd_model *m)
{ dtd_model_list *l = sgml_calloc(1, sizeof(*l));

  l->model = m;

  for( ; *list; list = &(*list)->next)
    ;
  *list = l;
}


static transition *
state_transitions(dtd_state *state)
{ if ( !state->transitions && state->expander )
  { expander *ex = state->expander;
    
    switch(ex->type)
    { case EX_AND:
      { dtd_model_list *left = ex->kind.and.set;

	if ( !left )			/* empty AND (should not happen) */
	{ link(state, ex->target, NULL); 
	} else if ( !left->next )	/* only one left */
	{ translate_model(left->model, state, ex->target);
	} else
	{ for( ; left; left = left->next )
	  { dtd_state *tmp = new_dtd_state();
	    expander *nex = sgml_calloc(1, sizeof(*nex));
	    dtd_model_list *l;

	    translate_model(left->model, state, tmp);
	    tmp->expander = nex;
	    nex->target = ex->target;
	    nex->type = EX_AND;
	    for(l=ex->kind.and.set; l; l=l->next)
	    { if ( l != left )
		add_model_list(&nex->kind.and.set, l->model);
	    }
	  }
	}
      }
    }
  }

  return state->transitions;
}


		 /*******************************
		 *	   TRANSLATION		*
		 *******************************/


static void
translate_one(dtd_model *m, dtd_state *from, dtd_state *to)
{ switch(m->type)
  { case MT_ELEMENT:
    { dtd_element *e = m->content.element;

      link(from, to, e);
      return;
    }
    case MT_SEQ:			/* a,b,... */
    { dtd_model *sub;

      for( sub = m->content.group; sub->next; sub = sub->next )
      { dtd_state *tmp = new_dtd_state();
	translate_model(sub, from, tmp);
	from = tmp;
      }
      translate_model(sub, from, to);
      return;
    }
    case MT_AND:			/* a&b&... */
    { expander *ex = sgml_calloc(1, sizeof(*ex));
      dtd_model *sub;

      ex->target = to;
      ex->type   = EX_AND;
      
      for( sub = m->content.group; sub; sub = sub->next )
	add_model_list(&ex->kind.and.set, sub);

      from->expander = ex;
      return;
    }
    case MT_OR:				/* a|b|... */
    { dtd_model *sub;

      for( sub = m->content.group; sub; sub = sub->next )
	translate_model(sub, from, to);
      return;
    }
    case MT_PCDATA:
    case MT_UNDEF:
      assert(0);
  }

}


static void
translate_model(dtd_model *m, dtd_state *from, dtd_state *to)
{ if ( m->type == MT_PCDATA )
  { link(from, from, CDATA_ELEMENT);
    link(from, to, NULL);
    return;
  }

  switch(m->cardinality)
  { case MC_OPT:			/* ? */
      link(from, to, NULL);
    /*FALLTHROUGH*/
    case MC_ONE:
      translate_one(m, from, to);
      return;
    case MC_REP:			/* * */
      translate_one(m, from, from);
      link(from, to, NULL);
      return;
    case MC_PLUS:			/* + */
      translate_one(m, from, to);
      translate_one(m, to, to);
      return;
  }
}


dtd_state *
make_state_engine(dtd_element *e)
{ if ( e->structure )
  { dtd_edef *def = e->structure;

    if ( !def->initial_state )
    { if ( def->content )
      { def->initial_state = new_dtd_state();
	def->final_state   = new_dtd_state();
    
	translate_model(def->content, def->initial_state, def->final_state);
      } else if ( def->type == C_CDATA || def->type == C_RCDATA )
      { def->initial_state = new_dtd_state();
	def->final_state   = new_dtd_state();

	link(def->initial_state, def->initial_state, CDATA_ELEMENT);
	link(def->initial_state, def->final_state, NULL);
      } else
	return NULL;
    }

    return def->initial_state;
  }
  
  return NULL;
}


		 /*******************************
		 *	       FREE		*
		 *******************************/

static void do_free_state_engine(dtd_state *state, visited *visited);

static void
free_model_list(dtd_model_list *l)
{ dtd_model_list *next;

  for( ; l; l=next)
  { next = l->next;

    sgml_free(l);
  }
}


static void
free_expander(expander *e, visited *visited)
{ if ( visit(e->target, visited) )
    do_free_state_engine(e->target, visited);

  switch(e->type)
  { case EX_AND:
      free_model_list(e->kind.and.set);
    default:
      ;
  }

  sgml_free(e);
}


static void
do_free_state_engine(dtd_state *state, visited *visited)
{ transition *t, *next;
  
  for(t=state->transitions; t; t=next)
  { next = t->next;

    if ( visit(t->state, visited) )
      do_free_state_engine(t->state, visited);

    sgml_free(t);
  }

  if ( state->expander )
    free_expander(state->expander, visited);

  sgml_free(state);
}


void
free_state_engine(dtd_state *state)
{ if ( state )
  { visited visited;
    visited.size = 0;

    visit(state, &visited);
    do_free_state_engine(state, &visited);
  }
}



